package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.ShortBinaryOperator;
import com.landawn.abacus.util.function.ShortConsumer;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortToIntFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ShortStreamImpl extends ShortStream {
    private final short[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    ShortStreamImpl(short[] values) {
        this(values, null);
    }

    ShortStreamImpl(short[] values, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ShortStreamImpl(short[] values, boolean sorted, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    ShortStreamImpl(short[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ShortStreamImpl(short[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    ShortStreamImpl(short[] values, int fromIndex, int toIndex, boolean sorted, Collection<Runnable> closeHandlers) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > values.length) {
            throw new IllegalArgumentException("Invalid fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ")");
        }

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate) {
        return filter(predicate, Integer.MAX_VALUE);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate, final int max) {
        return new ShortStreamImpl(N.filter(elements, fromIndex, toIndex, predicate, max), sorted, closeHandlers);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate) {
        return takeWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate, final int max) {
        final ShortList list = ShortList.of(new short[N.min(9, max, (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new ShortStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate) {
        return dropWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate, final int max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final ShortList list = ShortList.of(new short[N.min(9, max, (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new ShortStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public ShortStream map(ShortUnaryOperator mapper) {
        final short[] a = new short[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsShort(elements[i]);
        }

        return new ShortStreamImpl(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(ShortToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final ShortFunction<? extends U> mapper) {
        //        final Object[] a = new Object[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.apply(elements[i]);
        //        }
        //
        //        return new ArrayStream<U>((U[]) a, closeHandlers);

        return new IteratorStream<U>(new Iterator<U>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public U next() {
                return mapper.apply(elements[cursor++]);
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMap(ShortFunction<? extends ShortStream> mapper) {
        final List<short[]> listOfArray = new ArrayList<short[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final short[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final short[] arrayOfAll = new short[lengthOfAll];
        int from = 0;
        for (short[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ShortStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(ShortFunction<? extends IntStream> mapper) {
        final List<int[]> listOfArray = new ArrayList<int[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final int[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final int[] arrayOfAll = new int[lengthOfAll];
        int from = 0;
        for (int[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new IntStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ShortFunction<? extends Stream<T>> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //        int lengthOfAll = 0;
        //
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final Object[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final Object[] arrayOfAll = new Object[lengthOfAll];
        //        int from = 0;
        //
        //        for (Object[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayStream<T>((T[]) arrayOfAll, closeHandlers);

        return new IteratorStream<T>(new Iterator<T>() {
            private int cursor = fromIndex;
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                return cur.next();
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }

        }, closeHandlers);
    }

    @Override
    public ShortStream distinct() {
        return new ShortStreamImpl(N.removeDuplicates(elements, fromIndex, toIndex, sorted), sorted, closeHandlers);
    }

    @Override
    public ShortStream sorted() {
        if (sorted) {
            return new ShortStreamImpl(elements, fromIndex, toIndex, sorted, closeHandlers);
        }

        final short[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ShortStreamImpl(a, true, closeHandlers);
    }

    @Override
    public ShortStream peek(ShortConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new ShortStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public ShortStream limit(long maxSize) {
        if (maxSize >= toIndex - fromIndex) {
            return new ShortStreamImpl(elements, fromIndex, toIndex, sorted, closeHandlers);
        } else {
            return new ShortStreamImpl(elements, fromIndex, (int) (fromIndex + maxSize), sorted, closeHandlers);
        }
    }

    @Override
    public ShortStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new ShortStreamImpl(N.EMPTY_SHORT_ARRAY, sorted, closeHandlers);
        } else {
            return new ShortStreamImpl(elements, (int) (fromIndex + n), toIndex, sorted, closeHandlers);
        }
    }

    @Override
    public void forEach(ShortConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public short[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public ShortList toShortList() {
        return ShortList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public short reduce(short identity, ShortBinaryOperator op) {
        short result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsShort(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalShort reduce(ShortBinaryOperator op) {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        short result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsShort(result, elements[i]);
        }

        return OptionalShort.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public long sum() {
        return N.sum(elements, fromIndex, toIndex).longValue();
    }

    @Override
    public OptionalShort min() {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        return OptionalShort.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalShort max() {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        return OptionalShort.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public OptionalDouble average() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.avg(elements, fromIndex, toIndex).doubleValue());
    }

    @Override
    public boolean anyMatch(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalShort findFirst() {
        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    }

    @Override
    public OptionalShort findAny() {
        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    }

    @Override
    public IntStream asIntStream() {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = elements[i];
        }

        return new IntStreamImpl(a, sorted, closeHandlers);
    }

    @Override
    public Stream<Short> boxed() {
        return new ArrayStream<Short>(Array.box(elements, fromIndex, toIndex), sorted, null, closeHandlers);
    }

    @Override
    public Iterator<Short> iterator() {
        return new ShortIterator(elements, fromIndex, toIndex);
    }

    @Override
    public ShortStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ShortStreamImpl(elements, fromIndex, toIndex, closeHandlerList);
    }

    @Override
    public void close() {
        if (N.notNullOrEmpty(closeHandlers)) {
            RuntimeException ex = null;

            for (Runnable closeHandler : closeHandlers) {
                try {
                    closeHandler.run();
                } catch (RuntimeException e) {
                    if (ex == null) {
                        ex = e;
                    } else {
                        ex.addSuppressed(e);
                    }
                }
            }

            if (ex != null) {
                throw ex;
            }
        }
    }

    static class ShortIterator extends ImmutableIterator<Short> {
        private final short[] values;
        private final int toIndex;
        private int cursor;

        ShortIterator(short[] array, int fromIndex, int toIndex) {
            this.values = array;
            this.toIndex = toIndex;
            this.cursor = fromIndex;
        }

        @Override
        public boolean hasNext() {
            return cursor < toIndex;
        }

        @Override
        public Short next() {
            return values[cursor++];
        }
    }
}
