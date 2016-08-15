package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.FloatBinaryOperator;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.FloatToDoubleFunction;
import com.landawn.abacus.util.function.FloatToIntFunction;
import com.landawn.abacus.util.function.FloatToLongFunction;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.ObjFloatConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class FloatStreamImpl extends FloatStream {
    private final float[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    FloatStreamImpl(float[] values) {
        this(values, null);
    }

    FloatStreamImpl(float[] values, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    FloatStreamImpl(float[] values, boolean sorted, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    FloatStreamImpl(float[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    FloatStreamImpl(float[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    FloatStreamImpl(float[] values, int fromIndex, int toIndex, boolean sorted, Collection<Runnable> closeHandlers) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > values.length) {
            throw new IllegalArgumentException("fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ") is invalid");
        }

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public FloatStream filter(FloatPredicate predicate) {
        return filter(predicate, Integer.MAX_VALUE);
    }

    @Override
    public FloatStream filter(final FloatPredicate predicate, final int max) {
        return new FloatStreamImpl(N.filter(elements, fromIndex, toIndex, predicate, max), sorted, closeHandlers);
    }

    @Override
    public FloatStream takeWhile(FloatPredicate predicate) {
        return takeWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public FloatStream takeWhile(FloatPredicate predicate, int max) {
        final FloatList list = FloatList.of(new float[N.min(9, max, (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new FloatStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public FloatStream dropWhile(FloatPredicate predicate) {
        return dropWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public FloatStream dropWhile(FloatPredicate predicate, int max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final FloatList list = FloatList.of(new float[N.min(9, max, (toIndex - index))], 0);
        int cnt = 0;
        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new FloatStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public FloatStream map(FloatUnaryOperator mapper) {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsFloat(elements[i]);
        }

        return new FloatStreamImpl(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(FloatToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public LongStream mapToLong(FloatToLongFunction mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(elements[i]);
        }

        return new LongStreamImpl(a, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(FloatToDoubleFunction mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(elements[i]);
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final FloatFunction<? extends U> mapper) {
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
    public FloatStream flatMap(FloatFunction<? extends FloatStream> mapper) {
        final List<float[]> listOfArray = new ArrayList<float[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final float[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final float[] arrayOfAll = new float[lengthOfAll];
        int from = 0;
        for (float[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new FloatStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(FloatFunction<? extends IntStream> mapper) {
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
    public LongStream flatMapToLong(FloatFunction<? extends LongStream> mapper) {
        final List<long[]> listOfArray = new ArrayList<long[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final long[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final long[] arrayOfAll = new long[lengthOfAll];
        int from = 0;
        for (long[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new LongStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(FloatFunction<? extends DoubleStream> mapper) {
        final List<double[]> listOfArray = new ArrayList<double[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final double[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final double[] arrayOfAll = new double[lengthOfAll];
        int from = 0;
        for (double[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new DoubleStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final FloatFunction<? extends Stream<T>> mapper) {
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
    public FloatStream distinct() {
        return new FloatStreamImpl(N.removeDuplicates(elements, fromIndex, toIndex, sorted), sorted, closeHandlers);
    }

    @Override
    public FloatStream sorted() {
        if (sorted) {
            return new FloatStreamImpl(elements, fromIndex, toIndex, sorted, closeHandlers);
        }

        final float[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new FloatStreamImpl(a, true, closeHandlers);
    }

    @Override
    public FloatStream peek(FloatConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new FloatStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public FloatStream limit(long maxSize) {
        if (maxSize >= toIndex - fromIndex) {
            return new FloatStreamImpl(elements, fromIndex, toIndex, sorted, closeHandlers);
        } else {
            return new FloatStreamImpl(elements, fromIndex, (int) (fromIndex + maxSize), sorted, closeHandlers);
        }
    }

    @Override
    public FloatStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new FloatStreamImpl(N.EMPTY_FLOAT_ARRAY, sorted, closeHandlers);
        } else {
            return new FloatStreamImpl(elements, (int) (fromIndex + n), toIndex, sorted, closeHandlers);
        }
    }

    @Override
    public void forEach(FloatConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public float[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public FloatList toFloatList() {
        return FloatList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public float reduce(float identity, FloatBinaryOperator op) {
        float result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsFloat(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalFloat reduce(FloatBinaryOperator op) {
        if (count() == 0) {
            return OptionalFloat.empty();
        }

        float result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsFloat(result, elements[i]);
        }

        return OptionalFloat.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjFloatConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public double sum() {
        return N.sum(elements, fromIndex, toIndex).doubleValue();
    }

    @Override
    public OptionalFloat min() {
        if (count() == 0) {
            return OptionalFloat.empty();
        }

        return OptionalFloat.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalFloat max() {
        if (count() == 0) {
            return OptionalFloat.empty();
        }

        return OptionalFloat.of(N.max(elements, fromIndex, toIndex));
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
    public boolean anyMatch(FloatPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(FloatPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(FloatPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalFloat findFirst() {
        return count() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elements[fromIndex]);
    }

    @Override
    public OptionalFloat findAny() {
        return count() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elements[fromIndex]);
    }

    @Override
    public DoubleStream asDoubleStream() {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = elements[i];
        }

        return new DoubleStreamImpl(a, sorted, closeHandlers);
    }

    @Override
    public Stream<Float> boxed() {
        return new ArrayStream<Float>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Float> iterator() {
        return new FloatIterator(elements, fromIndex, toIndex);
    }

    @Override
    public FloatStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new FloatStreamImpl(elements, fromIndex, toIndex, closeHandlerList);
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

    static class FloatIterator extends ImmutableIterator<Float> {
        private final float[] values;
        private final int toIndex;
        private int cursor;

        FloatIterator(float[] array, int fromIndex, int toIndex) {
            this.values = array;
            this.toIndex = toIndex;
            this.cursor = fromIndex;
        }

        @Override
        public boolean hasNext() {
            return cursor < toIndex;
        }

        @Override
        public Float next() {
            return values[cursor++];
        }
    }
}
