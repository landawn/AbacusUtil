package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.IntBinaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntToCharFunction;
import com.landawn.abacus.util.function.IntToDoubleFunction;
import com.landawn.abacus.util.function.IntToLongFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IntStreamImpl implements IntStream {
    private final int[] values;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final List<Runnable> closeHandlers;

    IntStreamImpl(int[] values) {
        this(values, null);
    }

    IntStreamImpl(int[] values, List<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    IntStreamImpl(int[] values, boolean sorted, List<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    IntStreamImpl(int[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    IntStreamImpl(int[] values, int fromIndex, int toIndex, List<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    IntStreamImpl(int[] values, int fromIndex, int toIndex, boolean sorted, List<Runnable> closeHandlers) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > values.length) {
            throw new IllegalArgumentException("Invalid fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ")");
        }

        this.values = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : N.newArrayList(closeHandlers);
    }

    @Override
    public IntStream filter(IntPredicate predicate) {
        final IntList list = new IntList();

        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                list.add(values[i]);
            }
        }

        return new IntStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public IntStream map(IntUnaryOperator mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(values[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(IntFunction<? extends U> mapper) {
        final Object[] a = new Object[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.apply(values[i]);
        }

        return new ArrayStream<U>((U[]) a, closeHandlers);
    }

    @Override
    public CharStream mapToChar(IntToCharFunction mapper) {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsChar(values[i]);
        }

        return new CharStreamImpl(a, closeHandlers);
    }

    @Override
    public LongStream mapToLong(IntToLongFunction mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(values[i]);
        }

        return new LongStreamImpl(a, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(IntToDoubleFunction mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(values[i]);
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public IntStream flatMap(IntFunction<? extends IntStream> mapper) {
        final List<int[]> listOfArray = new ArrayList<int[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final int[] tmp = mapper.apply(values[i]).toArray();
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
    public IntStream distinct() {
        return new IntStreamImpl(N.removeDuplicates(values, fromIndex, toIndex, sorted), closeHandlers);
    }

    @Override
    public IntStream sorted() {
        if (sorted) {
            return new IntStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        }

        final int[] a = N.copyOfRange(values, fromIndex, toIndex);
        N.sort(a);
        return new IntStreamImpl(a, true, closeHandlers);
    }

    @Override
    public IntStream peek(IntConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }

        // return new IntStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public IntStream limit(long maxSize) {
        if (maxSize >= toIndex - fromIndex) {
            return new IntStreamImpl(values, fromIndex, toIndex, closeHandlers);
        } else {
            return new IntStreamImpl(values, fromIndex, (int) (fromIndex + maxSize), closeHandlers);
        }
    }

    @Override
    public IntStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new IntStreamImpl(N.EMPTY_INT_ARRAY, closeHandlers);
        } else {
            return new IntStreamImpl(values, (int) (fromIndex + n), toIndex, closeHandlers);
        }
    }

    @Override
    public void forEach(IntConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }
    }

    @Override
    public int[] toArray() {
        return N.copyOfRange(values, fromIndex, toIndex);
    }

    @Override
    public int reduce(int identity, IntBinaryOperator op) {
        int result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsInt(result, values[i]);
        }

        return result;
    }

    @Override
    public OptionalInt reduce(IntBinaryOperator op) {
        if (count() == 0) {
            return OptionalInt.empty();
        }

        int result = values[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsInt(result, values[i]);
        }

        return OptionalInt.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, values[i]);
        }

        return result;
    }

    @Override
    public int sum() {
        return N.sum(values, fromIndex, toIndex).intValue();
    }

    @Override
    public OptionalInt min() {
        if (count() == 0) {
            return OptionalInt.empty();
        }

        return OptionalInt.of(N.min(values, fromIndex, toIndex));
    }

    @Override
    public OptionalInt max() {
        if (count() == 0) {
            return OptionalInt.empty();
        }

        return OptionalInt.of(N.max(values, fromIndex, toIndex));
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

        return OptionalDouble.of(N.avg(values, fromIndex, toIndex).doubleValue());
    }

    @Override
    public boolean anyMatch(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalInt findFirst() {
        return count() == 0 ? OptionalInt.empty() : OptionalInt.of(values[fromIndex]);
    }

    @Override
    public OptionalInt findAny() {
        return count() == 0 ? OptionalInt.empty() : OptionalInt.of(values[fromIndex]);
    }

    @Override
    public CharStream asCharStream() {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            if (values[i] < Character.MIN_VALUE || values[i] > Character.MAX_VALUE) {
                throw new ArithmeticException("overflow");
            }

            a[j] = (char) values[i];
        }

        return new CharStreamImpl(a, closeHandlers);
    }

    @Override
    public LongStream asLongStream() {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new LongStreamImpl(a, closeHandlers);
    }

    @Override
    public DoubleStream asDoubleStream() {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public Stream<Integer> boxed() {
        return new ArrayStream<Integer>(Array.wrap(values, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Integer> iterator() {
        return new IntegerIterator(values, fromIndex, toIndex);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = N.newArrayList(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new IntStreamImpl(values, fromIndex, toIndex, closeHandlerList);
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

    static class IntegerIterator extends ImmutableIterator<Integer> {
        private final int[] values;
        private final int toIndex;
        private int cursor;

        IntegerIterator(int[] array, int fromIndex, int toIndex) {
            this.values = array;
            this.toIndex = toIndex;
            this.cursor = fromIndex;
        }

        @Override
        public boolean hasNext() {
            return cursor < toIndex;
        }

        @Override
        public Integer next() {
            return values[cursor++];
        }
    }
}
