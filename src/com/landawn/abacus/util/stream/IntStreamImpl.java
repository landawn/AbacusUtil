package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

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
import com.landawn.abacus.util.function.IntToByteFunction;
import com.landawn.abacus.util.function.IntToCharFunction;
import com.landawn.abacus.util.function.IntToDoubleFunction;
import com.landawn.abacus.util.function.IntToFloatFunction;
import com.landawn.abacus.util.function.IntToLongFunction;
import com.landawn.abacus.util.function.IntToShortFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IntStreamImpl extends IntStream {
    private final int[] values;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    IntStreamImpl(int[] values) {
        this(values, null);
    }

    IntStreamImpl(int[] values, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    IntStreamImpl(int[] values, boolean sorted, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    IntStreamImpl(int[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    IntStreamImpl(int[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    IntStreamImpl(int[] values, int fromIndex, int toIndex, boolean sorted, Collection<Runnable> closeHandlers) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > values.length) {
            throw new IllegalArgumentException("Invalid fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ")");
        }

        this.values = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public IntStream filter(IntPredicate predicate) {
        return filter(predicate, Integer.MAX_VALUE);
    }

    @Override
    public IntStream filter(final IntPredicate predicate, final int max) {
        return new IntStreamImpl(N.filter(values, fromIndex, toIndex, predicate, max), sorted, closeHandlers);
    }

    @Override
    public IntStream takeWhile(IntPredicate predicate) {
        return takeWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public IntStream takeWhile(IntPredicate predicate, int max) {
        final IntList list = IntList.of(new int[N.min(9, max, (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(values[i])) {
                list.add(values[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new IntStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public IntStream dropWhile(IntPredicate predicate) {
        return dropWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public IntStream dropWhile(IntPredicate predicate, int max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(values[index])) {
            index++;
        }

        final IntList list = IntList.of(new int[N.min(9, max, (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(values[index]);
            index++;
            cnt++;
        }

        return new IntStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
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
    public CharStream mapToChar(IntToCharFunction mapper) {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsChar(values[i]);
        }

        return new CharStreamImpl(a, closeHandlers);
    }

    @Override
    public ByteStream mapToByte(IntToByteFunction mapper) {
        final byte[] a = new byte[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsByte(values[i]);
        }

        return new ByteStreamImpl(a, closeHandlers);
    }

    @Override
    public ShortStream mapToShort(IntToShortFunction mapper) {
        final short[] a = new short[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsShort(values[i]);
        }

        return new ShortStreamImpl(a, closeHandlers);
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
    public FloatStream mapToFloat(IntToFloatFunction mapper) {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsFloat(values[i]);
        }

        return new FloatStreamImpl(a, closeHandlers);
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
    public <U> Stream<U> mapToObj(IntFunction<? extends U> mapper) {
        final Object[] a = new Object[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.apply(values[i]);
        }

        return new ArrayStream<U>((U[]) a, closeHandlers);
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
    public CharStream flatMapToChar(IntFunction<? extends CharStream> mapper) {
        final List<char[]> listOfArray = new ArrayList<char[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final char[] tmp = mapper.apply(values[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final char[] arrayOfAll = new char[lengthOfAll];
        int from = 0;
        for (char[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new CharStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public ByteStream flatMapToByte(IntFunction<? extends ByteStream> mapper) {
        final List<byte[]> listOfArray = new ArrayList<byte[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final byte[] tmp = mapper.apply(values[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final byte[] arrayOfAll = new byte[lengthOfAll];
        int from = 0;
        for (byte[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ByteStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public ShortStream flatMapToShort(IntFunction<? extends ShortStream> mapper) {
        final List<short[]> listOfArray = new ArrayList<short[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final short[] tmp = mapper.apply(values[i]).toArray();
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
    public LongStream flatMapToLong(IntFunction<? extends LongStream> mapper) {
        final List<long[]> listOfArray = new ArrayList<long[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final long[] tmp = mapper.apply(values[i]).toArray();
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
    public FloatStream flatMapToFloat(IntFunction<? extends FloatStream> mapper) {
        final List<float[]> listOfArray = new ArrayList<float[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final float[] tmp = mapper.apply(values[i]).toArray();
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
    public DoubleStream flatMapToDouble(IntFunction<? extends DoubleStream> mapper) {
        final List<double[]> listOfArray = new ArrayList<double[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final double[] tmp = mapper.apply(values[i]).toArray();
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
    public <T> Stream<T> flatMapToObj(IntFunction<? extends Stream<T>> mapper) {
        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        int lengthOfAll = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            final Object[] tmp = mapper.apply(values[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final Object[] arrayOfAll = new Object[lengthOfAll];
        int from = 0;

        for (Object[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayStream<T>((T[]) arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream distinct() {
        return new IntStreamImpl(N.removeDuplicates(values, fromIndex, toIndex, sorted), sorted, closeHandlers);
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
            return new IntStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        } else {
            return new IntStreamImpl(values, fromIndex, (int) (fromIndex + maxSize), sorted, closeHandlers);
        }
    }

    @Override
    public IntStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new IntStreamImpl(N.EMPTY_INT_ARRAY, sorted, closeHandlers);
        } else {
            return new IntStreamImpl(values, (int) (fromIndex + n), toIndex, sorted, closeHandlers);
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
    public IntList toIntList() {
        return IntList.of(N.copyOfRange(values, fromIndex, toIndex));
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
    public long sum() {
        return N.sum(values, fromIndex, toIndex).longValue();
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
    public LongStream asLongStream() {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new LongStreamImpl(a, sorted, closeHandlers);
    }

    @Override
    public FloatStream asFloatStream() {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new FloatStreamImpl(a, sorted, closeHandlers);
    }

    @Override
    public DoubleStream asDoubleStream() {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new DoubleStreamImpl(a, sorted, closeHandlers);
    }

    @Override
    public Stream<Integer> boxed() {
        return new ArrayStream<Integer>(Array.box(values, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Integer> iterator() {
        return new IntegerIterator(values, fromIndex, toIndex);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

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
