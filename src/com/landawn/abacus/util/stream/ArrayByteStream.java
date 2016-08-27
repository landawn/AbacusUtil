package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteToIntFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ArrayByteStream extends ByteStream {
    private final byte[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    ArrayByteStream(byte[] values) {
        this(values, null);
    }

    ArrayByteStream(byte[] values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    ArrayByteStream(byte[] values, Collection<Runnable> closeHandlers, boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayByteStream(byte[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayByteStream(byte[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayByteStream(byte[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted) {
        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public ByteStream filter(BytePredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream filter(final BytePredicate predicate, final long max) {
        return new ArrayByteStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);
    }

    @Override
    public ByteStream takeWhile(BytePredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream takeWhile(BytePredicate predicate, long max) {
        final ByteList list = ByteList.of(new byte[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new ArrayByteStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public ByteStream dropWhile(BytePredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream dropWhile(BytePredicate predicate, long max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final ByteList list = ByteList.of(new byte[N.min(9, Stream.toInt(max), (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new ArrayByteStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public ByteStream map(ByteUnaryOperator mapper) {
        final byte[] a = new byte[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsByte(elements[i]);
        }

        return new ArrayByteStream(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(ByteToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new ArrayIntStream(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final ByteFunction<? extends U> mapper) {
        //        final Object[] a = new Object[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.apply(elements[i]);
        //        }
        //
        //        return new ArrayStream<U>((U[]) a, closeHandlers);

        return new IteratorStream<U>(new ImmutableIterator<U>() {
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
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) mapper.apply(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public ByteStream flatMap(ByteFunction<? extends ByteStream> mapper) {
        final List<byte[]> listOfArray = new ArrayList<byte[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final byte[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final byte[] arrayOfAll = new byte[lengthOfAll];
        int from = 0;
        for (byte[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayByteStream(arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(ByteFunction<? extends IntStream> mapper) {
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

        return new ArrayIntStream(arrayOfAll, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ByteFunction<? extends Stream<T>> mapper) {
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

        return new IteratorStream<T>(new ImmutableIterator<T>() {
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
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ByteStream> split(int size) {
        final List<byte[]> tmp = N.split(elements, fromIndex, toIndex, size);
        final ByteStream[] a = new ByteStream[tmp.size()];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = new ArrayByteStream(tmp.get(i), null, sorted);
        }

        return new ArrayStream<ByteStream>(a, closeHandlers);
    }

    @Override
    public ByteStream distinct() {
        return new ArrayByteStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public ByteStream sorted() {
        if (sorted) {
            return new ArrayByteStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        }

        final byte[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayByteStream(a, closeHandlers, true);
    }

    @Override
    public ByteStream peek(ByteConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new ByteStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public ByteStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        if (maxSize >= toIndex - fromIndex) {
            return new ArrayByteStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayByteStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
        }
    }

    @Override
    public ByteStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayByteStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayByteStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(ByteConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public byte[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public ByteList toByteList() {
        return ByteList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public byte reduce(byte identity, ByteBinaryOperator op) {
        byte result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsByte(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalByte reduce(ByteBinaryOperator op) {
        if (count() == 0) {
            return OptionalByte.empty();
        }

        byte result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsByte(result, elements[i]);
        }

        return OptionalByte.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public Long sum() {
        return N.sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalByte min() {
        if (count() == 0) {
            return OptionalByte.empty();
        }

        return OptionalByte.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalByte max() {
        if (count() == 0) {
            return OptionalByte.empty();
        }

        return OptionalByte.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalByte kthLargest(int k) {
        if (count() == 0) {
            return OptionalByte.empty();
        }

        return OptionalByte.of(N.kthLargest(elements, fromIndex, toIndex, k));
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

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public boolean anyMatch(BytePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(BytePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(BytePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalByte findFirst() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalByte findFirst(BytePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalByte.of(elements[i]);
            }
        }

        return OptionalByte.empty();
    }

    //    @Override
    //    public OptionalByte findLast() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalByte findLast(BytePredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalByte.of(elements[i]);
            }
        }

        return OptionalByte.empty();
    }

    //    @Override
    //    public OptionalByte findAny() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalByte findAny(BytePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalByte.of(elements[i]);
            }
        }

        return OptionalByte.empty();
    }

    @Override
    public IntStream asIntStream() {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = elements[i];
        }

        return new ArrayIntStream(a, closeHandlers, sorted);
    }

    @Override
    public Stream<Byte> boxed() {
        return new ArrayStream<Byte>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Byte> iterator() {
        return new ImmutableIterator<Byte>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Byte next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }
        };
    }

    @Override
    ImmutableByteIterator byteIterator() {
        return new ImmutableByteIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public byte next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public byte[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public ByteStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ArrayByteStream(elements, fromIndex, toIndex, closeHandlerList, sorted);
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
}
