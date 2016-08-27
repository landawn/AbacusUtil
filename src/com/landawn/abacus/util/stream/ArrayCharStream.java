package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.CharBinaryOperator;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharToIntFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ArrayCharStream extends CharStream {
    private final char[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    ArrayCharStream(char[] values) {
        this(values, null);
    }

    ArrayCharStream(char[] values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    ArrayCharStream(char[] values, Collection<Runnable> closeHandlers, boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayCharStream(char[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayCharStream(char[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayCharStream(char[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted) {
        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public CharStream filter(CharPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream filter(final CharPredicate predicate, final long max) {
        return new ArrayCharStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);
    }

    @Override
    public CharStream takeWhile(CharPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream takeWhile(CharPredicate predicate, long max) {
        final CharList list = CharList.of(new char[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new ArrayCharStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public CharStream dropWhile(CharPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream dropWhile(CharPredicate predicate, long max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final CharList list = CharList.of(new char[N.min(9, Stream.toInt(max), (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new ArrayCharStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public CharStream map(CharUnaryOperator mapper) {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsChar(elements[i]);
        }

        return new ArrayCharStream(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(CharToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new ArrayIntStream(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
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
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

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
    public CharStream flatMap(CharFunction<? extends CharStream> mapper) {
        final List<char[]> listOfArray = new ArrayList<char[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final char[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final char[] arrayOfAll = new char[lengthOfAll];
        int from = 0;
        for (char[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayCharStream(arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(CharFunction<? extends IntStream> mapper) {
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
    public <T> Stream<T> flatMapToObj(final CharFunction<? extends Stream<T>> mapper) {
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
    public Stream<CharStream> split(int size) {
        final List<char[]> tmp = N.split(elements, fromIndex, toIndex, size);
        final CharStream[] a = new CharStream[tmp.size()];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = new ArrayCharStream(tmp.get(i), null, sorted);
        }

        return new ArrayStream<CharStream>(a, closeHandlers);
    }

    @Override
    public CharStream distinct() {
        return new ArrayCharStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return new ArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        }

        final char[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayCharStream(a, closeHandlers, true);
    }

    @Override
    public CharStream peek(CharConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new CharStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public CharStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        if (maxSize >= toIndex - fromIndex) {
            return new ArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayCharStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
        }
    }

    @Override
    public CharStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayCharStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayCharStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(CharConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public char[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public CharList toCharList() {
        return CharList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public char reduce(char identity, CharBinaryOperator op) {
        char result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsChar(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalChar reduce(CharBinaryOperator op) {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        char result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsChar(result, elements[i]);
        }

        return OptionalChar.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalChar min() {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        return OptionalChar.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalChar max() {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        return OptionalChar.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalChar kthLargest(int k) {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        return OptionalChar.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public boolean anyMatch(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalChar findFirst() {
    //        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalChar findFirst(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalChar.of(elements[i]);
            }
        }

        return OptionalChar.empty();
    }

    //    @Override
    //    public OptionalChar findLast() {
    //        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalChar findLast(CharPredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalChar.of(elements[i]);
            }
        }

        return OptionalChar.empty();
    }

    //    @Override
    //    public OptionalChar findAny() {
    //        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalChar findAny(CharPredicate filter) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(elements[i])) {
                return OptionalChar.of(elements[i]);
            }
        }

        return OptionalChar.empty();
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
    public Stream<Character> boxed() {
        return new ArrayStream<Character>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Character> iterator() {
        return new ImmutableIterator<Character>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Character next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }
        };
    }

    @Override
    ImmutableCharIterator charIterator() {
        return new ImmutableCharIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public char[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ArrayCharStream(elements, fromIndex, toIndex, closeHandlerList, sorted);
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
