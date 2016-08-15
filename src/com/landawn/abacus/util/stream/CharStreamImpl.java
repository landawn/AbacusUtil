package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
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
final class CharStreamImpl extends CharStream {
    private final char[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    CharStreamImpl(char[] values) {
        this(values, null);
    }

    CharStreamImpl(char[] values, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    CharStreamImpl(char[] values, boolean sorted, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    CharStreamImpl(char[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    CharStreamImpl(char[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    CharStreamImpl(char[] values, int fromIndex, int toIndex, boolean sorted, Collection<Runnable> closeHandlers) {
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
    public CharStream filter(CharPredicate predicate) {
        return filter(predicate, Integer.MAX_VALUE);
    }

    @Override
    public CharStream filter(final CharPredicate predicate, final int max) {
        return new CharStreamImpl(N.filter(elements, fromIndex, toIndex, predicate, max), sorted, closeHandlers);
    }

    @Override
    public CharStream takeWhile(CharPredicate predicate) {
        return takeWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public CharStream takeWhile(CharPredicate predicate, int max) {
        final CharList list = CharList.of(new char[N.min(9, max, (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new CharStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public CharStream dropWhile(CharPredicate predicate) {
        return dropWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public CharStream dropWhile(CharPredicate predicate, int max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final CharList list = CharList.of(new char[N.min(9, max, (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new CharStreamImpl(list.trimToSize().array(), sorted, closeHandlers);
    }

    @Override
    public CharStream map(CharUnaryOperator mapper) {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsChar(elements[i]);
        }

        return new CharStreamImpl(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(CharToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
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

        return new CharStreamImpl(arrayOfAll, closeHandlers);
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

        return new IntStreamImpl(arrayOfAll, closeHandlers);
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
    public CharStream distinct() {
        return new CharStreamImpl(N.removeDuplicates(elements, fromIndex, toIndex, sorted), sorted, closeHandlers);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return new CharStreamImpl(elements, fromIndex, toIndex, sorted, closeHandlers);
        }

        final char[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new CharStreamImpl(a, true, closeHandlers);
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
        if (maxSize >= toIndex - fromIndex) {
            return new CharStreamImpl(elements, fromIndex, toIndex, sorted, closeHandlers);
        } else {
            return new CharStreamImpl(elements, fromIndex, (int) (fromIndex + maxSize), sorted, closeHandlers);
        }
    }

    @Override
    public CharStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new CharStreamImpl(N.EMPTY_CHAR_ARRAY, sorted, closeHandlers);
        } else {
            return new CharStreamImpl(elements, (int) (fromIndex + n), toIndex, sorted, closeHandlers);
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

    @Override
    public OptionalChar findFirst() {
        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[fromIndex]);
    }

    @Override
    public OptionalChar findAny() {
        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[fromIndex]);
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
    public Stream<Character> boxed() {
        return new ArrayStream<Character>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Character> iterator() {
        return new CharacterIterator(elements, fromIndex, toIndex);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new CharStreamImpl(elements, fromIndex, toIndex, closeHandlerList);
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

    static class CharacterIterator extends ImmutableIterator<Character> {
        private final char[] values;
        private final int toIndex;
        private int cursor;

        CharacterIterator(char[] array, int fromIndex, int toIndex) {
            this.values = array;
            this.toIndex = toIndex;
            this.cursor = fromIndex;
        }

        @Override
        public boolean hasNext() {
            return cursor < toIndex;
        }

        @Override
        public Character next() {
            return values[cursor++];
        }
    }
}
