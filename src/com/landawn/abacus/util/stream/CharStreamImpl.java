package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
    private final char[] values;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final List<Runnable> closeHandlers;

    CharStreamImpl(char[] values) {
        this(values, null);
    }

    CharStreamImpl(char[] values, List<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    CharStreamImpl(char[] values, boolean sorted, List<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    CharStreamImpl(char[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    CharStreamImpl(char[] values, int fromIndex, int toIndex, List<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    CharStreamImpl(char[] values, int fromIndex, int toIndex, boolean sorted, List<Runnable> closeHandlers) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > values.length) {
            throw new IllegalArgumentException("Invalid fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ")");
        }

        this.values = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new ArrayList<>(closeHandlers);
    }

    @Override
    public CharStream filter(CharPredicate predicate) {
        final CharList list = new CharList();

        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                list.add(values[i]);
            }
        }

        return new CharStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public CharStream map(CharUnaryOperator mapper) {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsChar(values[i]);
        }

        return new CharStreamImpl(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(CharFunction<? extends U> mapper) {
        final Object[] a = new Object[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.apply(values[i]);
        }

        return new ArrayStream<U>((U[]) a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(CharToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(values[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public CharStream flatMap(CharFunction<? extends CharStream> mapper) {
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
    public CharStream distinct() {
        return new CharStreamImpl(N.removeDuplicates(values, fromIndex, toIndex, sorted), closeHandlers);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return new CharStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        }

        final char[] a = N.copyOfRange(values, fromIndex, toIndex);
        N.sort(a);
        return new CharStreamImpl(a, true, closeHandlers);
    }

    @Override
    public CharStream peek(CharConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }

        // return new CharStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public CharStream limit(long maxSize) {
        if (maxSize >= toIndex - fromIndex) {
            return new CharStreamImpl(values, fromIndex, toIndex, closeHandlers);
        } else {
            return new CharStreamImpl(values, fromIndex, (int) (fromIndex + maxSize), closeHandlers);
        }
    }

    @Override
    public CharStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new CharStreamImpl(N.EMPTY_CHAR_ARRAY, closeHandlers);
        } else {
            return new CharStreamImpl(values, (int) (fromIndex + n), toIndex, closeHandlers);
        }
    }

    @Override
    public void forEach(CharConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }
    }

    @Override
    public char[] toArray() {
        return N.copyOfRange(values, fromIndex, toIndex);
    }

    @Override
    public char reduce(char identity, CharBinaryOperator op) {
        char result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsChar(result, values[i]);
        }

        return result;
    }

    @Override
    public OptionalChar reduce(CharBinaryOperator op) {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        char result = values[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsChar(result, values[i]);
        }

        return OptionalChar.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, values[i]);
        }

        return result;
    }

    @Override
    public OptionalChar min() {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        return OptionalChar.of(N.min(values, fromIndex, toIndex));
    }

    @Override
    public OptionalChar max() {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        return OptionalChar.of(N.max(values, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public boolean anyMatch(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalChar findFirst() {
        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(values[fromIndex]);
    }

    @Override
    public OptionalChar findAny() {
        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(values[fromIndex]);
    }

    @Override
    public IntStream asIntStream() {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public Stream<Character> boxed() {
        return new ArrayStream<Character>(Array.wrap(values, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Character> iterator() {
        return new CharacterIterator(values, fromIndex, toIndex);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new CharStreamImpl(values, fromIndex, toIndex, closeHandlerList);
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
