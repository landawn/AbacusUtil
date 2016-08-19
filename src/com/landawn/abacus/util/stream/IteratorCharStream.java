package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
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
final class IteratorCharStream extends CharStream {
    private final ImmutableCharIterator elements;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    IteratorCharStream(ImmutableCharIterator values) {
        this(values, null);
    }

    IteratorCharStream(ImmutableCharIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorCharStream(ImmutableCharIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        this.elements = values;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public CharStream filter(CharPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream filter(final CharPredicate predicate, final long max) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean hasNext = false;
            private char next = 0;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cnt < max) {
                    while (elements.hasNext()) {
                        next = elements.next();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public char next() {
                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream takeWhile(CharPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate, final long max) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean hasNext = false;
            private char next = 0;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cnt < max) {
                    while (elements.hasNext()) {
                        next = elements.next();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        } else {
                            cnt = Long.MAX_VALUE; // no more loop.
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public char next() {
                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public CharStream dropWhile(CharPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate, final long max) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean hasNext = false;
            private char next = 0;
            private long cnt = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cnt < max) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.next();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else {
                        if (elements.hasNext()) {
                            next = elements.next();
                            hasNext = true;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public char next() {
                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public CharStream map(final CharUnaryOperator mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char next() {
                return mapper.applyAsChar(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream mapToInt(final CharToIntFunction mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
                return mapper.applyAsInt(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ImmutableIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public CharStream flatMap(final CharFunction<? extends CharStream> mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private ImmutableCharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).charIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char next() {
                if (cur == null) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(final CharFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private ImmutableIntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).intIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int next() {
                if (cur == null) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final CharFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                if (cur == null) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public CharStream distinct() {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private Iterator<Character> distinctIter;

            @Override
            public boolean hasNext() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.hasNext();
            }

            @Override
            public char next() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.next();
            }

            private void removeDuplicated() {
                final Set<Character> set = new LinkedHashSet<>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                distinctIter = set.iterator();
            }

        }, closeHandlers, sorted);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return new IteratorCharStream(elements, closeHandlers, sorted);
        }

        return new IteratorCharStream(new ImmutableCharIterator() {
            char[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public char next() {
                if (a == null) {
                    sort();
                }

                if (cursor >= a.length) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    sort();
                }

                return a.length - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    sort();
                }

                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
            }

            @Override
            public char[] toArray() {
                if (a == null) {
                    sort();
                }

                if (cursor == 0) {
                    return a;
                } else {
                    return N.copyOfRange(a, cursor, a.length);
                }
            }

            private void sort() {
                a = elements.toArray();

                Arrays.sort(a);
            }
        }, closeHandlers, true);
    }

    @Override
    public CharStream peek(final CharConsumer action) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char next() {
                final char next = elements.next();

                //    try {
                //        action.accept(next);
                //    } catch (Throwable e) {
                //        // ignore.
                //    }

                action.accept(next);
                return next;
            }

            //    @Override
            //    public long count() {
            //        return elements.count();
            //    }
            //
            //    @Override
            //    public void skip(long n) {
            //        elements.skip(n);
            //    }
            //
            //    @Override
            //    public char[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorCharStream(new ImmutableCharIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public char next() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.next();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.hasNext();
            }

            @Override
            public char next() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.next();
            }

            @Override
            public long count() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.count();
            }

            @Override
            public void skip(long n) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n);
            }

            @Override
            public char[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(CharConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    @Override
    public char[] toArray() {
        return elements.toArray();
    }

    @Override
    public CharList toCharList() {
        return CharList.of(toArray());
    }

    @Override
    public char reduce(char identity, CharBinaryOperator op) {
        char result = identity;

        while (elements.hasNext()) {
            result = op.applyAsChar(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalChar reduce(CharBinaryOperator op) {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        char result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsChar(result, elements.next());
        }

        return OptionalChar.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalChar min() {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        char candidate = elements.next();
        char next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar max() {
        if (count() == 0) {
            return OptionalChar.empty();
        }

        char candidate = elements.next();
        char next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        }

        final Optional<Character> optional = boxed().kthLargest(k, new Comparator<Character>() {
            @Override
            public int compare(Character o1, Character o2) {
                return N.compare(o1.charValue(), o2.charValue());
            }
        });

        return optional.isPresent() ? OptionalChar.of(optional.get()) : OptionalChar.empty();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public boolean anyMatch(CharPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(CharPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(CharPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalChar findFirst() {
        return elements.hasNext() ? OptionalChar.empty() : OptionalChar.of(elements.next());
    }

    @Override
    public OptionalChar findAny() {
        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements.next());
    }

    @Override
    public IntStream asIntStream() {
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
                return elements.next();
            }
        }, sorted, closeHandlers);
    }

    @Override
    public Stream<Character> boxed() {
        return new IteratorStream<Character>(iterator(), sorted, sorted ? CHAR_COMPARATOR : null, closeHandlers);
    }

    @Override
    public Iterator<Character> iterator() {
        return new ImmutableIterator<Character>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Character next() {
                return elements.next();
            }
        };
    }

    @Override
    ImmutableCharIterator charIterator() {
        return elements;
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new IteratorCharStream(elements, closeHandlerList, sorted);
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
