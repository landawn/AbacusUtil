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

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
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
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IteratorShortStream extends ShortStream {
    private final ImmutableShortIterator elements;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    IteratorShortStream(ImmutableShortIterator values) {
        this(values, null);
    }

    IteratorShortStream(ImmutableShortIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorShortStream(ImmutableShortIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        this.elements = values;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public ShortStream filter(ShortPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate, final long max) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNext = false;
            private short next = 0;
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
            public short next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream takeWhile(ShortPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate, final long max) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNext = false;
            private short next = 0;
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
            public short next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream dropWhile(ShortPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate, final long max) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNext = false;
            private short next = 0;
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
            public short next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream map(final ShortUnaryOperator mapper) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short next() {
                return mapper.applyAsShort(elements.next());
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream mapToInt(final ShortToIntFunction mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
                return mapper.applyAsInt(elements.next());
            }
        }, sorted, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final ShortFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ImmutableIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.next());
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMap(final ShortFunction<? extends ShortStream> mapper) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private ImmutableShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).shortIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(final ShortFunction<? extends IntStream> mapper) {
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
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ShortFunction<? extends Stream<T>> mapper) {
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
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ShortStream> split(final int size) {
        return new IteratorStream<ShortStream>(new ImmutableIterator<ShortStream>() {

            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public ShortStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final short[] a = new short[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayShortStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public ShortStream distinct() {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private Iterator<Short> distinctIter;

            @Override
            public boolean hasNext() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.hasNext();
            }

            @Override
            public short next() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.next();
            }

            private void removeDuplicated() {
                final Set<Short> set = new LinkedHashSet<>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                distinctIter = set.iterator();
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream top(int n) {
        return top(n, SHORT_COMPARATOR);
    }

    @Override
    public ShortStream top(int n, Comparator<? super Short> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToShort(new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        });
    }

    @Override
    public ShortStream sorted() {
        if (sorted) {
            return new IteratorShortStream(elements, closeHandlers, sorted);
        }

        return new IteratorShortStream(new ImmutableShortIterator() {
            short[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public short next() {
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
            public short[] toArray() {
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
    public ShortStream peek(final ShortConsumer action) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short next() {
                final short next = elements.next();

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
            //    public short[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new IteratorShortStream(new ImmutableShortIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public short next() {
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
    public ShortStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorShortStream(new ImmutableShortIterator() {
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
            public short next() {
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
            public short[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(ShortConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    @Override
    public short[] toArray() {
        return elements.toArray();
    }

    @Override
    public ShortList toShortList() {
        return ShortList.of(toArray());
    }

    @Override
    public short reduce(short identity, ShortBinaryOperator op) {
        short result = identity;

        while (elements.hasNext()) {
            result = op.applyAsShort(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalShort reduce(ShortBinaryOperator op) {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        short result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsShort(result, elements.next());
        }

        return OptionalShort.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public Long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.next();
        }

        return result;
    }

    @Override
    public OptionalShort min() {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        short candidate = elements.next();
        short next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalShort.of(candidate);
    }

    @Override
    public OptionalShort max() {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        short candidate = elements.next();
        short next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalShort.of(candidate);
    }

    @Override
    public OptionalShort kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        }

        final Optional<Short> optional = boxed().kthLargest(k, SHORT_COMPARATOR);

        return optional.isPresent() ? OptionalShort.of(optional.get()) : OptionalShort.empty();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public OptionalDouble average() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        double result = 0d;
        long count = 0;

        while (elements.hasNext()) {
            result += elements.next();
            count++;
        }

        return OptionalDouble.of(result / count);
    }

    @Override
    public boolean anyMatch(ShortPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(ShortPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(ShortPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalShort findFirst() {
    //        return elements.hasNext() ? OptionalShort.empty() : OptionalShort.of(elements.next());
    //    }

    @Override
    public OptionalShort findFirst(ShortPredicate predicate) {
        while (elements.hasNext()) {
            short e = elements.next();

            if (predicate.test(e)) {
                return OptionalShort.of(e);
            }
        }

        return OptionalShort.empty();
    }

    //    @Override
    //    public OptionalShort findLast() {
    //        if (elements.hasNext() == false) {
    //            return OptionalShort.empty();
    //        }
    //
    //        short e = 0;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return OptionalShort.of(e);
    //    }

    @Override
    public OptionalShort findLast(ShortPredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        }

        boolean hasResult = false;
        short e = 0;
        short result = 0;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalShort.of(result) : OptionalShort.empty();
    }

    //    @Override
    //    public OptionalShort findAny() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements.next());
    //    }

    @Override
    public OptionalShort findAny(ShortPredicate predicate) {
        while (elements.hasNext()) {
            short e = elements.next();

            if (predicate.test(e)) {
                return OptionalShort.of(e);
            }
        }

        return OptionalShort.empty();
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
    public Stream<Short> boxed() {
        return new IteratorStream<Short>(iterator(), closeHandlers, sorted, sorted ? SHORT_COMPARATOR : null);
    }

    @Override
    public Iterator<Short> iterator() {
        return new ImmutableIterator<Short>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Short next() {
                return elements.next();
            }
        };
    }

    @Override
    ImmutableShortIterator shortIterator() {
        return elements;
    }

    @Override
    public ShortStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new IteratorShortStream(elements, closeHandlerList, sorted);
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
