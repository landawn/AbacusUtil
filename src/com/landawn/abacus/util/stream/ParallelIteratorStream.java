package com.landawn.abacus.util.stream;

import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.stream.ImmutableIterator.QueuedIterator;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 */
final class ParallelIteratorStream<T> extends AbstractStream<T> {
    private final ImmutableIterator<T> elements;
    private final boolean sorted;
    private final Comparator<? super T> cmp;
    private final int maxThreadNum;
    private final Splitter splitter;
    private volatile IteratorStream<T> sequential;

    ParallelIteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator,
            int maxThreadNum, Splitter splitter) {
        super(closeHandlers);

        N.requireNonNull(iterator);

        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        } else if (maxThreadNum > Stream.THREAD_POOL_SIZE) {
            if (logger.isWarnEnabled()) {
                logger.warn("'maxThreaddNum' is bigger than max thread pool size: " + Stream.THREAD_POOL_SIZE + ". It will reduced to max thread pool size: "
                        + Stream.THREAD_POOL_SIZE + " automatically");
            }
        }

        this.elements = iterator instanceof ImmutableIterator ? (ImmutableIterator<T>) iterator : new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public T next() {
                return iterator.next();
            }
        };

        this.sorted = sorted;
        this.cmp = comparator;
        this.maxThreadNum = N.min(maxThreadNum, Stream.THREAD_POOL_SIZE);
        this.splitter = splitter == null ? Stream.DEFAULT_SPILTTER : splitter;
    }

    ParallelIteratorStream(final Stream<T> stream, Set<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator, int maxThreadNum,
            Splitter splitter) {
        this(stream.iterator(), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, comparator, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> filter(Predicate<? super T> predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().filter(predicate, max).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);
        final AtomicLong cnt = new AtomicLong(0);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false && cnt.get() < max && cnt.incrementAndGet() <= max) {
                        while (true) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next)) {
                                hasNext = true;
                                break;
                            }
                        }
                    }

                    return hasNext;
                }

                @Override
                public T next() {
                    if (hasNext == false && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    hasNext = false;
                    return next;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> takeWhile(Predicate<? super T> predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().takeWhile(predicate, max).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);
        final AtomicLong cnt = new AtomicLong(0);
        final MutableBoolean hasMore = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false && hasMore.isTrue() && cnt.get() < max && cnt.incrementAndGet() <= max) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                                hasNext = true;
                            } else {
                                hasMore.setFalse();
                            }
                        }

                        if (hasNext && predicate.test(next) == false) {
                            hasNext = false;
                            hasMore.setFalse();
                        }
                    }

                    return hasNext;
                }

                @Override
                public T next() {
                    if (hasNext == false && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    hasNext = false;
                    return next;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> dropWhile(Predicate<? super T> predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().dropWhile(predicate, max).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);
        final AtomicLong cnt = new AtomicLong(0);
        final MutableBoolean dropped = MutableBoolean.of(false);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false && cnt.get() < max && cnt.incrementAndGet() <= max) {
                        // Only one thread is kept for running after it's dropped.
                        if (dropped.isTrue()) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                    hasNext = true;
                                }
                            }
                        } else {
                            while (dropped.isFalse()) {
                                synchronized (elements) {
                                    if (elements.hasNext()) {
                                        next = elements.next();
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next) == false) {
                                    hasNext = true;
                                    dropped.setTrue();
                                    break;
                                }
                            }

                            if (hasNext == false && dropped.isTrue()) {
                                synchronized (elements) {
                                    if (elements.hasNext()) {
                                        next = elements.next();
                                        hasNext = true;
                                    }
                                }
                            }
                        }
                    }

                    return hasNext;
                }

                @Override
                public T next() {
                    if (hasNext == false && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    hasNext = false;
                    return next;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<R>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public R next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    R result = mapper.apply((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().mapToChar(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Character>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Character next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Character result = mapper.applyAsChar((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().mapToByte(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Byte>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Byte next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Byte result = mapper.applyAsByte((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().mapToShort(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Short>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Short next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Short result = mapper.applyAsShort((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Integer>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Integer next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Integer result = mapper.applyAsInt((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Long>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Long next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Long result = mapper.applyAsLong((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Float>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Float next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Float result = mapper.applyAsFloat((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Double>() {
                private Object next = Stream.NONE;

                @Override
                public boolean hasNext() {
                    if (next == Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != Stream.NONE;
                }

                @Override
                public Double next() {
                    if (next == Stream.NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Double result = mapper.applyAsDouble((T) next);
                    next = Stream.NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMap(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return flatMap4(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return mapper.apply(t).iterator();
            }
        });
    }

    @Override
    public <R> Stream<R> flatMap2(final Function<? super T, ? extends R[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMap2(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<R>() {
                private T next = null;
                private R[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public R next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <R> Stream<R> flatMap3(final Function<? super T, ? extends Collection<? extends R>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMap3(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return flatMap4(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return mapper.apply(t).iterator();
            }
        });
    }

    private <R> Stream<R> flatMap4(final Function<? super T, ? extends Iterator<? extends R>> mapper) {
        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<R>() {
                private T next = null;
                private Iterator<? extends R> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public R next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMapToChar(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Character>() {
                private T next = null;
                private ImmutableCharIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).charIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Character next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public CharStream flatMapToChar2(final Function<? super T, char[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMapToChar2(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Character>() {
                private T next = null;
                private char[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Character next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public CharStream flatMapToChar3(final Function<? super T, ? extends Collection<Character>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMapToChar3(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Character>() {
                private T next = null;
                private Iterator<Character> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Character next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMapToByte(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Byte>() {
                private T next = null;
                private ImmutableByteIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).byteIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Byte next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream flatMapToByte2(final Function<? super T, byte[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMapToByte2(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Byte>() {
                private T next = null;
                private byte[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Byte next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream flatMapToByte3(final Function<? super T, ? extends Collection<Byte>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMapToByte3(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Byte>() {
                private T next = null;
                private Iterator<Byte> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Byte next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().flatMapToShort(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Short>() {
                private T next = null;
                private ImmutableShortIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).shortIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Short next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream flatMapToShort2(final Function<? super T, short[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().flatMapToShort2(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Short>() {
                private T next = null;
                private short[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Short next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream flatMapToShort3(final Function<? super T, ? extends Collection<Short>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().flatMapToShort3(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Short>() {
                private T next = null;
                private Iterator<Short> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Short next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Integer>() {
                private T next = null;
                private ImmutableIntIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).intIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Integer next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt2(final Function<? super T, int[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt2(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Integer>() {
                private T next = null;
                private int[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Integer next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt3(final Function<? super T, ? extends Collection<Integer>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt3(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Integer>() {
                private T next = null;
                private Iterator<Integer> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Integer next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Long>() {
                private T next = null;
                private ImmutableLongIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).longIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Long next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream flatMapToLong2(final Function<? super T, long[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong2(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Long>() {
                private T next = null;
                private long[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Long next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream flatMapToLong3(final Function<? super T, ? extends Collection<Long>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong3(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Long>() {
                private T next = null;
                private Iterator<Long> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Long next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Float>() {
                private T next = null;
                private ImmutableFloatIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).floatIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Float next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream flatMapToFloat2(final Function<? super T, float[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat2(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Float>() {
                private T next = null;
                private float[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Float next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream flatMapToFloat3(final Function<? super T, ? extends Collection<Float>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat3(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Float>() {
                private T next = null;
                private Iterator<Float> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Float next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Double>() {
                private T next = null;
                private ImmutableDoubleIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).doubleIterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Double next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream flatMapToDouble2(final Function<? super T, double[]> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble2(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Double>() {
                private T next = null;
                private double[] cur = null;
                private int curIndex = 0;

                @Override
                public boolean hasNext() {
                    while ((cur == null || curIndex >= cur.length) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                        curIndex = 0;
                    }

                    return cur != null && curIndex < cur.length;
                }

                @Override
                public Double next() {
                    if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur[curIndex++];
                }
            });
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream flatMapToDouble3(final Function<? super T, ? extends Collection<Double>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble3(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ImmutableIterator<Double>() {
                private T next = null;
                private Iterator<Double> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != Stream.NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) Stream.NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next).iterator();
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Double next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier, Supplier<Map<K, List<T>>> mapFactory) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier, mapFactory));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K, D, A> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream, mapFactory));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mapFactory));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapFactory));

        return new ParallelIteratorStream<>(map.entrySet().iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return new ParallelIteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Stream<T> next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Object[] a = new Object[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayStream<T>((T[]) a, 0, cnt, null, sorted, cmp);
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<List<T>> splitIntoList(final int size) {
        return new ParallelIteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> list = new ArrayList<>(size);
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    list.add(elements.next());
                    cnt++;
                }

                return list;
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<Set<T>> splitIntoSet(final int size) {
        return new ParallelIteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Set<T> next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Set<T> set = new HashSet<>(N.initHashCapacity(size));
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    set.add(elements.next());
                    cnt++;
                }

                return set;
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> distinct() {
        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Set<T> set = new HashSet<T>();
                final List<T> list = new ArrayList<T>();
                T element = null;

                while (elements.hasNext()) {
                    element = elements.next();
                    if (set.add(element)) {
                        list.add(element);
                    }
                }

                a = (T[]) list.toArray();
                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> distinct(final Comparator<? super T> comparator) {
        if (comparator == null) {
            return distinct();
        }

        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Set<T> set = new TreeSet<T>(comparator);
                final List<T> list = new ArrayList<T>();
                T element = null;
                boolean hasNull = false;

                while (elements.hasNext()) {
                    element = elements.next();
                    if (element == null) {
                        if (hasNull == false) {
                            hasNull = true;
                            list.add(element);
                        }
                    } else {
                        if (set.add(element)) {
                            list.add(element);
                        }
                    }
                }

                a = (T[]) list.toArray();
                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Set<Object> set = new HashSet<>();
                final List<T> list = new ArrayList<T>();
                T element = null;

                while (elements.hasNext()) {
                    element = elements.next();
                    if (set.add(keyMapper.apply(element))) {
                        list.add(element);
                    }
                }

                a = (T[]) list.toArray();
                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> top(int n) {
        return top(n, Stream.OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> top(final int n, final Comparator<? super T> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Comparator<Pair<T, Long>> pairCmp = new Comparator<Pair<T, Long>>() {
                    @Override
                    public int compare(final Pair<T, Long> o1, final Pair<T, Long> o2) {
                        return N.compare(o1.left, o2.left, comparator);
                    }
                };

                final Queue<Pair<T, Long>> heap = new PriorityQueue<Pair<T, Long>>(n, pairCmp);

                Pair<T, Long> pair = null;
                for (long i = 0; elements.hasNext(); i++) {
                    pair = Pair.of(elements.next(), i);

                    if (heap.size() >= n) {
                        if (pairCmp.compare(heap.peek(), pair) < 0) {
                            heap.poll();
                            heap.add(pair);
                        }
                    } else {
                        heap.offer(pair);
                    }
                }

                final Pair<T, Long>[] arrayOfPair = heap.toArray(new Pair[heap.size()]);

                N.sort(arrayOfPair, new Comparator<Pair<T, Long>>() {
                    @Override
                    public int compare(final Pair<T, Long> o1, final Pair<T, Long> o2) {
                        return N.compare(o1.right.longValue(), o2.right.longValue());
                    }
                });

                a = (T[]) new Object[arrayOfPair.length];

                for (int i = 0, len = arrayOfPair.length; i < len; i++) {
                    a[i] = arrayOfPair[i].left;
                }

                toIndex = a.length;
            }

        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> sorted() {
        return sorted(Stream.OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> sorted(final Comparator<? super T> comparator) {
        if (sorted && Stream.isSameComparator(comparator, cmp)) {
            return this;
        }

        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    parallelSort();
                }

                return cursor < a.length;
            }

            @Override
            public T next() {
                if (a == null) {
                    parallelSort();
                }

                if (cursor >= a.length) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    parallelSort();
                }

                return a.length - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    parallelSort();
                }

                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    parallelSort();
                }

                if (b.getClass().equals(a.getClass()) && b.length < a.length - cursor) {
                    if (cursor == 0) {
                        return (A[]) a;
                    } else {
                        return (A[]) N.copyOfRange(a, cursor, a.length);
                    }
                } else {
                    if (b.length < a.length - cursor) {
                        b = N.newArray(b.getClass().getComponentType(), a.length - cursor);
                    }

                    N.copy(a, cursor, b, 0, a.length - cursor);

                    return b;
                }
            }

            private void parallelSort() {
                a = (T[]) elements.toArray(N.EMPTY_OBJECT_ARRAY);

                if (comparator == null) {
                    N.parallelSort(a);
                } else {
                    N.parallelSort(a, comparator);
                }
            }
        }, closeHandlers, true, comparator, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public T next() {
                final T next = elements.next();

                //    try {
                //        action.accept(next);
                //    } catch (Throwable e) {
                //        // ignore.
                //    }

                action.accept(next);
                return next;
            }
        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public T next() {
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
        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
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
            public T next() {
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
            public void skip(long n2) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n2);
            }

            @Override
            public <A> A[] toArray(A[] a) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray(a);
            }
        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public void forEach(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            action.accept(next);
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    @Override
    public <U> U forEach(U identity, BiFunction<U, ? super T, U> accumulator, Predicate<? super U> till) {
        if (Stream.logger.isWarnEnabled()) {
            Stream.logger.warn("'forEach' is sequentially executed in parallel stream");
        }

        return sequential().forEach(identity, accumulator, till);
    }

    //    @Override
    //    public boolean forEach2(final Function<? super T, Boolean> action) {
    //        if (maxThreadNum <= 1) {
    //            return sequential().forEach2(action);
    //        }
    //
    //        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
    //        final Holder<Throwable> eHolder = new Holder<>();
    //        final MutableBoolean result = MutableBoolean.of(true);
    //
    //        for (int i = 0; i < maxThreadNum; i++) {
    //            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    T next = null;
    //
    //                    try {
    //                        while (result.isTrue() && eHolder.value() == null) {
    //                            synchronized (elements) {
    //                                if (elements.hasNext()) {
    //                                    next = elements.next();
    //                                } else {
    //                                    break;
    //                                }
    //                            }
    //
    //                            if (action.apply(next) == false) {
    //                                result.setFalse();
    //                                break;
    //                            }
    //                        }
    //                    } catch (Throwable e) {
    //                        Stream.setError(eHolder, e);
    //                    }
    //                }
    //            }));
    //        }
    //
    //        if (eHolder.value() != null) {
    //            throw N.toRuntimeException(eHolder.value());
    //        }
    //
    //        try {
    //            for (CompletableFuture<Void> future : futureList) {
    //                future.get();
    //            }
    //        } catch (Exception e) {
    //            throw N.toRuntimeException(e);
    //        }
    //
    //        return result.booleanValue();
    //    }

    @Override
    public Object[] toArray() {
        return toArray(N.EMPTY_OBJECT_ARRAY);
    }

    <A> A[] toArray(A[] a) {
        return elements.toArray(a);
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return toArray(generator.apply(0));
    }

    @Override
    public <A> ObjectList<A> toObjectList(Class<A> cls) {
        return ObjectList.of(toArray((A[]) N.newArray(cls, 0)));
    }

    @Override
    public List<T> toList() {
        final List<T> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<T> toList(Supplier<? extends List<T>> supplier) {
        final List<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<T> toSet(Supplier<? extends Set<T>> supplier) {
        final Set<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier) {
        final Multiset<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset() {
        final LongMultiset<T> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier) {
        final LongMultiset<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
        return collect(Collectors.groupingBy(classifier));
    }

    @Override
    public <K, M extends Map<K, List<T>>> M toMap(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        return collect(Collectors.groupingBy(classifier, mapFactory));
    }

    @Override
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        return collect(Collectors.groupingBy(classifier, downstream));
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        return collect(Collectors.groupingBy(classifier, downstream, mapFactory));
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return collect(Collectors.toMap(keyMapper, valueMapper));
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapSupplier) {
        return collect(Collectors.toMap(keyMapper, valueMapper, mapSupplier));
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction));
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier) {
        return collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapSupplier));
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return collect(Collectors.toMultimap(keyMapper, valueMapper));
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapSupplier) {
        return collect(Collectors.toMultimap(keyMapper, valueMapper, mapSupplier));
    }

    @Override
    public T reduce(final T identity, final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<T>() {
                @Override
                public T call() {
                    T result = identity;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.apply(result, next);
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return result;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        T result = (T) Stream.NONE;

        try {
            for (CompletableFuture<T> future : futureList) {
                if (result == Stream.NONE) {
                    result = future.get();
                } else {
                    result = accumulator.apply(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == Stream.NONE ? identity : result;
    }

    @Override
    public OptionalNullable<T> reduce(final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<T>() {
                @Override
                public T call() {
                    T result = (T) Stream.NONE;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = result == Stream.NONE ? next : accumulator.apply(result, next);
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return result;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        T result = (T) Stream.NONE;

        try {
            for (CompletableFuture<T> future : futureList) {
                final T tmp = future.get();

                if (tmp == Stream.NONE) {
                    continue;
                } else if (result == Stream.NONE) {
                    result = tmp;
                } else {
                    result = accumulator.apply(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == Stream.NONE ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(result);
    }

    @Override
    public <U> U reduce(final U identity, final BiFunction<U, ? super T, U> accumulator, final BinaryOperator<U> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator, combiner);
        }

        final List<CompletableFuture<U>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<U>() {
                @Override
                public U call() {
                    U result = identity;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.apply(result, next);
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return result;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        U result = (U) Stream.NONE;

        try {
            for (CompletableFuture<U> future : futureList) {
                final U tmp = future.get();

                if (result == Stream.NONE) {
                    result = tmp;
                } else {
                    result = combiner.apply(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == Stream.NONE ? identity : result;
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator) {
        if (Stream.logger.isWarnEnabled()) {
            Stream.logger.warn("'reduce' is sequentially executed in parallel stream");
        }

        return sequential().reduce(identity, accumulator);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super T> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().collect(supplier, accumulator, combiner);
        }

        final List<CompletableFuture<R>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<R>() {
                @Override
                public R call() {
                    R container = supplier.get();
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            accumulator.accept(container, next);
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return container;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        R container = (R) Stream.NONE;

        try {
            for (CompletableFuture<R> future : futureList) {
                final R tmp = future.get();

                if (container == Stream.NONE) {
                    container = tmp;
                } else {
                    combiner.accept(container, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return container == Stream.NONE ? supplier.get() : container;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator) {
        if (Stream.logger.isWarnEnabled()) {
            Stream.logger.warn("'collect' is sequentially executed in parallel stream");
        }

        return sequential().collect(supplier, accumulator);
    }

    @Override
    public <R, A> R collect(final Collector<? super T, A, R> collector) {
        if (maxThreadNum <= 1) {
            return sequential().collect(collector);
        }

        final Supplier<A> supplier = collector.supplier();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final BinaryOperator<A> combiner = collector.combiner();
        final Function<A, R> finisher = collector.finisher();

        final List<CompletableFuture<A>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<A>() {
                @Override
                public A call() {
                    A container = supplier.get();
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            accumulator.accept(container, next);
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return container;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        A container = (A) Stream.NONE;

        try {
            for (CompletableFuture<A> future : futureList) {
                final A tmp = future.get();

                if (container == Stream.NONE) {
                    container = tmp;
                } else {
                    combiner.apply(container, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return finisher.apply(container == Stream.NONE ? supplier.get() : container);
    }

    @Override
    public OptionalNullable<T> min(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? Stream.OBJECT_COMPARATOR : comparator;

        return collect(Collectors.minBy(comparator));
    }

    @Override
    public OptionalNullable<T> max(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? Stream.OBJECT_COMPARATOR : comparator;

        return collect(Collectors.maxBy(comparator));
    }

    @Override
    public OptionalNullable<T> kthLargest(int k, Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? Stream.OBJECT_COMPARATOR : comparator;

        return sequential().kthLargest(k, comparator);
    }

    @Override
    public Long sumInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.summingInt(mapper));
    }

    @Override
    public Long sumLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.summingLong(mapper));
    }

    @Override
    public Double sumDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.summingDouble(mapper));
    }

    @Override
    public OptionalDouble averageInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.averagingInt2(mapper));
    }

    @Override
    public OptionalDouble averageLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.averagingLong2(mapper));
    }

    @Override
    public OptionalDouble averageDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.averagingDouble2(mapper));
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public CharSummaryStatistics summarizeChar(ToCharFunction<? super T> mapper) {
        return collect(Collectors.summarizingChar(mapper));
    }

    @Override
    public ByteSummaryStatistics summarizeByte(ToByteFunction<? super T> mapper) {
        return collect(Collectors.summarizingByte(mapper));
    }

    @Override
    public ShortSummaryStatistics summarizeShort(ToShortFunction<? super T> mapper) {
        return collect(Collectors.summarizingShort(mapper));
    }

    @Override
    public IntSummaryStatistics summarizeInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.summarizingInt(mapper));
    }

    @Override
    public LongSummaryStatistics summarizeLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.summarizingLong(mapper));
    }

    @Override
    public FloatSummaryStatistics summarizeFloat(ToFloatFunction<? super T> mapper) {
        return collect(Collectors.summarizingFloat(mapper));
    }

    @Override
    public DoubleSummaryStatistics summarizeDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.summarizingDouble(mapper));
    }

    @Override
    public Optional<Map<String, T>> distribution() {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final Object[] a = sorted().toArray();

        return Optional.of((Map<String, T>) N.distribution(a));
    }

    @Override
    public Optional<Map<String, T>> distribution(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final Object[] a = sorted(comparator).toArray();

        return Optional.of((Map<String, T>) N.distribution(a));
    }

    @Override
    public boolean anyMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    T next = null;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next)) {
                                result.setTrue();
                                break;
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.booleanValue();
    }

    @Override
    public boolean allMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    T next = null;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next) == false) {
                                result.setFalse();
                                break;
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.booleanValue();
    }

    @Override
    public boolean noneMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    T next = null;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next)) {
                                result.setFalse();
                                break;
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.booleanValue();
    }

    @Override
    public OptionalNullable<T> findFirst(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Long, T>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, T>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, T>>() {
                @Override
                public Pair<Long, T> call() {
                    final Pair<Long, T> pair = new Pair<>();

                    try {
                        while (resultHolder.value() == null && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(pair.right)) {
                                synchronized (resultHolder) {
                                    if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                        resultHolder.setValue(pair);
                                    }
                                }

                                break;
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return pair;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Long, T>> future : futureList) {
                final Pair<Long, T> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(resultHolder.value().right);
    }

    @Override
    public OptionalNullable<T> findLast(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Long, T>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, T>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, T>>() {
                @Override
                public Pair<Long, T> call() {
                    final Pair<Long, T> pair = new Pair<>();

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(pair.right)) {
                                synchronized (resultHolder) {
                                    if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                        resultHolder.setValue(pair);
                                    }
                                }
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return pair;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Long, T>> future : futureList) {
                final Pair<Long, T> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(resultHolder.value().right);
    }

    @Override
    public OptionalNullable<T> findAny(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<T> resultHolder = Holder.of((T) Stream.NONE);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<T>() {
                @Override
                public T call() {
                    T next = null;

                    try {
                        while (resultHolder.value() == Stream.NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next)) {
                                synchronized (resultHolder) {
                                    if (resultHolder.value() == Stream.NONE) {
                                        resultHolder.setValue(next);
                                    }
                                }

                                break;
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }

                    return next;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<T> future : futureList) {
                if (resultHolder.value() == Stream.NONE) {
                    future.get();
                } else {
                    break;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == Stream.NONE ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(resultHolder.value());
    }

    @Override
    public Stream<T> except(Collection<?> c) {
        //        final Multiset<?> multiset = Multiset.of(c);
        //
        //        return filter(new Predicate<T>() {
        //            @Override
        //            public boolean test(T value) {
        //                synchronized (multiset) {
        //                    return multiset.getAndRemove(value) < 1;
        //                }
        //            }
        //        });

        //        if (maxThreadNum <= 1) {
        //            return new ParallelIteratorStream<>(sequential().except(c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        //        }
        //
        //        final Multiset<?> multiset = Multiset.of(c);
        //
        //        final Predicate<? super T> predicate = new Predicate<T>() {
        //            @Override
        //            public boolean test(T value) {
        //                return multiset.getAndRemove(value) < 1;
        //            }
        //        };
        //
        //        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
        //            private boolean hasNext = false;
        //            private T next = null;
        //
        //            @Override
        //            public boolean hasNext() {
        //                if (hasNext == false) {
        //                    while (elements.hasNext()) {
        //                        next = elements.next();
        //
        //                        if (predicate.test(next)) {
        //                            hasNext = true;
        //                            break;
        //                        }
        //                    }
        //                }
        //
        //                return hasNext;
        //            }
        //
        //            @Override
        //            public T next() {
        //                if (hasNext == false && hasNext() == false) {
        //                    throw new NoSuchElementException();
        //                }
        //
        //                hasNext = false;
        //
        //                return next;
        //            }
        //        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);

        return new ParallelIteratorStream<>(this.sequential().except(c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> except(final Function<? super T, ?> mapper, final Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersect(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        }

        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                final Object key = mapper.apply(value);

                synchronized (multiset) {
                    return multiset.getAndRemove(key) < 1;
                }
            }
        });
    }

    @Override
    public Stream<T> intersect(final Collection<?> c) {
        //        final Multiset<?> multiset = Multiset.of(c);
        //
        //        return filter(new Predicate<T>() {
        //            @Override
        //            public boolean test(T value) {
        //                synchronized (multiset) {
        //                    return multiset.getAndRemove(value) > 0;
        //                }
        //            }
        //        });

        //        if (maxThreadNum <= 1) {
        //            return new ParallelIteratorStream<>(sequential().intersect(c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        //        }
        //
        //        final Multiset<?> multiset = Multiset.of(c);
        //
        //        final Predicate<? super T> predicate = new Predicate<T>() {
        //            @Override
        //            public boolean test(T value) {
        //                return multiset.getAndRemove(value) > 0;
        //            }
        //        };
        //
        //        return new ParallelIteratorStream<T>(new ImmutableIterator<T>() {
        //            private boolean hasNext = false;
        //            private T next = null;
        //
        //            @Override
        //            public boolean hasNext() {
        //                if (hasNext == false) {
        //                    while (elements.hasNext()) {
        //                        next = elements.next();
        //
        //                        if (predicate.test(next)) {
        //                            hasNext = true;
        //                            break;
        //                        }
        //                    }
        //                }
        //
        //                return hasNext;
        //            }
        //
        //            @Override
        //            public T next() {
        //                if (hasNext == false && hasNext() == false) {
        //                    throw new NoSuchElementException();
        //                }
        //
        //                hasNext = false;
        //
        //                return next;
        //            }
        //        }, closeHandlers, sorted, cmp, maxThreadNum, splitter);

        return new ParallelIteratorStream<>(this.sequential().intersect(c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> intersect(final Function<? super T, ?> mapper, Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersect(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
        }

        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                final Object key = mapper.apply(value);

                synchronized (multiset) {
                    return multiset.getAndRemove(key) > 0;
                }
            }
        });
    }

    //    @Override
    //    public Stream<T> exclude(final Collection<?> c) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorStream<>(sequential().exclude(c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    //        }
    //
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    //    @Override
    //    public Stream<T> exclude(final Function<? super T, ?> mapper, final Collection<?> c) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorStream<>(sequential().exclude(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    //        }
    //
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return !set.contains(mapper.apply(value));
    //            }
    //        });
    //    }

    //    @Override
    //    public Stream<T> skipNull() {
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return value != null;
    //            }
    //        });
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileNull() {
    //        return new ParallelIteratorStream<>(NullBreakIterator.of(elements), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileError() {
    //        return new ParallelIteratorStream<>(ErrorBreakIterator.of(elements), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileError(int maxRetries, long retryInterval) {
    //        return new ParallelIteratorStream<>(ErrorBreakIterator.of(elements, maxRetries, retryInterval), closeHandlers, sorted, cmp, maxThreadNum, splitter);
    //    }

    @Override
    public Stream<T> queued() {
        return queued(DEFAULT_QUEUE_SIZE);
    }

    @Override
    public Stream<T> queued(int queueSize) {
        final Iterator<T> iter = iterator();

        if (iter instanceof QueuedIterator && ((QueuedIterator<? extends T>) iter).max() >= queueSize) {
            return this;
        } else {
            return new ParallelIteratorStream<>(Stream.parallelConcat(Arrays.asList(iter), queueSize, asyncExecutor), closeHandlers, sorted, cmp, maxThreadNum,
                    splitter);
        }
    }

    @Override
    public Stream<T> append(final Stream<T> stream) {
        return new ParallelIteratorStream<>(Stream.concat(this, stream), closeHandlers, false, null, maxThreadNum, splitter);
    }

    //    @Override
    //    public Stream<T> append(Iterator<? extends T> iterator) {
    //        return new IteratorParallelStream<>(Stream.concat(elements, iterator).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
    //    }

    @Override
    public Stream<T> merge(final Stream<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return new ParallelIteratorStream<>(Stream.merge(this, b, nextSelector), closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> cached(IntFunction<T[]> generator) {
        final T[] a = toArray(generator);
        return new ParallelArrayStream<T>(a, 0, a.length, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public long persist(final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {

        if (maxThreadNum <= 1) {
            return sequential().persist(stmt, batchSize, batchInterval, stmtSetter);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final AtomicLong result = new AtomicLong();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long cnt = 0;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            stmtSetter.accept(stmt, next);
                            stmt.addBatch();

                            if ((++cnt % batchSize) == 0) {
                                stmt.executeBatch();
                                stmt.clearBatch();

                                if (batchInterval > 0) {
                                    N.sleep(batchInterval);
                                }
                            }
                        }

                        if ((cnt % batchSize) > 0) {
                            stmt.executeBatch();
                            stmt.clearBatch();
                        }

                        result.addAndGet(cnt);
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.longValue();
    }

    @Override
    public ImmutableIterator<T> iterator() {
        return elements;
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public Stream<T> sequential() {
        IteratorStream<T> tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorStream<T>(elements, closeHandlers, sorted, cmp);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public Stream<T> parallel(int maxThreadNum, Splitter splitter) {
        if (this.maxThreadNum == maxThreadNum && this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorStream<T>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public Stream<T> maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorStream<T>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Splitter splitter() {
        return splitter;
    }

    @Override
    public Stream<T> splitter(Splitter splitter) {
        if (this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorStream<T>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorStream<T>(elements, newCloseHandlers, sorted, cmp, maxThreadNum, splitter);
    }
}
