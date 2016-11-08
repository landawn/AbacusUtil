package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongBinaryOperator;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.LongToDoubleFunction;
import com.landawn.abacus.util.function.LongToFloatFunction;
import com.landawn.abacus.util.function.LongToIntFunction;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ParallelIteratorLongStream extends AbstractLongStream {
    private final ImmutableLongIterator elements;
    private final boolean sorted;
    private final int maxThreadNum;
    private final Splitter splitter;
    private volatile IteratorLongStream sequential;
    private volatile Stream<Long> boxed;

    ParallelIteratorLongStream(ImmutableLongIterator values, Collection<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
        this.maxThreadNum = N.min(maxThreadNum, THREAD_POOL_SIZE);
        this.splitter = splitter == null ? DEFAULT_SPILTTER : splitter;
        this.sequential = new IteratorLongStream(this.elements, this.closeHandlers, this.sorted);
    }

    ParallelIteratorLongStream(LongStream stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(stream.longIterator(), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    ParallelIteratorLongStream(Stream<Long> stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(longIterator(stream.iterator()), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream filter(final LongPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().filter(predicate, max).longIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Long> stream = boxed().filter(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().takeWhile(predicate, max).longIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Long> stream = boxed().takeWhile(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().dropWhile(predicate, max).longIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Long> stream = boxed().dropWhile(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream map(final LongUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().map(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Long>() {
            @Override
            public long applyAsLong(Long value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream mapToInt(final LongToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Long>() {
            @Override
            public int applyAsInt(Long value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Long>() {
            @Override
            public float applyAsFloat(Long value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Long>() {
            @Override
            public double applyAsDouble(Long value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().map(new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public LongStream flatMap(final LongFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMap(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Long, LongStream>() {
            @Override
            public LongStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Long, IntStream>() {
            @Override
            public IntStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Long, FloatStream>() {
            @Override
            public FloatStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Long, DoubleStream>() {
            @Override
            public DoubleStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().flatMap(new Function<Long, Stream<T>>() {
            @Override
            public Stream<T> apply(Long value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<LongStream> split(final int size) {
        return new ParallelIteratorStream<LongStream>(new ImmutableIterator<LongStream>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public LongStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final long[] a = new long[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayLongStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<LongStream> split(final U boundary, final BiFunction<? super Long, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new ParallelIteratorStream<LongStream>(new ImmutableIterator<LongStream>() {
            private long next;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public LongStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final LongList result = LongList.of(N.EMPTY_LONG_ARRAY);

                if (hasNext == false) {
                    next = elements.next();
                    hasNext = true;
                }

                while (hasNext) {
                    if (predicate.apply(next, boundary)) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return LongStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public LongStream distinct() {
        final Set<Long> set = new LinkedHashSet<>();

        while (elements.hasNext()) {
            set.add(elements.next());
        }

        final long[] a = new long[set.size()];
        final Iterator<Long> iter = set.iterator();

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = iter.next();
        }

        return new ParallelArrayLongStream(a, 0, a.length, closeHandlers, sorted, maxThreadNum, splitter);

        //        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
        //            private Iterator<Long> distinctIter;
        //
        //            @Override
        //            public boolean hasNext() {
        //                if (distinctIter == null) {
        //                    removeDuplicated();
        //                }
        //
        //                return distinctIter.hasNext();
        //            }
        //
        //            @Override
        //            public long next() {
        //                if (distinctIter == null) {
        //                    removeDuplicated();
        //                }
        //
        //                return distinctIter.next();
        //            }
        //
        //            private void removeDuplicated() {
        //                final Set<Long> set = new LinkedHashSet<>();
        //
        //                while (elements.hasNext()) {
        //                    set.add(elements.next());
        //                }
        //
        //                distinctIter = set.iterator();
        //            }
        //
        //        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream top(int n) {
        return top(n, LONG_COMPARATOR);
    }

    @Override
    public LongStream top(int n, Comparator<? super Long> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToLong(new ToLongFunction<Long>() {
            @Override
            public long applyAsLong(Long value) {
                return value;
            }
        });
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
            long[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public long next() {
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
            public long[] toArray() {
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

                N.parallelSort(a);
            }
        }, closeHandlers, true, maxThreadNum, splitter);
    }

    @Override
    public LongStream peek(final LongConsumer action) {
        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long next() {
                final long next = elements.next();

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
            //    public long[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public long next() {
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
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
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
            public long next() {
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
            public long[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public void forEach(final LongConsumer action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

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
                        setError(eHolder, e);
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

    //    @Override
    //    public boolean forEach2(final LongFunction<Boolean> action) {
    //        if (maxThreadNum <= 1) {
    //            return sequential().forEach2(action);
    //        }
    //
    //        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
    //        final Holder<Throwable> eHolder = new Holder<>();
    //        final MutableBoolean result = MutableBoolean.of(true);
    //
    //        for (int i = 0; i < maxThreadNum; i++) {
    //            futureList.add(asyncExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    long next = 0;
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
    //                        setError(eHolder, e);
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
    public long[] toArray() {
        return elements.toArray();
    }

    @Override
    public LongList toLongList() {
        return LongList.of(toArray());
    }

    @Override
    public List<Long> toList() {
        final List<Long> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Long> toList(Supplier<? extends List<Long>> supplier) {
        final List<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Long> toSet() {
        final Set<Long> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Long> toSet(Supplier<? extends Set<Long>> supplier) {
        final Set<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        final Multiset<Long> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset(Supplier<? extends Multiset<Long>> supplier) {
        final Multiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset() {
        final LongMultiset<Long> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset(Supplier<? extends LongMultiset<Long>> supplier) {
        final LongMultiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Long>> toMap(LongFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Long>>>() {
            @Override
            public Map<K, List<Long>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Long>>> M toMap(LongFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Long, ?, List<Long>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(LongFunction<? extends K> classifier, Collector<Long, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final LongFunction<? extends K> classifier, final Collector<Long, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Long, ? extends K> classifier2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final LongFunction<? extends K> keyMapper, final LongFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
        }

        final Function<? super Long, ? extends K> keyMapper2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Long, ? extends U> valueMapper2 = new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final LongFunction<? extends K> keyMapper, final LongFunction<? extends U> valueMapper,
            final Supplier<Multimap<K, U, V>> mapSupplier) {

        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapSupplier);
        }

        final Function<? super Long, ? extends K> keyMapper2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Long, ? extends U> valueMapper2 = new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapSupplier);
    }

    @Override
    public long reduce(final long identity, final LongBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    long result = identity;
                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = op.applyAsLong(result, next);
                        }
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }

                    return result;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        Long result = null;

        try {
            for (CompletableFuture<Long> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsLong(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalLong reduce(final LongBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    long result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.next();
                        } else {
                            return null;
                        }
                    }

                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.applyAsLong(result, next);
                        }
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }

                    return result;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        Long result = null;

        try {
            for (CompletableFuture<Long> future : futureList) {
                final Long tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsLong(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalLong.empty() : OptionalLong.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjLongConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().collect(supplier, accumulator, combiner);
        }

        final List<CompletableFuture<R>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<R>() {
                @Override
                public R call() {
                    R container = supplier.get();
                    long next = 0;

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
                        setError(eHolder, e);
                    }

                    return container;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        R container = (R) NONE;

        try {
            for (CompletableFuture<R> future : futureList) {
                final R tmp = future.get();

                if (container == NONE) {
                    container = tmp;
                } else {
                    combiner.accept(container, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return container == NONE ? supplier.get() : container;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public OptionalLong min() {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        long candidate = elements.next();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong max() {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        long candidate = elements.next();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        final OptionalNullable<Long> optional = boxed().kthLargest(k, LONG_COMPARATOR);

        return optional.isPresent() ? OptionalLong.of(optional.get()) : OptionalLong.empty();
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
    public OptionalDouble average() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        return sequential().average();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public LongSummaryStatistics summarize() {
        final LongSummaryStatistics result = new LongSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public boolean anyMatch(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

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
                        setError(eHolder, e);
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
    public boolean allMatch(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

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
                        setError(eHolder, e);
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
    public boolean noneMatch(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

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
                        setError(eHolder, e);
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

    //    @Override
    //    public OptionalLong findFirst() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalLong findFirst(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Long, Long>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Long>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Pair<Long, Long>>() {
                @Override
                public Pair<Long, Long> call() {
                    final Pair<Long, Long> pair = new Pair<>();

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
                        setError(eHolder, e);
                    }

                    return pair;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Long, Long>> future : futureList) {
                final Pair<Long, Long> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalLong findLast() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalLong findLast(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Long, Long>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Long>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Pair<Long, Long>>() {
                @Override
                public Pair<Long, Long> call() {
                    final Pair<Long, Long> pair = new Pair<>();

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
                                    if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                        resultHolder.setValue(pair);
                                    }
                                }

                                break;
                            }
                        }
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }

                    return pair;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Long, Long>> future : futureList) {
                final Pair<Long, Long> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalLong findAny() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalLong findAny(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Object>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(NONE);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Object>() {
                @Override
                public Object call() {
                    long next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next)) {
                                synchronized (resultHolder) {
                                    if (resultHolder.value() == NONE) {
                                        resultHolder.setValue(next);
                                    }
                                }

                                break;
                            }
                        }
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }

                    return next;
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Object> future : futureList) {
                if (resultHolder.value() == NONE) {
                    future.get();
                } else {
                    break;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == NONE ? OptionalLong.empty() : OptionalLong.of((Long) resultHolder.value());
    }

    @Override
    public LongStream except(final Collection<?> c) {
        return new ParallelIteratorLongStream(this.sequential().except(c).longIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream intersect(final Collection<?> c) {
        return new ParallelIteratorLongStream(this.sequential().intersect(c).longIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream xor(final Collection<Long> c) {
        return new ParallelIteratorLongStream(this.sequential().xor(c).longIterator(), closeHandlers, false, maxThreadNum, splitter);
    }

    //    @Override
    //    public LongStream exclude(final Collection<?> c) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorLongStream(sequential().exclude(c).longIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    //        }
    //
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new LongPredicate() {
    //            @Override
    //            public boolean test(long value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public FloatStream asFloatStream() {
        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                return elements.next();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new ParallelIteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                return elements.next();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public Stream<Long> boxed() {
        Stream<Long> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Long>(iterator(), closeHandlers, sorted, sorted ? LONG_COMPARATOR : null, maxThreadNum, splitter);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public LongStream append(final LongStream stream) {
        return new ParallelIteratorLongStream(LongStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream merge(final LongStream b, final LongBiFunction<Nth> nextSelector) {
        return new ParallelIteratorLongStream(LongStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream zipWith(LongStream b, LongBiFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, LongTriFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream zipWith(LongStream b, long valueForNoneA, long valueForNoneB, LongBiFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, long valueForNoneA, long valueForNoneB, long valueForNoneC, LongTriFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitter);
    }

    @Override
    public ImmutableIterator<Long> iterator() {
        return this.sequential().iterator();
    }

    @Override
    public ImmutableLongIterator longIterator() {
        return this.sequential().longIterator();
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public LongStream sequential() {
        IteratorLongStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorLongStream(elements, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public LongStream parallel(int maxThreadNum, Splitter splitter) {
        if (this.maxThreadNum == maxThreadNum && this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public LongStream maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public BaseStream.Splitter splitter() {
        return splitter;
    }

    @Override
    public LongStream splitter(BaseStream.Splitter splitter) {
        if (this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorLongStream(elements, newCloseHandlers, sorted, maxThreadNum, splitter);
    }
}
