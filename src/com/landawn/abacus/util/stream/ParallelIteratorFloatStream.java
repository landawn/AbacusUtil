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
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatBinaryOperator;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.FloatToDoubleFunction;
import com.landawn.abacus.util.function.FloatToIntFunction;
import com.landawn.abacus.util.function.FloatToLongFunction;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjFloatConsumer;
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
final class ParallelIteratorFloatStream extends AbstractFloatStream {
    private final ImmutableFloatIterator elements;
    private final boolean sorted;
    private final int maxThreadNum;
    private final Splitter splitter;
    private volatile IteratorFloatStream sequential;
    private volatile Stream<Float> boxed;

    ParallelIteratorFloatStream(ImmutableFloatIterator values, Collection<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
        this.maxThreadNum = N.min(maxThreadNum, Stream.THREAD_POOL_SIZE);
        this.splitter = splitter == null ? Stream.DEFAULT_SPILTTER : splitter;
        this.sequential = new IteratorFloatStream(this.elements, this.closeHandlers, this.sorted);
    }

    ParallelIteratorFloatStream(FloatStream stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(stream.floatIterator(), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    ParallelIteratorFloatStream(Stream<Float> stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(Stream.floatIterator(stream.iterator()), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    @Override
    public FloatStream filter(final FloatPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public FloatStream filter(final FloatPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().filter(predicate, max).floatIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Float> stream = boxed().filter(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream takeWhile(final FloatPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public FloatStream takeWhile(final FloatPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().takeWhile(predicate, max).floatIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Float> stream = boxed().takeWhile(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream dropWhile(final FloatPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public FloatStream dropWhile(final FloatPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().dropWhile(predicate, max).floatIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Float> stream = boxed().dropWhile(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream map(final FloatUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().map(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Float>() {
            @Override
            public float applyAsFloat(Float value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream mapToInt(final FloatToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Float>() {
            @Override
            public int applyAsInt(Float value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream mapToLong(final FloatToLongFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Float>() {
            @Override
            public long applyAsLong(Float value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream mapToDouble(final FloatToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Float>() {
            @Override
            public double applyAsDouble(Float value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<U> mapToObj(final FloatFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().map(new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public FloatStream flatMap(final FloatFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMap(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Float, FloatStream>() {
            @Override
            public FloatStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt(final FloatFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Float, IntStream>() {
            @Override
            public IntStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public LongStream flatMapToLong(final FloatFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Float, LongStream>() {
            @Override
            public LongStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream flatMapToDouble(final FloatFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Float, DoubleStream>() {
            @Override
            public DoubleStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final FloatFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().flatMap(new Function<Float, Stream<T>>() {
            @Override
            public Stream<T> apply(Float value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<FloatStream> split(final int size) {
        return new ParallelIteratorStream<FloatStream>(new ImmutableIterator<FloatStream>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public FloatStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final float[] a = new float[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayFloatStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public Stream<FloatStream> split(final FloatPredicate predicate) {
        return new ParallelIteratorStream<FloatStream>(new ImmutableIterator<FloatStream>() {
            private float next;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public FloatStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final FloatList result = FloatList.of(N.EMPTY_FLOAT_ARRAY);

                if (hasNext == false) {
                    next = elements.next();
                    hasNext = true;
                }

                while (hasNext) {
                    if (predicate.test(next)) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        break;
                    }
                }

                return FloatStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public FloatStream distinct() {
        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
            private Iterator<Float> distinctIter;

            @Override
            public boolean hasNext() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.hasNext();
            }

            @Override
            public float next() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.next();
            }

            private void removeDuplicated() {
                final Set<Float> set = new LinkedHashSet<>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                distinctIter = set.iterator();
            }

        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public FloatStream top(int n) {
        return top(n, Stream.FLOAT_COMPARATOR);
    }

    @Override
    public FloatStream top(int n, Comparator<? super Float> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToFloat(new ToFloatFunction<Float>() {
            @Override
            public float applyAsFloat(Float value) {
                return value;
            }
        });
    }

    @Override
    public FloatStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
            float[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public float next() {
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
            public float[] toArray() {
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
    public FloatStream peek(final FloatConsumer action) {
        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                final float next = elements.next();

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
            //    public float[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public FloatStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public float next() {
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
    public FloatStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
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
            public float next() {
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
            public float[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public void forEach(final FloatConsumer action) {
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
                    float next = 0;

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

    //    @Override
    //    public boolean forEach2(final FloatFunction<Boolean> action) {
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
    //                    float next = 0;
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
    public float[] toArray() {
        return elements.toArray();
    }

    @Override
    public FloatList toFloatList() {
        return FloatList.of(toArray());
    }

    @Override
    public List<Float> toList() {
        final List<Float> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Float> toList(Supplier<? extends List<Float>> supplier) {
        final List<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Float> toSet() {
        final Set<Float> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Float> toSet(Supplier<? extends Set<Float>> supplier) {
        final Set<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset() {
        final Multiset<Float> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset(Supplier<? extends Multiset<Float>> supplier) {
        final Multiset<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset() {
        final LongMultiset<Float> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset(Supplier<? extends LongMultiset<Float>> supplier) {
        final LongMultiset<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Float>> toMap(FloatFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Float>>>() {
            @Override
            public Map<K, List<Float>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Float>>> M toMap(FloatFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Float, ?, List<Float>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(FloatFunction<? extends K> classifier, Collector<Float, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final FloatFunction<? extends K> classifier, final Collector<Float, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Float, ? extends K> classifier2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(FloatFunction<? extends K> keyMapper, FloatFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(FloatFunction<? extends K> keyMapper, FloatFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(FloatFunction<? extends K> keyMapper, FloatFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
        }

        final Function<? super Float, ? extends K> keyMapper2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Float, ? extends U> valueMapper2 = new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(FloatFunction<? extends K> keyMapper, FloatFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final FloatFunction<? extends K> keyMapper,
            final FloatFunction<? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapSupplier) {

        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapSupplier);
        }

        final Function<? super Float, ? extends K> keyMapper2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Float, ? extends U> valueMapper2 = new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapSupplier);
    }

    @Override
    public float reduce(final float identity, final FloatBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Float>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Float>() {
                @Override
                public Float call() {
                    float result = identity;
                    float next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = op.applyAsFloat(result, next);
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

        Float result = null;

        try {
            for (CompletableFuture<Float> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsFloat(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalFloat reduce(final FloatBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Float>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Float>() {
                @Override
                public Float call() {
                    float result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.next();
                        } else {
                            return null;
                        }
                    }

                    float next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.applyAsFloat(result, next);
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

        Float result = null;

        try {
            for (CompletableFuture<Float> future : futureList) {
                final Float tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsFloat(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalFloat.empty() : OptionalFloat.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjFloatConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                    float next = 0;

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
    public <R> R collect(Supplier<R> supplier, ObjFloatConsumer<R> accumulator) {
        if (Stream.logger.isWarnEnabled()) {
            Stream.logger.warn("'collect' is sequentially executed in parallel stream");
        }

        return sequential().collect(supplier, accumulator);
    }

    @Override
    public OptionalFloat min() {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float candidate = elements.next();
        float next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat max() {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float candidate = elements.next();
        float next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        final OptionalNullable<Float> optional = boxed().kthLargest(k, Stream.FLOAT_COMPARATOR);

        return optional.isPresent() ? OptionalFloat.of(optional.get()) : OptionalFloat.empty();
    }

    @Override
    public Double sum() {
        return sequential().sum();
    }

    @Override
    public OptionalDouble average() {
        return sequential().average();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public FloatSummaryStatistics summarize() {
        final FloatSummaryStatistics result = new FloatSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public Optional<Map<String, Float>> distribution() {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final float[] a = sorted().toArray();

        return Optional.of(N.distribution(a));
    }

    @Override
    public boolean anyMatch(final FloatPredicate predicate) {
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
                    float next = 0;

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
    public boolean allMatch(final FloatPredicate predicate) {
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
                    float next = 0;

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
    public boolean noneMatch(final FloatPredicate predicate) {
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
                    float next = 0;

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

    //    @Override
    //    public OptionalFloat findFirst() {
    //        return count() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalFloat findFirst(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Long, Float>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Float>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, Float>>() {
                @Override
                public Pair<Long, Float> call() {
                    final Pair<Long, Float> pair = new Pair<>();

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
            for (CompletableFuture<Pair<Long, Float>> future : futureList) {
                final Pair<Long, Float> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalFloat.empty() : OptionalFloat.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalFloat findLast() {
    //        return count() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalFloat findLast(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Long, Float>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Float>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, Float>>() {
                @Override
                public Pair<Long, Float> call() {
                    final Pair<Long, Float> pair = new Pair<>();

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
            for (CompletableFuture<Pair<Long, Float>> future : futureList) {
                final Pair<Long, Float> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalFloat.empty() : OptionalFloat.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalFloat findAny() {
    //        return count() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalFloat findAny(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Object>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(Stream.NONE);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Object>() {
                @Override
                public Object call() {
                    float next = 0;

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
            for (CompletableFuture<Object> future : futureList) {
                if (resultHolder.value() == Stream.NONE) {
                    future.get();
                } else {
                    break;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == Stream.NONE ? OptionalFloat.empty() : OptionalFloat.of((Float) resultHolder.value());
    }

    @Override
    public FloatStream except(final Collection<?> c) {
        return new ParallelIteratorFloatStream(this.sequential().except(c).floatIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public FloatStream intersect(final Collection<?> c) {
        return new ParallelIteratorFloatStream(this.sequential().intersect(c).floatIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    //    @Override
    //    public FloatStream exclude(final Collection<?> c) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorFloatStream(sequential().exclude(c).floatIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    //        }
    //
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new FloatPredicate() {
    //            @Override
    //            public boolean test(float value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

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
    public Stream<Float> boxed() {
        Stream<Float> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Float>(iterator(), closeHandlers, sorted, sorted ? Stream.FLOAT_COMPARATOR : null, maxThreadNum, splitter);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public FloatStream append(final FloatStream stream) {
        return new ParallelIteratorFloatStream(FloatStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public FloatStream merge(final FloatStream b, final FloatBiFunction<Nth> nextSelector) {
        return new ParallelIteratorFloatStream(FloatStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ImmutableIterator<Float> iterator() {
        return this.sequential().iterator();
    }

    @Override
    public ImmutableFloatIterator floatIterator() {
        return this.sequential().floatIterator();
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public FloatStream sequential() {
        IteratorFloatStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorFloatStream(elements, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public FloatStream parallel(int maxThreadNum, Splitter splitter) {
        if (this.maxThreadNum == maxThreadNum && this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public FloatStream maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public BaseStream.Splitter splitter() {
        return splitter;
    }

    @Override
    public FloatStream splitter(BaseStream.Splitter splitter) {
        if (this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public FloatStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorFloatStream(elements, newCloseHandlers, sorted, maxThreadNum, splitter);
    }
}
