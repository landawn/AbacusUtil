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
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortBinaryOperator;
import com.landawn.abacus.util.function.ShortConsumer;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortToIntFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ParallelIteratorShortStream extends AbstractShortStream {
    private final ImmutableShortIterator elements;
    private final boolean sorted;
    private final int maxThreadNum;
    private final Splitter splitter;
    private volatile IteratorShortStream sequential;
    private volatile Stream<Short> boxed;

    ParallelIteratorShortStream(ImmutableShortIterator values, Collection<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
        this.maxThreadNum = N.min(maxThreadNum, Stream.THREAD_POOL_SIZE);
        this.splitter = splitter == null ? Stream.DEFAULT_SPILTTER : splitter;
        this.sequential = new IteratorShortStream(this.elements, this.closeHandlers, this.sorted);
    }

    ParallelIteratorShortStream(ShortStream stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(stream.shortIterator(), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    ParallelIteratorShortStream(Stream<Short> stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(Stream.shortIterator(stream.iterator()), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().filter(predicate, max).shortIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Short> stream = boxed().filter(new Predicate<Short>() {
            @Override
            public boolean test(Short value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorShortStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().takeWhile(predicate, max).shortIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Short> stream = boxed().takeWhile(new Predicate<Short>() {
            @Override
            public boolean test(Short value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorShortStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().dropWhile(predicate, max).shortIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Short> stream = boxed().dropWhile(new Predicate<Short>() {
            @Override
            public boolean test(Short value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorShortStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream map(final ShortUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().map(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final ShortStream stream = boxed().mapToShort(new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return mapper.applyAsShort(value);
            }
        });

        return new ParallelIteratorShortStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream mapToInt(final ShortToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Short>() {
            @Override
            public int applyAsInt(Short value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<U> mapToObj(final ShortFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().map(new Function<Short, U>() {
            @Override
            public U apply(Short value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public ShortStream flatMap(final ShortFunction<? extends ShortStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().flatMap(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final ShortStream stream = boxed().flatMapToShort(new Function<Short, ShortStream>() {
            @Override
            public ShortStream apply(Short value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorShortStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt(final ShortFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Short, IntStream>() {
            @Override
            public IntStream apply(Short value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ShortFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().flatMap(new Function<Short, Stream<T>>() {
            @Override
            public Stream<T> apply(Short value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<ShortStream> split(final int size) {
        return new ParallelIteratorStream<ShortStream>(new ImmutableIterator<ShortStream>() {
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

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public ShortStream distinct() {
        return new ParallelIteratorShortStream(new ImmutableShortIterator() {
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

        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ShortStream top(int n) {
        return top(n, Stream.SHORT_COMPARATOR);
    }

    @Override
    public ShortStream top(int n, Comparator<? super Short> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToShort(new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value;
            }
        });
    }

    @Override
    public ShortStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorShortStream(new ImmutableShortIterator() {
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

                N.parallelSort(a);
            }
        }, closeHandlers, true, maxThreadNum, splitter);
    }

    @Override
    public ShortStream peek(final ShortConsumer action) {
        return new ParallelIteratorShortStream(new ImmutableShortIterator() {
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
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ShortStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new ParallelIteratorShortStream(new ImmutableShortIterator() {
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
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ShortStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorShortStream(new ImmutableShortIterator() {
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
            public void skip(long n2) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n2);
            }

            @Override
            public short[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public void forEach(final ShortConsumer action) {
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
                    short next = 0;

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
    public boolean forEach2(final ShortFunction<Boolean> action) {
        if (maxThreadNum <= 1) {
            return sequential().forEach2(action);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    short next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (action.apply(next) == false) {
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
    public short[] toArray() {
        return elements.toArray();
    }

    @Override
    public ShortList toShortList() {
        return ShortList.of(toArray());
    }

    @Override
    public List<Short> toList() {
        final List<Short> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Short> toList(Supplier<? extends List<Short>> supplier) {
        final List<Short> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Short> toSet() {
        final Set<Short> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Short> toSet(Supplier<? extends Set<Short>> supplier) {
        final Set<Short> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset() {
        final Multiset<Short> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset(Supplier<? extends Multiset<Short>> supplier) {
        final Multiset<Short> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset() {
        final LongMultiset<Short> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset(Supplier<? extends LongMultiset<Short>> supplier) {
        final LongMultiset<Short> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Short>> toMap(ShortFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Short>>>() {
            @Override
            public Map<K, List<Short>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Short>>> M toMap(ShortFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Short, ?, List<Short>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(ShortFunction<? extends K> classifier, Collector<Short, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final ShortFunction<? extends K> classifier, final Collector<Short, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Short, ? extends K> classifier2 = new Function<Short, K>() {
            @Override
            public K apply(Short value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final ShortFunction<? extends K> keyMapper, final ShortFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
        }

        final Function<? super Short, ? extends K> keyMapper2 = new Function<Short, K>() {
            @Override
            public K apply(Short value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Short, ? extends U> valueMapper2 = new Function<Short, U>() {
            @Override
            public U apply(Short value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final ShortFunction<? extends K> keyMapper,
            final ShortFunction<? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapSupplier) {

        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapSupplier);
        }

        final Function<? super Short, ? extends K> keyMapper2 = new Function<Short, K>() {
            @Override
            public K apply(Short value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Short, ? extends U> valueMapper2 = new Function<Short, U>() {
            @Override
            public U apply(Short value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapSupplier);
    }

    @Override
    public short reduce(final short identity, final ShortBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Short>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Short>() {
                @Override
                public Short call() {
                    short result = identity;
                    short next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = op.applyAsShort(result, next);
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

        Short result = null;

        try {
            for (CompletableFuture<Short> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsShort(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public OptionalShort reduce(final ShortBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Short>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Short>() {
                @Override
                public Short call() {
                    short result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.next();
                        } else {
                            return null;
                        }
                    }

                    short next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.applyAsShort(result, next);
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

        Short result = null;

        try {
            for (CompletableFuture<Short> future : futureList) {
                final Short tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsShort(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalShort.empty() : OptionalShort.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjShortConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                    short next = 0;

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

        return container;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator) {
        if (Stream.logger.isWarnEnabled()) {
            Stream.logger.warn("'collect' is sequentially executed in parallel stream");
        }

        return sequential().collect(supplier, accumulator);
    }

    @Override
    public OptionalShort min() {
        if (elements.hasNext() == false) {
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
        if (elements.hasNext() == false) {
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

        final Optional<Short> optional = boxed().kthLargest(k, Stream.SHORT_COMPARATOR);

        return optional.isPresent() ? OptionalShort.of(optional.get()) : OptionalShort.empty();
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
    public ShortSummaryStatistics summarize() {
        final ShortSummaryStatistics result = new ShortSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public Optional<Map<String, Short>> distribution() {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final short[] a = sorted().toArray();

        return Optional.of(N.distribution(a));
    }

    @Override
    public boolean anyMatch(final ShortPredicate predicate) {
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
                    short next = 0;

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
    public boolean allMatch(final ShortPredicate predicate) {
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
                    short next = 0;

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
    public boolean noneMatch(final ShortPredicate predicate) {
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
                    short next = 0;

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
    //    public OptionalShort findFirst() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalShort findFirst(final ShortPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Long, Short>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Short>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, Short>>() {
                @Override
                public Pair<Long, Short> call() {
                    final Pair<Long, Short> pair = new Pair<>();

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
            for (CompletableFuture<Pair<Long, Short>> future : futureList) {
                final Pair<Long, Short> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalShort.empty() : OptionalShort.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalShort findLast() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalShort findLast(final ShortPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Long, Short>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Short>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, Short>>() {
                @Override
                public Pair<Long, Short> call() {
                    final Pair<Long, Short> pair = new Pair<>();

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
            for (CompletableFuture<Pair<Long, Short>> future : futureList) {
                final Pair<Long, Short> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalShort.empty() : OptionalShort.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalShort findAny() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalShort findAny(final ShortPredicate predicate) {
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
                    short next = 0;

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

        return resultHolder.value() == Stream.NONE ? OptionalShort.empty() : OptionalShort.of((Short) resultHolder.value());
    }

    @Override
    public IntStream asIntStream() {
        return new ParallelIteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
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
    public Stream<Short> boxed() {
        Stream<Short> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Short>(iterator(), closeHandlers, sorted, sorted ? Stream.SHORT_COMPARATOR : null, maxThreadNum, splitter);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public ShortStream append(final ShortStream stream) {
        return new ParallelIteratorShortStream(ShortStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ShortStream merge(final ShortStream b, final ShortBiFunction<Nth> nextSelector) {
        return new ParallelIteratorShortStream(ShortStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ImmutableIterator<Short> iterator() {
        return this.sequential().iterator();
    }

    @Override
    public ImmutableShortIterator shortIterator() {
        return this.sequential().shortIterator();
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public ShortStream sequential() {
        IteratorShortStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorShortStream(elements, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public ShortStream parallel(int maxThreadNum, Splitter splitter) {
        if (this.maxThreadNum == maxThreadNum && this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorShortStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public ShortStream maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorShortStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public BaseStream.Splitter splitter() {
        return splitter;
    }

    @Override
    public ShortStream splitter(BaseStream.Splitter splitter) {
        if (this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorShortStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ShortStream onClose(Runnable closeHandler) {
        final List<Runnable> newCloseHandlers = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorShortStream(elements, newCloseHandlers, sorted, maxThreadNum, splitter);
    }
}
