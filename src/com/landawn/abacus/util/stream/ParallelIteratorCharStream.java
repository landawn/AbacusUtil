package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
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
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharBinaryOperator;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharToIntFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ParallelIteratorCharStream extends AbstractCharStream {
    private final ImmutableCharIterator elements;
    private final boolean sorted;
    private final int maxThreadNum;
    private final Splitter splitter;
    private volatile IteratorCharStream sequential;
    private volatile Stream<Character> boxed;

    ParallelIteratorCharStream(ImmutableCharIterator values, Collection<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
        this.maxThreadNum = N.min(maxThreadNum, Stream.THREAD_POOL_SIZE);
        this.splitter = splitter == null ? Stream.DEFAULT_SPILTTER : splitter;
        this.sequential = new IteratorCharStream(this.elements, this.closeHandlers, this.sorted);
    }

    ParallelIteratorCharStream(CharStream stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(stream.charIterator(), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    ParallelIteratorCharStream(Stream<Character> stream, Set<Runnable> closeHandlers, boolean sorted, int maxThreadNum, Splitter splitter) {
        this(Stream.charIterator(stream.iterator()), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitter);
    }

    @Override
    public CharStream filter(final CharPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream filter(final CharPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().filter(predicate, max).charIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Character> stream = boxed().filter(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().takeWhile(predicate, max).charIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Character> stream = boxed().takeWhile(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().dropWhile(predicate, max).charIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Character> stream = boxed().dropWhile(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return predicate.test(value);
            }
        }, max);

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public CharStream map(final CharUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().map(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final CharStream stream = boxed().mapToChar(new ToCharFunction<Character>() {
            @Override
            public char applyAsChar(Character value) {
                return mapper.applyAsChar(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream mapToInt(final CharToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Character>() {
            @Override
            public int applyAsInt(Character value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().map(new Function<Character, U>() {
            @Override
            public U apply(Character value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public CharStream flatMap(final CharFunction<? extends CharStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMap(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final CharStream stream = boxed().flatMapToChar(new Function<Character, CharStream>() {
            @Override
            public CharStream apply(Character value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt(final CharFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Character, IntStream>() {
            @Override
            public IntStream apply(Character value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final CharFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().flatMap(new Function<Character, Stream<T>>() {
            @Override
            public Stream<T> apply(Character value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<CharStream> split(final int size) {
        return new ParallelIteratorStream<CharStream>(new ImmutableIterator<CharStream>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public CharStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final char[] a = new char[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayCharStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public CharStream distinct() {
        return new ParallelIteratorCharStream(new ImmutableCharIterator() {
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

        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorCharStream(new ImmutableCharIterator() {
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

                N.parallelSort(a);
            }
        }, closeHandlers, true, maxThreadNum, splitter);
    }

    @Override
    public CharStream peek(final CharConsumer action) {
        return new ParallelIteratorCharStream(new ImmutableCharIterator() {
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
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public CharStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new ParallelIteratorCharStream(new ImmutableCharIterator() {
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
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public CharStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorCharStream(new ImmutableCharIterator() {
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
            public void skip(long n2) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n2);
            }

            @Override
            public char[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public void forEach(final CharConsumer action) {
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
                    char next = 0;

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
    //    public boolean forEach2(final CharFunction<Boolean> action) {
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
    //                    char next = 0;
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
    public char[] toArray() {
        return elements.toArray();
    }

    @Override
    public CharList toCharList() {
        return CharList.of(toArray());
    }

    @Override
    public List<Character> toList() {
        final List<Character> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Character> toList(Supplier<? extends List<Character>> supplier) {
        final List<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Character> toSet() {
        final Set<Character> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Character> toSet(Supplier<? extends Set<Character>> supplier) {
        final Set<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset() {
        final Multiset<Character> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset(Supplier<? extends Multiset<Character>> supplier) {
        final Multiset<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset() {
        final LongMultiset<Character> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset(Supplier<? extends LongMultiset<Character>> supplier) {
        final LongMultiset<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Character>> toMap(CharFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Character>>>() {
            @Override
            public Map<K, List<Character>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Character>>> M toMap(CharFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Character, ?, List<Character>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(CharFunction<? extends K> classifier, Collector<Character, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final CharFunction<? extends K> classifier, final Collector<Character, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Character, ? extends K> classifier2 = new Function<Character, K>() {
            @Override
            public K apply(Character value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final CharFunction<? extends K> keyMapper, final CharFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
        }

        final Function<? super Character, ? extends K> keyMapper2 = new Function<Character, K>() {
            @Override
            public K apply(Character value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Character, ? extends U> valueMapper2 = new Function<Character, U>() {
            @Override
            public U apply(Character value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final CharFunction<? extends K> keyMapper, final CharFunction<? extends U> valueMapper,
            final Supplier<Multimap<K, U, V>> mapSupplier) {

        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapSupplier);
        }

        final Function<? super Character, ? extends K> keyMapper2 = new Function<Character, K>() {
            @Override
            public K apply(Character value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Character, ? extends U> valueMapper2 = new Function<Character, U>() {
            @Override
            public U apply(Character value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapSupplier);
    }

    @Override
    public char reduce(final char identity, final CharBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Character>() {
                @Override
                public Character call() {
                    char result = identity;
                    char next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = op.applyAsChar(result, next);
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

        Character result = null;

        try {
            for (CompletableFuture<Character> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsChar(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalChar reduce(final CharBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Character>() {
                @Override
                public Character call() {
                    char result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.next();
                        } else {
                            return null;
                        }
                    }

                    char next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.applyAsChar(result, next);
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

        Character result = null;

        try {
            for (CompletableFuture<Character> future : futureList) {
                final Character tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsChar(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalChar.empty() : OptionalChar.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjCharConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                    char next = 0;

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
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator) {
        if (Stream.logger.isWarnEnabled()) {
            Stream.logger.warn("'collect' is sequentially executed in parallel stream");
        }

        return sequential().collect(supplier, accumulator);
    }

    @Override
    public OptionalChar min() {
        if (elements.hasNext() == false) {
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
        if (elements.hasNext() == false) {
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

        final OptionalNullable<Character> optional = boxed().kthLargest(k, Stream.CHAR_COMPARATOR);

        return optional.isPresent() ? OptionalChar.of(optional.get()) : OptionalChar.empty();
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
    public CharSummaryStatistics summarize() {
        final CharSummaryStatistics result = new CharSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public Optional<Map<String, Character>> distribution() {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final char[] a = sorted().toArray();

        return Optional.of(N.distribution(a));
    }

    @Override
    public boolean anyMatch(final CharPredicate predicate) {
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
                    char next = 0;

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
    public boolean allMatch(final CharPredicate predicate) {
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
                    char next = 0;

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
    public boolean noneMatch(final CharPredicate predicate) {
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
                    char next = 0;

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
    //    public OptionalChar findFirst() {
    //        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalChar findFirst(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Long, Character>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Character>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, Character>>() {
                @Override
                public Pair<Long, Character> call() {
                    final Pair<Long, Character> pair = new Pair<>();

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
            for (CompletableFuture<Pair<Long, Character>> future : futureList) {
                final Pair<Long, Character> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalChar.empty() : OptionalChar.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalChar findLast() {
    //        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalChar findLast(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Long, Character>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Character>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Callable<Pair<Long, Character>>() {
                @Override
                public Pair<Long, Character> call() {
                    final Pair<Long, Character> pair = new Pair<>();

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
            for (CompletableFuture<Pair<Long, Character>> future : futureList) {
                final Pair<Long, Character> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalChar.empty() : OptionalChar.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalChar findAny() {
    //        return count() == 0 ? OptionalChar.empty() : OptionalChar.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalChar findAny(final CharPredicate predicate) {
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
                    char next = 0;

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

        return resultHolder.value() == Stream.NONE ? OptionalChar.empty() : OptionalChar.of((Character) resultHolder.value());
    }

    @Override
    public CharStream except(final Collection<?> c) {
        return new ParallelIteratorCharStream(this.sequential().except(c).charIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public CharStream intersect(final Collection<?> c) {
        return new ParallelIteratorCharStream(this.sequential().intersect(c).charIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    //    @Override
    //    public CharStream exclude(final Collection<?> c) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorCharStream(sequential().exclude(c).charIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    //        }
    //
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new CharPredicate() {
    //            @Override
    //            public boolean test(char value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

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
    public Stream<Character> boxed() {
        Stream<Character> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Character>(iterator(), closeHandlers, sorted, sorted ? Stream.CHAR_COMPARATOR : null, maxThreadNum, splitter);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public CharStream append(final CharStream stream) {
        return new ParallelIteratorCharStream(CharStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public CharStream merge(final CharStream b, final CharBiFunction<Nth> nextSelector) {
        return new ParallelIteratorCharStream(CharStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ImmutableIterator<Character> iterator() {
        return this.sequential().iterator();
    }

    @Override
    public ImmutableCharIterator charIterator() {
        return this.sequential().charIterator();
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public CharStream sequential() {
        IteratorCharStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorCharStream(elements, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public CharStream parallel(int maxThreadNum, Splitter splitter) {
        if (this.maxThreadNum == maxThreadNum && this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public CharStream maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public BaseStream.Splitter splitter() {
        return splitter;
    }

    @Override
    public CharStream splitter(BaseStream.Splitter splitter) {
        if (this.splitter == splitter) {
            return this;
        }

        return new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorCharStream(elements, newCloseHandlers, sorted, maxThreadNum, splitter);
    }
}
