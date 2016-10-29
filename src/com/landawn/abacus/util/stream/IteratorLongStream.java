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

import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.LongBinaryOperator;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.LongToDoubleFunction;
import com.landawn.abacus.util.function.LongToFloatFunction;
import com.landawn.abacus.util.function.LongToIntFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IteratorLongStream extends AbstractLongStream {
    private final ImmutableLongIterator elements;
    private final boolean sorted;

    IteratorLongStream(ImmutableLongIterator values) {
        this(values, null);
    }

    IteratorLongStream(ImmutableLongIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorLongStream(ImmutableLongIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
    }

    @Override
    public LongStream filter(LongPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream filter(final LongPredicate predicate, final long max) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private boolean hasNext = false;
            private long next = 0;
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
            public long next() {
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
    public LongStream takeWhile(LongPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate, final long max) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private boolean hasNext = false;
            private long next = 0;
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
            public long next() {
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
    public LongStream dropWhile(LongPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate, final long max) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private boolean hasNext = false;
            private long next = 0;
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
            public long next() {
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
    public LongStream map(final LongUnaryOperator mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long next() {
                return mapper.applyAsLong(elements.next());
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
    public IntStream mapToInt(final LongToIntFunction mapper) {
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
        }, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                return mapper.applyAsFloat(elements.next());
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
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                return mapper.applyAsDouble(elements.next());
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
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
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
    public LongStream flatMap(final LongFunction<? extends LongStream> mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private ImmutableLongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).longIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
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
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private ImmutableFloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).floatIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private ImmutableDoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).doubleIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
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
    public Stream<LongStream> split(final int size) {
        return new IteratorStream<LongStream>(new ImmutableIterator<LongStream>() {

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

        }, closeHandlers);
    }

    @Override
    public Stream<LongStream> split(final LongPredicate predicate) {
        return new IteratorStream<LongStream>(new ImmutableIterator<LongStream>() {
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
                    if (predicate.test(next)) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        break;
                    }
                }

                return LongStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public LongStream distinct() {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private Iterator<Long> distinctIter;

            @Override
            public boolean hasNext() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.hasNext();
            }

            @Override
            public long next() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.next();
            }

            private void removeDuplicated() {
                final Set<Long> set = new LinkedHashSet<>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                distinctIter = set.iterator();
            }

        }, closeHandlers, sorted);
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
                return value.longValue();
            }
        });
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorLongStream(new ImmutableLongIterator() {
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

                N.sort(a);
            }
        }, closeHandlers, true);
    }

    //    @Override
    //    public LongStream parallelSorted() {
    //        if (sorted) {
    //            return this;
    //        }
    //
    //        return new IteratorLongStream(new ImmutableLongIterator() {
    //            long[] a = null;
    //            int cursor = 0;
    //
    //            @Override
    //            public boolean hasNext() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                return cursor < a.length;
    //            }
    //
    //            @Override
    //            public long next() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                if (cursor >= a.length) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return a[cursor++];
    //            }
    //
    //            @Override
    //            public long count() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                return a.length - cursor;
    //            }
    //
    //            @Override
    //            public void skip(long n) {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
    //            }
    //
    //            @Override
    //            public long[] toArray() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                if (cursor == 0) {
    //                    return a;
    //                } else {
    //                    return N.copyOfRange(a, cursor, a.length);
    //                }
    //            }
    //
    //            private void parallelSort() {
    //                a = elements.toArray();
    //
    //                N.parallelSort(a);
    //            }
    //        }, closeHandlers, true);
    //    }

    @Override
    public LongStream peek(final LongConsumer action) {
        return new IteratorLongStream(new ImmutableLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public LongStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new IteratorLongStream(new ImmutableLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public LongStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorLongStream(new ImmutableLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(LongConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    //    @Override
    //    public boolean forEach2(LongFunction<Boolean> action) {
    //        while (elements.hasNext()) {
    //            if (action.apply(elements.next()).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
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
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Long> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        long element = 0;

        while (elements.hasNext()) {
            element = elements.next();

            key = N.requireNonNull(classifier.apply(element), "element cannot be mapped to a null key");
            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, element);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        Collectors.replaceAll(intermediate, function);

        return result;
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
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        long element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        long element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public long reduce(long identity, LongBinaryOperator op) {
        long result = identity;

        while (elements.hasNext()) {
            result = op.applyAsLong(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalLong reduce(LongBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        long result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsLong(result, elements.next());
        }

        return OptionalLong.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
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

        long sum = 0;
        long count = 0;

        while (elements.hasNext()) {
            sum += elements.next();
            count++;
        }

        return OptionalDouble.of(((double) sum) / count);
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
    public boolean anyMatch(LongPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(LongPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(LongPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalLong findFirst() {
    //        return elements.hasNext() ? OptionalLong.empty() : OptionalLong.of(elements.next());
    //    }

    @Override
    public OptionalLong findFirst(LongPredicate predicate) {
        while (elements.hasNext()) {
            long e = elements.next();

            if (predicate.test(e)) {
                return OptionalLong.of(e);
            }
        }

        return OptionalLong.empty();
    }

    //    @Override
    //    public OptionalLong findLast() {
    //        if (elements.hasNext() == false) {
    //            return OptionalLong.empty();
    //        }
    //
    //        long e = 0;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return OptionalLong.of(e);
    //    }

    @Override
    public OptionalLong findLast(LongPredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        boolean hasResult = false;
        long e = 0;
        long result = 0;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalLong.of(result) : OptionalLong.empty();
    }

    //    @Override
    //    public OptionalLong findAny() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements.next());
    //    }

    @Override
    public OptionalLong findAny(LongPredicate predicate) {
        while (elements.hasNext()) {
            long e = elements.next();

            if (predicate.test(e)) {
                return OptionalLong.of(e);
            }
        }

        return OptionalLong.empty();
    }

    @Override
    public LongStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public LongStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    //    @Override
    //    public LongStream exclude(Collection<?> c) {
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
        return new IteratorFloatStream(new ImmutableFloatIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Long> boxed() {
        return new IteratorStream<Long>(iterator(), closeHandlers, sorted, sorted ? LONG_COMPARATOR : null);
    }

    @Override
    public ImmutableIterator<Long> iterator() {
        return new ImmutableIterator<Long>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Long next() {
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
        };
    }

    @Override
    public ImmutableLongIterator longIterator() {
        return elements;
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public LongStream sequential() {
        return this;
    }

    @Override
    public LongStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
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

        return new IteratorLongStream(elements, newCloseHandlers, sorted);
    }
}
