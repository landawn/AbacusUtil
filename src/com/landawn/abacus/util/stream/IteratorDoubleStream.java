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

import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleToFloatFunction;
import com.landawn.abacus.util.function.DoubleToIntFunction;
import com.landawn.abacus.util.function.DoubleToLongFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IteratorDoubleStream extends AbstractDoubleStream {
    private final ImmutableDoubleIterator elements;
    private final boolean sorted;

    IteratorDoubleStream(ImmutableDoubleIterator values) {
        this(values, null);
    }

    IteratorDoubleStream(ImmutableDoubleIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorDoubleStream(ImmutableDoubleIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
    }

    @Override
    public DoubleStream filter(DoublePredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate, final long max) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNext = false;
            private double next = 0;
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
            public double next() {
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
    public DoubleStream takeWhile(DoublePredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate, final long max) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNext = false;
            private double next = 0;
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
            public double next() {
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
    public DoubleStream dropWhile(DoublePredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate, final long max) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNext = false;
            private double next = 0;
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
            public double next() {
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
    public DoubleStream map(final DoubleUnaryOperator mapper) {
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
    public IntStream mapToInt(final DoubleToIntFunction mapper) {
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
    public LongStream mapToLong(final DoubleToLongFunction mapper) {
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
    public FloatStream mapToFloat(final DoubleToFloatFunction mapper) {
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
    public <U> Stream<U> mapToObj(final DoubleFunction<? extends U> mapper) {
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
    public DoubleStream flatMap(final DoubleFunction<? extends DoubleStream> mapper) {
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
    public IntStream flatMapToInt(final DoubleFunction<? extends IntStream> mapper) {
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
    public LongStream flatMapToLong(final DoubleFunction<? extends LongStream> mapper) {
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
    public FloatStream flatMapToFloat(final DoubleFunction<? extends FloatStream> mapper) {
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
    public <T> Stream<T> flatMapToObj(final DoubleFunction<? extends Stream<T>> mapper) {
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
    public Stream<DoubleStream> split(final int size) {
        return new IteratorStream<DoubleStream>(new ImmutableIterator<DoubleStream>() {

            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public DoubleStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final double[] a = new double[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayDoubleStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<DoubleStream> split(final DoublePredicate predicate) {
        return new IteratorStream<DoubleStream>(new ImmutableIterator<DoubleStream>() {
            private double next;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public DoubleStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final DoubleList result = DoubleList.of(N.EMPTY_DOUBLE_ARRAY);

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

                return DoubleStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public DoubleStream distinct() {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private Iterator<Double> distinctIter;

            @Override
            public boolean hasNext() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.hasNext();
            }

            @Override
            public double next() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.next();
            }

            private void removeDuplicated() {
                final Set<Double> set = new LinkedHashSet<>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                distinctIter = set.iterator();
            }

        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream top(int n) {
        return top(n, Stream.DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream top(int n, Comparator<? super Double> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToDouble(new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        });
    }

    @Override
    public DoubleStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            double[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public double next() {
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
            public double[] toArray() {
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
    //    public DoubleStream parallelSorted() {
    //        if (sorted) {
    //            return this;
    //        }
    //
    //        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
    //            double[] a = null;
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
    //            public double next() {
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
    //            public double[] toArray() {
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
    //
    //        }, closeHandlers, true);
    //    }

    @Override
    public DoubleStream peek(final DoubleConsumer action) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                final double next = elements.next();

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
            //    public double[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public double next() {
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
    public DoubleStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
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
            public double next() {
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
            public double[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(DoubleConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    //    @Override
    //    public boolean forEach2(DoubleFunction<Boolean> action) {
    //        while (elements.hasNext()) {
    //            if (action.apply(elements.next()).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public double[] toArray() {
        return elements.toArray();
    }

    @Override
    public DoubleList toDoubleList() {
        return DoubleList.of(toArray());
    }

    @Override
    public List<Double> toList() {
        final List<Double> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Double> toList(Supplier<? extends List<Double>> supplier) {
        final List<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Double> toSet() {
        final Set<Double> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Double> toSet(Supplier<? extends Set<Double>> supplier) {
        final Set<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset() {
        final Multiset<Double> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset(Supplier<? extends Multiset<Double>> supplier) {
        final Multiset<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset() {
        final LongMultiset<Double> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset(Supplier<? extends LongMultiset<Double>> supplier) {
        final LongMultiset<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Double>> toMap(DoubleFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Double>>>() {
            @Override
            public Map<K, List<Double>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Double>>> M toMap(DoubleFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Double, ?, List<Double>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(DoubleFunction<? extends K> classifier, Collector<Double, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Double> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        double element = 0;

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
    public <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        double element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        double element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public double reduce(double identity, DoubleBinaryOperator op) {
        double result = identity;

        while (elements.hasNext()) {
            result = op.applyAsDouble(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalDouble reduce(DoubleBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        double result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsDouble(result, elements.next());
        }

        return OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalDouble min() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        double candidate = elements.next();
        double next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble max() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        double candidate = elements.next();
        double next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        final OptionalNullable<Double> optional = boxed().kthLargest(k, Stream.DOUBLE_COMPARATOR);

        return optional.isPresent() ? OptionalDouble.of(optional.get()) : OptionalDouble.empty();
    }

    @Override
    public Double sum() {
        //        double result = 0d;
        //
        //        while (elements.hasNext()) {
        //            result += elements.next();
        //        }
        //
        //        return result;

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[3];
            }
        };

        final ObjDoubleConsumer<double[]> accumulator = new ObjDoubleConsumer<double[]>() {
            @Override
            public void accept(double[] ll, double d) {
                Collectors.sumWithCompensation(ll, d);
                ll[2] += d;
            }
        };

        final BiConsumer<double[], double[]> combiner = new BiConsumer<double[], double[]>() {
            @Override
            public void accept(double[] ll, double[] rr) {
                Collectors.sumWithCompensation(ll, rr[0]);
                Collectors.sumWithCompensation(ll, rr[1]);
                ll[2] += rr[2];
            }
        };

        final double[] summation = collect(supplier, accumulator, combiner);

        return Collectors.computeFinalSum(summation);
    }

    @Override
    public OptionalDouble average() {
        //        if (elements.hasNext() == false) {
        //            return OptionalDouble.empty();
        //        }
        //    
        //        double result = 0d;
        //        long count = 0;
        //    
        //        while (elements.hasNext()) {
        //            result += elements.next();
        //            count++;
        //        }
        //    
        //        return OptionalDouble.of(result / count);

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[4];
            }
        };

        final ObjDoubleConsumer<double[]> accumulator = new ObjDoubleConsumer<double[]>() {
            @Override
            public void accept(double[] ll, double d) {
                ll[2]++;
                Collectors.sumWithCompensation(ll, d);
                ll[3] += d;
            }
        };

        final BiConsumer<double[], double[]> combiner = new BiConsumer<double[], double[]>() {
            @Override
            public void accept(double[] ll, double[] rr) {
                Collectors.sumWithCompensation(ll, rr[0]);
                Collectors.sumWithCompensation(ll, rr[1]);
                ll[2] += rr[2];
                ll[3] += rr[3];
            }
        };

        final double[] avg = collect(supplier, accumulator, combiner);

        return avg[2] > 0 ? OptionalDouble.of(Collectors.computeFinalSum(avg) / avg[2]) : OptionalDouble.empty();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public DoubleSummaryStatistics summarize() {
        final DoubleSummaryStatistics result = new DoubleSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public Optional<Map<String, Double>> distribution() {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final double[] a = sorted().toArray();

        return Optional.of(N.distribution(a));
    }

    @Override
    public boolean anyMatch(DoublePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(DoublePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(DoublePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalDouble findFirst() {
    //        return elements.hasNext() ? OptionalDouble.empty() : OptionalDouble.of(elements.next());
    //    }

    @Override
    public OptionalDouble findFirst(DoublePredicate predicate) {
        while (elements.hasNext()) {
            double e = elements.next();

            if (predicate.test(e)) {
                return OptionalDouble.of(e);
            }
        }

        return OptionalDouble.empty();
    }

    //    @Override
    //    public OptionalDouble findLast() {
    //        if (elements.hasNext() == false) {
    //            return OptionalDouble.empty();
    //        }
    //
    //        double e = 0;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return OptionalDouble.of(e);
    //    }

    @Override
    public OptionalDouble findLast(DoublePredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        boolean hasResult = false;
        double e = 0;
        double result = 0;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalDouble.of(result) : OptionalDouble.empty();
    }

    //    @Override
    //    public OptionalDouble findAny() {
    //        return count() == 0 ? OptionalDouble.empty() : OptionalDouble.of(elements.next());
    //    }

    @Override
    public OptionalDouble findAny(DoublePredicate predicate) {
        while (elements.hasNext()) {
            double e = elements.next();

            if (predicate.test(e)) {
                return OptionalDouble.of(e);
            }
        }

        return OptionalDouble.empty();
    }

    @Override
    public DoubleStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public DoubleStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    //    @Override
    //    public DoubleStream exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new DoublePredicate() {
    //            @Override
    //            public boolean test(double value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public Stream<Double> boxed() {
        return new IteratorStream<Double>(iterator(), closeHandlers, sorted, sorted ? Stream.DOUBLE_COMPARATOR : null);
    }

    @Override
    public DoubleStream append(DoubleStream stream) {
        return DoubleStream.concat(this, stream);
    }

    @Override
    public DoubleStream merge(DoubleStream b, DoubleBiFunction<Nth> nextSelector) {
        return DoubleStream.merge(this, b, nextSelector);
    }

    @Override
    public ImmutableIterator<Double> iterator() {
        return new ImmutableIterator<Double>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Double next() {
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
    public ImmutableDoubleIterator doubleIterator() {
        return elements;
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public DoubleStream sequential() {
        return this;
    }

    @Override
    public DoubleStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelIteratorDoubleStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorDoubleStream(elements, newCloseHandlers, sorted);
    }
}
