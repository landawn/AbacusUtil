package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.IndexedDouble;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractDoubleStream extends DoubleStream {

    AbstractDoubleStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
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
    public <K, U> Multimap<K, U, List<U>> toMultimap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public Double sum() {
        // return N.sum(elements, fromIndex, toIndex);

        //        double[] summation = collect(() -> new double[3], (ll, d) -> {
        //            Collectors.sumWithCompensation(ll, d);
        //            ll[2] += d;
        //        }, (ll, rr) -> {
        //            Collectors.sumWithCompensation(ll, rr[0]);
        //            Collectors.sumWithCompensation(ll, rr[1]);
        //            ll[2] += rr[2];
        //        });
        //
        //        return Collectors.computeFinalSum(summation);

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
        //        if (count() == 0) {
        //            return OptionalDouble.empty();
        //        }
        //
        //        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));

        //        double[] avg = collect(() -> new double[4], (ll, d) -> {
        //            ll[2]++;
        //            Collectors.sumWithCompensation(ll, d);
        //            ll[3] += d;
        //        }, (ll, rr) -> {
        //            Collectors.sumWithCompensation(ll, rr[0]);
        //            Collectors.sumWithCompensation(ll, rr[1]);
        //            ll[2] += rr[2];
        //            ll[3] += rr[3];
        //        });
        //        
        //        return avg[2] > 0 ? OptionalDouble.of(Collectors.computeFinalSum(avg) / avg[2]) : OptionalDouble.empty();

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

    @Override
    public DoubleStream xor(Collection<Double> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToDouble(new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        }));
    }

    @Override
    public Stream<DoubleStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final DoubleIterator iter = this.doubleIterator();
        final DoubleList list = new DoubleList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final DoubleStream[] a = new DoubleStream[] { new ArrayDoubleStream(list.array(), 0, list.size(), null, sorted),
                new IteratorDoubleStream(iter instanceof ImmutableDoubleIterator ? (ImmutableDoubleIterator) iter : new ImmutableDoubleIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public double next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (this.isParallel()) {
            return new ParallelArrayStream<DoubleStream>(a, 0, a.length, closeHandlers, false, null, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayStream<DoubleStream>(a, closeHandlers);
        }
    }

    @Override
    public DoubleStream reverse() {
        final double[] a = toArray();

        //        N.reverse(a);
        //
        //        return newStream(a, false);

        return newStream(new ImmutableDoubleIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public double next() {
                if (cursor <= 0) {
                    throw new NoSuchElementException();
                }

                return a[--cursor];
            }

            @Override
            public long count() {
                return cursor - 0;
            }

            @Override
            public void skip(long n) {
                cursor = cursor > n ? cursor - (int) n : 0;
            }
        }, false);
    }

    @Override
    public Optional<Map<Percentage, Double>> distribution() {
        final double[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarize2() {
        final double[] a = sorted().toArray();

        final DoubleSummaryStatistics summaryStatistics = new DoubleSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Double>> distribution = a.length == 0 ? Optional.<Map<Percentage, Double>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedDouble> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new DoubleFunction<IndexedDouble>() {
            @Override
            public IndexedDouble apply(double t) {
                return IndexedDouble.of(idx.getAndIncrement(), t);
            }
        });
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
    public DoubleStream zipWith(DoubleStream b, DoubleBiFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, zipFunction);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, DoubleTriFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, c, zipFunction);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, double valueForNoneA, double valueForNoneB, DoubleBiFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, double valueForNoneA, double valueForNoneB, double valueForNoneC,
            DoubleTriFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public DoubleStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public DoubleStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public DoubleStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public DoubleStream maxThreadNum(int maxThreadNum) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");  

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public Splitter splitter() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return DEFAULT_SPILTTER;
    }

    @Override
    public DoubleStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    protected DoubleStream newStream(final double[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayDoubleStream(a, 0, a.length, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayDoubleStream(a, closeHandlers, sorted);
        }
    }

    protected DoubleStream newStream(final DoubleIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            final ImmutableDoubleIterator doubleIter = iter instanceof ImmutableDoubleIterator ? (ImmutableDoubleIterator) iter
                    : new ImmutableDoubleIterator() {
                        @Override
                        public boolean hasNext() {
                            return iter.hasNext();
                        }

                        @Override
                        public double next() {
                            return iter.next();
                        }
                    };

            return new ParallelIteratorDoubleStream(doubleIter, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            final ImmutableDoubleIterator doubleIter = iter instanceof ImmutableDoubleIterator ? (ImmutableDoubleIterator) iter
                    : new ImmutableDoubleIterator() {
                        @Override
                        public boolean hasNext() {
                            return iter.hasNext();
                        }

                        @Override
                        public double next() {
                            return iter.next();
                        }
                    };

            return new IteratorDoubleStream(doubleIter, closeHandlers, sorted);
        }
    }
}
