/*
 * Copyright (C) 2016 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.IndexedDouble;
import com.landawn.abacus.util.Joiner;
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
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractDoubleStream extends DoubleStream {

    AbstractDoubleStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public DoubleStream drop(final long n, final DoubleConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return dropWhile(new DoublePredicate() {
                @Override
                public boolean test(double value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return dropWhile(new DoublePredicate() {
                @Override
                public boolean test(double value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        }
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate, final DoubleConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public Stream<DoubleStream> split(final int size) {
        return split0(size).map(new Function<DoubleList, DoubleStream>() {
            @Override
            public DoubleStream apply(DoubleList t) {
                return new ArrayDoubleStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public <U> Stream<DoubleStream> split(final U identity, final BiFunction<? super Double, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return split0(identity, predicate, identityUpdate).map(new Function<DoubleList, DoubleStream>() {
            @Override
            public DoubleStream apply(DoubleList t) {
                return new ArrayDoubleStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public Stream<DoubleStream> sliding(final int windowSize, final int increment) {
        return sliding0(windowSize, increment).map(new Function<DoubleList, DoubleStream>() {
            @Override
            public DoubleStream apply(DoubleList t) {
                return new ArrayDoubleStream(t.array(), 0, t.size(), null, sorted);
            }
        });
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
    public <K> Multimap<K, Double, List<Double>> toMultimap(DoubleFunction<? extends K> keyMapper) {
        return toMultimap(keyMapper, DoubleFunction.BOX);
    }

    @Override
    public <K, V extends Collection<Double>> Multimap<K, Double, V> toMultimap(DoubleFunction<? extends K> keyMapper,
            Supplier<Multimap<K, Double, V>> mapSupplier) {
        return toMultimap(keyMapper, DoubleFunction.BOX, mapSupplier);
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
    public DoubleStream distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return set.add(value);
            }
        }).doubleIterator(), sorted);
    }

    @Override
    public Double sum() {
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
    public OptionalDouble first() {
        final DoubleIterator iter = this.doubleIterator();

        return iter.hasNext() ? OptionalDouble.of(iter.next()) : OptionalDouble.empty();
    }

    @Override
    public OptionalDouble last() {
        final DoubleIterator iter = this.doubleIterator();

        if (iter.hasNext() == false) {
            return OptionalDouble.empty();
        }

        double next = iter.next();

        while (iter.hasNext()) {
            next = iter.next();
        }

        return OptionalDouble.of(next);
    }

    @Override
    public DoubleStream except(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return newStream(this.sequential().filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).doubleIterator(), sorted);
    }

    @Override
    public DoubleStream intersect(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return newStream(this.sequential().filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).doubleIterator(), sorted);
    }

    @Override
    public DoubleStream xor(final Collection<Double> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return newStream(this.sequential().filter(new DoublePredicate() {
            @Override
            public boolean test(double value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToDouble(ToDoubleFunction.UNBOX)).doubleIterator(), false);
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

        final DoubleStream[] a = { new ArrayDoubleStream(list.array(), 0, list.size(), null, sorted), new IteratorDoubleStream(iter, null, sorted) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<DoubleStream> splitBy(DoublePredicate where) {
        N.requireNonNull(where);

        final DoubleIterator iter = this.doubleIterator();
        final DoubleList list = new DoubleList();
        double next = 0;
        DoubleStream s = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (where.test(next)) {
                list.add(next);
            } else {
                s = DoubleStream.of(next);

                break;
            }
        }

        final DoubleStream[] a = { new ArrayDoubleStream(list.array(), 0, list.size(), null, sorted), new IteratorDoubleStream(iter, null, sorted) };

        if (s != null) {
            if (sorted) {
                a[1] = new IteratorDoubleStream(a[1].prepend(s).doubleIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(s);
            }
        }

        return this.newStream(a, false, null);
    }

    @Override
    public DoubleStream reverse() {
        final double[] tmp = toArray();

        return newStream(new ImmutableDoubleIterator() {
            private int cursor = tmp.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public double next() {
                if (cursor <= 0) {
                    throw new NoSuchElementException();
                }

                return tmp[--cursor];
            }

            @Override
            public long count() {
                return cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < cursor ? cursor - (int) n : 0;
            }

            @Override
            public double[] toArray() {
                final double[] a = new double[cursor];

                for (int i = 0, len = tmp.length; i < len; i++) {
                    a[i] = tmp[cursor - i - 1];
                }

                return a;
            }
        }, false);
    }

    @Override
    public DoubleStream shuffle() {
        final double[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public DoubleStream rotate(int distance) {
        final double[] a = toArray();

        N.rotate(a, distance);

        return newStream(a, false);
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

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new DoubleSummaryStatistics(), Optional.<Map<Percentage, Double>> empty());
        } else {
            return Pair.of(new DoubleSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public String join(final CharSequence delimiter) {
        return join(delimiter, "", "");
    }

    @Override
    public String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Supplier<Joiner> supplier = new Supplier<Joiner>() {
            @Override
            public Joiner get() {
                return new Joiner(delimiter, prefix, suffix);
            }
        };

        final ObjDoubleConsumer<Joiner> accumulator = new ObjDoubleConsumer<Joiner>() {
            @Override
            public void accept(Joiner a, double t) {
                a.add(t);
            }
        };

        final BiConsumer<Joiner, Joiner> combiner = new BiConsumer<Joiner, Joiner>() {
            @Override
            public void accept(Joiner a, Joiner b) {
                a.merge(b);
            }
        };

        final Joiner joiner = collect(supplier, accumulator, combiner);

        return joiner.toString();
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedDouble> indexed() {
        final MutableLong idx = new MutableLong();

        return newStream(this.sequential().mapToObj(new DoubleFunction<IndexedDouble>() {
            @Override
            public IndexedDouble apply(double t) {
                return IndexedDouble.of(idx.getAndIncrement(), t);
            }
        }).iterator(), true, INDEXED_DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream append(DoubleStream stream) {
        return DoubleStream.concat(this, stream);
    }

    @Override
    public DoubleStream prepend(DoubleStream stream) {
        return DoubleStream.concat(stream, this);
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
    public DoubleStream cached() {
        return this.newStream(toArray(), sorted);
    }
}
