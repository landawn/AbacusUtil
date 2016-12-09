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

import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.IndexedFloat;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.FloatTriFunction;
import com.landawn.abacus.util.function.ObjFloatConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToFloatFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractFloatStream extends FloatStream {

    AbstractFloatStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public FloatStream drop(final long n, final FloatConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return dropWhile(new FloatPredicate() {
                @Override
                public boolean test(float value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return dropWhile(new FloatPredicate() {
                @Override
                public boolean test(float value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        }
    }

    @Override
    public FloatStream dropWhile(final FloatPredicate predicate, final FloatConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
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
    public <K> Multimap<K, Float, List<Float>> toMultimap(FloatFunction<? extends K> keyMapper) {
        return toMultimap(keyMapper, new FloatFunction<Float>() {
            @Override
            public Float apply(float value) {
                return value;
            }
        });
    }

    @Override
    public <K, V extends Collection<Float>> Multimap<K, Float, V> toMultimap(FloatFunction<? extends K> keyMapper,
            Supplier<Multimap<K, Float, V>> mapSupplier) {
        return toMultimap(keyMapper, new FloatFunction<Float>() {
            @Override
            public Float apply(float value) {
                return value;
            }
        }, mapSupplier);
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
    public FloatStream distinct() {
        return newStream(this.sequential().filter(new FloatPredicate() {
            private final Set<Object> set = new HashSet<>();

            @Override
            public boolean test(float value) {
                return set.add(value);
            }
        }).floatIterator(), sorted);
    }

    @Override
    public Double sum() {
        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[3];
            }
        };

        final ObjFloatConsumer<double[]> accumulator = new ObjFloatConsumer<double[]>() {
            @Override
            public void accept(double[] ll, float f) {
                Collectors.sumWithCompensation(ll, f);
                ll[2] += f;
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

        final ObjFloatConsumer<double[]> accumulator = new ObjFloatConsumer<double[]>() {
            @Override
            public void accept(double[] ll, float f) {
                ll[2]++;
                Collectors.sumWithCompensation(ll, f);
                ll[3] += f;
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
    public OptionalFloat first() {
        final FloatIterator iter = this.floatIterator();

        return iter.hasNext() ? OptionalFloat.of(iter.next()) : OptionalFloat.empty();
    }

    @Override
    public OptionalFloat last() {
        final FloatIterator iter = this.floatIterator();

        if (iter.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float next = 0;

        while (iter.hasNext()) {
            next = iter.next();
        }

        return OptionalFloat.of(next);
    }

    @Override
    public FloatStream except(final Collection<?> c) {
        return newStream(this.sequential().filter(new FloatPredicate() {
            final Multiset<?> multiset = Multiset.of(c);

            @Override
            public boolean test(float value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).floatIterator(), sorted);
    }

    @Override
    public FloatStream intersect(final Collection<?> c) {
        return newStream(this.sequential().filter(new FloatPredicate() {
            final Multiset<?> multiset = Multiset.of(c);

            @Override
            public boolean test(float value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).floatIterator(), sorted);
    }

    @Override
    public FloatStream xor(final Collection<Float> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return newStream(this.sequential().filter(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToFloat(ToFloatFunction.UNBOX)).floatIterator(), false);
    }

    @Override
    public Stream<FloatStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final FloatIterator iter = this.floatIterator();
        final FloatList list = new FloatList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final FloatStream[] a = new FloatStream[] { new ArrayFloatStream(list.array(), 0, list.size(), null, sorted),
                new IteratorFloatStream(iter instanceof ImmutableFloatIterator ? (ImmutableFloatIterator) iter : new ImmutableFloatIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public float next() {
                        return iter.next();
                    }

                }, null, sorted) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<FloatStream> splitBy(FloatPredicate where) {
        N.requireNonNull(where);

        final FloatIterator iter = this.floatIterator();
        final FloatList list = new FloatList();
        float next = 0;
        FloatStream p = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (where.test(next)) {
                list.add(next);
            } else {
                p = FloatStream.of(next);

                break;
            }
        }

        final FloatStream[] a = new FloatStream[] { new ArrayFloatStream(list.array(), 0, list.size(), null, sorted),
                new IteratorFloatStream(iter instanceof ImmutableFloatIterator ? (ImmutableFloatIterator) iter : new ImmutableFloatIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public float next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (p != null) {
            a[1] = a[1].prepend(p);
        }

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<FloatList> sliding(int windowSize) {
        return sliding(windowSize, 1);
    }

    @Override
    public FloatStream reverse() {
        final float[] a = toArray();
        return newStream(new ImmutableFloatIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public float next() {
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
    public FloatStream shuffle() {
        final float[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public FloatStream rotate(int distance) {
        final float[] a = toArray();

        N.rotate(a, distance);

        return newStream(a, false);
    }

    @Override
    public Optional<Map<Percentage, Float>> distribution() {
        final float[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<FloatSummaryStatistics, Optional<Map<Percentage, Float>>> summarize2() {
        final float[] a = sorted().toArray();

        final FloatSummaryStatistics summaryStatistics = new FloatSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Float>> distribution = a.length == 0 ? Optional.<Map<Percentage, Float>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
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

        final ObjFloatConsumer<Joiner> accumulator = new ObjFloatConsumer<Joiner>() {
            @Override
            public void accept(Joiner a, float t) {
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
    public <R> R collect(Supplier<R> supplier, ObjFloatConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedFloat> indexed() {
        return newStream(this.sequential().mapToObj(new FloatFunction<IndexedFloat>() {
            final MutableLong idx = new MutableLong();

            @Override
            public IndexedFloat apply(float t) {
                return IndexedFloat.of(idx.getAndIncrement(), t);
            }
        }).iterator(), false, null);
    }

    @Override
    public FloatStream append(FloatStream stream) {
        return FloatStream.concat(this, stream);
    }

    @Override
    public FloatStream merge(FloatStream b, FloatBiFunction<Nth> nextSelector) {
        return FloatStream.merge(this, b, nextSelector);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatBiFunction<Float> zipFunction) {
        return FloatStream.zip(this, b, zipFunction);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatStream c, FloatTriFunction<Float> zipFunction) {
        return FloatStream.zip(this, b, c, zipFunction);
    }

    @Override
    public FloatStream zipWith(FloatStream b, float valueForNoneA, float valueForNoneB, FloatBiFunction<Float> zipFunction) {
        return FloatStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatStream c, float valueForNoneA, float valueForNoneB, float valueForNoneC,
            FloatTriFunction<Float> zipFunction) {
        return FloatStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public FloatStream cached() {
        return this.newStream(toArray(), sorted);
    }
}
