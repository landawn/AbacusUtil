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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatMatrix;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IndexedFloat;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableFloat;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatBiPredicate;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.FloatTriFunction;
import com.landawn.abacus.util.function.Function;
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
    public FloatStream remove(final long n, final FloatConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return removeWhile(new FloatPredicate() {
                @Override
                public boolean test(float value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return removeWhile(new FloatPredicate() {
                @Override
                public boolean test(float value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        }
    }

    @Override
    public FloatStream removeIf(final FloatPredicate predicate) {
        N.requireNonNull(predicate);

        return filter(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public FloatStream removeIf(final FloatPredicate predicate, final FloatConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(predicate);

        return filter(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public FloatStream removeWhile(final FloatPredicate predicate, final FloatConsumer action) {
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
    public FloatStream step(final long step) {
        N.checkArgument(step > 0, "'step' can't be 0 or negative: %s", step);

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final ExFloatIterator iter = this.exIterator();

        final FloatIterator floatIterator = new ExFloatIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public float nextFloat() {
                final float next = iter.nextFloat();
                iter.skip(skip);
                return next;
            }
        };

        return newStream(floatIterator, sorted);
    }

    @Override
    public Stream<FloatStream> split(final int size) {
        return split2(size).map(new Function<FloatList, FloatStream>() {
            @Override
            public FloatStream apply(FloatList t) {
                return new ArrayFloatStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public <U> Stream<FloatStream> split(final U identity, final BiFunction<? super Float, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return split2(identity, predicate, identityUpdate).map(new Function<FloatList, FloatStream>() {
            @Override
            public FloatStream apply(FloatList t) {
                return new ArrayFloatStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public Stream<FloatStream> sliding(final int windowSize, final int increment) {
        return sliding2(windowSize, increment).map(new Function<FloatList, FloatStream>() {
            @Override
            public FloatStream apply(FloatList t) {
                return new ArrayFloatStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public FloatStream collapse(final FloatBiPredicate collapsible, final FloatBiFunction<Float> mergeFunction) {
        final ExFloatIterator iter = exIterator();

        return this.newStream(new ExFloatIterator() {
            private boolean hasNext = false;
            private float next = 0;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public float nextFloat() {
                float res = hasNext ? next : (next = iter.nextFloat());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.nextFloat()))) {
                        res = mergeFunction.apply(res, next);
                    } else {
                        break;
                    }
                }

                return res;
            }
        }, false);
    }

    @Override
    public FloatStream scan(final FloatBiFunction<Float> accumulator) {
        final ExFloatIterator iter = exIterator();

        return this.newStream(new ExFloatIterator() {
            private float res = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public float nextFloat() {
                if (isFirst) {
                    isFirst = false;
                    return (res = iter.nextFloat());
                } else {
                    return (res = accumulator.apply(res, iter.nextFloat()));
                }
            }
        }, false);
    }

    @Override
    public FloatStream scan(final float seed, final FloatBiFunction<Float> accumulator) {
        final ExFloatIterator iter = exIterator();

        return this.newStream(new ExFloatIterator() {
            private float res = seed;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public float nextFloat() {
                return (res = accumulator.apply(res, iter.nextFloat()));
            }
        }, false);
    }

    @Override
    public FloatStream reverseSorted() {
        return sorted().reversed();
    }

    @Override
    public <K, U> Map<K, U> toMap(FloatFunction<? extends K> keyExtractor, FloatFunction<? extends U> valueMapper) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(FloatFunction<? extends K> keyExtractor, FloatFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(FloatFunction<? extends K> keyExtractor, FloatFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(FloatFunction<? extends K> classifier, Collector<Float, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K> Multimap<K, Float, List<Float>> toMultimap(FloatFunction<? extends K> keyExtractor) {
        return toMultimap(keyExtractor, FloatFunction.BOX);
    }

    @Override
    public <K, V extends Collection<Float>> Multimap<K, Float, V> toMultimap(FloatFunction<? extends K> keyExtractor,
            Supplier<Multimap<K, Float, V>> mapFactory) {
        return toMultimap(keyExtractor, FloatFunction.BOX, mapFactory);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(FloatFunction<? extends K> keyExtractor, FloatFunction<? extends U> valueMapper) {
        return toMultimap(keyExtractor, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public FloatMatrix toMatrix() {
        return FloatMatrix.of(toArray());
    }

    @Override
    public FloatStream distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                return set.add(value);
            }
        }).exIterator(), sorted);
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
        final FloatIterator iter = this.exIterator();

        return iter.hasNext() ? OptionalFloat.of(iter.nextFloat()) : OptionalFloat.empty();
    }

    @Override
    public OptionalFloat last() {
        final FloatIterator iter = this.exIterator();

        if (iter.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float next = iter.nextFloat();

        while (iter.hasNext()) {
            next = iter.nextFloat();
        }

        return OptionalFloat.of(next);
    }

    @Override
    public OptionalFloat findFirstOrLast(FloatPredicate predicateForFirst, FloatPredicate predicateForLast) {
        final ExFloatIterator iter = exIterator();
        MutableFloat last = null;
        float next = 0;

        while (iter.hasNext()) {
            next = iter.nextFloat();

            if (predicateForFirst.test(next)) {
                return OptionalFloat.of(next);
            } else if (predicateForLast.test(next)) {
                if (last == null) {
                    last = MutableFloat.of(next);
                } else {
                    last.setValue(next);
                }
            }
        }

        return last == null ? OptionalFloat.empty() : OptionalFloat.of(last.value());
    }

    @Override
    public Pair<OptionalFloat, OptionalFloat> findFirstAndLast(FloatPredicate predicateForFirst, FloatPredicate predicateForLast) {
        final Pair<OptionalFloat, OptionalFloat> result = new Pair<>();
        final ExFloatIterator iter = exIterator();
        MutableFloat last = null;
        float next = 0;

        while (iter.hasNext()) {
            next = iter.nextFloat();

            if (result.left == null && predicateForFirst.test(next)) {
                result.left = OptionalFloat.of(next);
            }

            if (predicateForLast.test(next)) {
                if (last == null) {
                    last = MutableFloat.of(next);
                } else {
                    last.setValue(next);
                }
            }
        }

        if (result.left == null) {
            result.left = OptionalFloat.empty();
        }

        result.right = last == null ? OptionalFloat.empty() : OptionalFloat.of(last.value());

        return result;
    }

    @Override
    public FloatStream intersection(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).exIterator(), sorted);
    }

    @Override
    public FloatStream difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new FloatPredicate() {
            @Override
            public boolean test(float value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).exIterator(), sorted);
    }

    @Override
    public FloatStream symmetricDifference(final Collection<Float> c) {
        final Multiset<?> multiset = Multiset.from(c);

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
        }).mapToFloat(ToFloatFunction.UNBOX)).exIterator(), false);
    }

    @Override
    public Stream<FloatStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final FloatIterator iter = this.exIterator();
        final FloatList list = new FloatList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.nextFloat());
        }

        final FloatStream[] a = { new ArrayFloatStream(list.array(), 0, list.size(), null, sorted), new IteratorFloatStream(iter, null, sorted) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<FloatStream> splitBy(FloatPredicate where) {
        N.requireNonNull(where);

        final FloatIterator iter = this.exIterator();
        final FloatList list = new FloatList();
        float next = 0;
        FloatStream s = null;

        while (iter.hasNext()) {
            next = iter.nextFloat();

            if (where.test(next)) {
                list.add(next);
            } else {
                s = FloatStream.of(next);

                break;
            }
        }

        final FloatStream[] a = { new ArrayFloatStream(list.array(), 0, list.size(), null, sorted), new IteratorFloatStream(iter, null, sorted) };

        if (s != null) {
            if (sorted) {
                a[1] = new IteratorFloatStream(a[1].prepend(s).exIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(s);
            }
        }

        return this.newStream(a, false, null);
    }

    @Override
    public FloatStream reversed() {
        final float[] tmp = toArray();

        return newStream(new ExFloatIterator() {
            private int cursor = tmp.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public float nextFloat() {
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
            public float[] toArray() {
                final float[] a = new float[cursor];

                for (int i = 0, len = tmp.length; i < len; i++) {
                    a[i] = tmp[cursor - i - 1];
                }

                return a;
            }
        }, false);
    }

    @Override
    public FloatStream shuffled() {
        final float[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public FloatStream shuffled(final Random rnd) {
        final float[] a = toArray();

        N.shuffle(a, rnd);

        return newStream(a, false);
    }

    @Override
    public FloatStream rotated(int distance) {
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

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new FloatSummaryStatistics(), Optional.<Map<Percentage, Float>> empty());
        } else {
            return Pair.of(new FloatSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
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
                return Joiner.with(delimiter, prefix, suffix);
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
    public Pair<OptionalFloat, FloatStream> headAndTail() {
        return Pair.of(head(), tail());
    }

    @Override
    public Pair<FloatStream, OptionalFloat> headAndTail2() {
        return Pair.of(head2(), tail2());
    }

    @Override
    public Stream<IndexedFloat> indexed() {
        final MutableLong idx = MutableLong.of(0);

        return newStream(this.sequential().mapToObj(new FloatFunction<IndexedFloat>() {
            @Override
            public IndexedFloat apply(float t) {
                return IndexedFloat.of(t, idx.getAndIncrement());
            }
        }).iterator(), true, INDEXED_FLOAT_COMPARATOR);
    }

    @Override
    public FloatStream append(FloatStream stream) {
        return FloatStream.concat(this, stream);
    }

    @Override
    public FloatStream prepend(FloatStream stream) {
        return FloatStream.concat(stream, this);
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
