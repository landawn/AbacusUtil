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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.exception.AbacusIOException;
import com.landawn.abacus.exception.AbacusSQLException;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.BufferedWriter;
import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.ExList;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.Indexed;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Matrix;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.ObjectFactory;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.PermutationIterator;
import com.landawn.abacus.util.Seq;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractStream<T> extends Stream<T> {

    AbstractStream(final Collection<Runnable> closeHandlers, final boolean sorted, final Comparator<? super T> cmp) {
        super(closeHandlers, sorted, cmp);
    }

    @Override
    public <U> Stream<T> filter(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    @Override
    public <U> Stream<T> takeWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    @Override
    public <U> Stream<T> dropWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    @Override
    public Stream<T> remove(final long n, final Consumer<? super T> action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return removeWhile(new Predicate<T>() {
                @Override
                public boolean test(T value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return removeWhile(new Predicate<T>() {

                @Override
                public boolean test(T value) {
                    return cnt.getAndDecrement() > 0;
                }

            }, action);
        }
    }

    @Override
    public Stream<T> removeIf(final Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public <U> Stream<T> removeIf(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        N.requireNonNull(predicate);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed) == false;
            }
        });
    }

    @Override
    public Stream<T> removeIf(final Predicate<? super T> predicate, final Consumer<? super T> action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public <U> Stream<T> removeIf(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super T> action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                if (predicate.test(value, seed)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public Stream<T> removeWhile(final Predicate<? super T> predicate, final Consumer<? super T> action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public <U> Stream<T> removeWhile(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super T> action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                if (predicate.test(value, seed)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public Stream<T> step(final long step) {
        N.checkArgument(step > 0, "'step' can't be 0 or negative: %s", step);

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final ExIterator<T> iter = exIterator();

        final Iterator<T> iterator = new ExIterator<T>() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public T next() {
                final T next = iter.next();
                iter.skip(skip);
                return next;
            }
        };

        return newStream(iterator, sorted, cmp);
    }

    @Override
    public <U, R> Stream<R> map(final U seed, final BiFunction<? super T, ? super U, ? extends R> mapper) {
        return map(new Function<T, R>() {
            @Override
            public R apply(T t) {
                return mapper.apply(t, seed);
            }
        });
    }

    @Override
    public <R> Stream<R> map2(BiFunction<? super T, ? super T, ? extends R> mapper) {
        return map2(mapper, false);
    }

    @Override
    public <R> Stream<R> map3(TriFunction<? super T, ? super T, ? super T, ? extends R> mapper) {
        return map3(mapper, false);
    }

    abstract <R> Stream<R> flatMap0(final Function<? super T, ? extends Iterator<? extends R>> mapper);

    @Override
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
        return flatMap0(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return mapper.apply(t).iterator();
            }
        });
    }

    @Override
    public <U, R> Stream<R> flatMap(final U seed, final BiFunction<? super T, ? super U, ? extends Stream<? extends R>> mapper) {
        return flatMap(new Function<T, Stream<? extends R>>() {
            @Override
            public Stream<? extends R> apply(T t) {
                return mapper.apply(t, seed);
            }
        });
    }

    @Override
    public <R> Stream<R> flatMap2(final Function<? super T, ? extends Collection<? extends R>> mapper) {
        return flatMap0(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return mapper.apply(t).iterator();
            }
        });
    }

    @Override
    public <U, R> Stream<R> flatMap2(final U seed, final BiFunction<? super T, ? super U, ? extends Collection<? extends R>> mapper) {
        return flatMap2(new Function<T, Collection<? extends R>>() {
            @Override
            public Collection<? extends R> apply(T t) {
                return mapper.apply(t, seed);
            }
        });
    }

    @Override
    public <R> Stream<R> flatMap3(final Function<? super T, ? extends R[]> mapper) {
        return flatMap0(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return ExIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public <U, R> Stream<R> flatMap3(final U seed, final BiFunction<? super T, ? super U, ? extends R[]> mapper) {
        return flatMap3(new Function<T, R[]>() {
            @Override
            public R[] apply(T t) {
                return mapper.apply(t, seed);
            }
        });
    }

    @Override
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
        return flatMapToChar0(new Function<T, CharIterator>() {
            @Override
            public CharIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract CharStream flatMapToChar0(Function<? super T, CharIterator> function);

    @Override
    public CharStream flatMapToChar2(final Function<? super T, ? extends Collection<Character>> mapper) {
        return flatMapToChar0(new Function<T, CharIterator>() {
            @Override
            public CharIterator apply(T t) {
                final Iterator<Character> iter = mapper.apply(t).iterator();

                return new ExCharIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public char nextChar() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public CharStream flatMapToChar3(final Function<? super T, char[]> mapper) {
        return flatMapToChar0(new Function<T, CharIterator>() {
            @Override
            public CharIterator apply(T t) {
                return ExCharIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        return flatMapToByte0(new Function<T, ByteIterator>() {
            @Override
            public ByteIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract ByteStream flatMapToByte0(Function<? super T, ByteIterator> function);

    @Override
    public ByteStream flatMapToByte2(final Function<? super T, ? extends Collection<Byte>> mapper) {
        return flatMapToByte0(new Function<T, ByteIterator>() {
            @Override
            public ByteIterator apply(T t) {
                final Iterator<Byte> iter = mapper.apply(t).iterator();

                return new ExByteIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public byte nextByte() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public ByteStream flatMapToByte3(final Function<? super T, byte[]> mapper) {
        return flatMapToByte0(new Function<T, ByteIterator>() {
            @Override
            public ByteIterator apply(T t) {
                return ExByteIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        return flatMapToShort0(new Function<T, ShortIterator>() {
            @Override
            public ShortIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract ShortStream flatMapToShort0(Function<? super T, ShortIterator> function);

    @Override
    public ShortStream flatMapToShort2(final Function<? super T, ? extends Collection<Short>> mapper) {
        return flatMapToShort0(new Function<T, ShortIterator>() {
            @Override
            public ShortIterator apply(T t) {
                final Iterator<Short> iter = mapper.apply(t).iterator();

                return new ExShortIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public short nextShort() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public ShortStream flatMapToShort3(final Function<? super T, short[]> mapper) {
        return flatMapToShort0(new Function<T, ShortIterator>() {
            @Override
            public ShortIterator apply(T t) {
                return ExShortIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        return flatMapToInt0(new Function<T, IntIterator>() {
            @Override
            public IntIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract IntStream flatMapToInt0(Function<? super T, IntIterator> function);

    @Override
    public IntStream flatMapToInt2(final Function<? super T, ? extends Collection<Integer>> mapper) {
        return flatMapToInt0(new Function<T, IntIterator>() {
            @Override
            public IntIterator apply(T t) {
                final Iterator<Integer> iter = mapper.apply(t).iterator();

                return new ExIntIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public int nextInt() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public IntStream flatMapToInt3(final Function<? super T, int[]> mapper) {
        return flatMapToInt0(new Function<T, IntIterator>() {
            @Override
            public IntIterator apply(T t) {
                return ExIntIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        return flatMapToLong0(new Function<T, LongIterator>() {
            @Override
            public LongIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract LongStream flatMapToLong0(Function<? super T, LongIterator> function);

    @Override
    public LongStream flatMapToLong2(final Function<? super T, ? extends Collection<Long>> mapper) {
        return flatMapToLong0(new Function<T, LongIterator>() {
            @Override
            public LongIterator apply(T t) {
                final Iterator<Long> iter = mapper.apply(t).iterator();

                return new ExLongIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public long nextLong() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public LongStream flatMapToLong3(final Function<? super T, long[]> mapper) {
        return flatMapToLong0(new Function<T, LongIterator>() {
            @Override
            public LongIterator apply(T t) {
                return ExLongIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        return flatMapToFloat0(new Function<T, FloatIterator>() {
            @Override
            public FloatIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract FloatStream flatMapToFloat0(Function<? super T, FloatIterator> function);

    @Override
    public FloatStream flatMapToFloat2(final Function<? super T, ? extends Collection<Float>> mapper) {
        return flatMapToFloat0(new Function<T, FloatIterator>() {
            @Override
            public FloatIterator apply(T t) {
                final Iterator<Float> iter = mapper.apply(t).iterator();

                return new ExFloatIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public float nextFloat() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public FloatStream flatMapToFloa3(final Function<? super T, float[]> mapper) {
        return flatMapToFloat0(new Function<T, FloatIterator>() {
            @Override
            public FloatIterator apply(T t) {
                return ExFloatIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        return flatMapToDouble0(new Function<T, DoubleIterator>() {
            @Override
            public DoubleIterator apply(T t) {
                return mapper.apply(t).exIterator();
            }
        });
    }

    abstract DoubleStream flatMapToDouble0(Function<? super T, DoubleIterator> function);

    @Override
    public DoubleStream flatMapToDouble2(final Function<? super T, ? extends Collection<Double>> mapper) {
        return flatMapToDouble0(new Function<T, DoubleIterator>() {
            @Override
            public DoubleIterator apply(T t) {
                final Iterator<Double> iter = mapper.apply(t).iterator();

                return new ExDoubleIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public double nextDouble() {
                        return iter.next();
                    }
                };
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble3(final Function<? super T, double[]> mapper) {
        return flatMapToDouble0(new Function<T, DoubleIterator>() {
            @Override
            public DoubleIterator apply(T t) {
                return ExDoubleIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return split0(size).map(new Function<ExList<T>, Stream<T>>() {
            @Override
            public Stream<T> apply(ExList<T> t) {
                return new ArrayStream<>((T[]) t.array(), 0, t.size(), null, sorted, cmp);
            }
        });
    }

    @Override
    public <U> Stream<Stream<T>> split(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate) {
        return split0(identity, predicate, identityUpdate).map(new Function<ExList<T>, Stream<T>>() {
            @Override
            public Stream<T> apply(ExList<T> t) {
                return new ArrayStream<>((T[]) t.array(), 0, t.size(), null, sorted, cmp);
            }
        });
    }

    @Override
    public Stream<Stream<T>> sliding(final int windowSize, final int increment) {
        return sliding0(windowSize, increment).map(new Function<ExList<T>, Stream<T>>() {
            @Override
            public Stream<T> apply(ExList<T> t) {
                return new ArrayStream<>((T[]) t.array(), 0, t.size(), null, sorted, cmp);
            }
        });
    }

    @Override
    public Stream<T> collapse(final BiPredicate<? super T, ? super T> collapsible, final BiFunction<? super T, ? super T, T> mergeFunction) {
        final ExIterator<T> iter = exIterator();

        return this.newStream(new ExIterator<T>() {
            private T pre = null;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public T next() {
                T res = hasNext ? pre : (pre = iter.next());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(pre, (pre = iter.next()))) {
                        res = mergeFunction.apply(res, pre);
                    } else {
                        break;
                    }
                }

                return res;
            }
        }, false, null);
    }

    @Override
    public <R> Stream<R> collapse(final R seed, final BiPredicate<? super T, ? super T> collapsible, final BiFunction<? super R, ? super T, R> mergeFunction) {
        final ExIterator<T> iter = exIterator();

        return this.newStream(new ExIterator<R>() {
            private T pre = null;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public R next() {
                R res = mergeFunction.apply(seed, hasNext ? pre : (pre = iter.next()));

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(pre, (pre = iter.next()))) {
                        res = mergeFunction.apply(res, pre);
                    } else {
                        break;
                    }
                }

                return res;
            }
        }, false, null);
    }

    @Override
    public <C extends Collection<?>> Stream<C> collapse(final Supplier<C> supplier, final BiPredicate<? super T, ? super T> collapsible,
            final BiConsumer<? super C, ? super T> mergeFunction) {
        final ExIterator<T> iter = exIterator();

        return this.newStream(new ExIterator<C>() {
            private T pre = null;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public C next() {
                final C c = supplier.get();
                mergeFunction.accept(c, hasNext ? pre : (pre = iter.next()));

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(pre, (pre = iter.next()))) {
                        mergeFunction.accept(c, pre);
                    } else {
                        break;
                    }
                }

                return c;
            }
        }, false, null);
    }

    @Override
    public Stream<T> scan(final BiFunction<? super T, ? super T, T> accumulator) {
        final ExIterator<T> iter = exIterator();

        return this.newStream(new ExIterator<T>() {
            private T res = null;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public T next() {
                if (isFirst) {
                    isFirst = false;
                    return (res = iter.next());
                } else {
                    return (res = accumulator.apply(res, iter.next()));
                }
            }
        }, false, null);
    }

    @Override
    public <R> Stream<R> scan(final R seed, final BiFunction<? super R, ? super T, R> accumulator) {
        final ExIterator<T> iter = exIterator();

        return this.newStream(new ExIterator<R>() {
            private R res = seed;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public R next() {
                return (res = accumulator.apply(res, iter.next()));
            }
        }, false, null);
    }

    @Override
    public Stream<T> intersperse(final T value) {
        return newStream(new ExIterator<T>() {
            private final ExIterator<T> iter = exIterator();
            private T next = (T) Stream.NONE;
            private boolean toInsert = false;

            @Override
            public boolean hasNext() {
                if (next == Stream.NONE && iter.hasNext()) {
                    next = iter.next();
                }

                return next != Stream.NONE;
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (toInsert) {
                    toInsert = false;
                    return value;
                } else {
                    final T res = next;
                    next = (T) Stream.NONE;
                    toInsert = true;
                    return res;
                }
            }

        }, false, null);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier, Supplier<Map<K, List<T>>> mapFactory) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, List<U>>> groupBy(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        return groupBy(keyExtractor, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @Override
    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> Stream<Map.Entry<K, List<U>>> groupBy(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, Supplier<M> mapFactory) {
        final Map<K, List<U>> map = collect(
                Collectors.groupingBy(keyExtractor, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, java.util.stream.Collector<? super T, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return groupBy(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, java.util.stream.Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        return groupBy(classifier, Collector.of(downstream), mapFactory);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy2(final Function<? super T, ? extends K> keyExtractor, final Function<? super T, ? extends U> valueMapper) {
        final Map<K, U> map = collect(Collectors.toMap(keyExtractor, valueMapper));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy2(final Function<? super T, ? extends K> keyExtractor, final Function<? super T, ? extends U> valueMapper,
            Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyExtractor, valueMapper, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy2(final Function<? super T, ? extends K> keyExtractor, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Map<K, U> map = collect(Collectors.toMap(keyExtractor, valueMapper, mergeFunction));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy2(final Function<? super T, ? extends K> keyExtractor, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, java.util.stream.Collector<? super T, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier,
            final java.util.stream.Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) {

        return toMap(classifier, Collector.of(downstream), mapFactory);
    }

    @Override
    public <K> Map<K, List<T>> toMap2(Function<? super T, ? extends K> classifier) {
        final Supplier<Map<K, List<T>>> mapFactory = Fn.Suppliers.ofMap();

        return toMap2(classifier, mapFactory);
    }

    @Override
    public <K, M extends Map<K, List<T>>> M toMap2(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, U> Map<K, List<U>> toMap2(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyExtractor, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    @Override
    public <K, U, M extends Map<K, List<U>>> M toMap2(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        return toMap(keyExtractor, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    @Override
    public <K> Multimap<K, T, List<T>> toMultimap(Function<? super T, ? extends K> keyExtractor) {
        return toMultimap(keyExtractor, new Function<T, T>() {
            @Override
            public T apply(T t) {
                return t;
            }
        });
    }

    @Override
    public <K, V extends Collection<T>> Multimap<K, T, V> toMultimap(Function<? super T, ? extends K> keyExtractor, Supplier<Multimap<K, T, V>> mapFactory) {
        return toMultimap(keyExtractor, new Function<T, T>() {
            @Override
            public T apply(T t) {
                return t;
            }
        }, mapFactory);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        return toMultimap(keyExtractor, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public Matrix<T> toMatrix(final Class<T> type) {
        final T[] a = toArray(new IntFunction<T[]>() {
            @Override
            public T[] apply(int value) {
                return (T[]) N.newArray(type, 0);
            }
        });

        return Matrix.of(a);
    }

    @Override
    public Long sumInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.summingInt(mapper));
    }

    @Override
    public Long sumLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.summingLong(mapper));
    }

    @Override
    public Double sumDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.summingDouble(mapper));
    }

    @Override
    public OptionalDouble averageInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.averagingInt2(mapper));
    }

    @Override
    public OptionalDouble averageLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.averagingLong2(mapper));
    }

    @Override
    public OptionalDouble averageDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.averagingDouble2(mapper));
    }

    @Override
    public <U> Stream<Pair<T, U>> innerJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);
        final Map<Object, Stream<U>> rightKeyStreamMap = new HashMap<>(N.initHashCapacity(rightKeyMap.size()));

        for (Map.Entry<Object, List<U>> entry : rightKeyMap.entrySet()) {
            rightKeyStreamMap.put(entry.getKey(), Stream.of(entry.getValue()).cached());
        }

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                final Stream<U> s = rightKeyStreamMap.get(leftKeyMapper.apply(t));

                return s == null ? Stream.<Pair<T, U>> empty() : s.map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        return Pair.of(t, u);
                    }
                });
            }
        });
    }

    @Override
    public <U> Stream<Pair<T, U>> innerJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final Stream<U> s = Stream.of(b).cached();

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                return s.filter(new Predicate<U>() {
                    @Override
                    public boolean test(final U u) {
                        return predicate.test(t, u);
                    }
                }).map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        return Pair.of(t, u);
                    }
                });
            }
        });
    }

    @Override
    public <U> Stream<Pair<T, U>> fullJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);
        final Map<Object, Stream<U>> rightKeyStreamMap = new HashMap<>(N.initHashCapacity(rightKeyMap.size()));
        final Map<U, U> joinedRights = new IdentityHashMap<>();
        final boolean isParallelStream = this.isParallel();

        for (Map.Entry<Object, List<U>> entry : rightKeyMap.entrySet()) {
            rightKeyStreamMap.put(entry.getKey(), Stream.of(entry.getValue()).cached());
        }

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                final Stream<U> s = rightKeyStreamMap.get(leftKeyMapper.apply(t));

                return s == null ? Stream.of(Pair.of(t, (U) null)) : s.map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        if (isParallelStream) {
                            synchronized (joinedRights) {
                                joinedRights.put(u, u);
                            }
                        } else {
                            joinedRights.put(u, u);
                        }

                        return Pair.of(t, u);
                    }
                });
            }
        }).append(Stream.of(b).filter(new Predicate<U>() {
            @Override
            public boolean test(U u) {
                return joinedRights.containsKey(u) == false;
            }
        }).map(new Function<U, Pair<T, U>>() {
            @Override
            public Pair<T, U> apply(U u) {
                return Pair.of((T) null, u);
            }
        }));
    }

    @Override
    public <U> Stream<Pair<T, U>> fullJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final Stream<U> s = Stream.of(b).cached();
        final Map<U, U> joinedRights = new IdentityHashMap<>();
        final boolean isParallelStream = this.isParallel();

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            private final MutableBoolean joined = MutableBoolean.of(false);

            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                return s.filter(new Predicate<U>() {
                    @Override
                    public boolean test(final U u) {
                        return predicate.test(t, u);
                    }
                }).map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        joined.setTrue();

                        if (isParallelStream) {
                            synchronized (joinedRights) {
                                joinedRights.put(u, u);
                            }
                        } else {
                            joinedRights.put(u, u);
                        }

                        return Pair.of(t, u);
                    }
                }).append(Stream.iterate(new Supplier<Boolean>() {
                    @Override
                    public Boolean get() {
                        return joined.isFalse();
                    }
                }, new Supplier<Pair<T, U>>() {
                    @Override
                    public Pair<T, U> get() {
                        joined.setTrue();
                        return Pair.of(t, (U) null);
                    }
                }));
            }
        }).append(Stream.of(b).filter(new Predicate<U>() {
            @Override
            public boolean test(U u) {
                return joinedRights.containsKey(u) == false;
            }
        }).map(new Function<U, Pair<T, U>>() {
            @Override
            public Pair<T, U> apply(U u) {
                return Pair.of((T) null, u);
            }
        }));
    }

    @Override
    public <U> Stream<Pair<T, U>> leftJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);
        final Map<Object, Stream<U>> rightKeyStreamMap = new HashMap<>(N.initHashCapacity(rightKeyMap.size()));

        for (Map.Entry<Object, List<U>> entry : rightKeyMap.entrySet()) {
            rightKeyStreamMap.put(entry.getKey(), Stream.of(entry.getValue()).cached());
        }

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                final Stream<U> s = rightKeyStreamMap.get(leftKeyMapper.apply(t));

                return s == null ? Stream.of(Pair.of(t, (U) null)) : s.map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        return Pair.of(t, u);
                    }
                });
            }
        });
    }

    @Override
    public <U> Stream<Pair<T, U>> leftJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final Stream<U> s = Stream.of(b).cached();

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            private final MutableBoolean joined = MutableBoolean.of(false);

            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                return s.filter(new Predicate<U>() {
                    @Override
                    public boolean test(final U u) {
                        return predicate.test(t, u);
                    }
                }).map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        joined.setTrue();

                        return Pair.of(t, u);
                    }
                }).append(Stream.iterate(new Supplier<Boolean>() {
                    @Override
                    public Boolean get() {
                        return joined.isFalse();
                    }
                }, new Supplier<Pair<T, U>>() {
                    @Override
                    public Pair<T, U> get() {
                        joined.setTrue();
                        return Pair.of(t, (U) null);
                    }
                }));
            }
        });
    }

    @Override
    public <U> Stream<Pair<T, U>> rightJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);
        final Map<Object, Stream<U>> rightKeyStreamMap = new HashMap<>(N.initHashCapacity(rightKeyMap.size()));
        final Map<U, U> joinedRights = new IdentityHashMap<>();
        final boolean isParallelStream = this.isParallel();

        for (Map.Entry<Object, List<U>> entry : rightKeyMap.entrySet()) {
            rightKeyStreamMap.put(entry.getKey(), Stream.of(entry.getValue()).cached());
        }

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {
            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                final Stream<U> s = rightKeyStreamMap.get(leftKeyMapper.apply(t));

                return s == null ? Stream.<Pair<T, U>> empty() : s.map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {
                        if (isParallelStream) {
                            synchronized (joinedRights) {
                                joinedRights.put(u, u);
                            }
                        } else {
                            joinedRights.put(u, u);
                        }

                        return Pair.of(t, u);
                    }
                });
            }
        }).append(Stream.of(b).filter(new Predicate<U>() {
            @Override
            public boolean test(U u) {
                return joinedRights.containsKey(u) == false;
            }
        }).map(new Function<U, Pair<T, U>>() {
            @Override
            public Pair<T, U> apply(U u) {
                return Pair.of((T) null, u);
            }
        }));
    }

    @Override
    public <U> Stream<Pair<T, U>> rightJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final Stream<U> s = Stream.of(b).cached();
        final Map<U, U> joinedRights = new IdentityHashMap<>();
        final boolean isParallelStream = this.isParallel();

        return flatMap(new Function<T, Stream<Pair<T, U>>>() {

            @Override
            public Stream<Pair<T, U>> apply(final T t) {
                return s.filter(new Predicate<U>() {
                    @Override
                    public boolean test(final U u) {
                        return predicate.test(t, u);
                    }
                }).map(new Function<U, Pair<T, U>>() {
                    @Override
                    public Pair<T, U> apply(U u) {

                        if (isParallelStream) {
                            synchronized (joinedRights) {
                                joinedRights.put(u, u);
                            }
                        } else {
                            joinedRights.put(u, u);
                        }

                        return Pair.of(t, u);
                    }
                });
            }
        }).append(Stream.of(b).filter(new Predicate<U>() {
            @Override
            public boolean test(U u) {
                return joinedRights.containsKey(u) == false;
            }
        }).map(new Function<U, Pair<T, U>>() {
            @Override
            public Pair<T, U> apply(U u) {
                return Pair.of((T) null, u);
            }
        }));
    }

    @Override
    public CharSummaryStatistics summarizeChar(ToCharFunction<? super T> mapper) {
        return collect(Collectors.summarizingChar(mapper));
    }

    @Override
    public ByteSummaryStatistics summarizeByte(ToByteFunction<? super T> mapper) {
        return collect(Collectors.summarizingByte(mapper));
    }

    @Override
    public ShortSummaryStatistics summarizeShort(ToShortFunction<? super T> mapper) {
        return collect(Collectors.summarizingShort(mapper));
    }

    @Override
    public IntSummaryStatistics summarizeInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.summarizingInt(mapper));
    }

    @Override
    public LongSummaryStatistics summarizeLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.summarizingLong(mapper));
    }

    @Override
    public FloatSummaryStatistics summarizeFloat(ToFloatFunction<? super T> mapper) {
        return collect(Collectors.summarizingFloat(mapper));
    }

    @Override
    public DoubleSummaryStatistics summarizeDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.summarizingDouble(mapper));
    }

    @Override
    public <U> NullabLe<T> findFirst(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return findFirst(new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return predicate.test(t, seed);
            }
        });
    }

    @Override
    public <U> NullabLe<T> findLast(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return findLast(new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return predicate.test(t, seed);
            }
        });
    }

    @Override
    public <U> NullabLe<T> findAny(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return findAny(new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return predicate.test(t, seed);
            }
        });
    }

    @Override
    public NullabLe<T> findFirstOrLast(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
        final ExIterator<T> iter = exIterator();
        T last = (T) NONE;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (predicateForFirst.test(next)) {
                return NullabLe.of(next);
            } else if (predicateForLast.test(next)) {
                last = next;
            }
        }

        return last == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(last);
    }

    @Override
    public <U> NullabLe<T> findFirstOrLast(final U seed, final BiPredicate<? super T, ? super U> predicateForFirst,
            final BiPredicate<? super T, ? super U> predicateForLast) {
        final ExIterator<T> iter = exIterator();
        T last = (T) NONE;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (predicateForFirst.test(next, seed)) {
                return NullabLe.of(next);
            } else if (predicateForLast.test(next, seed)) {
                last = next;
            }
        }

        return last == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(last);
    }

    @Override
    public <U> NullabLe<T> findFirstOrLast(final Function<? super T, U> preFunc, final BiPredicate<? super T, ? super U> predicateForFirst,
            final BiPredicate<? super T, ? super U> predicateForLast) {
        final ExIterator<T> iter = exIterator();
        U seed = null;
        T last = (T) NONE;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();
            seed = preFunc.apply(next);

            if (predicateForFirst.test(next, seed)) {
                return NullabLe.of(next);
            } else if (predicateForLast.test(next, seed)) {
                last = next;
            }
        }

        return last == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(last);
    }

    @Override
    public Pair<NullabLe<T>, NullabLe<T>> findFirstAndLast(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
        final Pair<NullabLe<T>, NullabLe<T>> result = new Pair<>();
        final ExIterator<T> iter = exIterator();
        T last = (T) NONE;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (result.left == null && predicateForFirst.test(next)) {
                result.left = NullabLe.of(next);
            }

            if (predicateForLast.test(next)) {
                last = next;
            }
        }

        if (result.left == null) {
            result.left = NullabLe.empty();
        }

        result.right = last == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(last);

        return result;
    }

    @Override
    public <U> Pair<NullabLe<T>, NullabLe<T>> findFirstAndLast(final U seed, final BiPredicate<? super T, ? super U> predicateForFirst,
            final BiPredicate<? super T, ? super U> predicateForLast) {
        final Pair<NullabLe<T>, NullabLe<T>> result = new Pair<>();
        final ExIterator<T> iter = exIterator();
        T last = (T) NONE;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (result.left == null && predicateForFirst.test(next, seed)) {
                result.left = NullabLe.of(next);
            }

            if (predicateForLast.test(next, seed)) {
                last = next;
            }
        }

        if (result.left == null) {
            result.left = NullabLe.empty();
        }

        result.right = last == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(last);

        return result;
    }

    @Override
    public <U> Pair<NullabLe<T>, NullabLe<T>> findFirstAndLast(final Function<? super T, U> preFunc, final BiPredicate<? super T, ? super U> predicateForFirst,
            final BiPredicate<? super T, ? super U> predicateForLast) {
        final Pair<NullabLe<T>, NullabLe<T>> result = new Pair<>();
        final ExIterator<T> iter = exIterator();
        U seed = null;
        T last = (T) NONE;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();
            seed = preFunc.apply(next);

            if (result.left == null && predicateForFirst.test(next, seed)) {
                result.left = NullabLe.of(next);
            }

            if (predicateForLast.test(next, seed)) {
                last = next;
            }
        }

        if (result.left == null) {
            result.left = NullabLe.empty();
        }

        result.right = last == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(last);

        return result;
    }

    @Override
    public <U> boolean anyMatch(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return anyMatch(new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return predicate.test(t, seed);
            }
        });
    }

    @Override
    public <U> boolean allMatch(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return allMatch(new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return predicate.test(t, seed);
            }
        });
    }

    @Override
    public <U> boolean noneMatch(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return noneMatch(new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return predicate.test(t, seed);
            }
        });
    }

    @Override
    public NullabLe<T> first() {
        final Iterator<T> iter = this.iterator();

        if (iter.hasNext() == false) {
            return NullabLe.empty();
        }

        return NullabLe.of(iter.next());
    }

    @Override
    public NullabLe<T> last() {
        final Iterator<T> iter = this.iterator();

        if (iter.hasNext() == false) {
            return NullabLe.empty();
        }

        T next = iter.next();

        while (iter.hasNext()) {
            next = iter.next();
        }

        return NullabLe.of(next);
    }

    @Override
    public Stream<T> skipNull() {
        return filter(Fn.notNull());
    }

    @Override
    public Stream<Stream<T>> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final Iterator<T> iter = this.iterator();
        final ExList<T> list = new ExList<>();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final Stream<T>[] a = new Stream[] { new ArrayStream<>((T[]) list.array(), 0, list.size(), null, sorted, cmp),
                new IteratorStream<>(iter, null, sorted, cmp) };

        return newStream(a, false, null);
    }

    @Override
    public Stream<Stream<T>> splitBy(Predicate<? super T> where) {
        N.requireNonNull(where);

        final Iterator<T> iter = this.iterator();
        final List<T> list = new ArrayList<>();
        T next = null;
        Stream<T> s = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (where.test(next)) {
                list.add(next);
            } else {
                s = Stream.of(next);

                break;
            }
        }

        final Stream<T>[] a = new Stream[] { new ArrayStream<>((T[]) list.toArray(), null, sorted, cmp), new IteratorStream<>(iter, null, sorted, cmp) };

        if (s != null) {
            if (sorted) {
                a[1] = new IteratorStream<>(a[1].prepend(s).iterator(), null, sorted, cmp);
            } else {
                a[1] = a[1].prepend(s);
            }
        }

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<List<T>> sliding2(int windowSize) {
        return sliding2(windowSize, 1);
    }

    @Override
    public Stream<T> intersection(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).iterator(), sorted, cmp);
    }

    @Override
    public Stream<T> intersection(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(mapper.apply(value)) > 0;
            }
        }).iterator(), sorted, cmp);
    }

    @Override
    public Stream<T> difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).iterator(), sorted, cmp);
    }

    @Override
    public Stream<T> difference(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(mapper.apply(value)) < 1;
            }
        }).iterator(), sorted, cmp);
    }

    @Override
    public Stream<T> symmetricDifference(final Collection<T> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) > 0;
            }
        })).iterator(), false, null);
    }

    @Override
    public Stream<T> reversed() {
        final T[] tmp = (T[]) toArray();

        return newStream(new ExIterator<T>() {
            private int cursor = tmp.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public T next() {
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
            public <A> A[] toArray(A[] a) {
                a = a.length >= cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), cursor);

                for (int i = 0, len = a.length; i < len; i++) {
                    a[i] = (A) tmp[cursor - i - 1];
                }

                return a;
            }
        }, false, null);
    }

    @Override
    public Stream<T> shuffled() {
        final T[] a = (T[]) toArray();

        N.shuffle(a);

        return newStream(a, false, null);
    }

    @Override
    public Stream<T> shuffled(final Random rnd) {
        final T[] a = (T[]) toArray();

        N.shuffle(a, rnd);

        return newStream(a, false, null);
    }

    @Override
    public Stream<T> rotated(int distance) {
        final T[] a = (T[]) toArray();

        N.rotate(a, distance);

        return newStream(a, false, null);
    }

    @Override
    public Stream<T> distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return set.add(hashKey(value));
            }
        }).iterator(), sorted, cmp);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyExtractor) {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return set.add(hashKey(keyExtractor.apply(value)));
            }
        }).iterator(), sorted, cmp);
    }

    @Override
    public Optional<Map<Percentage, T>> distribution() {
        final Object[] a = sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Optional.empty();
        }

        return Optional.of((Map<Percentage, T>) N.distribution(a));
    }

    @Override
    public Optional<Map<Percentage, T>> distribution(Comparator<? super T> comparator) {
        final Object[] a = sorted(comparator).toArray();

        if (N.isNullOrEmpty(a)) {
            return Optional.empty();
        }

        return Optional.of((Map<Percentage, T>) N.distribution(a));
    }

    @Override
    public Pair<CharSummaryStatistics, Optional<Map<Percentage, Character>>> summarizeChar2(ToCharFunction<? super T> mapper) {
        final char[] a = mapToChar(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new CharSummaryStatistics(), Optional.<Map<Percentage, Character>> empty());
        } else {
            return Pair.of(new CharSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarizeByte2(ToByteFunction<? super T> mapper) {
        final byte[] a = mapToByte(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new ByteSummaryStatistics(), Optional.<Map<Percentage, Byte>> empty());
        } else {
            return Pair.of(new ByteSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public Pair<ShortSummaryStatistics, Optional<Map<Percentage, Short>>> summarizeShort2(ToShortFunction<? super T> mapper) {
        final short[] a = mapToShort(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new ShortSummaryStatistics(), Optional.<Map<Percentage, Short>> empty());
        } else {
            return Pair.of(new ShortSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public Pair<IntSummaryStatistics, Optional<Map<Percentage, Integer>>> summarizeInt2(ToIntFunction<? super T> mapper) {
        final int[] a = mapToInt(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new IntSummaryStatistics(), Optional.<Map<Percentage, Integer>> empty());
        } else {
            return Pair.of(new IntSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public Pair<LongSummaryStatistics, Optional<Map<Percentage, Long>>> summarizeLong2(ToLongFunction<? super T> mapper) {
        final long[] a = mapToLong(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new LongSummaryStatistics(), Optional.<Map<Percentage, Long>> empty());
        } else {
            return Pair.of(new LongSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public Pair<FloatSummaryStatistics, Optional<Map<Percentage, Float>>> summarizeFloat2(ToFloatFunction<? super T> mapper) {
        final float[] a = mapToFloat(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new FloatSummaryStatistics(), Optional.<Map<Percentage, Float>> empty());
        } else {
            return Pair.of(new FloatSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarizeDouble2(ToDoubleFunction<? super T> mapper) {
        final double[] a = mapToDouble(mapper).sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new DoubleSummaryStatistics(), Optional.<Map<Percentage, Double>> empty());
        } else {
            return Pair.of(new DoubleSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    //    @Override
    //    public Stream<Set<T>> powerSet() {
    //        final Set<T> set = toSet(new Supplier<Set<T>>() {
    //            @Override
    //            public Set<T> get() {
    //                return new LinkedHashSet<>();
    //            }
    //        });
    //
    //        return newStream(Seq.powerSet(set).iterator(), false, null);
    //    }

    @Override
    public Stream<ExList<T>> combinations() {
        if (this instanceof ArrayStream) {
            return newStream(IntStream.rangeClosed(0, (int) count()).flatMapToObj(new IntFunction<Stream<ExList<T>>>() {
                @Override
                public Stream<ExList<T>> apply(int value) {
                    return combinations(value);
                }
            }).iterator(), false, null);
        } else {
            return newStream((T[]) toArray(), false, null).combinations();
        }
    }

    @Override
    public Stream<ExList<T>> combinations(final int len) {
        if (this instanceof ArrayStream) {
            N.checkFromIndexSize(0, len, (int) count());

            if (len == 0) {
                return newStream(N.asArray(ExList.<T> empty()), false, null);
            } else if (len == 1) {
                return map(new Function<T, ExList<T>>() {
                    @Override
                    public ExList<T> apply(T t) {
                        return ExList.of(t);
                    }
                });
            } else if (len == count()) {
                return newStream(N.asArray(toExList()), false, null);
            } else {
                final T[] a = ((ArrayStream<T>) this).elements;
                final int fromIndex = ((ArrayStream<T>) this).fromIndex;
                final int toIndex = ((ArrayStream<T>) this).toIndex;

                return newStream(new ExIterator<ExList<T>>() {

                    private final int[] indices = Array.range(fromIndex, fromIndex + len);

                    @Override
                    public boolean hasNext() {
                        return indices[0] <= toIndex - len;
                    }

                    @Override
                    public ExList<T> next() {
                        final ExList<T> result = new ExList<>(len);

                        for (int idx : indices) {
                            result.add(a[idx]);
                        }

                        if (++indices[len - 1] == toIndex) {
                            for (int i = len - 1; i > 0; i--) {
                                if (indices[i] > toIndex - (len - i)) {
                                    indices[i - 1]++;

                                    for (int j = i; j < len; j++) {
                                        indices[j] = indices[j - 1] + 1;
                                    }
                                }
                            }
                        }

                        return result;
                    }

                }, false, null);
            }
        } else {
            return newStream((T[]) toArray(), false, null).combinations(len);
        }
    }

    @Override
    public Stream<ExList<T>> permutations() {
        return newStream(PermutationIterator.of(toList()), false, null);
    }

    @Override
    public Stream<ExList<T>> orderedPermutations() {
        return orderedPermutations(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<ExList<T>> orderedPermutations(Comparator<? super T> comparator) {
        final Iterator<ExList<T>> iter = PermutationIterator.ordered(toList(), comparator == null ? OBJECT_COMPARATOR : comparator);

        return newStream(iter, false, null);
    }

    @Override
    public Stream<ExList<T>> cartesianProduct(final Collection<? extends T>... cs) {
        return cartesianProduct(Arrays.asList(cs));
    }

    @Override
    public Stream<ExList<T>> cartesianProduct(final Collection<? extends Collection<? extends T>> cs) {
        final List<Collection<? extends T>> cList = new ArrayList<>(cs.size() + 1);
        cList.add(this.toList());
        cList.addAll(cs);

        return newStream(Seq.cartesianProduct(cList).iterator(), false, null);
    }

    @Override
    public DataSet toDataSet() {
        return toDataSet(null);
    }

    @Override
    public DataSet toDataSet(List<String> columnNames) {
        return N.newDataSet(columnNames, toList());
    }

    @Override
    public String join(CharSequence delimiter) {
        final Function<T, String> mapper = new Function<T, String>() {
            @SuppressWarnings("rawtypes")
            @Override
            public String apply(T t) {
                return t instanceof BaseStream ? ((BaseStream) t).join(", ", "[", "]") : N.toString(t);
            }
        };

        return this.map(mapper).collect(Collectors.joining(delimiter));
    }

    @Override
    public String join(CharSequence delimiter, CharSequence prefix, CharSequence suffix) {
        final Function<T, String> mapper = new Function<T, String>() {
            @SuppressWarnings("rawtypes")
            @Override
            public String apply(T t) {
                return t instanceof BaseStream ? ((BaseStream) t).join(", ", "[", "]") : N.toString(t);
            }
        };

        return this.map(mapper).collect(Collectors.joining(delimiter, prefix, suffix));
    }

    @Override
    public boolean hasDuplicates() {
        final Set<T> set = new HashSet<>();
        final Iterator<T> iter = iterator();

        while (iter.hasNext()) {
            if (set.add(iter.next()) == false) {
                return true;
            }
        }

        return false;
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator) {
        final BinaryOperator<U> combiner = reducingCombiner;

        return reduce(identity, accumulator, combiner);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public <R, A> R collect(java.util.stream.Collector<? super T, A, R> collector) {
        return collect(Collector.of(collector));
    }

    @Override
    public Pair<NullabLe<T>, Stream<T>> headAndTail() {
        return Pair.of(head(), tail());
    }

    @Override
    public Pair<Stream<T>, NullabLe<T>> headAndTail2() {
        return Pair.of(head2(), tail2());
    }

    @Override
    public Stream<Indexed<T>> indexed() {
        final MutableLong idx = new MutableLong();

        return newStream(this.sequential().map(new Function<T, Indexed<T>>() {
            @Override
            public Indexed<T> apply(T t) {
                return Indexed.of(idx.getAndIncrement(), t);
            }
        }).iterator(), true, INDEXED_COMPARATOR);
    }

    @Override
    public Stream<T> queued() {
        return queued(DEFAULT_QUEUE_SIZE_PER_ITERATOR);
    }

    @Override
    public Stream<T> append(Stream<T> stream) {
        return Stream.concat(this, stream);
    }

    @Override
    public Stream<T> append(Collection<? extends T> c) {
        return append(Stream.of(c));
    }

    @Override
    public Stream<T> prepend(Stream<T> stream) {
        return Stream.concat(stream, this);
    }

    @Override
    public Stream<T> prepend(Collection<? extends T> c) {
        return prepend(Stream.of(c));
    }

    @Override
    public Stream<T> merge(final Stream<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return Stream.merge(this, b, nextSelector);
    }

    @Override
    public <T2, R> Stream<R> zipWith(Stream<T2> b, BiFunction<? super T, ? super T2, R> zipFunction) {
        return Stream.zip(this, b, zipFunction);
    }

    @Override
    public <T2, T3, R> Stream<R> zipWith(Stream<T2> b, Stream<T3> c, TriFunction<? super T, ? super T2, ? super T3, R> zipFunction) {
        return Stream.zip(this, b, c, zipFunction);
    }

    @Override
    public <T2, R> Stream<R> zipWith(Stream<T2> b, T valueForNoneA, T2 valueForNoneB, BiFunction<? super T, ? super T2, R> zipFunction) {
        return Stream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public <T2, T3, R> Stream<R> zipWith(Stream<T2> b, Stream<T3> c, T valueForNoneA, T2 valueForNoneB, T3 valueForNoneC,
            TriFunction<? super T, ? super T2, ? super T3, R> zipFunction) {
        return Stream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public Stream<T> cached() {
        return newStream((T[]) toArray(), sorted, cmp);
    }

    @Override
    public Stream<T> cached(IntFunction<T[]> generator) {
        return newStream(toArray(generator), sorted, cmp);
    }

    @Override
    public long persist(File file, Function<? super T, String> toLine) {
        Writer writer = null;

        try {
            writer = new FileWriter(file);
            return persist(writer, toLine);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(writer);
        }
    }

    @Override
    public long persist(OutputStream os, Function<? super T, String> toLine) {
        final BufferedWriter bw = ObjectFactory.createBufferedWriter(os);

        try {
            return persist(bw, toLine);
        } finally {
            ObjectFactory.recycle(bw);
        }
    }

    @Override
    public long persist(Writer writer, Function<? super T, String> toLine) {
        final Iterator<T> iter = iterator();
        final BufferedWriter bw = writer instanceof BufferedWriter ? (BufferedWriter) writer : ObjectFactory.createBufferedWriter(writer);
        long cnt = 0;

        try {
            while (iter.hasNext()) {
                bw.write(toLine.apply(iter.next()));
                bw.write(N.LINE_SEPARATOR);
                cnt++;
            }
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (bw != writer) {
                ObjectFactory.recycle(bw);
            }
        }
        return cnt;
    }

    @Override
    public long persist(final Connection conn, final String insertSQL, final int batchSize, final int batchInterval,
            final BiConsumer<? super T, ? super PreparedStatement> stmtSetter) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

            return persist(stmt, batchSize, batchInterval, stmtSetter);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    @Override
    public long persist(final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final BiConsumer<? super T, ? super PreparedStatement> stmtSetter) {
        final Iterator<T> iter = iterator();

        long cnt = 0;
        try {
            while (iter.hasNext()) {
                stmtSetter.accept(iter.next(), stmt);

                stmt.addBatch();

                if ((++cnt % batchSize) == 0) {
                    stmt.executeBatch();
                    stmt.clearBatch();

                    if (batchInterval > 0) {
                        N.sleep(batchInterval);
                    }
                }
            }

            if ((cnt % batchSize) > 0) {
                stmt.executeBatch();
                stmt.clearBatch();
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        }

        return cnt;
    }

    private static void closeQuietly(final Statement stmt) {
        if (stmt != null) {
            if (stmt instanceof PreparedStatement) {
                try {
                    ((PreparedStatement) stmt).clearParameters();
                } catch (Throwable e) {
                    logger.error("Failed to clear parameters", e);
                }
            }

            try {
                stmt.close();
            } catch (Throwable e) {
                logger.error("Failed to close Statement", e);
            }
        }
    }
}
