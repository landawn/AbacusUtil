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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.exception.AbacusIOException;
import com.landawn.abacus.exception.AbacusSQLException;
import com.landawn.abacus.util.BufferedWriter;
import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.Indexed;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.JdbcUtil;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.ObjectFactory;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.PermutationIterator;
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
    public Stream<T> filter(final Predicate<? super T> predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public <U> Stream<T> filter(final U check, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, check);
            }
        });
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public <U> Stream<T> takeWhile(final U check, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, check);
            }
        });
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public <U> Stream<T> dropWhile(final U check, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, check);
            }
        });
    }

    @Override
    public Stream<T> drop(final long n, final Consumer<? super T> action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return dropWhile(new Predicate<T>() {
                @Override
                public boolean test(T value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return dropWhile(new Predicate<T>() {
                @Override
                public boolean test(T value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        }
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate, final Consumer<? super T> action) {
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
    public <U> Stream<T> dropWhile(final U check, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super T> action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, check);
            }
        }, action);
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
    public <U, R> Stream<R> flatMap(final U seed, final BiFunction<? super T, ? super U, ? extends Stream<? extends R>> mapper) {
        return flatMap(new Function<T, Stream<? extends R>>() {
            @Override
            public Stream<? extends R> apply(T t) {
                return mapper.apply(t, seed);
            }
        });
    }

    @Override
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
        return flatMap0(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return mapper.apply(t).iterator();
            }
        });
    }

    abstract <R> Stream<R> flatMap0(final Function<? super T, ? extends Iterator<? extends R>> mapper);

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
    public <R> Stream<R> flatMap3(final Function<? super T, ? extends R[]> mapper) {
        return flatMap0(new Function<T, Iterator<? extends R>>() {
            @Override
            public Iterator<? extends R> apply(T t) {
                return ImmutableIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
        return flatMapToChar0(new Function<T, CharIterator>() {
            @Override
            public CharIterator apply(T t) {
                return mapper.apply(t).charIterator();
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

                return new ImmutableCharIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public char next() {
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
                return ImmutableCharIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        return flatMapToByte0(new Function<T, ByteIterator>() {
            @Override
            public ByteIterator apply(T t) {
                return mapper.apply(t).byteIterator();
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

                return new ImmutableByteIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public byte next() {
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
                return ImmutableByteIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        return flatMapToShort0(new Function<T, ShortIterator>() {
            @Override
            public ShortIterator apply(T t) {
                return mapper.apply(t).shortIterator();
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

                return new ImmutableShortIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public short next() {
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
                return ImmutableShortIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        return flatMapToInt0(new Function<T, IntIterator>() {
            @Override
            public IntIterator apply(T t) {
                return mapper.apply(t).intIterator();
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

                return new ImmutableIntIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public int next() {
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
                return ImmutableIntIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        return flatMapToLong0(new Function<T, LongIterator>() {
            @Override
            public LongIterator apply(T t) {
                return mapper.apply(t).longIterator();
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

                return new ImmutableLongIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public long next() {
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
                return ImmutableLongIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        return flatMapToFloat0(new Function<T, FloatIterator>() {
            @Override
            public FloatIterator apply(T t) {
                return mapper.apply(t).floatIterator();
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

                return new ImmutableFloatIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public float next() {
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
                return ImmutableFloatIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        return flatMapToDouble0(new Function<T, DoubleIterator>() {
            @Override
            public DoubleIterator apply(T t) {
                return mapper.apply(t).doubleIterator();
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

                return new ImmutableDoubleIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public double next() {
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
                return ImmutableDoubleIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return splitIntoList(size).map(new Function<List<T>, Stream<T>>() {
            @Override
            public Stream<T> apply(List<T> t) {
                return Stream.of(t);
            }
        });
    }

    @Override
    public <U> Stream<Stream<T>> split(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> boundaryUpdate) {
        return splitIntoList(boundary, predicate, boundaryUpdate).map(new Function<List<T>, Stream<T>>() {
            @Override
            public Stream<T> apply(List<T> t) {
                return Stream.of(t);
            }
        });
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
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, D, A> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapFactory));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K, U> Stream<Entry<K, List<U>>> groupBy2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return groupBy(keyMapper, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @Override
    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> Stream<Map.Entry<K, List<U>>> groupBy2(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<M> mapSupplier) {
        final Map<K, List<U>> map = collect(
                Collectors.groupingBy(keyMapper, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapSupplier));

        return newStream(map.entrySet().iterator(), false, null);
    }

    @Override
    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<T>>>() {
            @Override
            public Map<K, List<T>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<T>>> M toMap(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U> Map<K, List<U>> toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyMapper, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    @Override
    public <K, U, M extends Map<K, List<U>>> M toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapSupplier) {
        return toMap(keyMapper, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapSupplier);
    }

    @Override
    public <K> Multimap<K, T, List<T>> toMultimap(Function<? super T, ? extends K> keyMapper) {
        return toMultimap(keyMapper, new Function<T, T>() {
            @Override
            public T apply(T t) {
                return t;
            }
        });
    }

    @Override
    public <K, V extends Collection<T>> Multimap<K, T, V> toMultimap(Function<? super T, ? extends K> keyMapper, Supplier<Multimap<K, T, V>> mapSupplier) {
        return toMultimap(keyMapper, new Function<T, T>() {
            @Override
            public T apply(T t) {
                return t;
            }
        }, mapSupplier);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
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
    public OptionalNullable<T> first() {
        final Iterator<T> iter = this.iterator();

        if (iter.hasNext() == false) {
            return OptionalNullable.empty();
        }

        return OptionalNullable.of(iter.next());
    }

    @Override
    public OptionalNullable<T> last() {
        final Iterator<T> iter = this.iterator();

        if (iter.hasNext() == false) {
            return OptionalNullable.empty();
        }

        T next = null;

        while (iter.hasNext()) {
            next = iter.next();
        }

        return OptionalNullable.of(next);
    }

    //    @Override
    //    public OptionalNullable<T> any() {
    //        return findAny(Predicate.ALWAYS_TRUE);
    //    }

    @Override
    public Stream<T> skipNull() {
        return filter(Predicate.NOT_NULL);
    }

    @Override
    public Stream<T> except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public Stream<T> except(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(mapper.apply(value)) < 1;
            }
        });
    }

    @Override
    public Stream<T> intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    @Override
    public Stream<T> intersect(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(mapper.apply(value)) > 0;
            }
        });
    }

    @Override
    public Stream<T> xor(final Collection<? extends T> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append((Stream<T>) Stream.of(c).filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) > 0;
            }
        }));
    }

    @Override
    public Stream<Stream<T>> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final Iterator<T> iter = this.iterator();
        final List<T> list = new ArrayList<>();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final Stream<T>[] a = new Stream[] { new ArrayStream<T>((T[]) list.toArray(), null, sorted, cmp), new IteratorStream<T>(iter, null, sorted, cmp) };

        return newStream(a, false, null);
    }

    @Override
    public Stream<Stream<T>> splitBy(Predicate<? super T> where) {
        N.requireNonNull(where);

        final Iterator<T> iter = this.iterator();
        final List<T> list = new ArrayList<>();
        T next = null;
        Stream<T> p = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (where.test(next)) {
                list.add(next);
            } else {
                p = Stream.of(next);

                break;
            }
        }

        final Stream<T>[] a = new Stream[] { new ArrayStream<T>((T[]) list.toArray(), null, sorted, cmp), new IteratorStream<T>(iter, null, sorted, cmp) };

        if (p != null) {
            a[1] = a[1].prepend(p);
        }

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<List<T>> sliding(int windowSize) {
        return sliding(windowSize, 1);
    }

    @Override
    public Stream<T> reverse() {
        final T[] a = (T[]) toArray();

        //        N.reverse(a);
        //
        //        return newStream((T[]) a, false, null);

        return newStream(new ImmutableIterator<T>() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public T next() {
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
        }, false, null);
    }

    @Override
    public Stream<T> shuffle() {
        final T[] a = (T[]) toArray();

        N.shuffle(a);

        return newStream(a, false, null);
    }

    @Override
    public Optional<Map<Percentage, T>> distribution() {
        final Object[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of((Map<Percentage, T>) N.distribution(a));
    }

    @Override
    public Optional<Map<Percentage, T>> distribution(Comparator<? super T> comparator) {
        final Object[] a = sorted(comparator).toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of((Map<Percentage, T>) N.distribution(a));
    }

    @Override
    public Pair<CharSummaryStatistics, Optional<Map<Percentage, Character>>> summarizeChar2(ToCharFunction<? super T> mapper) {
        final char[] a = mapToChar(mapper).sorted().toArray();

        final CharSummaryStatistics summaryStatistics = new CharSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Character>> distribution = a.length == 0 ? Optional.<Map<Percentage, Character>> empty()
                : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Stream<List<T>> permutation() {
        return newStream(PermutationIterator.of(toList()), false, null);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Stream<List<T>> orderedPermutation() {
        final Iterator<List<T>> iter = PermutationIterator.ordered((List) toList());
        return newStream(iter, false, null);
    }

    @Override
    public Stream<List<T>> orderedPermutation(Comparator<? super T> comparator) {
        final Iterator<List<T>> iter = PermutationIterator.ordered(toList(), comparator == null ? OBJECT_COMPARATOR : comparator);
        return newStream(iter, false, null);
    }

    @Override
    public Stream<Set<T>> powerSet() {
        final Set<T> set = toSet(new Supplier<Set<T>>() {
            @Override
            public Set<T> get() {
                return new LinkedHashSet<>();
            }
        });

        return newStream(N.powerSet(set).iterator(), false, null);
    }

    @Override
    public Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarizeByte2(ToByteFunction<? super T> mapper) {
        final byte[] a = mapToByte(mapper).sorted().toArray();

        final ByteSummaryStatistics summaryStatistics = new ByteSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Byte>> distribution = a.length == 0 ? Optional.<Map<Percentage, Byte>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<ShortSummaryStatistics, Optional<Map<Percentage, Short>>> summarizeShort2(ToShortFunction<? super T> mapper) {
        final short[] a = mapToShort(mapper).sorted().toArray();

        final ShortSummaryStatistics summaryStatistics = new ShortSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Short>> distribution = a.length == 0 ? Optional.<Map<Percentage, Short>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<IntSummaryStatistics, Optional<Map<Percentage, Integer>>> summarizeInt2(ToIntFunction<? super T> mapper) {
        final int[] a = mapToInt(mapper).sorted().toArray();

        final IntSummaryStatistics summaryStatistics = new IntSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Integer>> distribution = a.length == 0 ? Optional.<Map<Percentage, Integer>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<LongSummaryStatistics, Optional<Map<Percentage, Long>>> summarizeLong2(ToLongFunction<? super T> mapper) {
        final long[] a = mapToLong(mapper).sorted().toArray();

        final LongSummaryStatistics summaryStatistics = new LongSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Long>> distribution = a.length == 0 ? Optional.<Map<Percentage, Long>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<FloatSummaryStatistics, Optional<Map<Percentage, Float>>> summarizeFloat2(ToFloatFunction<? super T> mapper) {
        final float[] a = mapToFloat(mapper).sorted().toArray();

        final FloatSummaryStatistics summaryStatistics = new FloatSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Float>> distribution = a.length == 0 ? Optional.<Map<Percentage, Float>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarizeDouble2(ToDoubleFunction<? super T> mapper) {
        final double[] a = mapToDouble(mapper).sorted().toArray();

        final DoubleSummaryStatistics summaryStatistics = new DoubleSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Double>> distribution = a.length == 0 ? Optional.<Map<Percentage, Double>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public DataSet toDataSet(List<String> columnNames) {
        return N.newDataSet(columnNames, toList());
    }

    @Override
    public String join(CharSequence delimiter) {
        final Function<T, String> mapper = new Function<T, String>() {
            @Override
            public String apply(T t) {
                return N.toString(t);
            }
        };

        return this.map(mapper).collect(Collectors.joining(delimiter));
    }

    @Override
    public String join(CharSequence delimiter, CharSequence prefix, CharSequence suffix) {
        final Function<T, String> mapper = new Function<T, String>() {
            @Override
            public String apply(T t) {
                return N.toString(t);
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
    public Stream<Indexed<T>> indexed() {
        final MutableLong idx = new MutableLong();

        return map(new Function<T, Indexed<T>>() {
            @Override
            public Indexed<T> apply(T t) {
                return Indexed.of(idx.getAndIncrement(), t);
            }
        });
    }

    @Override
    public Stream<T> append(final Stream<T> stream) {
        return Stream.concat(this, stream);
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
            JdbcUtil.closeQuietly(stmt);
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
}
