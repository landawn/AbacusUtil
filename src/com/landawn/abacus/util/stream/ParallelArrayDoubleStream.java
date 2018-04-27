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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleToFloatFunction;
import com.landawn.abacus.util.function.DoubleToIntFunction;
import com.landawn.abacus.util.function.DoubleToLongFunction;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelArrayDoubleStream extends ArrayDoubleStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayDoubleStream sequential;
    private volatile Stream<Double> boxed;

    ParallelArrayDoubleStream(final double[] values, final int fromIndex, final int toIndex, final boolean sorted, int maxThreadNum, Splitor splitor,
            final Collection<Runnable> closeHandlers) {
        super(values, fromIndex, toIndex, sorted, closeHandlers);

        this.maxThreadNum = checkMaxThreadNum(maxThreadNum);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return super.filter(predicate);
        }

        final Stream<Double> stream = boxed().filter(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return super.takeWhile(predicate);
        }

        final Stream<Double> stream = boxed().takeWhile(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return super.dropWhile(predicate);
        }

        final Stream<Double> stream = boxed().dropWhile(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream map(final DoubleUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return super.map(mapper);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final DoubleToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return super.mapToInt(mapper);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Double>() {
            @Override
            public int applyAsInt(Double value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final DoubleToLongFunction mapper) {
        if (maxThreadNum <= 1) {
            return super.mapToLong(mapper);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Double>() {
            @Override
            public long applyAsLong(Double value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final DoubleToFloatFunction mapper) {
        if (maxThreadNum <= 1) {
            return super.mapToFloat(mapper);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Double>() {
            @Override
            public float applyAsFloat(Double value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final DoubleFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return super.mapToObj(mapper);
        }

        return boxed().map(new Function<Double, U>() {
            @Override
            public U apply(Double value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public DoubleStream flatMap(final DoubleFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMap(mapper), false, maxThreadNum, splitor, null);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Double, DoubleStream>() {
            @Override
            public DoubleStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public IntStream flatMapToInt(final DoubleFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper), false, maxThreadNum, splitor, null);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Double, IntStream>() {
            @Override
            public IntStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public LongStream flatMapToLong(final DoubleFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper), false, maxThreadNum, splitor, null);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Double, LongStream>() {
            @Override
            public LongStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public FloatStream flatMapToFloat(final DoubleFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper), false, maxThreadNum, splitor, null);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Double, FloatStream>() {
            @Override
            public FloatStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final DoubleFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper), false, null, maxThreadNum, splitor, null);
        }

        return boxed().flatMap(new Function<Double, Stream<T>>() {
            @Override
            public Stream<T> apply(Double value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public DoubleStream top(int n) {
        return top(n, DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream top(int n, Comparator<? super Double> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, DOUBLE_COMPARATOR)) {
            return new ParallelArrayDoubleStream(elements, toIndex - n, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            final double[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayDoubleStream(a, 0, a.length, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public DoubleStream peek(final DoubleConsumer action) {
        if (maxThreadNum <= 1) {
            return super.peek(action);
        }

        final DoubleStream stream = boxed().peek(new Consumer<Double>() {
            @Override
            public void accept(Double t) {
                action.accept(t);
            }
        }).sequential().mapToDouble(ToDoubleFunction.UNBOX);

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream limit(long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

        if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, (int) (fromIndex + maxSize), sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream skip(long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayDoubleStream(elements, toIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            return new ParallelArrayDoubleStream(elements, (int) (fromIndex + n), toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public <E extends Exception> void forEach(final Try.DoubleConsumer<E> action) throws E {
        if (maxThreadNum <= 1) {
            super.forEach(action);
            return;
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                action.accept(elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        double next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                action.accept(next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final DoubleFunction<? extends K> keyExtractor, final DoubleFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return super.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Double, ? extends K> keyExtractor2 = new Function<Double, K>() {
            @Override
            public K apply(Double value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Double, ? extends U> valueMapper2 = new Function<Double, U>() {
            @Override
            public U apply(Double value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyExtractor2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return super.toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Double, ? extends K> classifier2 = new Function<Double, K>() {
            @Override
            public K apply(Double value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public double reduce(final double identity, final DoubleBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return super.reduce(identity, op);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Double>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Double>() {
                    @Override
                    public Double call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        double result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = op.applyAsDouble(result, elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Double>() {
                    @Override
                    public Double call() {
                        double result = identity;
                        double next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = op.applyAsDouble(result, next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        Double result = null;

        try {
            for (CompletableFuture<Double> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsDouble(result, future.get());
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalDouble reduce(final DoubleBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return super.reduce(accumulator);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Double>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Double>() {
                    @Override
                    public Double call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return null;
                        }

                        double result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.applyAsDouble(result, elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Double>() {
                    @Override
                    public Double call() {
                        double result = 0;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return null;
                            }
                        }

                        double next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = accumulator.applyAsDouble(result, next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        Double result = null;

        try {
            for (CompletableFuture<Double> future : futureList) {
                final Double tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsDouble(result, tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalDouble.empty() : OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjDoubleConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return super.collect(supplier, accumulator, combiner);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<R>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        final R container = supplier.get();

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                accumulator.accept(container, elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return container;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        final R container = supplier.get();
                        double next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                accumulator.accept(container, next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return container;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        R container = (R) NONE;

        try {
            for (CompletableFuture<R> future : futureList) {
                if (container == NONE) {
                    container = future.get();
                } else {
                    combiner.accept(container, future.get());
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return container == NONE ? supplier.get() : container;
    }

    @Override
    public DoubleStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayDoubleStream(elements, fromIndex + 1, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream headd() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex - 1, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public OptionalDouble min() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[fromIndex]);
        } else if (maxThreadNum <= 1) {
            return OptionalDouble.of(N.min(elements, fromIndex, toIndex));
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Double>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Double>() {
                @Override
                public Double call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : N.min(elements, cursor, to);
                }
            }));
        }

        Double candidate = null;

        try {
            for (CompletableFuture<Double> future : futureList) {
                final Double tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || N.compare(tmp.doubleValue(), candidate.doubleValue()) < 0) {
                    candidate = tmp;
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalDouble.empty() : OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble max() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[toIndex - 1]);
        } else if (maxThreadNum <= 1) {
            return OptionalDouble.of(N.max(elements, fromIndex, toIndex));
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Double>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Double>() {
                @Override
                public Double call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    return cursor >= to ? null : N.max(elements, cursor, to);
                }
            }));
        }

        Double candidate = null;

        try {
            for (CompletableFuture<Double> future : futureList) {
                final Double tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || N.compare(tmp.doubleValue(), candidate.doubleValue()) > 0) {
                    candidate = tmp;
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalDouble.empty() : OptionalDouble.of(candidate);
    }

    @Override
    public double sum() {
        if (fromIndex == toIndex) {
            return 0d;
        } else if (maxThreadNum <= 1) {
            return super.sum();
        }

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

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<double[]>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<double[]>() {
                @Override
                public double[] call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    double[] container = supplier.get();

                    while (cursor < to) {
                        accumulator.accept(container, elements[cursor++]);
                    }

                    return container;
                }
            }));
        }

        double[] summation = null;

        try {
            for (CompletableFuture<double[]> future : futureList) {
                final double[] tmp = future.get();

                if (summation == null) {
                    summation = tmp;
                } else {
                    combiner.accept(summation, tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return Collectors.computeFinalSum(summation);
    }

    @Override
    public OptionalDouble average() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        } else if (maxThreadNum <= 1) {
            return super.average();
        }

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

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<double[]>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<double[]>() {
                @Override
                public double[] call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    double[] container = supplier.get();

                    while (cursor < to) {
                        accumulator.accept(container, elements[cursor++]);
                    }

                    return container;
                }
            }));
        }

        double[] avg = null;

        try {
            for (CompletableFuture<double[]> future : futureList) {
                final double[] tmp = future.get();

                if (avg == null) {
                    avg = tmp;
                } else {
                    combiner.accept(avg, tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return avg[2] > 0 ? OptionalDouble.of(Collectors.computeFinalSum(avg) / avg[2]) : OptionalDouble.empty();
    }

    @Override
    public DoubleSummaryStatistics summarize() {
        if (fromIndex == toIndex) {
            return new DoubleSummaryStatistics();
        } else if (maxThreadNum <= 1) {
            return super.summarize();
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<DoubleSummaryStatistics>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<DoubleSummaryStatistics>() {
                @Override
                public DoubleSummaryStatistics call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    final DoubleSummaryStatistics result = new DoubleSummaryStatistics();

                    for (int i = cursor; i < to; i++) {
                        result.accept(elements[i]);
                    }

                    return result;
                }
            }));
        }

        DoubleSummaryStatistics result = null;

        try {
            for (CompletableFuture<DoubleSummaryStatistics> future : futureList) {
                final DoubleSummaryStatistics tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result.combine(tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.DoublePredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return super.anyMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && result.isFalse() && eHolder.value() == null) {
                                if (predicate.test(elements[cursor++])) {
                                    result.setTrue();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        double next = 0;

                        try {
                            while (result.isFalse() && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.DoublePredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return super.allMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && result.isTrue() && eHolder.value() == null) {
                                if (predicate.test(elements[cursor++]) == false) {
                                    result.setFalse();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        double next = 0;

                        try {
                            while (result.isTrue() && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.DoublePredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return super.noneMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && result.isTrue() && eHolder.value() == null) {
                                if (predicate.test(elements[cursor++])) {
                                    result.setFalse();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        double next = 0;

                        try {
                            while (result.isTrue() && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> OptionalDouble findFirst(final Try.DoublePredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return super.findFirst(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Double>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, Double> pair = new Pair<>();

                        try {
                            while (cursor < to && (resultHolder.value() == null || cursor < resultHolder.value().left) && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[cursor++];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final Pair<Integer, Double> pair = new Pair<>();

                        try {
                            while (resultHolder.value() == null && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        pair.left = cursor.intValue();
                                        pair.right = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? OptionalDouble.empty() : OptionalDouble.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> OptionalDouble findLast(final Try.DoublePredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return super.findLast(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Double>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, Double> pair = new Pair<>();

                        try {
                            while (cursor > from && (resultHolder.value() == null || cursor > resultHolder.value().left) && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[--cursor];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(toIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final Pair<Integer, Double> pair = new Pair<>();

                        try {
                            while (resultHolder.value() == null && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() > fromIndex) {
                                        pair.left = cursor.intValue();
                                        pair.right = elements[cursor.decrementAndGet()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? OptionalDouble.empty() : OptionalDouble.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> OptionalDouble findAny(final Try.DoublePredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return super.findAny(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(NONE);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        double next = 0;

                        try {
                            while (cursor < to && resultHolder.value() == NONE && eHolder.value() == null) {
                                next = elements[cursor++];

                                if (predicate.test(next)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == NONE) {
                                            resultHolder.setValue(next);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        double next = 0;

                        try {
                            while (resultHolder.value() == NONE && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == NONE) {
                                            resultHolder.setValue(next);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complette(futureList, eHolder, (E) null);

        return resultHolder.value() == NONE ? OptionalDouble.empty() : OptionalDouble.of((Double) resultHolder.value());
    }

    @Override
    public Stream<Double> boxed() {
        Stream<Double> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Double>(iterator(), sorted, sorted ? DOUBLE_COMPARATOR : null, maxThreadNum, splitor, closeHandlers);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public DoubleStream append(DoubleStream stream) {
        return new ParallelIteratorDoubleStream(DoubleStream.concat(this, stream), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream prepend(DoubleStream stream) {
        return new ParallelIteratorDoubleStream(DoubleStream.concat(stream, this), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream merge(final DoubleStream b, final DoubleBiFunction<Nth> nextSelector) {
        return new ParallelIteratorDoubleStream(DoubleStream.merge(this, b, nextSelector), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleBiFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, DoubleTriFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, c, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, double valueForNoneA, double valueForNoneB, DoubleBiFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), false, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, double valueForNoneA, double valueForNoneB, double valueForNoneC,
            DoubleTriFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), false, maxThreadNum,
                splitor, closeHandlers);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public DoubleStream sequential() {
        ArrayDoubleStream tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayDoubleStream(elements, fromIndex, toIndex, sorted, closeHandlers);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public DoubleStream parallel(int maxThreadNum, Splitor splitor) {
        if (this.maxThreadNum == checkMaxThreadNum(maxThreadNum) && this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public DoubleStream maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == checkMaxThreadNum(maxThreadNum)) {
            return this;
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public DoubleStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, newCloseHandlers);
    }
}
