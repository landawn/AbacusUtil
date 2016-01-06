/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.DoubleSummaryStatistics;
import java.util.List;
import java.util.OptionalDouble;
import java.util.PrimitiveIterator.OfDouble;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.BiConsumer;
import java.util.function.DoubleBinaryOperator;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleFunction;
import java.util.function.DoublePredicate;
import java.util.function.DoubleToIntFunction;
import java.util.function.DoubleToLongFunction;
import java.util.function.DoubleUnaryOperator;
import java.util.function.ObjDoubleConsumer;
import java.util.function.Supplier;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @author HaiYang Li
 *
 */
final class DoubleStreamImpl implements DoubleStream {
    private final double[] values;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final List<Runnable> closeHandlers;

    DoubleStreamImpl(double[] values) {
        this(values, null);
    }

    DoubleStreamImpl(double[] values, List<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    DoubleStreamImpl(double[] values, boolean sorted, List<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    DoubleStreamImpl(double[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    DoubleStreamImpl(double[] values, int fromIndex, int toIndex, List<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    DoubleStreamImpl(double[] values, int fromIndex, int toIndex, boolean sorted, List<Runnable> closeHandlers) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > values.length) {
            throw new IllegalArgumentException("fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ") is invalid");
        }

        this.values = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : N.newArrayList(closeHandlers);
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public DoubleStream unordered() {
        return this;
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = N.asList(closeHandler);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        return new DoubleStreamImpl(values, fromIndex, toIndex, closeHandlerList);
    }

    @Override
    public void close() {
        if (N.notNullOrEmpty(closeHandlers)) {
            RuntimeException ex = null;

            for (Runnable closeHandler : closeHandlers) {
                try {
                    closeHandler.run();
                } catch (RuntimeException e) {
                    if (ex == null) {
                        ex = e;
                    } else {
                        ex.addSuppressed(e);
                    }
                }
            }

            if (ex != null) {
                throw ex;
            }
        }
    }

    @Override
    public DoubleStream filter(DoublePredicate predicate) {
        final DoubleList list = new DoubleList();

        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                list.add(values[i]);
            }
        }

        return new DoubleStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public DoubleStream map(DoubleUnaryOperator mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(values[i]);
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(DoubleFunction<? extends U> mapper) {
        final Object[] a = new Object[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.apply(values[i]);
        }

        return new ArrayStream<U>((U[]) a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(DoubleToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(values[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public LongStream mapToLong(DoubleToLongFunction mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(values[i]);
        }

        return new LongStreamImpl(a, closeHandlers);
    }

    @Override
    public DoubleStream flatMap(DoubleFunction<? extends DoubleStream> mapper) {
        final List<double[]> listOfArray = new ArrayList<>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final double[] tmp = mapper.apply(values[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final double[] arrayOfAll = new double[lengthOfAll];
        int from = 0;
        for (double[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new DoubleStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public DoubleStream distinct() {
        return new DoubleStreamImpl(N.removeDuplicates(values, fromIndex, toIndex, sorted), closeHandlers);
    }

    @Override
    public DoubleStream sorted() {
        if (sorted) {
            return new DoubleStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        }

        final double[] a = N.copyOfRange(values, fromIndex, toIndex);
        N.sort(a);
        return new DoubleStreamImpl(a, true, closeHandlers);
    }

    @Override
    public DoubleStream peek(DoubleConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }

        return new DoubleStreamImpl(values, fromIndex, toIndex, closeHandlers);
    }

    @Override
    public DoubleStream limit(long maxSize) {
        if (maxSize >= toIndex - fromIndex) {
            return new DoubleStreamImpl(values, fromIndex, toIndex, closeHandlers);
        } else {
            return new DoubleStreamImpl(values, fromIndex, (int) (fromIndex + maxSize), closeHandlers);
        }
    }

    @Override
    public DoubleStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new DoubleStreamImpl(N.EMPTY_DOUBLE_ARRAY, closeHandlers);
        } else {
            return new DoubleStreamImpl(values, (int) (fromIndex + n), toIndex, closeHandlers);
        }
    }

    @Override
    public void forEach(DoubleConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }
    }

    @Override
    public void forEachOrdered(DoubleConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }
    }

    @Override
    public double[] toArray() {
        return N.copyOfRange(values, fromIndex, toIndex);
    }

    @Override
    public double reduce(double identity, DoubleBinaryOperator op) {
        double result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsDouble(result, values[i]);
        }

        return result;
    }

    @Override
    public OptionalDouble reduce(DoubleBinaryOperator op) {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        double result = values[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsDouble(result, values[i]);
        }

        return OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, values[i]);
        }

        return result;
    }

    @Override
    public double sum() {
        return N.sum(values, fromIndex, toIndex).doubleValue();
    }

    @Override
    public OptionalDouble min() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.min(values, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble max() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.max(values, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public OptionalDouble average() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.avg(values, fromIndex, toIndex).doubleValue());
    }

    @Override
    public DoubleSummaryStatistics summaryStatistics() {
        final DoubleSummaryStatistics stat = new DoubleSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            stat.accept(values[i]);
        }

        return stat;
    }

    @Override
    public boolean anyMatch(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalDouble findFirst() {
        return count() == 0 ? OptionalDouble.empty() : OptionalDouble.of(values[fromIndex]);
    }

    @Override
    public OptionalDouble findAny() {
        return count() == 0 ? OptionalDouble.empty() : OptionalDouble.of(values[fromIndex]);
    }

    @Override
    public Stream<Double> boxed() {
        return new ArrayStream<Double>(N.wrap(values, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public DoubleStream sequential() {
        return this;
    }

    @Override
    public DoubleStream parallel() {
        return Arrays.stream(values, fromIndex, toIndex).parallel();
    }

    @Override
    public OfDouble iterator() {
        return Spliterators.iterator(spliterator());
    }

    @Override
    public java.util.Spliterator.OfDouble spliterator() {
        return Spliterators.spliterator(values, fromIndex, toIndex, Spliterator.ORDERED | Spliterator.IMMUTABLE);
    }
}
