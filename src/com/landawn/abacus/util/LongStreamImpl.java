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
import java.util.List;
import java.util.LongSummaryStatistics;
import java.util.OptionalDouble;
import java.util.OptionalLong;
import java.util.PrimitiveIterator.OfLong;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.BiConsumer;
import java.util.function.LongBinaryOperator;
import java.util.function.LongConsumer;
import java.util.function.LongFunction;
import java.util.function.LongPredicate;
import java.util.function.LongToDoubleFunction;
import java.util.function.LongToIntFunction;
import java.util.function.LongUnaryOperator;
import java.util.function.ObjLongConsumer;
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
final class LongStreamImpl implements LongStream {
    private final long[] values;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final List<Runnable> closeHandlers;

    LongStreamImpl(long[] values) {
        this(values, null);
    }

    LongStreamImpl(long[] values, List<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    LongStreamImpl(long[] values, boolean sorted, List<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    LongStreamImpl(long[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    LongStreamImpl(long[] values, int fromIndex, int toIndex, List<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    LongStreamImpl(long[] values, int fromIndex, int toIndex, boolean sorted, List<Runnable> closeHandlers) {
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
    public LongStream unordered() {
        return this;
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = N.asList(closeHandler);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        return new LongStreamImpl(values, fromIndex, toIndex, closeHandlerList);
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
    public LongStream filter(LongPredicate predicate) {
        final LongList list = new LongList();

        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                list.add(values[i]);
            }
        }

        return new LongStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public LongStream map(LongUnaryOperator mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(values[i]);
        }

        return new LongStreamImpl(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(LongFunction<? extends U> mapper) {
        final Object[] a = new Object[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.apply(values[i]);
        }

        return new ArrayStream<U>((U[]) a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(LongToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(values[i]);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(LongToDoubleFunction mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(values[i]);
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public LongStream flatMap(LongFunction<? extends LongStream> mapper) {
        final List<long[]> listOfArray = new ArrayList<>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final long[] tmp = mapper.apply(values[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final long[] arrayOfAll = new long[lengthOfAll];
        int from = 0;
        for (long[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new LongStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public LongStream distinct() {
        return new LongStreamImpl(N.removeDuplicates(values, fromIndex, toIndex, sorted), closeHandlers);
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return new LongStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        }

        final long[] a = N.copyOfRange(values, fromIndex, toIndex);
        N.sort(a);
        return new LongStreamImpl(a, true, closeHandlers);
    }

    @Override
    public LongStream peek(LongConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }

        return new LongStreamImpl(values, fromIndex, toIndex, closeHandlers);
    }

    @Override
    public LongStream limit(long maxSize) {
        if (maxSize >= toIndex - fromIndex) {
            return new LongStreamImpl(values, fromIndex, toIndex, closeHandlers);
        } else {
            return new LongStreamImpl(values, fromIndex, (int) (fromIndex + maxSize), closeHandlers);
        }
    }

    @Override
    public LongStream skip(long n) {
        if (n >= toIndex - fromIndex) {
            return new LongStreamImpl(N.EMPTY_LONG_ARRAY, closeHandlers);
        } else {
            return new LongStreamImpl(values, (int) (fromIndex + n), toIndex, closeHandlers);
        }
    }

    @Override
    public void forEach(LongConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }
    }

    @Override
    public void forEachOrdered(LongConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(values[i]);
        }
    }

    @Override
    public long[] toArray() {
        return N.copyOfRange(values, fromIndex, toIndex);
    }

    @Override
    public long reduce(long identity, LongBinaryOperator op) {
        long result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsLong(result, values[i]);
        }

        return result;
    }

    @Override
    public OptionalLong reduce(LongBinaryOperator op) {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        long result = values[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsLong(result, values[i]);
        }

        return OptionalLong.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, values[i]);
        }

        return result;
    }

    @Override
    public long sum() {
        return N.sum(values, fromIndex, toIndex).longValue();
    }

    @Override
    public OptionalLong min() {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        return OptionalLong.of(N.min(values, fromIndex, toIndex));
    }

    @Override
    public OptionalLong max() {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        return OptionalLong.of(N.max(values, fromIndex, toIndex));
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
    public LongSummaryStatistics summaryStatistics() {
        final LongSummaryStatistics stat = new LongSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            stat.accept(values[i]);
        }

        return stat;
    }

    @Override
    public boolean anyMatch(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(values[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalLong findFirst() {
        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(values[fromIndex]);
    }

    @Override
    public OptionalLong findAny() {
        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(values[fromIndex]);
    }

    @Override
    public DoubleStream asDoubleStream() {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = values[i];
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public Stream<Long> boxed() {
        return new ArrayStream<Long>(N.wrap(values, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public LongStream sequential() {
        return this;
    }

    @Override
    public LongStream parallel() {
        return Arrays.stream(values, fromIndex, toIndex).parallel();
    }

    @Override
    public OfLong iterator() {
        return Spliterators.iterator(spliterator());
    }

    @Override
    public java.util.Spliterator.OfLong spliterator() {
        return Spliterators.spliterator(values, fromIndex, toIndex, Spliterator.ORDERED | Spliterator.IMMUTABLE);
    }
}
