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
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.function.ToLongFunction;
import java.util.stream.BaseStream;
import java.util.stream.Collector;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @author HaiYang Li
 *
 * @param <T>
 */
final class CollectionStream<T> implements Stream<T>, BaseStream<T, Stream<T>> {
    private final Collection<T> values;
    private final List<Runnable> closeHandlers;

    public CollectionStream(final Collection<T> c) {
        this(c, null);
    }

    public CollectionStream(final Collection<T> c, List<Runnable> closeHandlers) {
        Objects.requireNonNull(c);

        this.values = c;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : N.newArrayList(closeHandlers);
    }

    @Override
    public Iterator<T> iterator() {
        return values.iterator();
    }

    @Override
    public Spliterator<T> spliterator() {
        return Spliterators.spliterator(values,
                ((values instanceof List || values instanceof LinkedHashSet) ? Spliterator.ORDERED : 0) | Spliterator.IMMUTABLE);
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public Stream<T> sequential() {
        return this;
    }

    @Override
    public Stream<T> parallel() {
        return values.parallelStream();
    }

    @Override
    public Stream<T> unordered() {
        return this;
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = N.asList(closeHandler);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        return new CollectionStream<T>(values, closeHandlerList);
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
    public Stream<T> filter(Predicate<? super T> predicate) {
        final Collection<T> c = newCollection(values.getClass(), 9);

        for (T e : values) {
            if (predicate.test(e)) {
                c.add(e);
            }
        }

        return new CollectionStream<T>(c, closeHandlers);
    }

    @Override
    public <R> Stream<R> map(Function<? super T, ? extends R> mapper) {
        final Collection<R> c = newCollection(values.getClass(), values.size());

        for (T e : values) {
            c.add(mapper.apply(e));
        }

        return new CollectionStream<R>(c, closeHandlers);
    }

    @Override
    public IntStream mapToInt(ToIntFunction<? super T> mapper) {
        final int[] a = new int[values.size()];

        int idx = 0;
        for (T e : values) {
            a[idx++] = mapper.applyAsInt(e);
        }

        return new IntStreamImpl(a, closeHandlers);
    }

    @Override
    public LongStream mapToLong(ToLongFunction<? super T> mapper) {
        final long[] a = new long[values.size()];

        int idx = 0;
        for (T e : values) {
            a[idx++] = mapper.applyAsLong(e);
        }

        return new LongStreamImpl(a, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(ToDoubleFunction<? super T> mapper) {
        final double[] a = new double[values.size()];

        int idx = 0;
        for (T e : values) {
            a[idx++] = mapper.applyAsDouble(e);
        }

        return new DoubleStreamImpl(a, closeHandlers);
    }

    @Override
    public <R> Stream<R> flatMap(Function<? super T, ? extends Stream<? extends R>> mapper) {
        final Collection<R> c = newCollection(values.getClass(), values.size());

        for (T e : values) {
            final Iterator<? extends R> it = mapper.apply(e).iterator();

            while (it.hasNext()) {
                c.add(it.next());
            }
        }

        return new CollectionStream<R>(c, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(Function<? super T, ? extends IntStream> mapper) {
        final List<int[]> listOfArray = new ArrayList<>();

        int lengthOfAll = 0;
        for (T e : values) {
            final int[] tmp = mapper.apply(e).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final int[] arrayOfAll = new int[lengthOfAll];
        int from = 0;
        for (int[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new IntStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong(Function<? super T, ? extends LongStream> mapper) {
        final List<long[]> listOfArray = new ArrayList<>();

        int lengthOfAll = 0;
        for (T e : values) {
            final long[] tmp = mapper.apply(e).toArray();
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
    public DoubleStream flatMapToDouble(Function<? super T, ? extends DoubleStream> mapper) {
        final List<double[]> listOfArray = new ArrayList<>();

        int lengthOfAll = 0;
        for (T e : values) {
            final double[] tmp = mapper.apply(e).toArray();
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
    public Stream<T> distinct() {
        final Set<T> set = new LinkedHashSet<T>();

        for (T e : values) {
            set.add(e);
        }

        return new CollectionStream<T>(set, closeHandlers);
    }

    @Override
    public Stream<T> sorted() {
        final T[] a = (T[]) values.toArray();

        Arrays.sort(a);

        return new ArrayStream<T>(a, closeHandlers);
    }

    @Override
    public Stream<T> sorted(Comparator<? super T> comparator) {
        final T[] a = (T[]) values.toArray();

        Arrays.sort(a, comparator);

        return new ArrayStream<T>(a, closeHandlers);
    }

    @Override
    public Stream<T> peek(Consumer<? super T> action) {
        final Collection<T> c = newCollection(values.getClass(), values.size());

        for (T e : values) {
            action.accept(e);
            c.add(e);
        }

        return new CollectionStream<T>(c, closeHandlers);
    }

    @Override
    public Stream<T> limit(long maxSize) {
        final Collection<T> c = newCollection(values.getClass(), (int) (Math.min(values.size(), maxSize)));

        for (T e : values) {
            if (maxSize-- > 0) {
                c.add(e);
            } else {
                break;
            }
        }

        return new CollectionStream<T>(c, closeHandlers);
    }

    @Override
    public Stream<T> skip(long n) {
        final Collection<T> c = newCollection(values.getClass(), (int) (values.size() < n ? 0 : values.size() - n));

        if (values.size() > n) {
            for (T e : values) {
                if (n-- > 0) {
                    continue;
                }

                c.add(e);
            }
        }

        return new CollectionStream<T>(c, closeHandlers);
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        for (T e : values) {
            action.accept(e);
        }
    }

    @Override
    public void forEachOrdered(Consumer<? super T> action) {
        for (T e : values) {
            action.accept(e);
        }
    }

    @Override
    public Object[] toArray() {
        return values.toArray();
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        final A[] a = generator.apply(values.size());
        int idx = 0;

        for (T e : values) {
            a[idx++] = (A) e;
        }

        return a;
    }

    @Override
    public T reduce(T identity, BinaryOperator<T> accumulator) {
        T result = identity;

        for (T e : values) {
            result = accumulator.apply(result, e);
        }

        return result;
    }

    @Override
    public Optional<T> reduce(BinaryOperator<T> accumulator) {
        if (values.isEmpty()) {
            Optional.empty();
        }

        final Iterator<T> it = values.iterator();
        T result = it.next();

        while (it.hasNext()) {
            result = accumulator.apply(result, it.next());
        }

        return Optional.of(result);
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {
        U result = identity;

        for (T e : values) {
            result = accumulator.apply(result, e);
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (T e : values) {
            accumulator.accept(result, e);
        }

        return result;
    }

    @Override
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        for (T t : values) {
            accumulator.accept(container, t);
        }

        return collector.finisher().apply(container);
    }

    @Override
    public Optional<T> min(Comparator<? super T> comparator) {
        if (values.isEmpty()) {
            return Optional.empty();
        }

        final Iterator<T> it = values.iterator();
        T candidate = it.next();
        T next = null;

        while (it.hasNext()) {
            next = it.next();
            if (comparator.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return Optional.of(candidate);
    }

    @Override
    public Optional<T> max(Comparator<? super T> comparator) {
        if (values.isEmpty()) {
            return Optional.empty();
        }

        final Iterator<T> it = values.iterator();
        T candidate = it.next();
        T next = null;

        while (it.hasNext()) {
            next = it.next();
            if (comparator.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return Optional.of(candidate);
    }

    @Override
    public long count() {
        return values.size();
    }

    @Override
    public boolean anyMatch(Predicate<? super T> predicate) {
        for (T e : values) {
            if (predicate.test(e)) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(Predicate<? super T> predicate) {
        for (T e : values) {
            if (predicate.test(e) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(Predicate<? super T> predicate) {
        for (T e : values) {
            if (predicate.test(e)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public Optional<T> findFirst() {
        return values.isEmpty() ? (Optional<T>) Optional.empty() : Optional.of(values.iterator().next());
    }

    @Override
    public Optional<T> findAny() {
        return values.isEmpty() ? (Optional<T>) Optional.empty() : Optional.of(values.iterator().next());
    }

    private <C> C newCollection(Class<C> cls, int initCapacity) {
        /*
        final Constructor<C> constructor = N.getDeclaredConstructor(cls, int.class);
        
        if (constructor == null) {
            return N.newInstance(cls);
        } else {
            return N.invokeConstructor(constructor, initCapacity);
        }
         */

        return (C) new ArrayList<Object>(initCapacity);
    }
}
