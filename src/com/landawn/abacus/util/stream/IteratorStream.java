package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 */
final class IteratorStream<T> extends Stream<T> implements BaseStream<T, Stream<T>> {
    private final Iterator<T> values;
    private final Set<Runnable> closeHandlers;

    public IteratorStream(final Iterator<T> iterator) {
        this(iterator, null);
    }

    public IteratorStream(final Iterator<T> iterator, Collection<Runnable> closeHandlers) {
        Objects.requireNonNull(iterator);

        this.values = iterator;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public Stream<T> filter(Predicate<? super T> predicate) {
        return filter(predicate, Integer.MAX_VALUE);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate, final int max) {
        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, max)], 0);

        int cnt = 0;
        T e = null;
        while (cnt < max && values.hasNext()) {
            e = values.next();

            if (predicate.test(e)) {
                list.add(e);
                cnt++;
            }
        }

        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public Stream<T> takeWhile(Predicate<? super T> predicate) {
        return takeWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public Stream<T> takeWhile(Predicate<? super T> predicate, int max) {
        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, max)], 0);

        int cnt = 0;
        T e = null;
        while (cnt < max && values.hasNext()) {
            e = values.next();

            if (predicate.test(e)) {
                list.add(e);
                cnt++;
            } else {
                break;
            }
        }

        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public Stream<T> dropWhile(Predicate<? super T> predicate) {
        return dropWhile(predicate, Integer.MAX_VALUE);
    }

    @Override
    public Stream<T> dropWhile(Predicate<? super T> predicate, int max) {
        while (values.hasNext() && predicate.test(values.next())) {
        }

        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, max)], 0);
        int cnt = 0;

        while (cnt < max && values.hasNext()) {
            list.add(values.next());
            cnt++;
        }

        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public <R> Stream<R> map(Function<? super T, ? extends R> mapper) {
        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);

        while (values.hasNext()) {
            list.add(mapper.apply(values.next()));
        }

        return new ArrayStream<R>((R[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public CharStream mapToChar(ToCharFunction<? super T> mapper) {
        final CharList list = CharList.of(new char[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsChar(values.next()));
        }

        return new CharStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public ByteStream mapToByte(ToByteFunction<? super T> mapper) {
        final ByteList list = ByteList.of(new byte[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsByte(values.next()));
        }

        return new ByteStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public ShortStream mapToShort(ToShortFunction<? super T> mapper) {
        final ShortList list = ShortList.of(new short[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsShort(values.next()));
        }

        return new ShortStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public IntStream mapToInt(ToIntFunction<? super T> mapper) {
        final IntList list = IntList.of(new int[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsInt(values.next()));
        }

        return new IntStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public LongStream mapToLong(ToLongFunction<? super T> mapper) {
        final LongList list = LongList.of(new long[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsLong(values.next()));
        }

        return new LongStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(ToFloatFunction<? super T> mapper) {
        final FloatList list = FloatList.of(new float[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsFloat(values.next()));
        }

        return new FloatStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(ToDoubleFunction<? super T> mapper) {
        final DoubleList list = DoubleList.of(new double[9], 0);

        while (values.hasNext()) {
            list.add(mapper.applyAsDouble(values.next()));
        }

        return new DoubleStreamImpl(list.trimToSize().array(), closeHandlers);
    }

    @Override
    public <R> Stream<R> flatMap(Function<? super T, ? extends Stream<? extends R>> mapper) {
        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);

        while (values.hasNext()) {
            final Iterator<? extends R> it = mapper.apply(values.next()).iterator();

            while (it.hasNext()) {
                list.add(it.next());
            }
        }

        return new ArrayStream<R>((R[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public CharStream flatMapToChar(Function<? super T, ? extends CharStream> mapper) {
        final List<char[]> listOfArray = new ArrayList<char[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final char[] tmp = mapper.apply(values.next()).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final char[] arrayOfAll = new char[lengthOfAll];
        int from = 0;
        for (char[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new CharStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public ByteStream flatMapToByte(Function<? super T, ? extends ByteStream> mapper) {
        final List<byte[]> listOfArray = new ArrayList<byte[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final byte[] tmp = mapper.apply(values.next()).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final byte[] arrayOfAll = new byte[lengthOfAll];
        int from = 0;
        for (byte[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ByteStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public ShortStream flatMapToShort(Function<? super T, ? extends ShortStream> mapper) {
        final List<short[]> listOfArray = new ArrayList<short[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final short[] tmp = mapper.apply(values.next()).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final short[] arrayOfAll = new short[lengthOfAll];
        int from = 0;
        for (short[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ShortStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(Function<? super T, ? extends IntStream> mapper) {
        final List<int[]> listOfArray = new ArrayList<int[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final int[] tmp = mapper.apply(values.next()).toArray();
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
        final List<long[]> listOfArray = new ArrayList<long[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final long[] tmp = mapper.apply(values.next()).toArray();
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
    public FloatStream flatMapToFloat(Function<? super T, ? extends FloatStream> mapper) {
        final List<float[]> listOfArray = new ArrayList<float[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final float[] tmp = mapper.apply(values.next()).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final float[] arrayOfAll = new float[lengthOfAll];
        int from = 0;
        for (float[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new FloatStreamImpl(arrayOfAll, closeHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(Function<? super T, ? extends DoubleStream> mapper) {
        final List<double[]> listOfArray = new ArrayList<double[]>();

        int lengthOfAll = 0;
        while (values.hasNext()) {
            final double[] tmp = mapper.apply(values.next()).toArray();
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
    public <K> Stream<Entry<K, List<T>>> groupBy(Function<? super T, ? extends K> classifier) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(Function<? super T, ? extends K> classifier, Supplier<Map<K, List<T>>> mapFactory) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, D, A> Stream<Entry<K, D>> groupBy(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public Stream<T> distinct() {
        final Set<T> set = new LinkedHashSet<T>();

        while (values.hasNext()) {
            set.add(values.next());
        }

        return new ArrayStream<T>((T[]) set.toArray(), closeHandlers);
    }

    @Override
    public Stream<T> sorted() {
        return sorted(null);
    }

    @Override
    public Stream<T> sorted(Comparator<? super T> comparator) {
        final Object[] a = toArray();

        if (comparator == null) {
            Arrays.sort(a);
        } else {
            Arrays.sort((T[]) a, comparator);
        }

        return new ArrayStream<T>((T[]) a, true, comparator, closeHandlers);
    }

    @Override
    public Stream<T> peek(Consumer<? super T> action) {
        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);

        T e = null;
        while (values.hasNext()) {
            e = values.next();
            list.add(e);
            action.accept(e);
        }

        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public Stream<T> limit(long maxSize) {
        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);
        long cnt = 0;

        while (values.hasNext() && cnt < maxSize) {
            list.add(values.next());
            cnt++;
        }

        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public Stream<T> skip(long n) {
        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);

        long cnt = 0;
        while (values.hasNext() && cnt < n) {
            values.next();
            cnt++;
        }

        while (values.hasNext()) {
            list.add(values.next());
        }

        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        while (values.hasNext()) {
            action.accept(values.next());
        }
    }

    @Override
    public Object[] toArray() {
        final ObjectList<Object> list = toObjectList(Object.class);

        return list.trimToSize().array();
    }

    @Override
    public <A> A[] toArray(A[] a) {
        final ObjectList<Object> list = toObjectList(Object.class);

        if (a.length < list.size()) {
            a = N.newArray(a.getClass().getComponentType(), list.size());
        }

        if (list.size() > 0) {
            N.copy(list.array(), 0, a, 0, list.size());
        }

        return a;

    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        final ObjectList<Object> list = toObjectList(Object.class);

        final A[] a = generator.apply(list.size());

        if (list.size() > 0) {
            N.copy(list.array(), 0, a, 0, list.size());
        }

        return a;
    }

    @Override
    public <A> ObjectList<A> toObjectList(Class<A> cls) {
        final ObjectList<A> list = ObjectList.of((A[]) N.newArray(cls, 9), 0);

        while (values.hasNext()) {
            list.add((A) values.next());
        }

        return list;
    }

    @Override
    public T reduce(T identity, BinaryOperator<T> accumulator) {
        T result = identity;

        while (values.hasNext()) {
            result = accumulator.apply(result, values.next());
        }

        return result;
    }

    @Override
    public Optional<T> reduce(BinaryOperator<T> accumulator) {
        if (values.hasNext() == false) {
            Optional.empty();
        }

        T result = values.next();

        while (values.hasNext()) {
            result = accumulator.apply(result, values.next());
        }

        return Optional.of(result);
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {
        U result = identity;

        while (values.hasNext()) {
            result = accumulator.apply(result, values.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (values.hasNext()) {
            accumulator.accept(result, values.next());
        }

        return result;
    }

    @Override
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (values.hasNext()) {
            accumulator.accept(container, values.next());
        }

        return collector.finisher().apply(container);
    }

    @Override
    public Optional<T> min(Comparator<? super T> comparator) {
        if (values.hasNext() == false) {
            return Optional.empty();
        }

        T candidate = values.next();
        T next = null;

        while (values.hasNext()) {
            next = values.next();
            if (comparator.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return Optional.of(candidate);
    }

    @Override
    public Optional<T> max(Comparator<? super T> comparator) {
        if (values.hasNext() == false) {
            return Optional.empty();
        }

        T candidate = values.next();
        T next = null;

        while (values.hasNext()) {
            next = values.next();
            if (comparator.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return Optional.of(candidate);
    }

    @Override
    public long count() {
        long result = 0;

        while (values.hasNext()) {
            values.next();
            result++;
        }

        return result;
    }

    @Override
    public boolean anyMatch(Predicate<? super T> predicate) {
        while (values.hasNext()) {
            if (predicate.test(values.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(Predicate<? super T> predicate) {
        while (values.hasNext()) {
            if (predicate.test(values.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(Predicate<? super T> predicate) {
        while (values.hasNext()) {
            if (predicate.test(values.next())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public Optional<T> findFirst() {
        return values.hasNext() ? (Optional<T>) Optional.empty() : Optional.of(values.next());
    }

    @Override
    public Optional<T> findAny() {
        return values.hasNext() ? (Optional<T>) Optional.empty() : Optional.of(values.next());
    }

    @Override
    public Iterator<T> iterator() {
        return values;
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new IteratorStream<T>(values, closeHandlerList);
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
}
