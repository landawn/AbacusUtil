package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteToIntFunction;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ParallelArrayByteStream extends AbstractByteStream {
    private final byte[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final int maxThreadNum;
    private final Splitter splitter;
    private volatile ArrayByteStream sequential;
    private volatile Stream<Byte> boxed;

    ParallelArrayByteStream(byte[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted, int maxThreadNum,
            Splitter splitter) {
        super(closeHandlers, sorted);

        checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.maxThreadNum = fromIndex >= toIndex ? 1 : N.min(maxThreadNum, THREAD_POOL_SIZE, toIndex - fromIndex);
        this.splitter = splitter == null ? DEFAULT_SPILTTER : splitter;
    }

    @Override
    public ByteStream filter(final BytePredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().filter(predicate, max).byteIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Byte> stream = boxed().filter(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return predicate.test(value.byteValue());
            }
        }, max);

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream takeWhile(final BytePredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().takeWhile(predicate, max).byteIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Byte> stream = boxed().takeWhile(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return predicate.test(value.byteValue());
            }
        }, max);

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream dropWhile(final BytePredicate predicate, final long max) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().dropWhile(predicate, max).byteIterator(), closeHandlers, sorted, maxThreadNum, splitter);
        }

        final Stream<Byte> stream = boxed().dropWhile(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return predicate.test(value.byteValue());
            }
        }, max);

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream map(final ByteUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().map(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final ByteStream stream = boxed().mapToByte(new ToByteFunction<Byte>() {
            @Override
            public byte applyAsByte(Byte value) {
                return mapper.applyAsByte(value.byteValue());
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream mapToInt(final ByteToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Byte>() {
            @Override
            public int applyAsInt(Byte value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<U> mapToObj(final ByteFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().map(new Function<Byte, U>() {
            @Override
            public U apply(Byte value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public ByteStream flatMap(final ByteFunction<? extends ByteStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMap(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final ByteStream stream = boxed().flatMapToByte(new Function<Byte, ByteStream>() {
            @Override
            public ByteStream apply(Byte value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public IntStream flatMapToInt(final ByteFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitter);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Byte, IntStream>() {
            @Override
            public IntStream apply(Byte value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ByteFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitter);
        }

        return boxed().flatMap(new Function<Byte, Stream<T>>() {
            @Override
            public Stream<T> apply(Byte value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<ByteStream> split(final int size) {
        return new ParallelIteratorStream<ByteStream>(new ImmutableIterator<ByteStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ByteStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayByteStream(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex), null, sorted);
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public <U> Stream<ByteStream> split(final U boundary, final BiFunction<? super Byte, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new ParallelIteratorStream<ByteStream>(new ImmutableIterator<ByteStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ByteStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ByteList result = ByteList.of(N.EMPTY_BYTE_ARRAY);

                while (cursor < toIndex) {
                    if (predicate.apply(elements[cursor], boundary)) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return ByteStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers, false, null, maxThreadNum, splitter);
    }

    @Override
    public ByteStream distinct() {
        final byte[] a = N.removeDuplicates(elements, fromIndex, toIndex, sorted);
        return new ParallelArrayByteStream(a, 0, a.length, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ByteStream sorted() {
        if (sorted) {
            return this;
        }

        final byte[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a);
        return new ParallelArrayByteStream(a, 0, a.length, closeHandlers, true, maxThreadNum, splitter);
    }

    @Override
    public ByteStream peek(ByteConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new ByteStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public ByteStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayByteStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ByteStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayByteStream(elements, toIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
        } else {
            return new ParallelArrayByteStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted, maxThreadNum, splitter);
        }
    }

    @Override
    public void forEach(final ByteConsumer action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        byte next = 0;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    //    @Override
    //    public boolean forEach2(final ByteFunction<Boolean> action) {
    //        if (maxThreadNum <= 1) {
    //            return sequential().forEach2(action);
    //        }
    //
    //        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
    //        final Holder<Throwable> eHolder = new Holder<>();
    //        final MutableBoolean result = MutableBoolean.of(true);
    //
    //        if (splitter == Splitter.ARRAY) {
    //            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;
    //
    //            for (int i = 0; i < maxThreadNum; i++) {
    //                final int sliceIndex = i;
    //
    //                futureList.add(asyncExecutor.execute(new Runnable() {
    //                    @Override
    //                    public void run() {
    //                        int cursor = fromIndex + sliceIndex * sliceSize;
    //                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
    //
    //                        try {
    //                            while (cursor < to && result.isTrue() && eHolder.value() == null) {
    //                                if (action.apply(elements[cursor++]) == false) {
    //                                    result.setFalse();
    //                                    break;
    //                                }
    //                            }
    //                        } catch (Throwable e) {
    //                            setError(eHolder, e);
    //                        }
    //                    }
    //                }));
    //            }
    //        } else {
    //            final MutableInt cursor = MutableInt.of(fromIndex);
    //
    //            for (int i = 0; i < maxThreadNum; i++) {
    //                futureList.add(asyncExecutor.execute(new Runnable() {
    //                    @Override
    //                    public void run() {
    //                        byte next = 0;
    //
    //                        try {
    //                            while (result.isTrue() && eHolder.value() == null) {
    //                                synchronized (elements) {
    //                                    if (cursor.intValue() < toIndex) {
    //                                        next = elements[cursor.getAndIncrement()];
    //                                    } else {
    //                                        break;
    //                                    }
    //                                }
    //
    //                                if (action.apply(next) == false) {
    //                                    result.setFalse();
    //                                    break;
    //                                }
    //                            }
    //                        } catch (Throwable e) {
    //                            setError(eHolder, e);
    //                        }
    //                    }
    //                }));
    //            }
    //        }
    //
    //        if (eHolder.value() != null) {
    //            throw N.toRuntimeException(eHolder.value());
    //        }
    //
    //        try {
    //            for (CompletableFuture<Void> future : futureList) {
    //                future.get();
    //            }
    //        } catch (Exception e) {
    //            throw N.toRuntimeException(e);
    //        }
    //
    //        return result.booleanValue();
    //    }

    @Override
    public byte[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public ByteList toByteList() {
        return ByteList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Byte> toList() {
        final List<Byte> result = new ArrayList<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<Byte> toList(Supplier<? extends List<Byte>> supplier) {
        final List<Byte> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Byte> toSet() {
        final Set<Byte> result = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Byte> toSet(Supplier<? extends Set<Byte>> supplier) {
        final Set<Byte> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset() {
        final Multiset<Byte> result = new Multiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset(Supplier<? extends Multiset<Byte>> supplier) {
        final Multiset<Byte> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset() {
        final LongMultiset<Byte> result = new LongMultiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset(Supplier<? extends LongMultiset<Byte>> supplier) {
        final LongMultiset<Byte> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Byte, ? extends K> classifier2 = new Function<Byte, K>() {
            @Override
            public K apply(Byte value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
        }

        final Function<? super Byte, ? extends K> keyMapper2 = new Function<Byte, K>() {
            @Override
            public K apply(Byte value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Byte, ? extends U> valueMapper2 = new Function<Byte, U>() {
            @Override
            public U apply(Byte value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper,
            final Supplier<Multimap<K, U, V>> mapSupplier) {

        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapSupplier);
        }

        final Function<? super Byte, ? extends K> keyMapper2 = new Function<Byte, K>() {
            @Override
            public K apply(Byte value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Byte, ? extends U> valueMapper2 = new Function<Byte, U>() {
            @Override
            public U apply(Byte value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapSupplier);
    }

    @Override
    public byte reduce(final byte identity, final ByteBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Byte>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                    @Override
                    public Byte call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        byte result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = op.applyAsByte(result, elements[cursor++]);
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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                    @Override
                    public Byte call() {
                        byte result = identity;
                        byte next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = op.applyAsByte(result, next);
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

        Byte result = null;

        try {
            for (CompletableFuture<Byte> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsByte(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalByte reduce(final ByteBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Byte>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                    @Override
                    public Byte call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return null;
                        }

                        byte result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.applyAsByte(result, elements[cursor++]);
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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                    @Override
                    public Byte call() {
                        byte result = 0;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return null;
                            }
                        }

                        byte next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = accumulator.applyAsByte(result, next);
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

        Byte result = null;

        try {
            for (CompletableFuture<Byte> future : futureList) {
                final Byte tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsByte(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalByte.empty() : OptionalByte.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjByteConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().collect(supplier, accumulator, combiner);
        }

        final List<CompletableFuture<R>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        R container = supplier.get();

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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        R container = supplier.get();
                        byte next = 0;

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
                final R tmp = future.get();

                if (container == NONE) {
                    container = tmp;
                } else {
                    combiner.accept(container, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return container == NONE ? supplier.get() : container;
    }

    @Override
    public OptionalByte min() {
        if (count() == 0) {
            return OptionalByte.empty();
        } else if (sorted) {
            return OptionalByte.of(elements[fromIndex]);
        } else if (maxThreadNum <= 1) {
            return OptionalByte.of(N.min(elements, fromIndex, toIndex));
        }

        final List<CompletableFuture<Byte>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                @Override
                public Byte call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : N.min(elements, cursor, to);
                }
            }));
        }

        Byte min = null;

        try {
            for (CompletableFuture<Byte> future : futureList) {
                final Byte tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (min == null || tmp.byteValue() < min.byteValue()) {
                    min = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return min == null ? OptionalByte.empty() : OptionalByte.of(min);
    }

    @Override
    public OptionalByte max() {
        if (count() == 0) {
            return OptionalByte.empty();
        } else if (sorted) {
            return OptionalByte.of(elements[toIndex - 1]);
        } else if (maxThreadNum <= 1) {
            return OptionalByte.of(N.max(elements, fromIndex, toIndex));
        }

        final List<CompletableFuture<Byte>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                @Override
                public Byte call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    return cursor >= to ? null : N.max(elements, cursor, to);
                }
            }));
        }

        Byte min = null;

        try {
            for (CompletableFuture<Byte> future : futureList) {
                final Byte tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (min == null || tmp.byteValue() > min.byteValue()) {
                    min = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return min == null ? OptionalByte.empty() : OptionalByte.of(min);
    }

    @Override
    public OptionalByte kthLargest(int k) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return OptionalByte.empty();
        } else if (sorted) {
            return OptionalByte.of(elements[toIndex - k]);
        }

        return OptionalByte.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public Long sum() {
        if (count() == 0) {
            return 0L;
        } else if (maxThreadNum <= 1) {
            return N.sum(elements, fromIndex, toIndex);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : N.sum(elements, cursor, to);
                }
            }));
        }

        long result = 0;

        try {
            for (CompletableFuture<Long> future : futureList) {
                final Long tmp = future.get();

                if (tmp == null) {
                    continue;
                } else {
                    result += tmp.longValue();
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public OptionalDouble average() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(sum() / count());
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public ByteStream reverse() {
        return new ParallelIteratorByteStream(new ImmutableByteIterator() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public byte next() {
                if (cursor <= fromIndex) {
                    throw new NoSuchElementException();
                }

                return elements[--cursor];
            }
        }, closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteSummaryStatistics summarize() {
        if (count() == 0) {
            return new ByteSummaryStatistics();
        } else if (maxThreadNum <= 1) {
            return sequential().summarize();
        }

        final List<CompletableFuture<ByteSummaryStatistics>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<ByteSummaryStatistics>() {
                @Override
                public ByteSummaryStatistics call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    final ByteSummaryStatistics result = new ByteSummaryStatistics();

                    for (int i = cursor; i < to; i++) {
                        result.accept(elements[i]);
                    }

                    return result;
                }
            }));
        }

        ByteSummaryStatistics result = null;

        try {
            for (CompletableFuture<ByteSummaryStatistics> future : futureList) {
                final ByteSummaryStatistics tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result.combine(tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        byte next = 0;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.booleanValue();
    }

    @Override
    public boolean allMatch(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        byte next = 0;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.booleanValue();
    }

    @Override
    public boolean noneMatch(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
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

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        byte next = 0;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.booleanValue();
    }

    //    @Override
    //    public OptionalByte findFirst() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalByte findFirst(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Integer, Byte>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Byte>> resultHolder = new Holder<>();

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, Byte>>() {
                    @Override
                    public Pair<Integer, Byte> call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, Byte> pair = new Pair<>();

                        try {
                            while (cursor < to && resultHolder.value() == null && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[cursor++];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, Byte>>() {
                    @Override
                    public Pair<Integer, Byte> call() {
                        final Pair<Integer, Byte> pair = new Pair<>();

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
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Integer, Byte>> future : futureList) {
                final Pair<Integer, Byte> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalByte.empty() : OptionalByte.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalByte findLast() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalByte findLast(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Integer, Byte>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Byte>> resultHolder = new Holder<>();

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, Byte>>() {
                    @Override
                    public Pair<Integer, Byte> call() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, Byte> pair = new Pair<>();

                        try {
                            while (cursor > from && resultHolder.value() == null && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[--cursor];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(toIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, Byte>>() {
                    @Override
                    public Pair<Integer, Byte> call() {
                        final Pair<Integer, Byte> pair = new Pair<>();

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
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Integer, Byte>> future : futureList) {
                final Pair<Integer, Byte> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? OptionalByte.empty() : OptionalByte.of(resultHolder.value().right);
    }

    //    @Override
    //    public OptionalByte findAny() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalByte findAny(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Object>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(NONE);

        if (splitter == Splitter.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Object>() {
                    @Override
                    public Object call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        byte next = 0;

                        try {
                            while (cursor < to && resultHolder.value() == null && eHolder.value() == null) {
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

                        return next;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Object>() {
                    @Override
                    public Object call() {
                        byte next = 0;

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

                        return next;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Object> future : futureList) {
                if (resultHolder.value() == NONE) {
                    future.get();
                } else {
                    break;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == NONE ? OptionalByte.empty() : OptionalByte.of((Byte) resultHolder.value());
    }

    @Override
    public ByteStream except(final Collection<?> c) {
        return new ParallelIteratorByteStream(this.sequential().except(c).byteIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ByteStream intersect(final Collection<?> c) {
        return new ParallelIteratorByteStream(this.sequential().intersect(c).byteIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ByteStream xor(final Collection<Byte> c) {
        return new ParallelIteratorByteStream(this.sequential().xor(c).byteIterator(), closeHandlers, false, maxThreadNum, splitter);
    }

    //    @Override
    //    public ByteStream exclude(final Collection<?> c) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorByteStream(sequential().exclude(c).byteIterator(), closeHandlers, sorted, maxThreadNum, splitter);
    //        }
    //
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new BytePredicate() {
    //            @Override
    //            public boolean test(byte value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public IntStream asIntStream() {
        return new ParallelIteratorIntStream(new ImmutableIntIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = cursor, j = 0; i < toIndex; i++, j++) {
                    a[j] = elements[i];
                }

                return a;
            }
        }, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public Stream<Byte> boxed() {
        Stream<Byte> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Byte>(iterator(), closeHandlers, sorted, sorted ? BYTE_COMPARATOR : null, maxThreadNum, splitter);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public ByteStream append(final ByteStream stream) {
        return new ParallelIteratorByteStream(ByteStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream merge(final ByteStream b, final ByteBiFunction<Nth> nextSelector) {
        return new ParallelIteratorByteStream(ByteStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteBiFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteStream c, ByteTriFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream zipWith(ByteStream b, byte valueForNoneA, byte valueForNoneB, ByteBiFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum, splitter);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteStream c, byte valueForNoneA, byte valueForNoneB, byte valueForNoneC, ByteTriFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitter);
    }

    @Override
    public ImmutableIterator<Byte> iterator() {
        return this.sequential().iterator();
    }

    @Override
    public ImmutableByteIterator byteIterator() {
        return this.sequential().byteIterator();
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public ByteStream sequential() {
        ArrayByteStream tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayByteStream(elements, fromIndex, toIndex, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public ByteStream parallel(int maxThreadNum, Splitter splitter) {
        if (this.maxThreadNum == maxThreadNum && this.splitter == splitter) {
            return this;
        }

        return new ParallelArrayByteStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public ByteStream maxThreadNum(int maxThreadNum) {
        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelArrayByteStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public BaseStream.Splitter splitter() {
        return splitter;
    }

    @Override
    public ByteStream splitter(BaseStream.Splitter splitter) {
        if (this.splitter == splitter) {
            return this;
        }

        return new ParallelArrayByteStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ByteStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayByteStream(elements, fromIndex, toIndex, newCloseHandlers, sorted, maxThreadNum, splitter);
    }
}
