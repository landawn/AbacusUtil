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

import java.lang.reflect.Field;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.AsyncExecutor;
import com.landawn.abacus.util.BiMap;
import com.landawn.abacus.util.BooleanList;
import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.Comparators;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.ImmutableList;
import com.landawn.abacus.util.ImmutableSet;
import com.landawn.abacus.util.Indexed;
import com.landawn.abacus.util.IndexedByte;
import com.landawn.abacus.util.IndexedChar;
import com.landawn.abacus.util.IndexedDouble;
import com.landawn.abacus.util.IndexedFloat;
import com.landawn.abacus.util.IndexedInt;
import com.landawn.abacus.util.IndexedLong;
import com.landawn.abacus.util.IndexedShort;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Sheet;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.Wrapper;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class StreamBase<T, A, P, C, PL, OT, IT, ITER, S extends StreamBase<T, A, P, C, PL, OT, IT, ITER, S>>
        implements BaseStream<T, A, P, C, PL, OT, IT, ITER, S> {
    static final Logger logger = LoggerFactory.getLogger(StreamBase.class);

    static final Object NONE = new Object();

    // public static final int MAX_THREAD_POOL_SIZE = 8192;
    static final int MAX_THREAD_POOL_SIZE = Integer.MAX_VALUE;
    static final int MAX_THREAD_NUM_PER_OPERATION = 1024;

    static final int CORE_THREAD_POOL_SIZE = 64;

    static final int DEFAULT_MAX_THREAD_NUM = IOUtil.CPU_CORES;
    static final int DEFAULT_READING_THREAD_NUM = 8;

    static final int MAX_QUEUE_SIZE = 8192;
    static final int DEFAULT_QUEUE_SIZE_PER_ITERATOR = 32;
    static final Splitor DEFAULT_SPLITOR = Splitor.ITERATOR;
    static final Random RAND = new SecureRandom();

    @SuppressWarnings("rawtypes")
    static final Comparator NATURAL_COMPARATOR = Comparators.naturalOrder();

    @SuppressWarnings("rawtypes")
    static final Comparator REVERSED_COMPARATOR = Comparators.reversedOrder();

    static final Comparator<Character> CHAR_COMPARATOR = new Comparator<Character>() {
        @Override
        public int compare(Character o1, Character o2) {
            return Character.compare(o1, o2);
        }
    };

    static final Comparator<Byte> BYTE_COMPARATOR = new Comparator<Byte>() {
        @Override
        public int compare(Byte o1, Byte o2) {
            return Byte.compare(o1, o2);
        }
    };

    static final Comparator<Short> SHORT_COMPARATOR = new Comparator<Short>() {
        @Override
        public int compare(Short o1, Short o2) {
            return Short.compare(o1, o2);
        }
    };

    static final Comparator<Integer> INT_COMPARATOR = new Comparator<Integer>() {
        @Override
        public int compare(Integer o1, Integer o2) {
            return Integer.compare(o1, o2);
        }
    };

    static final Comparator<Long> LONG_COMPARATOR = new Comparator<Long>() {
        @Override
        public int compare(Long o1, Long o2) {
            return Long.compare(o1, o2);
        }
    };

    static final Comparator<Float> FLOAT_COMPARATOR = new Comparator<Float>() {
        @Override
        public int compare(Float o1, Float o2) {
            return Float.compare(o1, o2);
        }
    };

    static final Comparator<Double> DOUBLE_COMPARATOR = new Comparator<Double>() {
        @Override
        public int compare(Double o1, Double o2) {
            return Double.compare(o1, o2);
        }
    };

    static final Comparator<IndexedByte> INDEXED_BYTE_COMPARATOR = new Comparator<IndexedByte>() {
        @Override
        public int compare(IndexedByte a, IndexedByte b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<IndexedChar> INDEXED_CHAR_COMPARATOR = new Comparator<IndexedChar>() {
        @Override
        public int compare(IndexedChar a, IndexedChar b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<IndexedShort> INDEXED_SHORT_COMPARATOR = new Comparator<IndexedShort>() {
        @Override
        public int compare(IndexedShort a, IndexedShort b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<IndexedInt> INDEXED_INT_COMPARATOR = new Comparator<IndexedInt>() {
        @Override
        public int compare(IndexedInt a, IndexedInt b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<IndexedLong> INDEXED_LONG_COMPARATOR = new Comparator<IndexedLong>() {
        @Override
        public int compare(IndexedLong a, IndexedLong b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<IndexedFloat> INDEXED_FLOAT_COMPARATOR = new Comparator<IndexedFloat>() {
        @Override
        public int compare(IndexedFloat a, IndexedFloat b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<IndexedDouble> INDEXED_DOUBLE_COMPARATOR = new Comparator<IndexedDouble>() {
        @Override
        public int compare(IndexedDouble a, IndexedDouble b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final Comparator<Indexed<?>> INDEXED_COMPARATOR = new Comparator<Indexed<?>>() {
        @Override
        public int compare(Indexed<?> a, Indexed<?> b) {
            return N.compare(a.longIndex(), b.longIndex());
        }
    };

    static final BiMap<Class<?>, Comparator<?>> defaultComparator = new BiMap<>();

    static {
        defaultComparator.put(char.class, CHAR_COMPARATOR);
        defaultComparator.put(byte.class, BYTE_COMPARATOR);
        defaultComparator.put(short.class, SHORT_COMPARATOR);
        defaultComparator.put(int.class, INT_COMPARATOR);
        defaultComparator.put(long.class, LONG_COMPARATOR);
        defaultComparator.put(float.class, FLOAT_COMPARATOR);
        defaultComparator.put(double.class, DOUBLE_COMPARATOR);
        defaultComparator.put(Object.class, NATURAL_COMPARATOR);
    }

    static final Map<Class<?>, Integer> clsNum = new BiMap<>();

    static {
        int idx = 0;
        clsNum.put(boolean[].class, idx++); // 0
        clsNum.put(char[].class, idx++);
        clsNum.put(byte[].class, idx++);
        clsNum.put(short[].class, idx++);
        clsNum.put(int[].class, idx++);
        clsNum.put(long[].class, idx++);
        clsNum.put(float[].class, idx++);
        clsNum.put(double[].class, idx++); // 7
        clsNum.put(BooleanList.class, idx++); // 8
        clsNum.put(CharList.class, idx++);
        clsNum.put(ByteList.class, idx++);
        clsNum.put(ShortList.class, idx++);
        clsNum.put(IntList.class, idx++);
        clsNum.put(LongList.class, idx++);
        clsNum.put(FloatList.class, idx++);
        clsNum.put(DoubleList.class, idx++); // 15
    }
    static final AsyncExecutor asyncExecutor;

    static {
        final ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(CORE_THREAD_POOL_SIZE, MAX_THREAD_POOL_SIZE, 0L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<Runnable>(1));

        asyncExecutor = new AsyncExecutor(threadPoolExecutor) {
            @Override
            public CompletableFuture<Void> execute(final Runnable command) {
                //    if (threadPoolExecutor.getActiveCount() >= MAX_THREAD_POOL_SIZE) {
                //        throw new RejectedExecutionException("Task is rejected due to exceed max thread pool size: " + MAX_THREAD_POOL_SIZE);
                //    }

                return super.execute(command);
            }

            @Override
            public <T> CompletableFuture<T> execute(final Callable<T> command) {
                //    if (threadPoolExecutor.getActiveCount() >= MAX_THREAD_POOL_SIZE) {
                //        throw new RejectedExecutionException("Task is rejected due to exceed max thread pool size: " + MAX_THREAD_POOL_SIZE);
                //    }

                return super.execute(command);
            }
        };
    }
    @SuppressWarnings("rawtypes")
    static final BinaryOperator reducingCombiner = new BinaryOperator() {
        @Override
        public Object apply(Object t, Object u) {
            if (t instanceof Multiset) {
                ((Multiset) t).addAll((Multiset) u);
                return t;
            } else if (t instanceof LongMultiset) {
                ((LongMultiset) t).addAll((LongMultiset) u);
                return t;
            } else if (t instanceof Multimap) {
                ((Multimap) t).putAll((Multimap) u);
                return t;
            } else if (t instanceof Collection) {
                ((Collection) t).addAll((Collection) u);
                return t;
            } else if (t instanceof Map) {
                ((Map) t).putAll((Map) u);
                return t;
            } else if (t instanceof Object[]) {
                return N.concat((Object[]) t, (Object[]) u);
            } else if (t instanceof StringBuilder) {
                return ((StringBuilder) t).append((StringBuilder) u);
            } else if (t instanceof String) {
                return (String) t + (String) u;
            } else if (t instanceof Sheet) {
                ((Sheet) t).putAll((Sheet) u);
                return t;
            } else {
                final Class<?> cls = t.getClass();
                final Integer num = clsNum.get(cls);

                if (num == null) {
                    throw new RuntimeException(cls.getCanonicalName()
                            + " can't be combined by default. Only Map/Collection/StringBuilder/String/Multiset/LongMultiset/Multimap/Sheet/BooleanList ... List/boolean[] ... Object[] are supported");
                }

                switch (num.intValue()) {
                    case 0:
                        return N.concat((boolean[]) t, (boolean[]) u);
                    case 1:
                        return N.concat((char[]) t, (char[]) u);
                    case 2:
                        return N.concat((byte[]) t, (byte[]) u);
                    case 3:
                        return N.concat((short[]) t, (short[]) u);
                    case 4:
                        return N.concat((int[]) t, (int[]) u);
                    case 5:
                        return N.concat((long[]) t, (long[]) u);
                    case 6:
                        return N.concat((float[]) t, (float[]) u);
                    case 7:
                        return N.concat((double[]) t, (double[]) u);
                    case 8:
                        ((BooleanList) t).addAll((BooleanList) u);
                        return t;
                    case 9:
                        ((CharList) t).addAll((CharList) u);
                        return t;
                    case 10:
                        ((ByteList) t).addAll((ByteList) u);
                        return t;
                    case 11:
                        ((ShortList) t).addAll((ShortList) u);
                        return t;
                    case 12:
                        ((IntList) t).addAll((IntList) u);
                        return t;
                    case 13:
                        ((LongList) t).addAll((LongList) u);
                        return t;
                    case 14:
                        ((FloatList) t).addAll((FloatList) u);
                        return t;
                    case 15:
                        ((DoubleList) t).addAll((DoubleList) u);
                        return t;

                    default:
                        throw new RuntimeException(cls.getCanonicalName()
                                + " can't be combined by default. Only Map/Collection/StringBuilder/String/Multiset/LongMultiset/Multimap/Sheet/BooleanList ... List/boolean[] ... Object[] are supported");
                }
            }
        }
    };

    @SuppressWarnings("rawtypes")
    static final BiConsumer collectingCombiner = new BiConsumer() {
        @Override
        public void accept(Object t, Object u) {
            if (t instanceof Multiset) {
                ((Multiset) t).addAll((Multiset) u);
            } else if (t instanceof LongMultiset) {
                ((LongMultiset) t).addAll((LongMultiset) u);
            } else if (t instanceof Multimap) {
                ((Multimap) t).putAll((Multimap) u);
            } else if (t instanceof Collection) {
                ((Collection) t).addAll((Collection) u);
            } else if (t instanceof Map) {
                ((Map) t).putAll((Map) u);
            } else if (t instanceof StringBuilder) {
                ((StringBuilder) t).append((StringBuilder) u);
            } else if (t instanceof Sheet) {
                ((Sheet) t).putAll((Sheet) u);
            } else {
                final Class<?> cls = t.getClass();
                Integer num = clsNum.get(cls);

                if (num == null) {
                    throw new RuntimeException(cls.getCanonicalName()
                            + " can't be combined by default. Only Map/Collection/StringBuilder/Multiset/LongMultiset/Multimap/Sheet/BooleanList ... List are supported");
                }

                switch (num.intValue()) {
                    case 8:
                        ((BooleanList) t).addAll((BooleanList) u);
                        break;
                    case 9:
                        ((CharList) t).addAll((CharList) u);
                        break;
                    case 10:
                        ((ByteList) t).addAll((ByteList) u);
                        break;
                    case 11:
                        ((ShortList) t).addAll((ShortList) u);
                        break;
                    case 12:
                        ((IntList) t).addAll((IntList) u);
                        break;
                    case 13:
                        ((LongList) t).addAll((LongList) u);
                        break;
                    case 14:
                        ((FloatList) t).addAll((FloatList) u);
                        break;
                    case 15:
                        ((DoubleList) t).addAll((DoubleList) u);
                        break;

                    default:
                        throw new RuntimeException(cls.getCanonicalName()
                                + " can't be combined by default. Only Map/Collection/StringBuilder/Multiset/LongMultiset/Multimap/Sheet/BooleanList ... List are supported");
                }
            }
        }
    };

    static final Field listElementDataField;
    static final Field listSizeField;

    static {
        Field tmp = null;

        try {
            tmp = ArrayList.class.getDeclaredField("elementData");
        } catch (Exception e) {
            // ignore.
        }

        listElementDataField = tmp != null && tmp.getType().equals(Object[].class) ? tmp : null;

        if (listElementDataField != null) {
            listElementDataField.setAccessible(true);
        }

        tmp = null;

        try {
            tmp = ArrayList.class.getDeclaredField("size");
        } catch (Exception e) {
            // ignore.
        }

        listSizeField = tmp != null && tmp.getType().equals(int.class) ? tmp : null;

        if (listSizeField != null) {
            listSizeField.setAccessible(true);
        }
    }

    static volatile boolean isListElementDataFieldGettable = true;

    final Set<Runnable> closeHandlers;
    final boolean sorted;
    final Comparator<? super T> cmp;

    private boolean isClosed = false;

    StreamBase(final boolean sorted, final Comparator<? super T> cmp, final Collection<Runnable> closeHandlers) {
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null
                : (closeHandlers instanceof LocalLinkedHashSet ? (LocalLinkedHashSet<Runnable>) closeHandlers : new LocalLinkedHashSet<>(closeHandlers));
        this.sorted = sorted;
        this.cmp = cmp;
    }

    @Override
    public S carry(C action) {
        return peek(action);
    }

    //    @Override
    //    public OT findAny(P predicate) {
    //        return findFirst(predicate);
    //    }

    @Override
    public Stream<S> sliding(int windowSize) {
        return sliding(windowSize, 1);
    }

    @Override
    public Stream<PL> slidingToList(int windowSize) {
        return slidingToList(windowSize, 1);
    }

    @Override
    public S shuffled() {
        return shuffled(RAND);
    }

    @Override
    public ImmutableList<T> toImmutableList() {
        return ImmutableList.of(toList());
    }

    @Override
    public ImmutableSet<T> toImmutableSet() {
        return ImmutableSet.of(toSet());
    }

    @Override
    public void println() {
        N.println(sequential().join(", ", "[", "]"));
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public S sequential() {
        return (S) this;
    }

    @Override
    public S parallel() {
        return parallel(DEFAULT_MAX_THREAD_NUM, DEFAULT_SPLITOR);
    }

    @Override
    public S parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPLITOR);
    }

    @Override
    public S parallel(Splitor splitor) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitor);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException();

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public S maxThreadNum(int maxThreadNum) {
        // throw new UnsupportedOperationException();  

        // ignore, do nothing if it's sequential stream.
        return (S) this;
    }

    @Override
    public Splitor splitor() {
        // throw new UnsupportedOperationException();

        // ignore, do nothing if it's sequential stream.
        return DEFAULT_SPLITOR;
    }

    @Override
    public S splitor(Splitor splitor) {
        // throw new UnsupportedOperationException();

        // ignore, do nothing if it's sequential stream.
        return (S) this;
    }

    //    @Override
    //    @SuppressWarnings("rawtypes")
    //    public <SS extends BaseStream> SS p_s(Function<? super S, SS> op) {
    //        return (SS) this.parallel().__(op).sequential();
    //    }
    //
    //    @Override
    //    @SuppressWarnings("rawtypes")
    //    public <SS extends BaseStream> SS p_s(int maxThreadNum, Function<? super S, SS> op) {
    //        return (SS) this.parallel(maxThreadNum).__(op).sequential();
    //    }

    @Override
    public Try<S> tried() {
        return Try.of((S) this);
    }

    @Override
    public synchronized void close() {
        if (isClosed || N.isNullOrEmpty(closeHandlers)) {
            return;
        }

        isClosed = true;
        close(closeHandlers);
    }

    static void close(final Set<Runnable> closeHandlers) {
        Throwable ex = null;

        for (Runnable closeHandler : closeHandlers) {
            try {
                closeHandler.run();
            } catch (Throwable e) {
                if (ex == null) {
                    ex = e;
                } else {
                    ex.addSuppressed(e);
                }
            }
        }

        if (ex != null) {
            throw N.toRuntimeException(ex);
        }
    }

    static void close(final Collection<? extends IteratorEx<?>> iters) {
        Throwable ex = null;

        for (IteratorEx<?> iter : iters) {
            try {
                iter.close();
            } catch (Throwable e) {
                if (ex == null) {
                    ex = e;
                } else {
                    ex.addSuppressed(e);
                }
            }
        }

        if (ex != null) {
            throw N.toRuntimeException(ex);
        }
    }

    protected CharStream newStream(final char[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayCharStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayCharStream(a, sorted, closeHandlers);
        }
    }

    protected CharStream newStream(final CharIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorCharStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorCharStream(iter, sorted, closeHandlers);
        }
    }

    protected ByteStream newStream(final byte[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayByteStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayByteStream(a, sorted, closeHandlers);
        }
    }

    protected ByteStream newStream(final ByteIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorByteStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorByteStream(iter, sorted, closeHandlers);
        }
    }

    protected ShortStream newStream(final short[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayShortStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayShortStream(a, sorted, closeHandlers);
        }
    }

    protected ShortStream newStream(final ShortIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorShortStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorShortStream(iter, sorted, closeHandlers);
        }
    }

    protected IntStream newStream(final int[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayIntStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayIntStream(a, sorted, closeHandlers);
        }
    }

    protected IntStream newStream(final IntIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorIntStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorIntStream(iter, sorted, closeHandlers);
        }
    }

    protected LongStream newStream(final long[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayLongStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayLongStream(a, sorted, closeHandlers);
        }
    }

    protected LongStream newStream(final LongIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorLongStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorLongStream(iter, sorted, closeHandlers);
        }
    }

    protected FloatStream newStream(final float[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayFloatStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayFloatStream(a, sorted, closeHandlers);
        }
    }

    protected FloatStream newStream(final FloatIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorFloatStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorFloatStream(iter, sorted, closeHandlers);
        }
    }

    protected DoubleStream newStream(final double[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayDoubleStream(a, 0, a.length, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayDoubleStream(a, sorted, closeHandlers);
        }
    }

    protected DoubleStream newStream(final DoubleIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelIteratorDoubleStream(iter, sorted, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorDoubleStream(iter, sorted, closeHandlers);
        }
    }

    protected <E> Stream<E> newStream(final E[] a, final boolean sorted, final Comparator<? super E> comparator) {
        if (this.isParallel()) {
            return new ParallelArrayStream<>(a, 0, a.length, sorted, comparator, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new ArrayStream<>(a, sorted, comparator, closeHandlers);
        }
    }

    protected <E> Stream<E> newStream(final Iterator<E> iter, final boolean sorted, final Comparator<? super E> comparator) {
        if (this.isParallel()) {
            return new ParallelIteratorStream<>(iter, sorted, comparator, this.maxThreadNum(), this.splitor(), closeHandlers);
        } else {
            return new IteratorStream<>(iter, sorted, comparator, closeHandlers);
        }
    }

    static void setError(final Holder<Throwable> errorHolder, Throwable e) {
        synchronized (errorHolder) {
            if (errorHolder.value() == null) {
                errorHolder.setValue(e);
            } else {
                errorHolder.value().addSuppressed(e);
            }
        }
    }

    static void setError(final Holder<Throwable> errorHolder, Throwable e, final MutableBoolean onGoing) {
        onGoing.setFalse();

        setError(errorHolder, e);
    }

    static void throwError(final Holder<Throwable> errorHolder, final MutableBoolean onGoing) {
        onGoing.setFalse();

        synchronized (errorHolder) {
            if (errorHolder.value() != null) {
                throw N.toRuntimeException(errorHolder.getAndSet(null));
            }
        }
    }

    static void complete(final List<CompletableFuture<Void>> futureList, final Holder<Throwable> eHolder) {
        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();

                if (eHolder.value() != null) {
                    break;
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            if (eHolder.value() != null) {
                throw N.toRuntimeException(eHolder.value());
            }

            throw N.toRuntimeException(e);
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }
    }

    static <E extends Exception> void complette(final List<CompletableFuture<Void>> futureList, final Holder<Throwable> eHolder, E none) throws E {
        if (eHolder.value() != null) {
            if (eHolder.value() instanceof Exception) {
                throw (E) eHolder.value();
            } else {
                throw N.toRuntimeException(eHolder.value());
            }
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();

                if (eHolder.value() != null) {
                    break;
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            if (eHolder.value() != null) {
                if (eHolder.value() instanceof Exception) {
                    throw (E) eHolder.value();
                } else {
                    throw N.toRuntimeException(eHolder.value());
                }
            }

            throw N.toRuntimeException(e);
        }

        if (eHolder.value() != null) {
            if (eHolder.value() instanceof Exception) {
                throw (E) eHolder.value();
            } else {
                throw N.toRuntimeException(eHolder.value());
            }
        }
    }

    static int calculateQueueSize(int len) {
        return N.min(MAX_QUEUE_SIZE, len * DEFAULT_QUEUE_SIZE_PER_ITERATOR);
    }

    static boolean isSameComparator(Comparator<?> a, Comparator<?> b) {
        if (a == b) {
            return true;
        } else if (a == null) {
            return defaultComparator.containsValue(b);
        } else if (b == null) {
            return defaultComparator.containsValue(a);
        } else {
            return (a == NATURAL_COMPARATOR && defaultComparator.containsValue(b)) || (b == NATURAL_COMPARATOR && defaultComparator.containsValue(a));
        }
    }

    static int checkMaxThreadNum(int maxThreadNum) {
        //    if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
        //        throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        //    }

        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1");
        }

        return N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
    }

    static int toInt(long max) {
        return max > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) max;
    }

    static CharIteratorEx charIterator(final ObjIteratorEx<Character> iter) {
        return CharIteratorEx.from(iter);
    }

    static ByteIteratorEx byteIterator(final ObjIteratorEx<Byte> iter) {
        return ByteIteratorEx.from(iter);
    }

    static ShortIteratorEx shortIterator(final ObjIteratorEx<Short> iter) {
        return ShortIteratorEx.from(iter);
    }

    static IntIteratorEx intIterator(final ObjIteratorEx<Integer> iter) {
        return IntIteratorEx.from(iter);
    }

    static LongIteratorEx longIterator(final ObjIteratorEx<Long> iter) {
        return LongIteratorEx.from(iter);
    }

    static FloatIteratorEx floatIterator(final ObjIteratorEx<Float> iter) {
        return FloatIteratorEx.from(iter);
    }

    static DoubleIteratorEx doubleIterator(final ObjIteratorEx<Double> iter) {
        return DoubleIteratorEx.from(iter);
    }

    static Runnable newCloseHandle(final AutoCloseable closeable) {
        return Fn.closeQuietly(closeable);
    }

    static Runnable newCloseHandler(final Collection<? extends StreamBase<?, ?, ?, ?, ?, ?, ?, ?, ?>> c) {
        return new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (StreamBase<?, ?, ?, ?, ?, ?, ?, ?, ?> s : c) {
                    try {
                        s.close();
                    } catch (Throwable throwable) {
                        if (runtimeException == null) {
                            runtimeException = N.toRuntimeException(throwable);
                        } else {
                            runtimeException.addSuppressed(throwable);
                        }
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        };
    }

    static Set<Runnable> mergeCloseHandlers(final StreamBase<?, ?, ?, ?, ?, ?, ?, ?, ?> stream, final Set<Runnable> closeHandlers) {
        return mergeCloseHandlers(stream.closeHandlers, closeHandlers);
    }

    static Set<Runnable> mergeCloseHandlers(final Set<Runnable> closeHandlersA, final Set<Runnable> closeHandlersB) {
        if (N.isNullOrEmpty(closeHandlersA) && closeHandlersB instanceof LocalLinkedHashSet) {
            return closeHandlersB;
        } else if (closeHandlersA instanceof LocalLinkedHashSet && N.isNullOrEmpty(closeHandlersB)) {
            return closeHandlersA;
        } else if (N.isNullOrEmpty(closeHandlersA) && N.isNullOrEmpty(closeHandlersB)) {
            return null;
        }

        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>();

        if (N.notNullOrEmpty(closeHandlersA)) {
            newCloseHandlers.addAll(closeHandlersA);
        }

        if (N.notNullOrEmpty(closeHandlersB)) {
            newCloseHandlers.addAll(closeHandlersB);
        }

        return newCloseHandlers;
    }

    static Object hashKey(Object obj) {
        return obj == null || obj.getClass().isArray() == false ? obj : Wrapper.of(obj);
    }

    static <T> T[] toArray(Collection<T> c) {
        if (isListElementDataFieldGettable && listElementDataField != null && c instanceof ArrayList) {
            try {
                return (T[]) listElementDataField.get(c);
            } catch (Throwable e) {
                // ignore;
                isListElementDataFieldGettable = false;
            }
        }

        return (T[]) c.toArray();
    }

    static long sum(final char[] a) {
        if (a == null || a.length == 0) {
            return 0L;
        }

        return sum(a, 0, a.length);
    }

    static long sum(final char[] a, final int from, final int to) {
        long sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    static long sum(final byte[] a) {
        if (a == null || a.length == 0) {
            return 0L;
        }

        return sum(a, 0, a.length);
    }

    static long sum(final byte[] a, final int from, final int to) {
        long sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    static long sum(final short[] a) {
        if (a == null || a.length == 0) {
            return 0L;
        }

        return sum(a, 0, a.length);
    }

    static long sum(final short[] a, final int from, final int to) {
        long sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    static long sum(final int[] a) {
        if (a == null || a.length == 0) {
            return 0L;
        }

        return sum(a, 0, a.length);
    }

    static long sum(final int[] a, final int from, final int to) {
        long sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    static long sum(final long[] a) {
        if (a == null || a.length == 0) {
            return 0L;
        }

        return sum(a, 0, a.length);
    }

    static long sum(final long[] a, final int from, final int to) {
        long sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    static double sum(final float[] a) {
        if (a == null || a.length == 0) {
            return 0d;
        }

        return sum(a, 0, a.length);
    }

    static double sum(final float[] a, final int from, final int to) {
        return FloatStream.of(a, from, to).sum();
    }

    static double sum(final double[] a) {
        if (a == null || a.length == 0) {
            return 0d;
        }

        return sum(a, 0, a.length);
    }

    static double sum(final double[] a, final int from, final int to) {
        return DoubleStream.of(a, from, to).sum();
    }

    static final class LocalLinkedHashSet<T> extends LinkedHashSet<T> {
        private static final long serialVersionUID = -97425473105100734L;

        public LocalLinkedHashSet() {
            super();
        }

        public LocalLinkedHashSet(int initialCapacity) {
            super(initialCapacity);
        }

        public LocalLinkedHashSet(int initialCapacity, float loadFactor) {
            super(initialCapacity, loadFactor);
        }

        public LocalLinkedHashSet(Collection<? extends T> c) {
            super(c);
        }
    }
}
