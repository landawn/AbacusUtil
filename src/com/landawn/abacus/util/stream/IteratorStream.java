package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
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
    private final ImmutableIterator<T> elements;
    private final boolean sorted;
    private final Comparator<? super T> cmp;
    private final Set<Runnable> closeHandlers;

    IteratorStream(final Iterator<? extends T> iterator) {
        this(iterator, null);
    }

    IteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers) {
        this(iterator, closeHandlers, false, null);
    }

    IteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> cmp) {
        Objects.requireNonNull(iterator);

        this.elements = iterator instanceof ImmutableIterator ? (ImmutableIterator<T>) iterator : new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public T next() {
                return iterator.next();
            }
        };

        this.sorted = sorted;
        this.cmp = cmp;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, max)], 0);
        //
        //        int cnt = 0;
        //        T e = null;
        //        while (cnt < max && elements.hasNext()) {
        //            e = elements.next();
        //
        //            if (predicate.test(e)) {
        //                list.add(e);
        //                cnt++;
        //            }
        //        }
        //
        //        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private boolean hasNext = false;
            private T next = null;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cnt < max) {
                    while (elements.hasNext()) {
                        next = elements.next();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, Stream.toInt(max))], 0);
        //
        //        int cnt = 0;
        //        T e = null;
        //        while (cnt < max && elements.hasNext()) {
        //            e = elements.next();
        //
        //            if (predicate.test(e)) {
        //                list.add(e);
        //                cnt++;
        //            } else {
        //                break;
        //            }
        //        }
        //
        //        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private boolean hasNext = false;
            private T next = null;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cnt < max) {
                    while (elements.hasNext()) {
                        next = elements.next();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        } else {
                            cnt = Long.MAX_VALUE; // no more loop.
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate, final long max) {
        //        while (elements.hasNext() && predicate.test(elements.next())) {
        //        }
        //
        //        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, Stream.toInt(max))], 0);
        //        int cnt = 0;
        //
        //        while (cnt < max && elements.hasNext()) {
        //            list.add(elements.next());
        //            cnt++;
        //        }
        //
        //        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private boolean hasNext = false;
            private T next = null;
            private long cnt = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cnt < max) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.next();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else {
                        if (elements.hasNext()) {
                            next = elements.next();
                            hasNext = true;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted, cmp);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        //        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);
        //
        //        while (values.hasNext()) {
        //            list.add(mapper.apply(values.next()));
        //        }
        //
        //        return new ArrayStream<R>((R[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public R next() {
                return mapper.apply(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char next() {
                return mapper.applyAsChar(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte next() {
                return mapper.applyAsByte(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short next() {
                return mapper.applyAsShort(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
                return mapper.applyAsInt(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long next() {
                return mapper.applyAsLong(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                return mapper.applyAsFloat(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                return mapper.applyAsDouble(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
        //        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);
        //
        //        while (values.hasNext()) {
        //            final Iterator<? extends R> it = mapper.apply(values.next()).iterator();
        //
        //            while (it.hasNext()) {
        //                list.add(it.next());
        //            }
        //        }
        //
        //        return new ArrayStream<R>((R[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            private Iterator<? extends R> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public R next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> flatMap2(final Function<? super T, ? extends R[]> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //
        //        int lengthOfAll = 0;
        //        while (values.hasNext()) {
        //            final Object[] tmp = mapper.apply(values.next());
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final Object[] arrayOfAll = new Object[lengthOfAll];
        //        int from = 0;
        //        for (Object[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayStream<R>((R[]) arrayOfAll, closeHandlers);

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            private R[] cur = null;
            private int curIndex = 0;

            @Override
            public boolean hasNext() {
                while ((cur == null || curIndex >= cur.length) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
                    curIndex = 0;
                }

                return cur != null && curIndex < cur.length;
            }

            @Override
            public R next() {
                if (cur == null || curIndex >= cur.length) {
                    throw new NoSuchElementException();
                }

                return cur[curIndex++];
            }
        }, closeHandlers);

    }

    @Override
    public <R> Stream<R> flatMap3(final Function<? super T, ? extends Collection<? extends R>> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //
        //        int lengthOfAll = 0;
        //        while (values.hasNext()) {
        //            final Object[] tmp = mapper.apply(values.next()).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final Object[] arrayOfAll = new Object[lengthOfAll];
        //        int from = 0;
        //        for (Object[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayStream<R>((R[]) arrayOfAll, closeHandlers);

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            Iterator<? extends R> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public R next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private ImmutableCharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).charIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public CharStream flatMapToChar2(final Function<? super T, char[]> mapper) {
        return flatMapToChar(new Function<T, CharStream>() {
            @Override
            public CharStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public CharStream flatMapToChar3(final Function<? super T, ? extends Collection<Character>> mapper) {
        return flatMapToChar(new Function<T, CharStream>() {
            @Override
            public CharStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToChar(new ToCharFunction<Character>() {
                    @Override
                    public char applyAsChar(Character value) {
                        return value == null ? 0 : value.charValue();
                    }
                });
            }
        });
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            private ImmutableByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).byteIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public byte next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public ByteStream flatMapToByte2(final Function<? super T, byte[]> mapper) {
        return flatMapToByte(new Function<T, ByteStream>() {
            @Override
            public ByteStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream flatMapToByte3(final Function<? super T, ? extends Collection<Byte>> mapper) {
        return flatMapToByte(new Function<T, ByteStream>() {
            @Override
            public ByteStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToByte(new ToByteFunction<Byte>() {
                    @Override
                    public byte applyAsByte(Byte value) {
                        return value == null ? 0 : value.byteValue();
                    }
                });
            }
        });
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private ImmutableShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).shortIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMapToShort2(final Function<? super T, short[]> mapper) {
        return flatMapToShort(new Function<T, ShortStream>() {
            @Override
            public ShortStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public ShortStream flatMapToShort3(final Function<? super T, ? extends Collection<Short>> mapper) {
        return flatMapToShort(new Function<T, ShortStream>() {
            @Override
            public ShortStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToShort(new ToShortFunction<Short>() {
                    @Override
                    public short applyAsShort(Short value) {
                        return value == null ? 0 : value.shortValue();
                    }
                });
            }
        });
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private ImmutableIntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).intIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt2(final Function<? super T, int[]> mapper) {
        return flatMapToInt(new Function<T, IntStream>() {
            @Override
            public IntStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public IntStream flatMapToInt3(final Function<? super T, ? extends Collection<Integer>> mapper) {
        return flatMapToInt(new Function<T, IntStream>() {
            @Override
            public IntStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToInt(new ToIntFunction<Integer>() {
                    @Override
                    public int applyAsInt(Integer value) {
                        return value == null ? 0 : value.intValue();
                    }
                });
            }
        });
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private ImmutableLongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).longIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong2(final Function<? super T, long[]> mapper) {
        return flatMapToLong(new Function<T, LongStream>() {
            @Override
            public LongStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public LongStream flatMapToLong3(final Function<? super T, ? extends Collection<Long>> mapper) {
        return flatMapToLong(new Function<T, LongStream>() {
            @Override
            public LongStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToLong(new ToLongFunction<Long>() {
                    @Override
                    public long applyAsLong(Long value) {
                        return value == null ? 0 : value.longValue();
                    }
                });
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private ImmutableFloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).floatIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream flatMapToFloat2(final Function<? super T, float[]> mapper) {
        return flatMapToFloat(new Function<T, FloatStream>() {
            @Override
            public FloatStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat3(final Function<? super T, ? extends Collection<Float>> mapper) {
        return flatMapToFloat(new Function<T, FloatStream>() {
            @Override
            public FloatStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToFloat(new ToFloatFunction<Float>() {
                    @Override
                    public float applyAsFloat(Float value) {
                        return value == null ? 0 : value.floatValue();
                    }
                });
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private ImmutableDoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).doubleIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble2(final Function<? super T, double[]> mapper) {
        return flatMapToDouble(new Function<T, DoubleStream>() {
            @Override
            public DoubleStream apply(T t) {
                return Stream.from(mapper.apply(t));
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble3(final Function<? super T, ? extends Collection<Double>> mapper) {
        return flatMapToDouble(new Function<T, DoubleStream>() {
            @Override
            public DoubleStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToDouble(new ToDoubleFunction<Double>() {
                    @Override
                    public double applyAsDouble(Double value) {
                        return value == null ? 0 : value.doubleValue();
                    }
                });
            }
        });
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier, Supplier<Map<K, List<T>>> mapFactory) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, D, A> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return new IteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {

            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Stream<T> next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Object[] a = new Object[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayStream<T>((T[]) a, 0, cnt, null, sorted, cmp);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<List<T>> splitIntoList(final int size) {
        return new IteratorStream<List<T>>(new ImmutableIterator<List<T>>() {

            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> list = new ArrayList<>(size);
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    list.add(elements.next());
                }

                return list;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<Set<T>> splitIntoSet(final int size) {
        return new IteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {

            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Set<T> next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Set<T> list = new LinkedHashSet<>(N.initHashCapacity(size));
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    list.add(elements.next());
                }

                return list;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<T> distinct() {
        //        final Set<T> set = new LinkedHashSet<T>();
        //
        //        while (elements.hasNext()) {
        //            set.add(elements.next());
        //        }
        //
        //        return new ArrayStream<T>((T[]) set.toArray(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Set<T> set = new LinkedHashSet<T>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                a = (T[]) set.toArray();
                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> distinct(final Comparator<? super T> comparator) {
        //        final Set<T> set = new LinkedHashSet<T>();
        //
        //        while (elements.hasNext()) {
        //            set.add(elements.next());
        //        }
        //
        //        return new ArrayStream<T>((T[]) set.toArray(), closeHandlers);

        if (comparator == null) {
            return distinct();
        }

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Set<T> set = new TreeSet<T>(comparator);

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                a = (T[]) set.toArray();
                toIndex = a.length;
            }
        }, closeHandlers);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        return new IteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final List<T> list = new ArrayList<>();
                final Set<Object> keySet = new HashSet<>();

                Object key = null;
                T e = null;

                while (elements.hasNext()) {
                    key = keyMapper.apply(e);

                    if (keySet.add(key)) {
                        list.add(e);
                    }
                }

                a = (T[]) list.toArray();
                toIndex = a.length;
            }
        }, closeHandlers);
    }

    @Override
    public Stream<T> top(int n) {
        return top(n, OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> top(final int n, final Comparator<? super T> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    getResult();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    getResult();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    getResult();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    getResult();
                }

                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    getResult();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void getResult() {
                final Comparator<Pair<T, Long>> pairCmp = new Comparator<Pair<T, Long>>() {
                    @Override
                    public int compare(final Pair<T, Long> o1, final Pair<T, Long> o2) {
                        return N.compare(o1.left, o2.left, cmp);
                    }
                };

                final Queue<Pair<T, Long>> heap = new PriorityQueue<Pair<T, Long>>(n, pairCmp);

                Pair<T, Long> pair = null;
                for (long i = 0; elements.hasNext(); i++) {
                    pair = Pair.of(elements.next(), i);

                    if (heap.size() >= n) {
                        if (pairCmp.compare(heap.peek(), pair) < 0) {
                            heap.poll();
                            heap.add(pair);
                        }
                    } else {
                        heap.offer(pair);
                    }
                }

                final Pair<T, Long>[] arrayOfPair = heap.toArray(new Pair[heap.size()]);

                N.sort(arrayOfPair, new Comparator<Pair<T, Long>>() {
                    @Override
                    public int compare(final Pair<T, Long> o1, final Pair<T, Long> o2) {
                        return o1.right.intValue() - o2.right.intValue();
                    }
                });

                a = (T[]) new Object[arrayOfPair.length];

                for (int i = 0, len = arrayOfPair.length; i < len; i++) {
                    a[i] = arrayOfPair[i].left;
                }

                toIndex = a.length;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<T> sorted() {
        return sorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> sorted(final Comparator<? super T> comparator) {
        if (sorted && this.cmp == comparator) {
            return new IteratorStream<T>(elements, closeHandlers, sorted, cmp);
        }

        //        final Object[] a = toArray();
        //
        //        if (comparator == null) {
        //            N.sort(a);
        //        } else {
        //            N.sort((T[]) a, comparator);
        //        }
        //
        //        return new ArrayStream<T>((T[]) a, true, comparator, closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public T next() {
                if (a == null) {
                    sort();
                }

                if (cursor >= a.length) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    sort();
                }

                return a.length - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    sort();
                }

                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    sort();
                }

                if (b.getClass().equals(a.getClass()) && b.length < a.length - cursor) {
                    if (cursor == 0) {
                        return (A[]) a;
                    } else {
                        return (A[]) N.copyOfRange(a, cursor, a.length);
                    }
                } else {
                    if (b.length < a.length - cursor) {
                        b = N.newArray(b.getClass().getComponentType(), a.length - cursor);
                    }

                    N.copy(a, cursor, b, 0, a.length - cursor);

                    return b;
                }
            }

            private void sort() {
                a = (T[]) elements.toArray(N.EMPTY_OBJECT_ARRAY);

                if (comparator == null) {
                    N.sort(a);
                } else {
                    N.sort(a, comparator);
                }
            }
        }, closeHandlers, true, comparator);
    }

    @Override
    public Stream<T> parallelSorted() {
        return parallelSorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> parallelSorted(final Comparator<? super T> comparator) {
        if (sorted && this.cmp == comparator) {
            return new IteratorStream<T>(elements, closeHandlers, sorted, cmp);
        }

        //        final Object[] a = toArray();
        //
        //        if (comparator == null) {
        //            N.parallelSort(a);
        //        } else {
        //            N.parallelSort((T[]) a, comparator);
        //        }
        //
        //        return new ArrayStream<T>((T[]) a, true, comparator, closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            T[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    parallelSort();
                }

                return cursor < a.length;
            }

            @Override
            public T next() {
                if (a == null) {
                    parallelSort();
                }

                if (cursor >= a.length) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    parallelSort();
                }

                return a.length - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    parallelSort();
                }

                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    parallelSort();
                }

                if (b.getClass().equals(a.getClass()) && b.length < a.length - cursor) {
                    if (cursor == 0) {
                        return (A[]) a;
                    } else {
                        return (A[]) N.copyOfRange(a, cursor, a.length);
                    }
                } else {
                    if (b.length < a.length - cursor) {
                        b = N.newArray(b.getClass().getComponentType(), a.length - cursor);
                    }

                    N.copy(a, cursor, b, 0, a.length - cursor);

                    return b;
                }
            }

            private void parallelSort() {
                a = (T[]) elements.toArray(N.EMPTY_OBJECT_ARRAY);

                if (comparator == null) {
                    N.parallelSort(a);
                } else {
                    N.parallelSort(a, comparator);
                }
            }
        }, closeHandlers, true, comparator);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        //        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);
        //
        //        T e = null;
        //        while (elements.hasNext()) {
        //            e = elements.next();
        //            list.add(e);
        //            action.accept(e);
        //        }
        //
        //        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public T next() {
                final T next = elements.next();

                //    try {
                //        action.accept(next);
                //    } catch (Throwable e) {
                //        // ignore.
                //    }

                action.accept(next);
                return next;
            }

            //    @Override
            //    public long count() {
            //        return elements.count();
            //    }
            //
            //    @Override
            //    public void skip(long n) {
            //        elements.skip(n);
            //    }
            //
            //    @Override
            //    public <A> A[] toArray(A[] a) {
            //        return elements.toArray(a);
            //    }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        //        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);
        //        long cnt = 0;
        //
        //        while (elements.hasNext() && cnt < maxSize) {
        //            list.add(elements.next());
        //            cnt++;
        //        }
        //
        //        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public T next() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.next();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        //        final ObjectList<Object> list = ObjectList.of(new Object[9], 0);
        //
        //        long cnt = 0;
        //        while (elements.hasNext() && cnt < n) {
        //            elements.next();
        //            cnt++;
        //        }
        //
        //        while (elements.hasNext()) {
        //            list.add(elements.next());
        //        }
        //
        //        return new ArrayStream<T>((T[]) list.trimToSize().array(), closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.hasNext();
            }

            @Override
            public T next() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.next();
            }

            @Override
            public long count() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.count();
            }

            @Override
            public void skip(long n) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n);
            }

            @Override
            public <A> A[] toArray(A[] a) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray(a);
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    @Override
    public boolean forEach2(Function<? super T, Boolean> action) {
        while (elements.hasNext()) {
            if (action.apply(elements.next()).booleanValue() == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public Object[] toArray() {
        return toArray(N.EMPTY_OBJECT_ARRAY);
    }

    <A> A[] toArray(A[] a) {
        return elements.toArray(a);
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return toArray(generator.apply(0));
    }

    @Override
    public <A> ObjectList<A> toObjectList(Class<A> cls) {
        return ObjectList.of(toArray((A[]) N.newArray(cls, 0)));
    }

    @Override
    public T reduce(T identity, BinaryOperator<T> accumulator) {
        T result = identity;

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return result;
    }

    @Override
    public Optional<T> reduce(BinaryOperator<T> accumulator) {
        if (elements.hasNext() == false) {
            Optional.empty();
        }

        T result = elements.next();

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return Optional.of(result);
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {
        U result = identity;

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (elements.hasNext()) {
            accumulator.accept(container, elements.next());
        }

        return collector.finisher().apply(container);
    }

    @Override
    public Optional<T> min(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        T candidate = elements.next();
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            if (comparator.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return Optional.of(candidate);
    }

    @Override
    public Optional<T> max(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        T candidate = elements.next();
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            if (comparator.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return Optional.of(candidate);
    }

    @Override
    public Optional<T> kthLargest(int k, Comparator<? super T> cmp) {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final Queue<T> queue = new PriorityQueue<T>(k);
        T e = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (queue.size() < k) {
                queue.add(e);
            } else {
                if (N.compare(e, queue.peek(), cmp) > 0) {
                    queue.remove();
                    queue.add(e);
                }
            }
        }

        return queue.size() < k ? (Optional<T>) Optional.empty() : Optional.of(queue.peek());
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public boolean anyMatch(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public Optional<T> findFirst() {
    //        return elements.hasNext() ? (Optional<T>) Optional.empty() : Optional.of(elements.next());
    //    }

    @Override
    public Optional<T> findFirst(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            T e = elements.next();

            if (predicate.test(e)) {
                return Optional.of(e);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    //    @Override
    //    public Optional<T> findLast() {
    //        if (elements.hasNext() == false) {
    //            return (Optional<T>) Optional.empty();
    //        }
    //
    //        T e = null;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return Optional.of(e);
    //    }

    @Override
    public Optional<T> findLast(Predicate<? super T> predicate) {
        if (elements.hasNext() == false) {
            return (Optional<T>) Optional.empty();
        }

        boolean hasResult = false;
        T e = null;
        T result = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? Optional.of(result) : (Optional<T>) Optional.empty();
    }

    //    @Override
    //    public Optional<T> findAny() {
    //        return elements.hasNext() ? (Optional<T>) Optional.empty() : Optional.of(elements.next());
    //    }

    @Override
    public Optional<T> findAny(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            T e = elements.next();

            if (predicate.test(e)) {
                return Optional.of(e);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    @Override
    public Iterator<T> iterator() {
        return elements;
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new IteratorStream<T>(elements, closeHandlerList, sorted, cmp);
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
