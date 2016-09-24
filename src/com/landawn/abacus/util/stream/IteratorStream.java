package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.ErrorBreakIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullBreakIterator;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortSummaryStatistics;
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
import com.landawn.abacus.util.stream.ImmutableIterator.QueuedIterator;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 */
final class IteratorStream<T> extends AbstractStream<T> {
    private final ImmutableIterator<T> elements;
    private final boolean sorted;
    private final Comparator<? super T> cmp;

    IteratorStream(final Iterator<? extends T> iterator) {
        this(iterator, null);
    }

    IteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers) {
        this(iterator, closeHandlers, false, null);
    }

    IteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator) {
        super(closeHandlers);

        N.requireNonNull(iterator);

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
        this.cmp = comparator;
    }

    IteratorStream(Stream<T> stream, Set<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator) {
        this(stream.iterator(), Stream.mergeCloseHandlers(stream, closeHandlers), sorted, comparator);
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
        }, closeHandlers);
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
        }, closeHandlers);
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
        }, closeHandlers);
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
        }, closeHandlers);
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
        }, closeHandlers);
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
        }, closeHandlers);
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
        }, closeHandlers);
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
                if ((cur == null || curIndex >= cur.length) && hasNext() == false) {
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
        return flatMapToChar4(new Function<T, CharIterator>() {
            @Override
            public CharIterator apply(T t) {
                return mapper.apply(t).charIterator();
            }
        });
    }

    @Override
    public CharStream flatMapToChar2(final Function<? super T, char[]> mapper) {
        return flatMapToChar4(new Function<T, CharIterator>() {
            @Override
            public CharIterator apply(T t) {
                return ImmutableCharIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public CharStream flatMapToChar3(final Function<? super T, ? extends Collection<Character>> mapper) {
        return flatMapToChar4(new Function<T, CharIterator>() {
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

    CharStream flatMapToChar4(final Function<? super T, CharIterator> mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private CharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        return flatMapToByte4(new Function<T, ByteIterator>() {
            @Override
            public ByteIterator apply(T t) {
                return mapper.apply(t).byteIterator();
            }
        });
    }

    @Override
    public ByteStream flatMapToByte2(final Function<? super T, byte[]> mapper) {
        return flatMapToByte4(new Function<T, ByteIterator>() {
            @Override
            public ByteIterator apply(T t) {
                return ImmutableByteIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream flatMapToByte3(final Function<? super T, ? extends Collection<Byte>> mapper) {
        return flatMapToByte4(new Function<T, ByteIterator>() {
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

    ByteStream flatMapToByte4(final Function<? super T, ByteIterator> mapper) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            private ByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        return flatMapToShort4(new Function<T, ShortIterator>() {
            @Override
            public ShortIterator apply(T t) {
                return mapper.apply(t).shortIterator();
            }
        });
    }

    @Override
    public ShortStream flatMapToShort2(final Function<? super T, short[]> mapper) {
        return flatMapToShort4(new Function<T, ShortIterator>() {
            @Override
            public ShortIterator apply(T t) {
                return ImmutableShortIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ShortStream flatMapToShort3(final Function<? super T, ? extends Collection<Short>> mapper) {
        return flatMapToShort4(new Function<T, ShortIterator>() {
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

    ShortStream flatMapToShort4(final Function<? super T, ShortIterator> mapper) {
        return new IteratorShortStream(new ImmutableShortIterator() {
            private ShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        return flatMapToInt4(new Function<T, IntIterator>() {
            @Override
            public IntIterator apply(T t) {
                return mapper.apply(t).intIterator();
            }
        });
    }

    @Override
    public IntStream flatMapToInt2(final Function<? super T, int[]> mapper) {
        return flatMapToInt4(new Function<T, IntIterator>() {
            @Override
            public IntIterator apply(T t) {
                return ImmutableIntIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public IntStream flatMapToInt3(final Function<? super T, ? extends Collection<Integer>> mapper) {
        return flatMapToInt4(new Function<T, IntIterator>() {
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

    IntStream flatMapToInt4(final Function<? super T, IntIterator> mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        return flatMapToLong4(new Function<T, LongIterator>() {
            @Override
            public LongIterator apply(T t) {
                return mapper.apply(t).longIterator();
            }
        });
    }

    @Override
    public LongStream flatMapToLong2(final Function<? super T, long[]> mapper) {
        return flatMapToLong4(new Function<T, LongIterator>() {
            @Override
            public LongIterator apply(T t) {
                return ImmutableLongIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public LongStream flatMapToLong3(final Function<? super T, ? extends Collection<Long>> mapper) {
        return flatMapToLong4(new Function<T, LongIterator>() {
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

    LongStream flatMapToLong4(final Function<? super T, LongIterator> mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        return flatMapToFloat4(new Function<T, FloatIterator>() {
            @Override
            public FloatIterator apply(T t) {
                return mapper.apply(t).floatIterator();
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat2(final Function<? super T, float[]> mapper) {
        return flatMapToFloat4(new Function<T, FloatIterator>() {
            @Override
            public FloatIterator apply(T t) {
                return ImmutableFloatIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat3(final Function<? super T, ? extends Collection<Float>> mapper) {
        return flatMapToFloat4(new Function<T, FloatIterator>() {
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

    FloatStream flatMapToFloat4(final Function<? super T, FloatIterator> mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        return flatMapToDouble4(new Function<T, DoubleIterator>() {
            @Override
            public DoubleIterator apply(T t) {
                return mapper.apply(t).doubleIterator();
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble2(final Function<? super T, double[]> mapper) {
        return flatMapToDouble4(new Function<T, DoubleIterator>() {
            @Override
            public DoubleIterator apply(T t) {
                return ImmutableDoubleIterator.of(mapper.apply(t));
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble3(final Function<? super T, ? extends Collection<Double>> mapper) {
        return flatMapToDouble4(new Function<T, DoubleIterator>() {
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

    DoubleStream flatMapToDouble4(final Function<? super T, DoubleIterator> mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
                    cnt++;
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

                final Set<T> set = new HashSet<>(N.initHashCapacity(size));
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    set.add(elements.next());
                    cnt++;
                }

                return set;
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
                final Set<T> set = new HashSet<T>();
                final List<T> list = new ArrayList<T>();
                T element = null;

                while (elements.hasNext()) {
                    element = elements.next();
                    if (set.add(element)) {
                        list.add(element);
                    }
                }

                a = (T[]) list.toArray();
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
                final List<T> list = new ArrayList<T>();
                T element = null;
                boolean hasNull = false;

                while (elements.hasNext()) {
                    element = elements.next();
                    if (element == null) {
                        if (hasNull == false) {
                            hasNull = true;
                            list.add(element);
                        }
                    } else {
                        if (set.add(element)) {
                            list.add(element);
                        }
                    }
                }

                a = (T[]) list.toArray();
                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp);
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
                final Set<Object> set = new HashSet<>();
                final List<T> list = new ArrayList<T>();
                T element = null;

                while (elements.hasNext()) {
                    element = elements.next();
                    if (set.add(keyMapper.apply(element))) {
                        list.add(element);
                    }
                }

                a = (T[]) list.toArray();
                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp);
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
                        return N.compare(o1.left, o2.left, comparator);
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
                        return N.compare(o1.right.longValue(), o2.right.longValue());
                    }
                });

                a = (T[]) new Object[arrayOfPair.length];

                for (int i = 0, len = arrayOfPair.length; i < len; i++) {
                    a[i] = arrayOfPair[i].left;
                }

                toIndex = a.length;
            }

        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> sorted() {
        return sorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> sorted(final Comparator<? super T> comparator) {
        if (sorted && isSameComparator(comparator, cmp)) {
            return this;
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

    //    @Override
    //    public Stream<T> parallelSorted() {
    //        return parallelSorted(OBJECT_COMPARATOR);
    //    }
    //
    //    @Override
    //    public Stream<T> parallelSorted(final Comparator<? super T> comparator) {
    //        if (sorted && isSameComparator(comparator, cmp)) {
    //            return new IteratorStream<T>(elements, closeHandlers, sorted, comparator);
    //        }
    //
    //        //        final Object[] a = toArray();
    //        //
    //        //        if (comparator == null) {
    //        //            N.parallelSort(a);
    //        //        } else {
    //        //            N.parallelSort((T[]) a, comparator);
    //        //        }
    //        //
    //        //        return new ArrayStream<T>((T[]) a, true, comparator, closeHandlers);
    //
    //        return new IteratorStream<T>(new ImmutableIterator<T>() {
    //            T[] a = null;
    //            int cursor = 0;
    //
    //            @Override
    //            public boolean hasNext() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                return cursor < a.length;
    //            }
    //
    //            @Override
    //            public T next() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                if (cursor >= a.length) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return a[cursor++];
    //            }
    //
    //            @Override
    //            public long count() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                return a.length - cursor;
    //            }
    //
    //            @Override
    //            public void skip(long n) {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
    //            }
    //
    //            @Override
    //            public <A> A[] toArray(A[] b) {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                if (b.getClass().equals(a.getClass()) && b.length < a.length - cursor) {
    //                    if (cursor == 0) {
    //                        return (A[]) a;
    //                    } else {
    //                        return (A[]) N.copyOfRange(a, cursor, a.length);
    //                    }
    //                } else {
    //                    if (b.length < a.length - cursor) {
    //                        b = N.newArray(b.getClass().getComponentType(), a.length - cursor);
    //                    }
    //
    //                    N.copy(a, cursor, b, 0, a.length - cursor);
    //
    //                    return b;
    //                }
    //            }
    //
    //            private void parallelSort() {
    //                a = (T[]) elements.toArray(N.EMPTY_OBJECT_ARRAY);
    //
    //                if (comparator == null) {
    //                    N.parallelSort(a);
    //                } else {
    //                    N.parallelSort(a, comparator);
    //                }
    //            }
    //        }, closeHandlers, true, comparator);
    //    }

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
            public void skip(long n2) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n2);
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
    public List<T> toList() {
        final List<T> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<T> toList(Supplier<? extends List<T>> supplier) {
        final List<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<T> toSet(Supplier<? extends Set<T>> supplier) {
        final Set<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier) {
        final Multiset<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset() {
        final LongMultiset<T> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier) {
        final LongMultiset<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
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
    public <K, D, A, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        T element = null;

        while (elements.hasNext()) {
            element = elements.next();

            key = N.requireNonNull(classifier.apply(element), "element cannot be mapped to a null key");
            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, element);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        Collectors.replaceAll(intermediate, function);

        return result;
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
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        T element = null;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        T element = null;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

        return result;
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
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator) {
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
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator) {
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
    public Optional<T> kthLargest(int k, Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        comparator = comparator == null ? Stream.OBJECT_COMPARATOR : comparator;
        final Queue<T> queue = new PriorityQueue<T>(k, comparator);
        T e = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (queue.size() < k) {
                queue.add(e);
            } else {
                if (N.compare(e, queue.peek(), comparator) > 0) {
                    queue.remove();
                    queue.add(e);
                }
            }
        }

        return queue.size() < k ? (Optional<T>) Optional.empty() : Optional.of(queue.peek());
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
    public long count() {
        return elements.count();
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
    public Optional<Map<String, T>> distribution() {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final Object[] a = sorted().toArray();

        return Optional.of((Map<String, T>) N.distribution(a));
    }

    @Override
    public Optional<Map<String, T>> distribution(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        final Object[] a = sorted(comparator).toArray();

        return Optional.of((Map<String, T>) N.distribution(a));
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
    public Stream<T> removeAll(Collection<?> c) {
        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !set.contains(value);
            }
        });
    }

    @Override
    public Stream<T> removeAll(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !set.contains(mapper.apply(value));
            }
        });
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
    public Stream<T> skipNull() {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return value != null;
            }
        });
    }

    @Override
    public Stream<T> breakWhileNull() {
        return new IteratorStream<>(NullBreakIterator.of(elements), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> breakWhileError() {
        return new IteratorStream<>(ErrorBreakIterator.of(elements), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> breakWhileError(int maxRetries, long retryInterval) {
        return new IteratorStream<>(ErrorBreakIterator.of(elements, maxRetries, retryInterval), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> queued() {
        return queued(DEFAULT_QUEUE_SIZE);
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
     * 
     * @param stream
     * @param queueSize Default value is 8
     * @return
     */
    @Override
    public Stream<T> queued(int queueSize) {
        final Iterator<T> iter = iterator();

        if (iter instanceof QueuedIterator && ((QueuedIterator<? extends T>) iter).max() >= queueSize) {
            return this;
        } else {
            return new IteratorStream<>(Stream.parallelConcat(Arrays.asList(iter), queueSize, asyncExecutor), closeHandlers, sorted, cmp);
        }
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
    public ImmutableIterator<T> iterator() {
        return elements;
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
    public Stream<T> parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelIteratorStream<T>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final List<Runnable> newCloseHandlers = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorStream<T>(elements, newCloseHandlers, sorted, cmp);
    }
}
