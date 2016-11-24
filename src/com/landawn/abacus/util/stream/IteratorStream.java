package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortIterator;
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

    IteratorStream(final Iterator<? extends T> iterator) {
        this(iterator, null);
    }

    IteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers) {
        this(iterator, closeHandlers, false, null);
    }

    IteratorStream(final Iterator<? extends T> iterator, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator) {
        super(closeHandlers, sorted, comparator);

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
    }

    IteratorStream(Stream<T> stream, Set<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator) {
        this(stream.iterator(), mergeCloseHandlers(stream, closeHandlers), sorted, comparator);
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
    public Stream<T> takeWhile(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, toInt(max))], 0);
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
    public Stream<T> dropWhile(final Predicate<? super T> predicate, final long max) {
        //        while (elements.hasNext() && predicate.test(elements.next())) {
        //        }
        //
        //        final ObjectList<Object> list = ObjectList.of(new Object[N.min(9, toInt(max))], 0);
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
    <R> Stream<R> flatMap4(final Function<? super T, ? extends Iterator<? extends R>> mapper) {
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
                    cur = mapper.apply(elements.next());
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

    //    @Override
    //    public Stream<Stream<T>> split(final Predicate<? super T> predicate) {
    //        return new IteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
    //            private T next = (T) NONE;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return next != NONE || elements.hasNext();
    //            }
    //
    //            @Override
    //            public Stream<T> next() {
    //                if (hasNext() == false) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                final List<T> result = new ArrayList<>();
    //
    //                if (next == NONE) {
    //                    next = elements.next();
    //                }
    //
    //                while (next != NONE) {
    //                    if (predicate.test(next)) {
    //                        result.add(next);
    //                        next = elements.hasNext() ? elements.next() : (T) NONE;
    //                    } else {
    //                        break;
    //                    }
    //                }
    //
    //                return Stream.of(result);
    //            }
    //
    //        }, closeHandlers);
    //    }
    //
    //    @Override
    //    public Stream<List<T>> splitIntoList(final Predicate<? super T> predicate) {
    //        return new IteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
    //            private T next = (T) NONE;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return next != NONE || elements.hasNext();
    //            }
    //
    //            @Override
    //            public List<T> next() {
    //                if (hasNext() == false) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                final List<T> result = new ArrayList<>();
    //
    //                if (next == NONE) {
    //                    next = elements.next();
    //                }
    //
    //                while (next != NONE) {
    //                    if (predicate.test(next)) {
    //                        result.add(next);
    //                        next = elements.hasNext() ? elements.next() : (T) NONE;
    //                    } else {
    //                        break;
    //                    }
    //                }
    //
    //                return result;
    //            }
    //
    //        }, closeHandlers);
    //    }
    //
    //    @Override
    //    public Stream<Set<T>> splitIntoSet(final Predicate<? super T> predicate) {
    //        return new IteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
    //            private T next = (T) NONE;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return next != NONE || elements.hasNext();
    //            }
    //
    //            @Override
    //            public Set<T> next() {
    //                if (hasNext() == false) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                final Set<T> result = new HashSet<>();
    //
    //                if (next == NONE) {
    //                    next = elements.next();
    //                }
    //
    //                while (next != NONE) {
    //                    if (predicate.test(next)) {
    //                        result.add(next);
    //
    //                        next = elements.hasNext() ? elements.next() : (T) NONE;
    //                    } else {
    //                        break;
    //                    }
    //                }
    //
    //                return result;
    //            }
    //
    //        }, closeHandlers);
    //    }

    @Override
    public <U> Stream<Stream<T>> split(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
            private T next = (T) NONE;

            @Override
            public boolean hasNext() {
                return next != NONE || elements.hasNext();
            }

            @Override
            public Stream<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

                if (next == NONE) {
                    next = elements.next();
                }

                while (next != NONE) {
                    if (predicate.apply(next, boundary)) {
                        result.add(next);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return Stream.of(result);
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<List<T>> splitIntoList(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            private T next = (T) NONE;

            @Override
            public boolean hasNext() {
                return next != NONE || elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

                if (next == NONE) {
                    next = elements.next();
                }

                while (next != NONE) {
                    if (predicate.apply(next, boundary)) {
                        result.add(next);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<Set<T>> splitIntoSet(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
            private T next = (T) NONE;

            @Override
            public boolean hasNext() {
                return next != NONE || elements.hasNext();
            }

            @Override
            public Set<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Set<T> result = new HashSet<>();

                if (next == NONE) {
                    next = elements.next();
                }

                while (next != NONE) {
                    if (predicate.apply(next, boundary)) {
                        result.add(next);

                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<T> distinct() {
        final Set<Object> set = new HashSet<>();
        final List<T> list = new ArrayList<>();
        T e = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (set.add(getHashKey(e))) {
                list.add(e);
            }
        }

        return new ArrayStream<T>((T[]) list.toArray(), closeHandlers, sorted, cmp);

        //        return new IteratorStream<T>(new ImmutableIterator<T>() {
        //            T[] a = null;
        //            int cursor = 0;
        //            int toIndex;
        //
        //            @Override
        //            public boolean hasNext() {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                return cursor < toIndex;
        //            }
        //
        //            @Override
        //            public T next() {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                if (cursor >= toIndex) {
        //                    throw new NoSuchElementException();
        //                }
        //
        //                return a[cursor++];
        //            }
        //
        //            @Override
        //            public long count() {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                return toIndex - cursor;
        //            }
        //
        //            @Override
        //            public void skip(long n) {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
        //            }
        //
        //            @Override
        //            public <A> A[] toArray(A[] b) {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);
        //
        //                N.copy(a, cursor, b, 0, toIndex - cursor);
        //
        //                return b;
        //            }
        //
        //            private void getResult() {
        //                final Set<Object> set = new HashSet<>();
        //                final List<T> list = new ArrayList<>();
        //                T e = null;
        //
        //                while (elements.hasNext()) {
        //                    e = elements.next();
        //
        //                    if (set.add(getHashKey(e))) {
        //                        list.add(e);
        //                    }
        //                }
        //
        //                a = (T[]) list.toArray();
        //                toIndex = a.length;
        //            }
        //        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        final Set<Object> set = new HashSet<>();
        final List<T> list = new ArrayList<>();
        T e = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (set.add(getHashKey(keyMapper.apply(e)))) {
                list.add(e);
            }
        }

        return new ArrayStream<T>((T[]) list.toArray(), closeHandlers, sorted, cmp);

        //        return new IteratorStream<T>(new ImmutableIterator<T>() {
        //            T[] a = null;
        //            int cursor = 0;
        //            int toIndex;
        //
        //            @Override
        //            public boolean hasNext() {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                return cursor < toIndex;
        //            }
        //
        //            @Override
        //            public T next() {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                if (cursor >= toIndex) {
        //                    throw new NoSuchElementException();
        //                }
        //
        //                return a[cursor++];
        //            }
        //
        //            @Override
        //            public long count() {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                return toIndex - cursor;
        //            }
        //
        //            @Override
        //            public void skip(long n) {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
        //            }
        //
        //            @Override
        //            public <A> A[] toArray(A[] b) {
        //                if (a == null) {
        //                    getResult();
        //                }
        //
        //                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);
        //
        //                N.copy(a, cursor, b, 0, toIndex - cursor);
        //
        //                return b;
        //            }
        //
        //            private void getResult() {
        //                final Set<Object> set = new HashSet<>();
        //                final List<T> list = new ArrayList<>();
        //                T e = null;
        //
        //                while (elements.hasNext()) {
        //                    e = elements.next();
        //
        //                    if (set.add(getHashKey(keyMapper.apply(e)))) {
        //                        list.add(e);
        //                    }
        //                }
        //
        //                a = (T[]) list.toArray();
        //                toIndex = a.length;
        //            }
        //        }, closeHandlers, sorted, cmp);
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

                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
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
    public <U> U forEach(U identity, BiFunction<U, ? super T, U> accumulator, Predicate<? super U> till) {
        U result = identity;

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());

            if (till.test(result)) {
                break;
            }
        }

        return result;
    }

    //    @Override
    //    public boolean forEach2(Function<? super T, Boolean> action) {
    //        while (elements.hasNext()) {
    //            if (action.apply(elements.next()).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

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
    public OptionalNullable<T> reduce(BinaryOperator<T> accumulator) {
        if (elements.hasNext() == false) {
            OptionalNullable.empty();
        }

        T result = elements.next();

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return OptionalNullable.of(result);
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
    public OptionalNullable<T> min(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;
        T candidate = elements.next();
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            if (comparator.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalNullable.of(candidate);
    }

    @Override
    public OptionalNullable<T> max(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;
        T candidate = elements.next();
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            if (comparator.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalNullable.of(candidate);
    }

    @Override
    public OptionalNullable<T> kthLargest(int k, Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;
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

        return queue.size() < k ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(queue.peek());
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
    //    public OptionalNullable<T> findFirst() {
    //        return elements.hasNext() ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elements.next());
    //    }

    @Override
    public OptionalNullable<T> findFirst(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            T e = elements.next();

            if (predicate.test(e)) {
                return OptionalNullable.of(e);
            }
        }

        return (OptionalNullable<T>) OptionalNullable.empty();
    }

    //    @Override
    //    public OptionalNullable<T> findLast() {
    //        if (elements.hasNext() == false) {
    //            return (OptionalNullable<T>) OptionalNullable.empty();
    //        }
    //
    //        T e = null;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return OptionalNullable.of(e);
    //    }

    @Override
    public OptionalNullable<T> findLast(Predicate<? super T> predicate) {
        if (elements.hasNext() == false) {
            return (OptionalNullable<T>) OptionalNullable.empty();
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

        return hasResult ? OptionalNullable.of(result) : (OptionalNullable<T>) OptionalNullable.empty();
    }

    //    @Override
    //    public OptionalNullable<T> findAny() {
    //        return elements.hasNext() ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elements.next());
    //    }

    @Override
    public OptionalNullable<T> findAny(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            T e = elements.next();

            if (predicate.test(e)) {
                return OptionalNullable.of(e);
            }
        }

        return (OptionalNullable<T>) OptionalNullable.empty();
    }

    //    @Override
    //    public Stream<T> exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }
    //
    //    @Override
    //    public Stream<T> exclude(final Function<? super T, ?> mapper, final Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return !set.contains(mapper.apply(value));
    //            }
    //        });
    //    }

    //    @Override
    //    public Stream<T> skipNull() {
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return value != null;
    //            }
    //        });
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileNull() {
    //        return new IteratorStream<>(NullBreakIterator.of(elements), closeHandlers, sorted, cmp);
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileError() {
    //        return new IteratorStream<>(ErrorBreakIterator.of(elements), closeHandlers, sorted, cmp);
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileError(int maxRetries, long retryInterval) {
    //        return new IteratorStream<>(ErrorBreakIterator.of(elements, maxRetries, retryInterval), closeHandlers, sorted, cmp);
    //    }

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
    public Stream<T> cached(IntFunction<T[]> generator) {
        return new ArrayStream<T>(toArray(generator), closeHandlers, sorted, cmp);
    }

    @Override
    public ImmutableIterator<T> iterator() {
        return elements;
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
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorStream<T>(elements, newCloseHandlers, sorted, cmp);
    }
}
