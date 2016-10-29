package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.IntBinaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntToByteFunction;
import com.landawn.abacus.util.function.IntToCharFunction;
import com.landawn.abacus.util.function.IntToDoubleFunction;
import com.landawn.abacus.util.function.IntToFloatFunction;
import com.landawn.abacus.util.function.IntToLongFunction;
import com.landawn.abacus.util.function.IntToShortFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IteratorIntStream extends AbstractIntStream {
    private final ImmutableIntIterator elements;
    private final boolean sorted;

    IteratorIntStream(ImmutableIntIterator values) {
        this(values, null);
    }

    IteratorIntStream(ImmutableIntIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorIntStream(ImmutableIntIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
    }

    @Override
    public IntStream filter(IntPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream filter(final IntPredicate predicate, final long max) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private boolean hasNext = false;
            private int next = 0;
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
            public int next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream takeWhile(IntPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream takeWhile(final IntPredicate predicate, final long max) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private boolean hasNext = false;
            private int next = 0;
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
            public int next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public IntStream dropWhile(IntPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate, final long max) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private boolean hasNext = false;
            private int next = 0;
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
            public int next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public IntStream map(final IntUnaryOperator mapper) {
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
    public CharStream mapToChar(final IntToCharFunction mapper) {
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
    public ByteStream mapToByte(final IntToByteFunction mapper) {
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
    public ShortStream mapToShort(final IntToShortFunction mapper) {
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
    public LongStream mapToLong(final IntToLongFunction mapper) {
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
    public FloatStream mapToFloat(final IntToFloatFunction mapper) {
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
    public DoubleStream mapToDouble(final IntToDoubleFunction mapper) {
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
    public <U> Stream<U> mapToObj(final IntFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ImmutableIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
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
    public IntStream flatMap(final IntFunction<? extends IntStream> mapper) {
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
    public CharStream flatMapToChar(final IntFunction<? extends CharStream> mapper) {
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
    public ByteStream flatMapToByte(final IntFunction<? extends ByteStream> mapper) {
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
    public ShortStream flatMapToShort(final IntFunction<? extends ShortStream> mapper) {
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
    public LongStream flatMapToLong(final IntFunction<? extends LongStream> mapper) {
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
    public FloatStream flatMapToFloat(final IntFunction<? extends FloatStream> mapper) {
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
    public DoubleStream flatMapToDouble(final IntFunction<? extends DoubleStream> mapper) {
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
    public <T> Stream<T> flatMapToObj(final IntFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public Stream<IntStream> split(final int size) {
        return new IteratorStream<IntStream>(new ImmutableIterator<IntStream>() {

            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public IntStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final int[] a = new int[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayIntStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<IntStream> split(final IntPredicate predicate) {
        return new IteratorStream<IntStream>(new ImmutableIterator<IntStream>() {
            private int next;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public IntStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final IntList result = IntList.of(N.EMPTY_INT_ARRAY);

                if (hasNext == false) {
                    next = elements.next();
                    hasNext = true;
                }

                while (hasNext) {
                    if (predicate.test(next)) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        break;
                    }
                }

                return IntStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public IntStream distinct() {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private Iterator<Integer> distinctIter;

            @Override
            public boolean hasNext() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.hasNext();
            }

            @Override
            public int next() {
                if (distinctIter == null) {
                    removeDuplicated();
                }

                return distinctIter.next();
            }

            private void removeDuplicated() {
                final Set<Integer> set = new LinkedHashSet<>();

                while (elements.hasNext()) {
                    set.add(elements.next());
                }

                distinctIter = set.iterator();
            }

        }, closeHandlers, sorted);
    }

    @Override
    public IntStream top(int n) {
        return top(n, INT_COMPARATOR);
    }

    @Override
    public IntStream top(int n, Comparator<? super Integer> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToInt(new ToIntFunction<Integer>() {
            @Override
            public int applyAsInt(Integer value) {
                return value.intValue();
            }
        });
    }

    @Override
    public IntStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorIntStream(new ImmutableIntIterator() {
            int[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public int next() {
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
            public int[] toArray() {
                if (a == null) {
                    sort();
                }

                if (cursor == 0) {
                    return a;
                } else {
                    return N.copyOfRange(a, cursor, a.length);
                }
            }

            private void sort() {
                a = elements.toArray();

                N.sort(a);
            }
        }, closeHandlers, true);
    }

    //    @Override
    //    public IntStream parallelSorted() {
    //        if (sorted) {
    //            return this;
    //        }
    //
    //        return new IteratorIntStream(new ImmutableIntIterator() {
    //            int[] a = null;
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
    //            public int next() {
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
    //            public int[] toArray() {
    //                if (a == null) {
    //                    parallelSort();
    //                }
    //
    //                if (cursor == 0) {
    //                    return a;
    //                } else {
    //                    return N.copyOfRange(a, cursor, a.length);
    //                }
    //            }
    //
    //            private void parallelSort() {
    //                a = elements.toArray();
    //
    //                N.parallelSort(a);
    //            }
    //        }, closeHandlers, true);
    //    }

    @Override
    public IntStream peek(final IntConsumer action) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
                final int next = elements.next();

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
            //    public int[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new IteratorIntStream(new ImmutableIntIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public int next() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorIntStream(new ImmutableIntIterator() {
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
            public int next() {
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
            public int[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(IntConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    //    @Override
    //    public boolean forEach2(IntFunction<Boolean> action) {
    //        while (elements.hasNext()) {
    //            if (action.apply(elements.next()).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public int[] toArray() {
        return elements.toArray();
    }

    @Override
    public IntList toIntList() {
        return IntList.of(toArray());
    }

    @Override
    public List<Integer> toList() {
        final List<Integer> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Integer> toList(Supplier<? extends List<Integer>> supplier) {
        final List<Integer> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Integer> toSet() {
        final Set<Integer> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Integer> toSet(Supplier<? extends Set<Integer>> supplier) {
        final Set<Integer> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Integer> toMultiset() {
        final Multiset<Integer> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Integer> toMultiset(Supplier<? extends Multiset<Integer>> supplier) {
        final Multiset<Integer> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Integer> toLongMultiset() {
        final LongMultiset<Integer> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Integer> toLongMultiset(Supplier<? extends LongMultiset<Integer>> supplier) {
        final LongMultiset<Integer> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Integer>> toMap(IntFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Integer>>>() {
            @Override
            public Map<K, List<Integer>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Integer>>> M toMap(IntFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Integer, ?, List<Integer>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(IntFunction<? extends K> classifier, Collector<Integer, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final IntFunction<? extends K> classifier, final Collector<Integer, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Integer> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        int element = 0;

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
    public <K, U> Map<K, U> toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        int element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        int element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public int reduce(int identity, IntBinaryOperator op) {
        int result = identity;

        while (elements.hasNext()) {
            result = op.applyAsInt(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalInt reduce(IntBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        int result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsInt(result, elements.next());
        }

        return OptionalInt.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalInt min() {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        int candidate = elements.next();
        int next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalInt.of(candidate);
    }

    @Override
    public OptionalInt max() {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        int candidate = elements.next();
        int next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalInt.of(candidate);
    }

    @Override
    public OptionalInt kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        final OptionalNullable<Integer> optional = boxed().kthLargest(k, INT_COMPARATOR);

        return optional.isPresent() ? OptionalInt.of(optional.get()) : OptionalInt.empty();
    }

    @Override
    public Long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.next();
        }

        return result;
    }

    @Override
    public OptionalDouble average() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        long sum = 0;
        long count = 0;

        while (elements.hasNext()) {
            sum += elements.next();
            count++;
        }

        return OptionalDouble.of(((double) sum) / count);
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public IntSummaryStatistics summarize() {
        final IntSummaryStatistics result = new IntSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public boolean anyMatch(IntPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(IntPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(IntPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalInt findFirst() {
    //        return elements.hasNext() ? OptionalInt.empty() : OptionalInt.of(elements.next());
    //    }

    @Override
    public OptionalInt findFirst(IntPredicate predicate) {
        while (elements.hasNext()) {
            int e = elements.next();

            if (predicate.test(e)) {
                return OptionalInt.of(e);
            }
        }

        return OptionalInt.empty();
    }

    //    @Override
    //    public OptionalInt findLast() {
    //        if (elements.hasNext() == false) {
    //            return OptionalInt.empty();
    //        }
    //
    //        int e = 0;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return OptionalInt.of(e);
    //    }

    @Override
    public OptionalInt findLast(IntPredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        boolean hasResult = false;
        int e = 0;
        int result = 0;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalInt.of(result) : OptionalInt.empty();
    }

    //    @Override
    //    public OptionalInt findAny() {
    //        return count() == 0 ? OptionalInt.empty() : OptionalInt.of(elements.next());
    //    }

    @Override
    public OptionalInt findAny(IntPredicate predicate) {
        while (elements.hasNext()) {
            int e = elements.next();

            if (predicate.test(e)) {
                return OptionalInt.of(e);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public IntStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public IntStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    //    @Override
    //    public IntStream exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new IntPredicate() {
    //            @Override
    //            public boolean test(int value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public LongStream asLongStream() {
        return new IteratorLongStream(new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long next() {
                return elements.next();
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
    public FloatStream asFloatStream() {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                return elements.next();
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
    public DoubleStream asDoubleStream() {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                return elements.next();
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
    public Stream<Integer> boxed() {
        return new IteratorStream<Integer>(iterator(), closeHandlers, sorted, sorted ? INT_COMPARATOR : null);
    }

    @Override
    public ImmutableIterator<Integer> iterator() {
        return new ImmutableIterator<Integer>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Integer next() {
                return elements.next();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        };
    }

    @Override
    public ImmutableIntIterator intIterator() {
        return elements;
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public IntStream sequential() {
        return this;
    }

    @Override
    public IntStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelIteratorIntStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorIntStream(elements, newCloseHandlers, sorted);
    }
}
