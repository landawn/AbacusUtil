package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortBinaryOperator;
import com.landawn.abacus.util.function.ShortConsumer;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortToIntFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ArrayShortStream extends AbstractShortStream {
    private final short[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;

    ArrayShortStream(short[] values) {
        this(values, null);
    }

    ArrayShortStream(short[] values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    ArrayShortStream(short[] values, Collection<Runnable> closeHandlers, boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayShortStream(short[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayShortStream(short[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayShortStream(short[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted) {
        super(closeHandlers);

        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate, final long max) {
        // return new ArrayShortStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex && cnt < max) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        } else {
                            cursor++;
                        }
                    } while (cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public short next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate, final long max) {
        //        final ShortList list = ShortList.of(new short[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);
        //
        //        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
        //            if (predicate.test(elements[i])) {
        //                list.add(elements[i]);
        //                cnt++;
        //            } else {
        //                break;
        //            }
        //        }
        //
        //        return new ArrayShortStream(list.trimToSize().array(), closeHandlers, sorted);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex && cnt < max) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        } else {
                            cursor = Integer.MAX_VALUE;
                        }
                    } while (cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public short next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate, final long max) {
        //        int cursor = fromIndex;
        //        while (cursor < toIndex && predicate.test(elements[cursor])) {
        //            cursor++;
        //        }
        //
        //        final ShortList list = ShortList.of(new short[N.min(9, Stream.toInt(max), (toIndex - cursor))], 0);
        //        int cnt = 0;
        //
        //        while (cursor < toIndex && cnt < max) {
        //            list.add(elements[cursor]);
        //            cursor++;
        //            cnt++;
        //        }
        //
        //        return new ArrayShortStream(list.trimToSize().array(), closeHandlers, sorted);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private long cnt = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex && cnt < max) {
                    if (dropped == false) {
                        do {
                            if (predicate.test(elements[cursor]) == false) {
                                hasNext = true;
                                break;
                            } else {
                                cursor++;
                            }
                        } while (cursor < toIndex);

                        dropped = true;
                    } else {
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public short next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream map(final ShortUnaryOperator mapper) {
        //        final short[] a = new short[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsShort(elements[i]);
        //        }
        //
        //        return new ArrayShortStream(a, closeHandlers);

        return new IteratorShortStream(new ImmutableShortIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsShort(elements[cursor++]);
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
            public short[] toArray() {
                final short[] a = new short[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsShort(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final ShortToIntFunction mapper) {
        //        final int[] a = new int[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsInt(elements[i]);
        //        }
        //
        //        return new ArrayIntStream(a, closeHandlers);

        return new IteratorIntStream(new ImmutableIntIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsInt(elements[cursor++]);
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

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsInt(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final ShortFunction<? extends U> mapper) {
        //        final Object[] a = new Object[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.apply(elements[i]);
        //        }
        //
        //        return new ArrayStream<U>((U[]) a, closeHandlers);

        return new IteratorStream<U>(new ImmutableIterator<U>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public U next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.apply(elements[cursor++]);
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
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) mapper.apply(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMap(final ShortFunction<? extends ShortStream> mapper) {
        //        final List<short[]> listOfArray = new ArrayList<short[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final short[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final short[] arrayOfAll = new short[lengthOfAll];
        //        int from = 0;
        //        for (short[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayShortStream(arrayOfAll, closeHandlers);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private int cursor = fromIndex;
            private ImmutableShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).shortIterator();
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
    public IntStream flatMapToInt(final ShortFunction<? extends IntStream> mapper) {
        //        final List<int[]> listOfArray = new ArrayList<int[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final int[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final int[] arrayOfAll = new int[lengthOfAll];
        //        int from = 0;
        //        for (int[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayIntStream(arrayOfAll, closeHandlers);

        return new IteratorIntStream(new ImmutableIntIterator() {
            private int cursor = fromIndex;
            private ImmutableIntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).intIterator();
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
    public <T> Stream<T> flatMapToObj(final ShortFunction<? extends Stream<T>> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //        int lengthOfAll = 0;
        //
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final Object[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final Object[] arrayOfAll = new Object[lengthOfAll];
        //        int from = 0;
        //
        //        for (Object[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayStream<T>((T[]) arrayOfAll, closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private int cursor = fromIndex;
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).iterator();
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
    public Stream<ShortStream> split(final int size) {
        //        final List<short[]> tmp = N.split(elements, fromIndex, toIndex, size);
        //        final ShortStream[] a = new ShortStream[tmp.size()];
        //
        //        for (int i = 0, len = a.length; i < len; i++) {
        //            a[i] = new ArrayShortStream(tmp.get(i), null, sorted);
        //        }
        //
        //        return new ArrayStream<ShortStream>(a, closeHandlers);

        return new IteratorStream<ShortStream>(new ImmutableIterator<ShortStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayShortStream(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex), null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<ShortStream> split(final ShortPredicate predicate) {
        return new IteratorStream<ShortStream>(new ImmutableIterator<ShortStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ShortList result = ShortList.of(N.EMPTY_SHORT_ARRAY);

                while (cursor < toIndex) {
                    if (predicate.test(elements[cursor])) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        break;
                    }
                }

                return ShortStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public ShortStream distinct() {
        return new ArrayShortStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public ShortStream top(int n) {
        return top(n, Stream.SHORT_COMPARATOR);
    }

    @Override
    public ShortStream top(int n, Comparator<? super Short> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && Stream.isSameComparator(comparator, Stream.SHORT_COMPARATOR)) {
            return new ArrayShortStream(elements, toIndex - n, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayShortStream(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers, sorted);
        }
    }

    @Override
    public ShortStream sorted() {
        if (sorted) {
            return this;
        }

        final short[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayShortStream(a, closeHandlers, true);
    }

    @Override
    public ShortStream peek(ShortConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new ShortStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public ShortStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayShortStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
    }

    @Override
    public ShortStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayShortStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayShortStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(ShortConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    //    @Override
    //    public boolean forEach2(ShortFunction<Boolean> action) {
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            if (action.apply(elements[i]).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public short[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public ShortList toShortList() {
        return ShortList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Short> toList() {
        final List<Short> result = new ArrayList<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<Short> toList(Supplier<? extends List<Short>> supplier) {
        final List<Short> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Short> toSet() {
        final Set<Short> result = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Short> toSet(Supplier<? extends Set<Short>> supplier) {
        final Set<Short> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset() {
        final Multiset<Short> result = new Multiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset(Supplier<? extends Multiset<Short>> supplier) {
        final Multiset<Short> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset() {
        final LongMultiset<Short> result = new LongMultiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset(Supplier<? extends LongMultiset<Short>> supplier) {
        final LongMultiset<Short> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Short>> toMap(ShortFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Short>>>() {
            @Override
            public Map<K, List<Short>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Short>>> M toMap(ShortFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Short, ?, List<Short>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(ShortFunction<? extends K> classifier, Collector<Short, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final ShortFunction<? extends K> classifier, final Collector<Short, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Short> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = N.requireNonNull(classifier.apply(elements[i]), "element cannot be mapped to a null key");
            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, elements[i]);
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
    public <K, U> Map<K, U> toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyMapper.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyMapper.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
    }

    @Override
    public short reduce(short identity, ShortBinaryOperator op) {
        short result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsShort(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalShort reduce(ShortBinaryOperator op) {
        if (count() == 0) {
            return OptionalShort.empty();
        }

        short result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsShort(result, elements[i]);
        }

        return OptionalShort.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalShort min() {
        if (count() == 0) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements[fromIndex]);
        }

        return OptionalShort.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalShort max() {
        if (count() == 0) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements[toIndex - 1]);
        }

        return OptionalShort.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalShort kthLargest(int k) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements[toIndex - k]);
        }

        return OptionalShort.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public Long sum() {
        return N.sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public ShortSummaryStatistics summarize() {
        final ShortSummaryStatistics result = new ShortSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalShort findFirst() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalShort findFirst(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalShort.of(elements[i]);
            }
        }

        return OptionalShort.empty();
    }

    //    @Override
    //    public OptionalShort findLast() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalShort findLast(final ShortPredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalShort.of(elements[i]);
            }
        }

        return OptionalShort.empty();
    }

    //    @Override
    //    public OptionalShort findAny() {
    //        return count() == 0 ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalShort findAny(final ShortPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalShort.of(elements[i]);
            }
        }

        return OptionalShort.empty();
    }

    @Override
    public ShortStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new ShortPredicate() {
            @Override
            public boolean test(short value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public ShortStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new ShortPredicate() {
            @Override
            public boolean test(short value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    //    @Override
    //    public ShortStream exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new ShortPredicate() {
    //            @Override
    //            public boolean test(short value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public IntStream asIntStream() {
        //        final int[] a = new int[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = elements[i];
        //        }
        //
        //        return new ArrayIntStream(a, closeHandlers, sorted);

        return new IteratorIntStream(new ImmutableIntIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Short> boxed() {
        return new IteratorStream<Short>(iterator(), closeHandlers, sorted, sorted ? Stream.SHORT_COMPARATOR : null);
    }

    @Override
    public ShortStream append(ShortStream stream) {
        return ShortStream.concat(this, stream);
    }

    @Override
    public ShortStream merge(ShortStream b, ShortBiFunction<Nth> nextSelector) {
        return ShortStream.merge(this, b, nextSelector);
    }

    @Override
    public ImmutableIterator<Short> iterator() {
        return new ImmutableIterator<Short>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Short next() {
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
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) Short.valueOf(elements[cursor++]);
                }

                return a;
            }
        };
    }

    @Override
    public ImmutableShortIterator shortIterator() {
        return new ImmutableShortIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short next() {
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
            public short[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public ShortStream sequential() {
        return this;
    }

    @Override
    public ShortStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelArrayShortStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ShortStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayShortStream(elements, fromIndex, toIndex, newCloseHandlers, sorted);
    }
}
