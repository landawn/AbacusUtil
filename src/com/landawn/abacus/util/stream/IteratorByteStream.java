package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteToIntFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class IteratorByteStream extends AbstractByteStream {
    private final ImmutableByteIterator elements;
    private final boolean sorted;

    IteratorByteStream(ImmutableByteIterator values) {
        this(values, null);
    }

    IteratorByteStream(ImmutableByteIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorByteStream(ImmutableByteIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        super(closeHandlers);

        this.elements = values;
        this.sorted = sorted;
    }

    @Override
    public ByteStream filter(BytePredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream filter(final BytePredicate predicate, final long max) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            private boolean hasNext = false;
            private byte next = 0;
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
            public byte next() {
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
    public ByteStream takeWhile(BytePredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream takeWhile(final BytePredicate predicate, final long max) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            private boolean hasNext = false;
            private byte next = 0;
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
            public byte next() {
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
    public ByteStream dropWhile(BytePredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream dropWhile(final BytePredicate predicate, final long max) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            private boolean hasNext = false;
            private byte next = 0;
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
            public byte next() {
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
    public ByteStream map(final ByteUnaryOperator mapper) {
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
    public IntStream mapToInt(final ByteToIntFunction mapper) {
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
    public <U> Stream<U> mapToObj(final ByteFunction<? extends U> mapper) {
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
    public ByteStream flatMap(final ByteFunction<? extends ByteStream> mapper) {
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
    public IntStream flatMapToInt(final ByteFunction<? extends IntStream> mapper) {
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
    public <T> Stream<T> flatMapToObj(final ByteFunction<? extends Stream<T>> mapper) {
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
    public Stream<ByteStream> split(final int size) {
        return new IteratorStream<ByteStream>(new ImmutableIterator<ByteStream>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public ByteStream next() {
                if (elements.hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final byte[] a = new byte[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayByteStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<ByteStream> split(final U boundary, final BiFunction<? super Byte, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<ByteStream>(new ImmutableIterator<ByteStream>() {
            private byte next;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public ByteStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ByteList result = ByteList.of(N.EMPTY_BYTE_ARRAY);

                if (hasNext == false) {
                    next = elements.next();
                    hasNext = true;
                }

                while (hasNext) {
                    if (predicate.apply(next, boundary)) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return ByteStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public ByteStream distinct() {
        final Set<Byte> set = new LinkedHashSet<>();

        while (elements.hasNext()) {
            set.add(elements.next());
        }

        final byte[] a = new byte[set.size()];
        final Iterator<Byte> iter = set.iterator();

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = iter.next();
        }

        return new ArrayByteStream(a, closeHandlers, sorted);

        //        return new IteratorByteStream(new ImmutableByteIterator() {
        //            private Iterator<Byte> distinctIter;
        //
        //            @Override
        //            public boolean hasNext() {
        //                if (distinctIter == null) {
        //                    removeDuplicated();
        //                }
        //
        //                return distinctIter.hasNext();
        //            }
        //
        //            @Override
        //            public byte next() {
        //                if (distinctIter == null) {
        //                    removeDuplicated();
        //                }
        //
        //                return distinctIter.next();
        //            }
        //
        //            private void removeDuplicated() {
        //                final Set<Byte> set = new LinkedHashSet<>();
        //
        //                while (elements.hasNext()) {
        //                    set.add(elements.next());
        //                }
        //
        //                distinctIter = set.iterator();
        //            }
        //
        //        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorByteStream(new ImmutableByteIterator() {
            byte[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public byte next() {
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
            public byte[] toArray() {
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

                Arrays.sort(a);
            }
        }, closeHandlers, true);
    }

    @Override
    public ByteStream peek(final ByteConsumer action) {
        return new IteratorByteStream(new ImmutableByteIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte next() {
                final byte next = elements.next();

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
            //    public byte[] toArray() {
            //        return elements.toArray();
            //    }
        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new IteratorByteStream(new ImmutableByteIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public byte next() {
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
    public ByteStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorByteStream(new ImmutableByteIterator() {
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
            public byte next() {
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
            public byte[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(ByteConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    //    @Override
    //    public boolean forEach2(ByteFunction<Boolean> action) {
    //        while (elements.hasNext()) {
    //            if (action.apply(elements.next()).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public byte[] toArray() {
        return elements.toArray();
    }

    @Override
    public ByteList toByteList() {
        return ByteList.of(toArray());
    }

    @Override
    public List<Byte> toList() {
        final List<Byte> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Byte> toList(Supplier<? extends List<Byte>> supplier) {
        final List<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Byte> toSet() {
        final Set<Byte> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Byte> toSet(Supplier<? extends Set<Byte>> supplier) {
        final Set<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset() {
        final Multiset<Byte> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset(Supplier<? extends Multiset<Byte>> supplier) {
        final Multiset<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset() {
        final LongMultiset<Byte> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset(Supplier<? extends LongMultiset<Byte>> supplier) {
        final LongMultiset<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Byte>> toMap(ByteFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Byte>>>() {
            @Override
            public Map<K, List<Byte>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Byte>>> M toMap(ByteFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Byte, ?, List<Byte>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(ByteFunction<? extends K> classifier, Collector<Byte, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Byte> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        byte element = 0;

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
    public <K, U> Map<K, U> toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        byte element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        byte element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public byte reduce(byte identity, ByteBinaryOperator op) {
        byte result = identity;

        while (elements.hasNext()) {
            result = op.applyAsByte(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalByte reduce(ByteBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        byte result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsByte(result, elements.next());
        }

        return OptionalByte.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalByte min() {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        byte candidate = elements.next();
        byte next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalByte.of(candidate);
    }

    @Override
    public OptionalByte max() {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        byte candidate = elements.next();
        byte next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalByte.of(candidate);
    }

    @Override
    public OptionalByte kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        final OptionalNullable<Byte> optional = boxed().kthLargest(k, BYTE_COMPARATOR);

        return optional.isPresent() ? OptionalByte.of(optional.get()) : OptionalByte.empty();
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
    public ByteSummaryStatistics summarize() {
        final ByteSummaryStatistics result = new ByteSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public boolean anyMatch(BytePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(BytePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(BytePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalByte findFirst() {
    //        return elements.hasNext() ? OptionalByte.empty() : OptionalByte.of(elements.next());
    //    }

    @Override
    public OptionalByte findFirst(BytePredicate predicate) {
        while (elements.hasNext()) {
            byte e = elements.next();

            if (predicate.test(e)) {
                return OptionalByte.of(e);
            }
        }

        return OptionalByte.empty();
    }

    //    @Override
    //    public OptionalByte findLast() {
    //        if (elements.hasNext() == false) {
    //            return OptionalByte.empty();
    //        }
    //
    //        byte e = 0;
    //
    //        while (elements.hasNext()) {
    //            e = elements.next();
    //        }
    //
    //        return OptionalByte.of(e);
    //    }

    @Override
    public OptionalByte findLast(BytePredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        boolean hasResult = false;
        byte e = 0;
        byte result = 0;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalByte.of(result) : OptionalByte.empty();
    }

    //    @Override
    //    public OptionalByte findAny() {
    //        return count() == 0 ? OptionalByte.empty() : OptionalByte.of(elements.next());
    //    }

    @Override
    public OptionalByte findAny(BytePredicate predicate) {
        while (elements.hasNext()) {
            byte e = elements.next();

            if (predicate.test(e)) {
                return OptionalByte.of(e);
            }
        }

        return OptionalByte.empty();
    }

    @Override
    public ByteStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public ByteStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    @Override
    public ByteStream xor(Collection<Byte> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToByte(new ToByteFunction<Byte>() {
            @Override
            public byte applyAsByte(Byte value) {
                return value.byteValue();
            }
        }));
    }

    //    @Override
    //    public ByteStream exclude(Collection<?> c) {
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
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
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
    public Stream<Byte> boxed() {
        return new IteratorStream<Byte>(iterator(), closeHandlers, sorted, sorted ? BYTE_COMPARATOR : null);
    }

    @Override
    public ImmutableIterator<Byte> iterator() {
        return new ImmutableIterator<Byte>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Byte next() {
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
    public ImmutableByteIterator byteIterator() {
        return elements;
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public ByteStream sequential() {
        return this;
    }

    @Override
    public ByteStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public ByteStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorByteStream(elements, newCloseHandlers, sorted);
    }
}
