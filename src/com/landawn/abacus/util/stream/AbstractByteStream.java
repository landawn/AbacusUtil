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

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteMatrix;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IndexedByte;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableByte;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteBiPredicate;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractByteStream extends ByteStream {

    AbstractByteStream(final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);
    }

    @Override
    public ByteStream flattMap(final ByteFunction<byte[]> mapper) {
        return flatMap(new ByteFunction<ByteStream>() {
            @Override
            public ByteStream apply(byte t) {
                return ByteStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public <T> Stream<T> flattMapToObj(final ByteFunction<? extends Collection<T>> mapper) {
        return flatMapToObj(new ByteFunction<Stream<T>>() {
            @Override
            public Stream<T> apply(byte t) {
                return Stream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream skip(final long n, final ByteConsumer action) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        final BytePredicate filter = isParallel() ? new BytePredicate() {
            final AtomicLong cnt = new AtomicLong(n);

            @Override
            public boolean test(byte value) {
                return cnt.getAndDecrement() > 0;
            }
        } : new BytePredicate() {
            final MutableLong cnt = MutableLong.of(n);

            @Override
            public boolean test(byte value) {
                return cnt.getAndDecrement() > 0;
            }
        };

        return dropWhile(filter, action);
    }

    @Override
    public ByteStream removeIf(final BytePredicate predicate) {
        N.checkArgNotNull(predicate);

        return filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public ByteStream removeIf(final BytePredicate predicate, final ByteConsumer action) {
        N.checkArgNotNull(predicate);
        N.checkArgNotNull(predicate);

        return filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public ByteStream dropWhile(final BytePredicate predicate, final ByteConsumer action) {
        N.checkArgNotNull(predicate);
        N.checkArgNotNull(action);

        return dropWhile(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public ByteStream step(final long step) {
        N.checkArgPositive(step, "step");

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final ByteIteratorEx iter = this.iteratorEx();

        final ByteIterator byteIterator = new ByteIteratorEx() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public byte nextByte() {
                final byte next = iter.nextByte();
                iter.skip(skip);
                return next;
            }
        };

        return newStream(byteIterator, sorted);
    }

    @Override
    public Stream<ByteStream> split(final int size) {
        return splitToList(size).map(new Function<ByteList, ByteStream>() {
            @Override
            public ByteStream apply(ByteList t) {
                return new ArrayByteStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<ByteStream> split(final BytePredicate predicate) {
        return splitToList(predicate).map(new Function<ByteList, ByteStream>() {
            @Override
            public ByteStream apply(ByteList t) {
                return new ArrayByteStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<ByteList> splitToList(final BytePredicate predicate) {
        final BiPredicate<Byte, Object> predicate2 = new BiPredicate<Byte, Object>() {

            @Override
            public boolean test(Byte t, Object u) {
                return predicate.test(t);
            }
        };

        return splitToList(null, predicate2, null);
    }

    @Override
    public <U> Stream<ByteStream> split(final U seed, final BiPredicate<? super Byte, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return splitToList(seed, predicate, seedUpdate).map(new Function<ByteList, ByteStream>() {
            @Override
            public ByteStream apply(ByteList t) {
                return new ArrayByteStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<ByteStream> sliding(final int windowSize, final int increment) {
        return slidingToList(windowSize, increment).map(new Function<ByteList, ByteStream>() {
            @Override
            public ByteStream apply(ByteList t) {
                return new ArrayByteStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public ByteStream collapse(final ByteBiPredicate collapsible, final ByteBiFunction<Byte> mergeFunction) {
        final ByteIteratorEx iter = iteratorEx();

        return newStream(new ByteIteratorEx() {
            private boolean hasNext = false;
            private byte next = 0;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public byte nextByte() {
                byte res = hasNext ? next : (next = iter.nextByte());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.nextByte()))) {
                        res = mergeFunction.apply(res, next);
                    } else {
                        break;
                    }
                }

                return res;
            }
        }, false);
    }

    @Override
    public ByteStream scan(final ByteBiFunction<Byte> accumulator) {
        final ByteIteratorEx iter = iteratorEx();

        return newStream(new ByteIteratorEx() {
            private byte res = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public byte nextByte() {
                if (isFirst) {
                    isFirst = false;
                    return (res = iter.nextByte());
                } else {
                    return (res = accumulator.apply(res, iter.nextByte()));
                }
            }
        }, false);
    }

    @Override
    public ByteStream scan(final byte seed, final ByteBiFunction<Byte> accumulator) {
        final ByteIteratorEx iter = iteratorEx();

        return newStream(new ByteIteratorEx() {
            private byte res = seed;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public byte nextByte() {
                return (res = accumulator.apply(res, iter.nextByte()));
            }
        }, false);
    }

    @Override
    public <K, V> Map<K, V> toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper) {
        final Supplier<Map<K, V>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, V, M extends Map<K, V>> M toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<V> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, V> Map<K, V> toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper, BinaryOperator<V> mergeFunction) {
        final Supplier<Map<K, V>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(ByteFunction<? extends K> classifier, Collector<Byte, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public ByteMatrix toMatrix() {
        return ByteMatrix.of(toArray());
    }

    @Override
    public ByteStream distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return set.add(value);
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public OptionalByte first() {
        final ByteIterator iter = this.iteratorEx();

        return iter.hasNext() ? OptionalByte.of(iter.nextByte()) : OptionalByte.empty();
    }

    @Override
    public OptionalByte last() {
        final ByteIterator iter = this.iteratorEx();

        if (iter.hasNext() == false) {
            return OptionalByte.empty();
        }

        byte next = iter.nextByte();

        while (iter.hasNext()) {
            next = iter.nextByte();
        }

        return OptionalByte.of(next);
    }

    @Override
    public <E extends Exception> OptionalByte findAny(final Try.BytePredicate<E> predicate) throws E {
        return findFirst(predicate);
    }

    @Override
    public <E extends Exception, E2 extends Exception> OptionalByte findFirstOrLast(Try.BytePredicate<E> predicateForFirst,
            Try.BytePredicate<E> predicateForLast) throws E, E2 {
        final ByteIteratorEx iter = iteratorEx();
        MutableByte last = null;
        byte next = 0;

        while (iter.hasNext()) {
            next = iter.nextByte();

            if (predicateForFirst.test(next)) {
                return OptionalByte.of(next);
            } else if (predicateForLast.test(next)) {
                if (last == null) {
                    last = MutableByte.of(next);
                } else {
                    last.setValue(next);
                }
            }
        }

        return last == null ? OptionalByte.empty() : OptionalByte.of(last.value());
    }

    @Override
    public ByteStream intersection(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public ByteStream difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public ByteStream symmetricDifference(final Collection<Byte> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToByte(ToByteFunction.UNBOX)).iteratorEx(), false);
    }

    @Override
    public Stream<ByteStream> splitAt(final int n) {
        N.checkArgNotNegative(n, "n");

        return newStream(new ObjIteratorEx<ByteStream>() {
            private ByteStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public ByteStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final ByteIterator iter = AbstractByteStream.this.iteratorEx();
                    final ByteList list = new ByteList();

                    while (list.size() < n && iter.hasNext()) {
                        list.add(iter.nextByte());
                    }

                    a = new ByteStream[] { new ArrayByteStream(list.array(), 0, list.size(), sorted, null), new IteratorByteStream(iter, sorted, null) };
                }
            }

        }, false, null);
    }

    @Override
    public Stream<ByteStream> splitBy(final BytePredicate where) {
        N.checkArgNotNull(where);

        return newStream(new ObjIteratorEx<ByteStream>() {
            private ByteStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public ByteStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final ByteIterator iter = AbstractByteStream.this.iteratorEx();
                    final ByteList list = new ByteList();
                    byte next = 0;
                    ByteStream s = null;

                    while (iter.hasNext()) {
                        next = iter.nextByte();

                        if (where.test(next)) {
                            list.add(next);
                        } else {
                            s = ByteStream.of(next);

                            break;
                        }
                    }

                    a = new ByteStream[] { new ArrayByteStream(list.array(), 0, list.size(), sorted, null), new IteratorByteStream(iter, sorted, null) };

                    if (s != null) {
                        if (sorted) {
                            a[1] = new IteratorByteStream(a[1].prepend(s).iteratorEx(), sorted, null);
                        } else {
                            a[1] = a[1].prepend(s);
                        }
                    }
                }
            }

        }, false, null);
    }

    @Override
    public ByteStream reversed() {
        return newStream(new ByteIteratorEx() {
            private boolean initialized = false;
            private byte[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public byte nextByte() {
                if (initialized == false) {
                    init();
                }

                if (cursor <= 0) {
                    throw new NoSuchElementException();
                }

                return aar[--cursor];
            }

            @Override
            public long count() {
                if (initialized == false) {
                    init();
                }

                return cursor;
            }

            @Override
            public void skip(long n) {
                if (initialized == false) {
                    init();
                }

                cursor = n < cursor ? cursor - (int) n : 0;
            }

            @Override
            public byte[] toArray() {
                if (initialized == false) {
                    init();
                }

                final byte[] a = new byte[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractByteStream.this.toArray();
                    cursor = aar.length;
                }
            }
        }, false);
    }

    @Override
    public ByteStream shuffled(final Random rnd) {
        return lazyLoad(new Function<byte[], byte[]>() {
            @Override
            public byte[] apply(final byte[] a) {
                N.shuffle(a, rnd);
                return a;
            }
        }, false);
    }

    @Override
    public ByteStream rotated(final int distance) {
        return newStream(new ByteIteratorEx() {
            private boolean initialized = false;
            private byte[] aar;
            private int len;
            private int start;
            private int cnt = 0;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cnt < len;
            }

            @Override
            public byte nextByte() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return aar[(start + cnt++) % len];
            }

            @Override
            public long count() {
                if (initialized == false) {
                    init();
                }

                return len - cnt;
            }

            @Override
            public void skip(long n) {
                if (initialized == false) {
                    init();
                }

                cnt = n < len - cnt ? cnt + (int) n : len;
            }

            @Override
            public byte[] toArray() {
                if (initialized == false) {
                    init();
                }

                final byte[] a = new byte[len - cnt];

                for (int i = cnt; i < len; i++) {
                    a[i - cnt] = aar[(start + i) % len];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractByteStream.this.toArray();
                    len = aar.length;

                    if (len > 0) {
                        start = distance % len;

                        if (start < 0) {
                            start += len;
                        }

                        start = len - start;
                    }
                }
            }
        }, distance == 0 && sorted);
    }

    @Override
    public ByteStream sorted() {
        if (sorted) {
            return this;
        }

        return lazyLoad(new Function<byte[], byte[]>() {
            @Override
            public byte[] apply(final byte[] a) {
                if (isParallel()) {
                    N.parallelSort(a);
                } else {
                    N.sort(a);
                }

                return a;
            }
        }, true);
    }

    @Override
    public ByteStream reverseSorted() {
        return newStream(new ByteIteratorEx() {
            private boolean initialized = false;
            private byte[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public byte nextByte() {
                if (initialized == false) {
                    init();
                }

                if (cursor <= 0) {
                    throw new NoSuchElementException();
                }

                return aar[--cursor];
            }

            @Override
            public long count() {
                if (initialized == false) {
                    init();
                }

                return cursor;
            }

            @Override
            public void skip(long n) {
                if (initialized == false) {
                    init();
                }

                cursor = n < cursor ? cursor - (int) n : 0;
            }

            @Override
            public byte[] toArray() {
                if (initialized == false) {
                    init();
                }

                final byte[] a = new byte[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractByteStream.this.toArray();

                    if (isParallel()) {
                        N.parallelSort(aar);
                    } else {
                        N.sort(aar);
                    }

                    cursor = aar.length;
                }
            }
        }, false);
    }

    private ByteStream lazyLoad(final Function<byte[], byte[]> op, final boolean sorted) {
        return newStream(new ByteIteratorEx() {
            private boolean initialized = false;
            private byte[] aar;
            private int cursor = 0;
            private int len;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor < len;
            }

            @Override
            public byte nextByte() {
                if (initialized == false) {
                    init();
                }

                if (cursor >= len) {
                    throw new NoSuchElementException();
                }

                return aar[cursor++];
            }

            @Override
            public long count() {
                if (initialized == false) {
                    init();
                }

                return len - cursor;
            }

            @Override
            public void skip(long n) {
                if (initialized == false) {
                    init();
                }

                cursor = n > len - cursor ? len : cursor + (int) n;
            }

            @Override
            public byte[] toArray() {
                if (initialized == false) {
                    init();
                }

                final byte[] a = new byte[len - cursor];

                for (int i = cursor; i < len; i++) {
                    a[i - cursor] = aar[i];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = op.apply(AbstractByteStream.this.toArray());
                    len = aar.length;
                }
            }
        }, sorted);
    }

    @Override
    public Optional<Map<Percentage, Byte>> percentiles() {
        final byte[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.percentiles(a));
    }

    @Override
    public Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarizze() {
        final byte[] a = sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new ByteSummaryStatistics(), Optional.<Map<Percentage, Byte>> empty());
        } else {
            return Pair.of(new ByteSummaryStatistics(a.length, sum(a), a[0], a[a.length - 1]), Optional.of(N.percentiles(a)));
        }
    }

    @Override
    public String join(final CharSequence delimiter) {
        return join(delimiter, "", "");
    }

    @Override
    public String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Joiner joiner = Joiner.with(delimiter, prefix, suffix).reuseStringBuilder(true);
        final ByteIteratorEx iter = this.iteratorEx();

        while (iter.hasNext()) {
            joiner.append(iter.nextByte());
        }

        return joiner.toString();
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Pair<OptionalByte, ByteStream> headAndTail() {
        return Pair.of(head(), tail());
    }

    @Override
    public Pair<ByteStream, OptionalByte> headAndTaill() {
        return Pair.of(headd(), taill());
    }

    @Override
    public Stream<IndexedByte> indexed() {
        final MutableLong idx = MutableLong.of(0);

        return newStream(this.sequential().mapToObj(new ByteFunction<IndexedByte>() {
            @Override
            public IndexedByte apply(byte t) {
                return IndexedByte.of(t, idx.getAndIncrement());
            }
        }).iterator(), true, INDEXED_BYTE_COMPARATOR);
    }

    @Override
    public ByteStream append(ByteStream stream) {
        return ByteStream.concat(this, stream);
    }

    @Override
    public ByteStream prepend(ByteStream stream) {
        return ByteStream.concat(stream, this);
    }

    @Override
    public ByteStream merge(ByteStream b, ByteBiFunction<Nth> nextSelector) {
        return ByteStream.merge(this, b, nextSelector);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteBiFunction<Byte> zipFunction) {
        return ByteStream.zip(this, b, zipFunction);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteStream c, ByteTriFunction<Byte> zipFunction) {
        return ByteStream.zip(this, b, c, zipFunction);
    }

    @Override
    public ByteStream zipWith(ByteStream b, byte valueForNoneA, byte valueForNoneB, ByteBiFunction<Byte> zipFunction) {
        return ByteStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteStream c, byte valueForNoneA, byte valueForNoneB, byte valueForNoneC, ByteTriFunction<Byte> zipFunction) {
        return ByteStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public ByteStream cached() {
        return newStream(toArray(), sorted);
    }
}
