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

import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IndexedLong;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMatrix;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongBiPredicate;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractLongStream extends LongStream {

    AbstractLongStream(final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);
    }

    @Override
    public LongStream flattMap(final LongFunction<long[]> mapper) {
        return flatMap(new LongFunction<LongStream>() {
            @Override
            public LongStream apply(long t) {
                return LongStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public <T> Stream<T> flattMapToObj(final LongFunction<? extends Collection<T>> mapper) {
        return flatMapToObj(new LongFunction<Stream<T>>() {
            @Override
            public Stream<T> apply(long t) {
                return Stream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public LongStream skip(final long n, final LongConsumer action) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        final LongPredicate filter = isParallel() ? new LongPredicate() {
            final AtomicLong cnt = new AtomicLong(n);

            @Override
            public boolean test(long value) {
                return cnt.getAndDecrement() > 0;
            }
        } : new LongPredicate() {
            final MutableLong cnt = MutableLong.of(n);

            @Override
            public boolean test(long value) {
                return cnt.getAndDecrement() > 0;
            }
        };

        return dropWhile(filter, action);
    }

    @Override
    public LongStream removeIf(final LongPredicate predicate) {
        N.requireNonNull(predicate);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public LongStream removeIf(final LongPredicate predicate, final LongConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(predicate);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate, final LongConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new LongPredicate() {
            @Override
            public boolean test(long value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public LongStream step(final long step) {
        N.checkArgPositive(step, "step");

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final LongIteratorEx iter = this.iteratorEx();

        final LongIterator longIterator = new LongIteratorEx() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public long nextLong() {
                final long next = iter.nextLong();
                iter.skip(skip);
                return next;
            }
        };

        return newStream(longIterator, sorted);
    }

    @Override
    public Stream<LongStream> split(final int size) {
        return splitToList(size).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<LongStream> split(final LongPredicate predicate) {
        return splitToList(predicate).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<LongList> splitToList(final LongPredicate predicate) {
        final BiPredicate<Long, Object> predicate2 = new BiPredicate<Long, Object>() {

            @Override
            public boolean test(Long t, Object u) {
                return predicate.test(t);
            }
        };

        return splitToList(null, predicate2, null);
    }

    @Override
    public <U> Stream<LongStream> split(final U seed, final BiPredicate<? super Long, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return splitToList(seed, predicate, seedUpdate).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<LongStream> sliding(final int windowSize, final int increment) {
        return slidingToList(windowSize, increment).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public LongStream collapse(final LongBiPredicate collapsible, final LongBiFunction<Long> mergeFunction) {
        final LongIteratorEx iter = iteratorEx();

        return newStream(new LongIteratorEx() {
            private boolean hasNext = false;
            private long next = 0;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public long nextLong() {
                long res = hasNext ? next : (next = iter.nextLong());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.nextLong()))) {
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
    public LongStream scan(final LongBiFunction<Long> accumulator) {
        final LongIteratorEx iter = iteratorEx();

        return newStream(new LongIteratorEx() {
            private long res = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public long nextLong() {
                if (isFirst) {
                    isFirst = false;
                    return (res = iter.nextLong());
                } else {
                    return (res = accumulator.apply(res, iter.nextLong()));
                }
            }
        }, false);
    }

    @Override
    public LongStream scan(final long seed, final LongBiFunction<Long> accumulator) {
        final LongIteratorEx iter = iteratorEx();

        return newStream(new LongIteratorEx() {
            private long res = seed;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public long nextLong() {
                return (res = accumulator.apply(res, iter.nextLong()));
            }
        }, false);
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyExtractor, LongFunction<? extends U> valueMapper) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyExtractor, LongFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyExtractor, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(LongFunction<? extends K> classifier, Collector<Long, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public LongMatrix toMatrix() {
        return LongMatrix.of(toArray());
    }

    @Override
    public LongStream distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return set.add(value);
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public OptionalLong first() {
        final LongIterator iter = this.iteratorEx();

        return iter.hasNext() ? OptionalLong.of(iter.nextLong()) : OptionalLong.empty();
    }

    @Override
    public OptionalLong last() {
        final LongIterator iter = this.iteratorEx();

        if (iter.hasNext() == false) {
            return OptionalLong.empty();
        }

        long next = iter.nextLong();

        while (iter.hasNext()) {
            next = iter.nextLong();
        }

        return OptionalLong.of(next);
    }

    @Override
    public <E extends Exception> OptionalLong findAny(final Try.LongPredicate<E> predicate) throws E {
        return findFirst(predicate);
    }

    @Override
    public <E extends Exception, E2 extends Exception> OptionalLong findFirstOrLast(Try.LongPredicate<E> predicateForFirst,
            Try.LongPredicate<E> predicateForLast) throws E, E2 {
        final LongIteratorEx iter = iteratorEx();
        MutableLong last = null;
        long next = 0;

        while (iter.hasNext()) {
            next = iter.nextLong();

            if (predicateForFirst.test(next)) {
                return OptionalLong.of(next);
            } else if (predicateForLast.test(next)) {
                if (last == null) {
                    last = MutableLong.of(next);
                } else {
                    last.setValue(next);
                }
            }
        }

        return last == null ? OptionalLong.empty() : OptionalLong.of(last.value());
    }

    @Override
    public LongStream intersection(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public LongStream difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public LongStream symmetricDifference(final Collection<Long> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToLong(ToLongFunction.UNBOX)).iteratorEx(), false);
    }

    @Override
    public Stream<LongStream> splitAt(final int n) {
        N.checkArgNotNegative(n, "n");

        return newStream(new ObjIteratorEx<LongStream>() {
            private LongStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public LongStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final LongIterator iter = AbstractLongStream.this.iteratorEx();
                    final LongList list = new LongList();

                    while (list.size() < n && iter.hasNext()) {
                        list.add(iter.nextLong());
                    }

                    a = new LongStream[] { new ArrayLongStream(list.array(), 0, list.size(), sorted, null), new IteratorLongStream(iter, sorted, null) };
                }
            }

        }, false, null);
    }

    @Override
    public Stream<LongStream> splitBy(final LongPredicate where) {
        N.requireNonNull(where);

        return newStream(new ObjIteratorEx<LongStream>() {
            private LongStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public LongStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final LongIterator iter = AbstractLongStream.this.iteratorEx();
                    final LongList list = new LongList();
                    long next = 0;
                    LongStream s = null;

                    while (iter.hasNext()) {
                        next = iter.nextLong();

                        if (where.test(next)) {
                            list.add(next);
                        } else {
                            s = LongStream.of(next);

                            break;
                        }
                    }

                    a = new LongStream[] { new ArrayLongStream(list.array(), 0, list.size(), sorted, null), new IteratorLongStream(iter, sorted, null) };

                    if (s != null) {
                        if (sorted) {
                            a[1] = new IteratorLongStream(a[1].prepend(s).iteratorEx(), sorted, null);
                        } else {
                            a[1] = a[1].prepend(s);
                        }
                    }
                }
            }

        }, false, null);
    }

    @Override
    public LongStream reversed() {
        return newStream(new LongIteratorEx() {
            private boolean initialized = false;
            private long[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public long nextLong() {
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
            public long[] toArray() {
                if (initialized == false) {
                    init();
                }

                final long[] a = new long[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractLongStream.this.toArray();
                    cursor = aar.length;
                }
            }
        }, false);
    }

    @Override
    public LongStream shuffled(final Random rnd) {
        return lazyLoad(new Function<long[], long[]>() {
            @Override
            public long[] apply(final long[] a) {
                N.shuffle(a, rnd);
                return a;
            }
        }, false);
    }

    @Override
    public LongStream rotated(final int distance) {
        return newStream(new LongIteratorEx() {
            private boolean initialized = false;
            private long[] aar;
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
            public long nextLong() {
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
            public long[] toArray() {
                if (initialized == false) {
                    init();
                }

                final long[] a = new long[len - cnt];

                for (int i = cnt; i < len; i++) {
                    a[i - cnt] = aar[(start + i) % len];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractLongStream.this.toArray();
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
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        return lazyLoad(new Function<long[], long[]>() {
            @Override
            public long[] apply(final long[] a) {
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
    public LongStream reverseSorted() {
        return newStream(new LongIteratorEx() {
            private boolean initialized = false;
            private long[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public long nextLong() {
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
            public long[] toArray() {
                if (initialized == false) {
                    init();
                }

                final long[] a = new long[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractLongStream.this.toArray();

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

    private LongStream lazyLoad(final Function<long[], long[]> op, final boolean sorted) {
        return newStream(new LongIteratorEx() {
            private boolean initialized = false;
            private long[] aar;
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
            public long nextLong() {
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
            public long[] toArray() {
                if (initialized == false) {
                    init();
                }

                final long[] a = new long[len - cursor];

                for (int i = cursor; i < len; i++) {
                    a[i - cursor] = aar[i];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = op.apply(AbstractLongStream.this.toArray());
                    len = aar.length;
                }
            }
        }, sorted);
    }

    @Override
    public Optional<Map<Percentage, Long>> percentiles() {
        final long[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.percentiles(a));
    }

    @Override
    public Pair<LongSummaryStatistics, Optional<Map<Percentage, Long>>> summarizze() {
        final long[] a = sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new LongSummaryStatistics(), Optional.<Map<Percentage, Long>> empty());
        } else {
            return Pair.of(new LongSummaryStatistics(a.length, sum(a), a[0], a[a.length - 1]), Optional.of(N.percentiles(a)));
        }
    }

    @Override
    public String join(final CharSequence delimiter) {
        return join(delimiter, "", "");
    }

    @Override
    public String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Joiner joiner = Joiner.with(delimiter, prefix, suffix).reuseStringBuilder(true);
        final LongIteratorEx iter = this.iteratorEx();

        while (iter.hasNext()) {
            joiner.append(iter.nextLong());
        }

        return joiner.toString();
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Pair<OptionalLong, LongStream> headAndTail() {
        return Pair.of(head(), tail());
    }

    @Override
    public Pair<LongStream, OptionalLong> headAndTaill() {
        return Pair.of(headd(), taill());
    }

    @Override
    public Stream<IndexedLong> indexed() {
        final MutableLong idx = MutableLong.of(0);

        return newStream(this.sequential().mapToObj(new LongFunction<IndexedLong>() {
            @Override
            public IndexedLong apply(long t) {
                return IndexedLong.of(t, idx.getAndIncrement());
            }
        }).iterator(), true, INDEXED_LONG_COMPARATOR);
    }

    @Override
    public LongStream append(LongStream stream) {
        return LongStream.concat(this, stream);
    }

    @Override
    public LongStream prepend(LongStream stream) {
        return LongStream.concat(stream, this);
    }

    @Override
    public LongStream merge(LongStream b, LongBiFunction<Nth> nextSelector) {
        return LongStream.merge(this, b, nextSelector);
    }

    @Override
    public LongStream zipWith(LongStream b, LongBiFunction<Long> zipFunction) {
        return LongStream.zip(this, b, zipFunction);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, LongTriFunction<Long> zipFunction) {
        return LongStream.zip(this, b, c, zipFunction);
    }

    @Override
    public LongStream zipWith(LongStream b, long valueForNoneA, long valueForNoneB, LongBiFunction<Long> zipFunction) {
        return LongStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, long valueForNoneA, long valueForNoneB, long valueForNoneC, LongTriFunction<Long> zipFunction) {
        return LongStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public LongStream cached() {
        return newStream(toArray(), sorted);
    }
}
