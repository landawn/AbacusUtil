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
import com.landawn.abacus.util.IndexedInt;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntMatrix;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntBiFunction;
import com.landawn.abacus.util.function.IntBiPredicate;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntTriFunction;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractIntStream extends IntStream {

    AbstractIntStream(final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);
    }

    @Override
    public IntStream flattMap(final IntFunction<int[]> mapper) {
        return flatMap(new IntFunction<IntStream>() {
            @Override
            public IntStream apply(int t) {
                return IntStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public <T> Stream<T> flattMapToObj(final IntFunction<? extends Collection<T>> mapper) {
        return flatMapToObj(new IntFunction<Stream<T>>() {
            @Override
            public Stream<T> apply(int t) {
                return Stream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public IntStream skip(final long n, final IntConsumer action) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        final IntPredicate filter = isParallel() ? new IntPredicate() {
            final AtomicLong cnt = new AtomicLong(n);

            @Override
            public boolean test(int value) {
                return cnt.getAndDecrement() > 0;
            }
        } : new IntPredicate() {
            final MutableLong cnt = MutableLong.of(n);

            @Override
            public boolean test(int value) {
                return cnt.getAndDecrement() > 0;
            }
        };

        return dropWhile(filter, action);
    }

    @Override
    public IntStream removeIf(final IntPredicate predicate) {
        N.requireNonNull(predicate);

        return filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public IntStream removeIf(final IntPredicate predicate, final IntConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(predicate);

        return filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate, final IntConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new IntPredicate() {
            @Override
            public boolean test(int value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public IntStream step(final long step) {
        N.checkArgPositive(step, "step");

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final IntIteratorEx iter = this.iteratorEx();

        final IntIterator intIterator = new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public int nextInt() {
                final int next = iter.nextInt();
                iter.skip(skip);
                return next;
            }
        };

        return newStream(intIterator, sorted);
    }

    @Override
    public Stream<IntStream> split(final int size) {
        return splitToList(size).map(new Function<IntList, IntStream>() {
            @Override
            public IntStream apply(IntList t) {
                return new ArrayIntStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<IntStream> split(final IntPredicate predicate) {
        return splitToList(predicate).map(new Function<IntList, IntStream>() {
            @Override
            public IntStream apply(IntList t) {
                return new ArrayIntStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<IntList> splitToList(final IntPredicate predicate) {
        final BiPredicate<Integer, Object> predicate2 = new BiPredicate<Integer, Object>() {

            @Override
            public boolean test(Integer t, Object u) {
                return predicate.test(t);
            }
        };

        return splitToList(null, predicate2, null);
    }

    @Override
    public <U> Stream<IntStream> split(final U seed, final BiPredicate<? super Integer, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return splitToList(seed, predicate, seedUpdate).map(new Function<IntList, IntStream>() {
            @Override
            public IntStream apply(IntList t) {
                return new ArrayIntStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<IntStream> sliding(final int windowSize, final int increment) {
        return slidingToList(windowSize, increment).map(new Function<IntList, IntStream>() {
            @Override
            public IntStream apply(IntList t) {
                return new ArrayIntStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public IntStream collapse(final IntBiPredicate collapsible, final IntBiFunction<Integer> mergeFunction) {
        final IntIteratorEx iter = iteratorEx();

        return newStream(new IntIteratorEx() {
            private boolean hasNext = false;
            private int next = 0;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public int nextInt() {
                int res = hasNext ? next : (next = iter.nextInt());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.nextInt()))) {
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
    public IntStream scan(final IntBiFunction<Integer> accumulator) {
        final IntIteratorEx iter = iteratorEx();

        return newStream(new IntIteratorEx() {
            private int res = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public int nextInt() {
                if (isFirst) {
                    isFirst = false;
                    return (res = iter.nextInt());
                } else {
                    return (res = accumulator.apply(res, iter.nextInt()));
                }
            }
        }, false);
    }

    @Override
    public IntStream scan(final int seed, final IntBiFunction<Integer> accumulator) {
        final IntIteratorEx iter = iteratorEx();

        return newStream(new IntIteratorEx() {
            private int res = seed;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public int nextInt() {
                return (res = accumulator.apply(res, iter.nextInt()));
            }
        }, false);
    }

    @Override
    public <K, U> Map<K, U> toMap(IntFunction<? extends K> keyExtractor, IntFunction<? extends U> valueMapper) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyExtractor, IntFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(IntFunction<? extends K> keyExtractor, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(IntFunction<? extends K> classifier, Collector<Integer, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public IntMatrix toMatrix() {
        return IntMatrix.of(toArray());
    }

    @Override
    public IntStream distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return set.add(value);
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public OptionalInt first() {
        final IntIterator iter = this.iteratorEx();

        return iter.hasNext() ? OptionalInt.of(iter.nextInt()) : OptionalInt.empty();
    }

    @Override
    public OptionalInt last() {
        final IntIterator iter = this.iteratorEx();

        if (iter.hasNext() == false) {
            return OptionalInt.empty();
        }

        int next = iter.nextInt();

        while (iter.hasNext()) {
            next = iter.nextInt();
        }

        return OptionalInt.of(next);
    }

    @Override
    public <E extends Exception> OptionalInt findAny(final Try.IntPredicate<E> predicate) throws E {
        return findFirst(predicate);
    }

    @Override
    public <E extends Exception, E2 extends Exception> OptionalInt findFirstOrLast(Try.IntPredicate<E> predicateForFirst, Try.IntPredicate<E> predicateForLast)
            throws E, E2 {
        final IntIteratorEx iter = iteratorEx();
        MutableInt last = null;
        int next = 0;

        while (iter.hasNext()) {
            next = iter.nextInt();

            if (predicateForFirst.test(next)) {
                return OptionalInt.of(next);
            } else if (predicateForLast.test(next)) {
                if (last == null) {
                    last = MutableInt.of(next);
                } else {
                    last.setValue(next);
                }
            }
        }

        return last == null ? OptionalInt.empty() : OptionalInt.of(last.value());
    }

    @Override
    public IntStream intersection(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public IntStream difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public IntStream symmetricDifference(final Collection<Integer> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Integer>() {
            @Override
            public boolean test(Integer value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToInt(ToIntFunction.UNBOX)).iteratorEx(), false);
    }

    @Override
    public Stream<IntStream> splitAt(final int n) {
        N.checkArgNotNegative(n, "n");

        return newStream(new ObjIteratorEx<IntStream>() {
            private IntStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public IntStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final IntIterator iter = AbstractIntStream.this.iteratorEx();
                    final IntList list = new IntList();

                    while (list.size() < n && iter.hasNext()) {
                        list.add(iter.nextInt());
                    }

                    a = new IntStream[] { new ArrayIntStream(list.array(), 0, list.size(), sorted, null), new IteratorIntStream(iter, sorted, null) };
                }
            }

        }, false, null);
    }

    @Override
    public Stream<IntStream> splitBy(final IntPredicate where) {
        N.requireNonNull(where);

        return newStream(new ObjIteratorEx<IntStream>() {
            private IntStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public IntStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final IntIterator iter = AbstractIntStream.this.iteratorEx();
                    final IntList list = new IntList();
                    int next = 0;
                    IntStream s = null;

                    while (iter.hasNext()) {
                        next = iter.nextInt();

                        if (where.test(next)) {
                            list.add(next);
                        } else {
                            s = IntStream.of(next);

                            break;
                        }
                    }

                    a = new IntStream[] { new ArrayIntStream(list.array(), 0, list.size(), sorted, null), new IteratorIntStream(iter, sorted, null) };

                    if (s != null) {
                        if (sorted) {
                            a[1] = new IteratorIntStream(a[1].prepend(s).iteratorEx(), sorted, null);
                        } else {
                            a[1] = a[1].prepend(s);
                        }
                    }
                }
            }

        }, false, null);
    }

    @Override
    public IntStream reversed() {
        return newStream(new IntIteratorEx() {
            private boolean initialized = false;
            private int[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public int nextInt() {
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
            public int[] toArray() {
                if (initialized == false) {
                    init();
                }

                final int[] a = new int[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractIntStream.this.toArray();
                    cursor = aar.length;
                }
            }
        }, false);
    }

    @Override
    public IntStream shuffled(final Random rnd) {
        return lazyLoad(new Function<int[], int[]>() {
            @Override
            public int[] apply(final int[] a) {
                N.shuffle(a, rnd);
                return a;
            }
        }, false);
    }

    @Override
    public IntStream rotated(final int distance) {
        return newStream(new IntIteratorEx() {
            private boolean initialized = false;
            private int[] aar;
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
            public int nextInt() {
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
            public int[] toArray() {
                if (initialized == false) {
                    init();
                }

                final int[] a = new int[len - cnt];

                for (int i = cnt; i < len; i++) {
                    a[i - cnt] = aar[(start + i) % len];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractIntStream.this.toArray();
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
    public IntStream sorted() {
        if (sorted) {
            return this;
        }

        return lazyLoad(new Function<int[], int[]>() {
            @Override
            public int[] apply(final int[] a) {
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
    public IntStream reverseSorted() {
        return newStream(new IntIteratorEx() {
            private boolean initialized = false;
            private int[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public int nextInt() {
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
            public int[] toArray() {
                if (initialized == false) {
                    init();
                }

                final int[] a = new int[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractIntStream.this.toArray();

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

    private IntStream lazyLoad(final Function<int[], int[]> op, final boolean sorted) {
        return newStream(new IntIteratorEx() {
            private boolean initialized = false;
            private int[] aar;
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
            public int nextInt() {
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
            public int[] toArray() {
                if (initialized == false) {
                    init();
                }

                final int[] a = new int[len - cursor];

                for (int i = cursor; i < len; i++) {
                    a[i - cursor] = aar[i];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = op.apply(AbstractIntStream.this.toArray());
                    len = aar.length;
                }
            }
        }, sorted);
    }

    @Override
    public Optional<Map<Percentage, Integer>> percentiles() {
        final int[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.percentiles(a));
    }

    @Override
    public Pair<IntSummaryStatistics, Optional<Map<Percentage, Integer>>> summarizze() {
        final int[] a = sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new IntSummaryStatistics(), Optional.<Map<Percentage, Integer>> empty());
        } else {
            return Pair.of(new IntSummaryStatistics(a.length, sum(a), a[0], a[a.length - 1]), Optional.of(N.percentiles(a)));
        }
    }

    @Override
    public String join(final CharSequence delimiter) {
        return join(delimiter, "", "");
    }

    @Override
    public String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Joiner joiner = Joiner.with(delimiter, prefix, suffix).reuseStringBuilder(true);
        final IntIteratorEx iter = this.iteratorEx();

        while (iter.hasNext()) {
            joiner.append(iter.nextInt());
        }

        return joiner.toString();
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Pair<OptionalInt, IntStream> headAndTail() {
        return Pair.of(head(), tail());
    }

    @Override
    public Pair<IntStream, OptionalInt> headAndTaill() {
        return Pair.of(headd(), taill());
    }

    @Override
    public Stream<IndexedInt> indexed() {
        final MutableLong idx = MutableLong.of(0);

        return newStream(this.sequential().mapToObj(new IntFunction<IndexedInt>() {
            @Override
            public IndexedInt apply(int t) {
                return IndexedInt.of(t, idx.getAndIncrement());
            }
        }).iterator(), true, INDEXED_INT_COMPARATOR);
    }

    @Override
    public IntStream append(IntStream stream) {
        return IntStream.concat(this, stream);
    }

    @Override
    public IntStream prepend(IntStream stream) {
        return IntStream.concat(stream, this);
    }

    @Override
    public IntStream merge(IntStream b, IntBiFunction<Nth> nextSelector) {
        return IntStream.merge(this, b, nextSelector);
    }

    @Override
    public IntStream zipWith(IntStream b, IntBiFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, zipFunction);
    }

    @Override
    public IntStream zipWith(IntStream b, IntStream c, IntTriFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, c, zipFunction);
    }

    @Override
    public IntStream zipWith(IntStream b, int valueForNoneA, int valueForNoneB, IntBiFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public IntStream zipWith(IntStream b, IntStream c, int valueForNoneA, int valueForNoneB, int valueForNoneC, IntTriFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public IntStream cached() {
        return newStream(toArray(), sorted);
    }
}
