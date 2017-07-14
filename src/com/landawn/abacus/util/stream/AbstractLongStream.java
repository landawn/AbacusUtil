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
import java.util.List;
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
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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

    AbstractLongStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public LongStream remove(final long n, final LongConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return removeWhile(new LongPredicate() {
                @Override
                public boolean test(long value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return removeWhile(new LongPredicate() {
                @Override
                public boolean test(long value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        }
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
    public LongStream removeWhile(final LongPredicate predicate, final LongConsumer action) {
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
        N.checkArgument(step > 0, "'step' can't be 0 or negative: %s", step);

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final ExLongIterator iter = this.exIterator();

        final LongIterator longIterator = new ExLongIterator() {
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
                return new ArrayLongStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public Stream<LongStream> split(final LongPredicate predicate) {
        return splitToList(predicate).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public Stream<LongList> splitToList(final LongPredicate predicate) {
        final BiFunction<Long, Object, Boolean> predicate2 = new BiFunction<Long, Object, Boolean>() {

            @Override
            public Boolean apply(Long t, Object u) {
                return predicate.test(t);
            }
        };

        return splitToList(null, predicate2, null);
    }

    @Override
    public <U> Stream<LongStream> split(final U identity, final BiFunction<? super Long, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return splitToList(identity, predicate, identityUpdate).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public Stream<LongStream> sliding(final int windowSize, final int increment) {
        return slidingToList(windowSize, increment).map(new Function<LongList, LongStream>() {
            @Override
            public LongStream apply(LongList t) {
                return new ArrayLongStream(t.array(), 0, t.size(), null, sorted);
            }
        });
    }

    @Override
    public LongStream collapse(final LongBiPredicate collapsible, final LongBiFunction<Long> mergeFunction) {
        final ExLongIterator iter = exIterator();

        return this.newStream(new ExLongIterator() {
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
        final ExLongIterator iter = exIterator();

        return this.newStream(new ExLongIterator() {
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
        final ExLongIterator iter = exIterator();

        return this.newStream(new ExLongIterator() {
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
    public LongStream reverseSorted() {
        return sorted().reversed();
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
    public <K> Multimap<K, Long, List<Long>> toMultimap(LongFunction<? extends K> keyExtractor) {
        return toMultimap(keyExtractor, LongFunction.BOX);
    }

    @Override
    public <K, V extends Collection<Long>> Multimap<K, Long, V> toMultimap(LongFunction<? extends K> keyExtractor, Supplier<Multimap<K, Long, V>> mapFactory) {
        return toMultimap(keyExtractor, LongFunction.BOX, mapFactory);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(LongFunction<? extends K> keyExtractor, LongFunction<? extends U> valueMapper) {
        return toMultimap(keyExtractor, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
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
        }).exIterator(), sorted);
    }

    @Override
    public OptionalLong first() {
        final LongIterator iter = this.exIterator();

        return iter.hasNext() ? OptionalLong.of(iter.nextLong()) : OptionalLong.empty();
    }

    @Override
    public OptionalLong last() {
        final LongIterator iter = this.exIterator();

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
    public OptionalLong findFirstOrLast(LongPredicate predicateForFirst, LongPredicate predicateForLast) {
        final ExLongIterator iter = exIterator();
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
        }).exIterator(), sorted);
    }

    @Override
    public LongStream difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).exIterator(), sorted);
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
        }).mapToLong(ToLongFunction.UNBOX)).exIterator(), false);
    }

    @Override
    public Stream<LongStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final LongIterator iter = this.exIterator();
        final LongList list = new LongList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.nextLong());
        }

        final LongStream[] a = { new ArrayLongStream(list.array(), 0, list.size(), null, sorted), new IteratorLongStream(iter, null, sorted) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<LongStream> splitBy(LongPredicate where) {
        N.requireNonNull(where);

        final LongIterator iter = this.exIterator();
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

        final LongStream[] a = { new ArrayLongStream(list.array(), 0, list.size(), null, sorted), new IteratorLongStream(iter, null, sorted) };

        if (s != null) {
            if (sorted) {
                a[1] = new IteratorLongStream(a[1].prepend(s).exIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(s);
            }
        }

        return this.newStream(a, false, null);
    }

    @Override
    public LongStream reversed() {
        final long[] tmp = toArray();

        return newStream(new ExLongIterator() {
            private int cursor = tmp.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public long nextLong() {
                if (cursor <= 0) {
                    throw new NoSuchElementException();
                }

                return tmp[--cursor];
            }

            @Override
            public long count() {
                return cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < cursor ? cursor - (int) n : 0;
            }

            @Override
            public long[] toArray() {
                final long[] a = new long[cursor];

                for (int i = 0, len = tmp.length; i < len; i++) {
                    a[i] = tmp[cursor - i - 1];
                }

                return a;
            }
        }, false);
    }

    @Override
    public LongStream shuffled() {
        final long[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public LongStream shuffled(final Random rnd) {
        final long[] a = toArray();

        N.shuffle(a, rnd);

        return newStream(a, false);
    }

    @Override
    public LongStream rotated(int distance) {
        final long[] a = toArray();

        N.rotate(a, distance);

        return newStream(a, false);
    }

    @Override
    public Optional<Map<Percentage, Long>> distribution() {
        final long[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<LongSummaryStatistics, Optional<Map<Percentage, Long>>> summarize2() {
        final long[] a = sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new LongSummaryStatistics(), Optional.<Map<Percentage, Long>> empty());
        } else {
            return Pair.of(new LongSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]), Optional.of(N.distribution(a)));
        }
    }

    @Override
    public String join(final CharSequence delimiter) {
        return join(delimiter, "", "");
    }

    @Override
    public String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Supplier<Joiner> supplier = new Supplier<Joiner>() {
            @Override
            public Joiner get() {
                return Joiner.with(delimiter, prefix, suffix);
            }
        };

        final ObjLongConsumer<Joiner> accumulator = new ObjLongConsumer<Joiner>() {
            @Override
            public void accept(Joiner a, long t) {
                a.add(t);
            }
        };

        final BiConsumer<Joiner, Joiner> combiner = new BiConsumer<Joiner, Joiner>() {
            @Override
            public void accept(Joiner a, Joiner b) {
                a.merge(b);
            }
        };

        final Joiner joiner = collect(supplier, accumulator, combiner);

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
    public Pair<LongStream, OptionalLong> headAndTail2() {
        return Pair.of(head2(), tail2());
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
        return this.newStream(toArray(), sorted);
    }
}
