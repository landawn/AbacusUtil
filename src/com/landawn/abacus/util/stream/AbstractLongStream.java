package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.IndexedLong;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.LongBiFunction;
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
 */
abstract class AbstractLongStream extends LongStream {

    AbstractLongStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public <K> Map<K, List<Long>> toMap(LongFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Long>>>() {
            @Override
            public Map<K, List<Long>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Long>>> M toMap(LongFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Long, ?, List<Long>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(LongFunction<? extends K> classifier, Collector<Long, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public LongStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public LongStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    @Override
    public LongStream xor(Collection<Long> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToLong(new ToLongFunction<Long>() {
            @Override
            public long applyAsLong(Long value) {
                return value.longValue();
            }
        }));
    }

    @Override
    public Stream<LongStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final LongIterator iter = this.longIterator();
        final LongList list = new LongList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final LongStream[] a = new LongStream[] { new ArrayLongStream(list.array(), 0, list.size(), null, sorted),
                new IteratorLongStream(iter instanceof ImmutableLongIterator ? (ImmutableLongIterator) iter : new ImmutableLongIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public long next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (this.isParallel()) {
            return new ParallelArrayStream<LongStream>(a, 0, a.length, closeHandlers, false, null, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayStream<LongStream>(a, closeHandlers);
        }
    }

    @Override
    public LongStream reverse() {
        final long[] a = toArray();

        //        N.reverse(a);
        //
        //        return newStream(a, false);

        return newStream(new ImmutableLongIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public long next() {
                if (cursor <= 0) {
                    throw new NoSuchElementException();
                }

                return a[--cursor];
            }

            @Override
            public long count() {
                return cursor - 0;
            }

            @Override
            public void skip(long n) {
                cursor = cursor > n ? cursor - (int) n : 0;
            }
        }, false);
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

        final LongSummaryStatistics summaryStatistics = new LongSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Long>> distribution = a.length == 0 ? Optional.<Map<Percentage, Long>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedLong> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new LongFunction<IndexedLong>() {
            @Override
            public IndexedLong apply(long t) {
                return IndexedLong.of(idx.getAndIncrement(), t);
            }
        });
    }

    @Override
    public LongStream append(LongStream stream) {
        return LongStream.concat(this, stream);
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
    public LongStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public LongStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public LongStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public LongStream maxThreadNum(int maxThreadNum) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");  

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public Splitter splitter() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return DEFAULT_SPILTTER;
    }

    @Override
    public LongStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    protected LongStream newStream(final long[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayLongStream(a, 0, a.length, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayLongStream(a, closeHandlers, sorted);
        }
    }

    protected LongStream newStream(final LongIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            final ImmutableLongIterator longIter = iter instanceof ImmutableLongIterator ? (ImmutableLongIterator) iter : new ImmutableLongIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public long next() {
                    return iter.next();
                }
            };

            return new ParallelIteratorLongStream(longIter, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            final ImmutableLongIterator longIter = iter instanceof ImmutableLongIterator ? (ImmutableLongIterator) iter : new ImmutableLongIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public long next() {
                    return iter.next();
                }
            };

            return new IteratorLongStream(longIter, closeHandlers, sorted);
        }
    }
}
