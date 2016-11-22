package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.IndexedInt;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
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
import com.landawn.abacus.util.function.IntBiFunction;
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
 */
abstract class AbstractIntStream extends IntStream {

    AbstractIntStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public IntStream filter(final IntPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream takeWhile(final IntPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
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
    public <K, U> Multimap<K, U, List<U>> toMultimap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
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

    @Override
    public IntStream xor(Collection<Integer> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new IntPredicate() {
            @Override
            public boolean test(int value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Integer>() {
            @Override
            public boolean test(Integer value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToInt(new ToIntFunction<Integer>() {
            @Override
            public int applyAsInt(Integer value) {
                return value.intValue();
            }
        }));
    }

    @Override
    public Stream<IntStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final IntIterator iter = this.intIterator();
        final IntList list = new IntList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final IntStream[] a = new IntStream[] { new ArrayIntStream(list.array(), 0, list.size(), null, sorted),
                new IteratorIntStream(iter instanceof ImmutableIntIterator ? (ImmutableIntIterator) iter : new ImmutableIntIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public int next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (this.isParallel()) {
            return new ParallelArrayStream<IntStream>(a, 0, a.length, closeHandlers, false, null, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayStream<IntStream>(a, closeHandlers);
        }
    }

    @Override
    public IntStream reverse() {
        final int[] a = toArray();

        //        N.reverse(a);
        //
        //        return newStream(a, false);

        return newStream(new ImmutableIntIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public int next() {
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
    public Optional<Map<Percentage, Integer>> distribution() {
        final int[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<IntSummaryStatistics, Optional<Map<Percentage, Integer>>> summarize2() {
        final int[] a = sorted().toArray();

        final IntSummaryStatistics summaryStatistics = new IntSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Integer>> distribution = a.length == 0 ? Optional.<Map<Percentage, Integer>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedInt> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new IntFunction<IndexedInt>() {
            @Override
            public IndexedInt apply(int t) {
                return IndexedInt.of(idx.getAndIncrement(), t);
            }
        });
    }

    @Override
    public IntStream append(IntStream stream) {
        return IntStream.concat(this, stream);
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
    public IntStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public IntStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public IntStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public IntStream maxThreadNum(int maxThreadNum) {
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
    public IntStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    protected IntStream newStream(final int[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayIntStream(a, 0, a.length, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayIntStream(a, closeHandlers, sorted);
        }
    }

    protected IntStream newStream(final IntIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            final ImmutableIntIterator intIter = iter instanceof ImmutableIntIterator ? (ImmutableIntIterator) iter : new ImmutableIntIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public int next() {
                    return iter.next();
                }
            };

            return new ParallelIteratorIntStream(intIter, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            final ImmutableIntIterator intIter = iter instanceof ImmutableIntIterator ? (ImmutableIntIterator) iter : new ImmutableIntIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public int next() {
                    return iter.next();
                }
            };

            return new IteratorIntStream(intIter, closeHandlers, sorted);
        }
    }

}
