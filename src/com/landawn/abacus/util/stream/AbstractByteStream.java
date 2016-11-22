package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.IndexedByte;
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
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractByteStream extends ByteStream {

    AbstractByteStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public ByteStream filter(BytePredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream takeWhile(BytePredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ByteStream dropWhile(BytePredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
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
    public <K, U> Multimap<K, U, List<U>> toMultimap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
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

    @Override
    public Stream<ByteStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final ByteIterator iter = this.byteIterator();
        final ByteList list = new ByteList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final ByteStream[] a = new ByteStream[] { new ArrayByteStream(list.array(), 0, list.size(), null, sorted),
                new IteratorByteStream(iter instanceof ImmutableByteIterator ? (ImmutableByteIterator) iter : new ImmutableByteIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public byte next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (this.isParallel()) {
            return new ParallelArrayStream<ByteStream>(a, 0, a.length, closeHandlers, false, null, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayStream<ByteStream>(a, closeHandlers);
        }
    }

    @Override
    public ByteStream reverse() {
        final byte[] a = toArray();

        //         N.reverse(a);
        //
        //         return newStream(a, false);

        return newStream(new ImmutableByteIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public byte next() {
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
    public Optional<Map<Percentage, Byte>> distribution() {
        final byte[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarize2() {
        final byte[] a = sorted().toArray();

        final ByteSummaryStatistics summaryStatistics = new ByteSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Byte>> distribution = a.length == 0 ? Optional.<Map<Percentage, Byte>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedByte> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new ByteFunction<IndexedByte>() {
            @Override
            public IndexedByte apply(byte t) {
                return IndexedByte.of(idx.getAndIncrement(), t);
            }
        });
    }

    @Override
    public ByteStream append(ByteStream stream) {
        return ByteStream.concat(this, stream);
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
    public ByteStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public ByteStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public ByteStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public ByteStream maxThreadNum(int maxThreadNum) {
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
    public ByteStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    protected ByteStream newStream(final byte[] a, final boolean sorted) {
        if (this.isParallel()) {
            return new ParallelArrayByteStream(a, 0, a.length, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            return new ArrayByteStream(a, closeHandlers, sorted);
        }
    }

    protected ByteStream newStream(final ByteIterator iter, final boolean sorted) {
        if (this.isParallel()) {
            final ImmutableByteIterator byteIter = iter instanceof ImmutableByteIterator ? (ImmutableByteIterator) iter : new ImmutableByteIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public byte next() {
                    return iter.next();
                }
            };

            return new ParallelIteratorByteStream(byteIter, closeHandlers, sorted, this.maxThreadNum(), this.splitter());
        } else {
            final ImmutableByteIterator byteIter = iter instanceof ImmutableByteIterator ? (ImmutableByteIterator) iter : new ImmutableByteIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public byte next() {
                    return iter.next();
                }
            };

            return new IteratorByteStream(byteIter, closeHandlers, sorted);
        }
    }
}
