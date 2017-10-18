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
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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
    public ByteStream flatArray(final ByteFunction<byte[]> mapper) {
        return flatMap(new ByteFunction<ByteStream>() {
            @Override
            public ByteStream apply(byte t) {
                return ByteStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream remove(final long n, final ByteConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return removeWhile(new BytePredicate() {
                @Override
                public boolean test(byte value) {
                    return cnt.getAndDecrement() > 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return removeWhile(new BytePredicate() {

                @Override
                public boolean test(byte value) {
                    return cnt.getAndDecrement() > 0;
                }

            }, action);
        }
    }

    @Override
    public ByteStream removeIf(final BytePredicate predicate) {
        N.requireNonNull(predicate);

        return filter(new BytePredicate() {
            @Override
            public boolean test(byte value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public ByteStream removeIf(final BytePredicate predicate, final ByteConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(predicate);

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
    public ByteStream removeWhile(final BytePredicate predicate, final ByteConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

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
        N.checkArgument(step > 0, "'step' can't be 0 or negative: %s", step);

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
        final BiFunction<Byte, Object, Boolean> predicate2 = new BiFunction<Byte, Object, Boolean>() {

            @Override
            public Boolean apply(Byte t, Object u) {
                return predicate.test(t);
            }
        };

        return splitToList(null, predicate2, null);
    }

    @Override
    public <U> Stream<ByteStream> split(final U seed, final BiFunction<? super Byte, ? super U, Boolean> predicate,
            final Consumer<? super U> seedUpdate) {
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

        return this.newStream(new ByteIteratorEx() {
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

        return this.newStream(new ByteIteratorEx() {
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

        return this.newStream(new ByteIteratorEx() {
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
    public ByteStream reverseSorted() {
        return sorted().reversed();
    }

    @Override
    public <K, U> Map<K, U> toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends U> valueMapper) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, U> Map<K, U> toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

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
    public OptionalByte findFirstOrLast(BytePredicate predicateForFirst, BytePredicate predicateForLast) {
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
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final ByteIterator iter = this.iteratorEx();
        final ByteList list = new ByteList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.nextByte());
        }

        final ByteStream[] a = { new ArrayByteStream(list.array(), 0, list.size(), sorted, null), new IteratorByteStream(iter, sorted, null) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<ByteStream> splitBy(BytePredicate where) {
        N.requireNonNull(where);

        final ByteIterator iter = this.iteratorEx();
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

        final ByteStream[] a = { new ArrayByteStream(list.array(), 0, list.size(), sorted, null), new IteratorByteStream(iter, sorted, null) };

        if (s != null) {
            if (sorted) {
                a[1] = new IteratorByteStream(a[1].prepend(s).iteratorEx(), sorted, null);
            } else {
                a[1] = a[1].prepend(s);
            }
        }

        return this.newStream(a, false, null);
    }

    @Override
    public ByteStream reversed() {
        final byte[] tmp = toArray();

        return newStream(new ByteIteratorEx() {
            private int cursor = tmp.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public byte nextByte() {
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
            public byte[] toArray() {
                final byte[] a = new byte[cursor];

                for (int i = 0, len = tmp.length; i < len; i++) {
                    a[i] = tmp[cursor - i - 1];
                }

                return a;
            }
        }, false);
    }

    @Override
    public ByteStream shuffled() {
        final byte[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public ByteStream shuffled(final Random rnd) {
        final byte[] a = toArray();

        N.shuffle(a, rnd);

        return newStream(a, false);
    }

    @Override
    public ByteStream rotated(int distance) {
        final byte[] a = toArray();

        N.rotate(a, distance);

        return newStream(a, false);
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
    public Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarize2() {
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
        final Supplier<Joiner> supplier = new Supplier<Joiner>() {
            @Override
            public Joiner get() {
                return Joiner.with(delimiter, prefix, suffix);
            }
        };

        final ObjByteConsumer<Joiner> accumulator = new ObjByteConsumer<Joiner>() {
            @Override
            public void accept(Joiner a, byte t) {
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
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Pair<OptionalByte, ByteStream> headAndTail() {
        return Pair.of(head(), tail());
    }

    @Override
    public Pair<ByteStream, OptionalByte> headAndTail2() {
        return Pair.of(head2(), tail2());
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
        return this.newStream(toArray(), sorted);
    }
}
