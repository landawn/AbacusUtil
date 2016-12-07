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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.util.IndexedShort;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortConsumer;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortTriFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractShortStream extends ShortStream {

    AbstractShortStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public ShortStream drop(final long n, final ShortConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return dropWhile(new ShortPredicate() {
                @Override
                public boolean test(short value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return dropWhile(new ShortPredicate() {
                @Override
                public boolean test(short value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        }
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate, final ShortConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new ShortPredicate() {
            @Override
            public boolean test(short value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
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
    public <K> Multimap<K, Short, List<Short>> toMultimap(ShortFunction<? extends K> keyMapper) {
        return toMultimap(keyMapper, new ShortFunction<Short>() {
            @Override
            public Short apply(short value) {
                return value;
            }
        });
    }

    @Override
    public <K, V extends Collection<Short>> Multimap<K, Short, V> toMultimap(ShortFunction<? extends K> keyMapper,
            Supplier<Multimap<K, Short, V>> mapSupplier) {
        return toMultimap(keyMapper, new ShortFunction<Short>() {
            @Override
            public Short apply(short value) {
                return value;
            }
        }, mapSupplier);
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
    public ShortStream distinct() {
        return boxed().distinct().mapToShort(new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        });
    }

    @Override
    public OptionalShort first() {
        final ShortIterator iter = this.shortIterator();

        return iter.hasNext() ? OptionalShort.of(iter.next()) : OptionalShort.empty();
    }

    @Override
    public OptionalShort last() {
        final ShortIterator iter = this.shortIterator();

        if (iter.hasNext() == false) {
            return OptionalShort.empty();
        }

        short next = 0;

        while (iter.hasNext()) {
            next = iter.next();
        }

        return OptionalShort.of(next);
    }

    //    @Override
    //    public OptionalShort any() {
    //        return findAny(ShortPredicate.ALWAYS_TRUE);
    //    }

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

    @Override
    public ShortStream xor(Collection<Short> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new ShortPredicate() {
            @Override
            public boolean test(short value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Short>() {
            @Override
            public boolean test(Short value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToShort(new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        }));
    }

    @Override
    public Stream<ShortStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final ShortIterator iter = this.shortIterator();
        final ShortList list = new ShortList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final ShortStream[] a = new ShortStream[] { new ArrayShortStream(list.array(), 0, list.size(), null, sorted),
                new IteratorShortStream(iter instanceof ImmutableShortIterator ? (ImmutableShortIterator) iter : new ImmutableShortIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public short next() {
                        return iter.next();
                    }

                }, null, sorted) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<ShortStream> splitBy(ShortPredicate where) {
        N.requireNonNull(where);

        final ShortIterator iter = this.shortIterator();
        final ShortList list = new ShortList();
        short next = 0;
        ShortStream p = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (where.test(next)) {
                list.add(next);
            } else {
                p = ShortStream.of(next);

                break;
            }
        }

        final ShortStream[] a = new ShortStream[] { new ArrayShortStream(list.array(), 0, list.size(), null, sorted),
                new IteratorShortStream(iter instanceof ImmutableShortIterator ? (ImmutableShortIterator) iter : new ImmutableShortIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public short next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (p != null) {
            a[1] = a[1].prepend(p);
        }

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<ShortList> sliding(int windowSize) {
        return sliding(windowSize, 1);
    }

    @Override
    public ShortStream reverse() {
        final short[] a = toArray();

        //        N.reverse(a);
        //
        //        return newStream(a, false);

        return newStream(new ImmutableShortIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public short next() {
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
    public ShortStream shuffle() {
        final short[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public ShortStream rotate(int distance) {
        final short[] a = toArray();

        N.rotate(a, distance);

        return newStream(a, false);
    }

    @Override
    public Optional<Map<Percentage, Short>> distribution() {
        final short[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<ShortSummaryStatistics, Optional<Map<Percentage, Short>>> summarize2() {
        final short[] a = sorted().toArray();

        final ShortSummaryStatistics summaryStatistics = new ShortSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Short>> distribution = a.length == 0 ? Optional.<Map<Percentage, Short>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
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
                return new Joiner(delimiter, prefix, suffix);
            }
        };

        final ObjShortConsumer<Joiner> accumulator = new ObjShortConsumer<Joiner>() {
            @Override
            public void accept(Joiner a, short t) {
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
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedShort> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new ShortFunction<IndexedShort>() {
            @Override
            public IndexedShort apply(short t) {
                return IndexedShort.of(idx.getAndIncrement(), t);
            }
        });
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
    public ShortStream zipWith(ShortStream b, ShortBiFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, zipFunction);
    }

    @Override
    public ShortStream zipWith(ShortStream b, ShortStream c, ShortTriFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, c, zipFunction);
    }

    @Override
    public ShortStream zipWith(ShortStream b, short valueForNoneA, short valueForNoneB, ShortBiFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public ShortStream zipWith(ShortStream b, ShortStream c, short valueForNoneA, short valueForNoneB, short valueForNoneC,
            ShortTriFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public ShortStream cached() {
        return this.newStream(toArray(), sorted);
    }
}
