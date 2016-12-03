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

import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.IndexedChar;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharTriFunction;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToCharFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class AbstractCharStream extends CharStream {

    AbstractCharStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);
    }

    @Override
    public CharStream filter(final CharPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public CharStream drop(final long n, final CharConsumer action) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be less than 0");
        } else if (n == 0) {
            return this;
        }

        if (this.isParallel()) {
            final AtomicLong cnt = new AtomicLong(n);

            return dropWhile(new CharPredicate() {
                @Override
                public boolean test(char value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        } else {
            final MutableLong cnt = MutableLong.of(n);

            return dropWhile(new CharPredicate() {
                @Override
                public boolean test(char value) {
                    return cnt.decrementAndGet() >= 0;
                }
            }, action);
        }
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate, final CharConsumer action) {
        N.requireNonNull(predicate);
        N.requireNonNull(action);

        return dropWhile(new CharPredicate() {
            @Override
            public boolean test(char value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return true;
                }

                return false;
            }
        });
    }

    @Override
    public <K> Map<K, List<Character>> toMap(CharFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Character>>>() {
            @Override
            public Map<K, List<Character>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Character>>> M toMap(CharFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Character, ?, List<Character>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(CharFunction<? extends K> classifier, Collector<Character, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U> Map<K, U> toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K> Multimap<K, Character, List<Character>> toMultimap(CharFunction<? extends K> keyMapper) {
        return toMultimap(keyMapper, new CharFunction<Character>() {
            @Override
            public Character apply(char value) {
                return value;
            }
        });
    }

    @Override
    public <K, V extends Collection<Character>> Multimap<K, Character, V> toMultimap(CharFunction<? extends K> keyMapper,
            Supplier<Multimap<K, Character, V>> mapSupplier) {
        return toMultimap(keyMapper, new CharFunction<Character>() {
            @Override
            public Character apply(char value) {
                return value;
            }
        }, mapSupplier);
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public OptionalChar first() {
        final CharIterator iter = this.charIterator();

        return iter.hasNext() ? OptionalChar.of(iter.next()) : OptionalChar.empty();
    }

    @Override
    public OptionalChar last() {
        final CharIterator iter = this.charIterator();

        if (iter.hasNext() == false) {
            return OptionalChar.empty();
        }

        char next = 0;

        while (iter.hasNext()) {
            next = iter.next();
        }

        return OptionalChar.of(next);
    }

    //    @Override
    //    public OptionalChar any() {
    //        return findAny(CharPredicate.ALWAYS_TRUE);
    //    }

    @Override
    public CharStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public CharStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    @Override
    public CharStream xor(Collection<Character> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToChar(new ToCharFunction<Character>() {
            @Override
            public char applyAsChar(Character value) {
                return value.charValue();
            }
        }));
    }

    @Override
    public Stream<CharStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final CharIterator iter = this.charIterator();
        final CharList list = new CharList();

        while (list.size() < n && iter.hasNext()) {
            list.add(iter.next());
        }

        final CharStream[] a = new CharStream[] { new ArrayCharStream(list.array(), 0, list.size(), null, sorted),
                new IteratorCharStream(iter instanceof ImmutableCharIterator ? (ImmutableCharIterator) iter : new ImmutableCharIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public char next() {
                        return iter.next();
                    }

                }, null, sorted) };

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<CharStream> splitBy(CharPredicate where) {
        N.requireNonNull(where);

        final CharIterator iter = this.charIterator();
        final CharList list = new CharList();
        char next = 0;
        CharStream p = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (where.test(next)) {
                list.add(next);
            } else {
                p = CharStream.of(next);

                break;
            }
        }

        final CharStream[] a = new CharStream[] { new ArrayCharStream(list.array(), 0, list.size(), null, sorted),
                new IteratorCharStream(iter instanceof ImmutableCharIterator ? (ImmutableCharIterator) iter : new ImmutableCharIterator() {
                    @Override
                    public boolean hasNext() {
                        return iter.hasNext();
                    }

                    @Override
                    public char next() {
                        return iter.next();
                    }

                }, null, sorted) };

        if (p != null) {
            a[1] = a[1].prepend(p);
        }

        return this.newStream(a, false, null);
    }

    @Override
    public Stream<CharList> sliding(int windowSize) {
        return sliding(windowSize, 1);
    }

    @Override
    public CharStream reverse() {
        final char[] a = toArray();

        //        N.reverse(a);
        //
        //        return newStream(a, false);

        return newStream(new ImmutableCharIterator() {
            private int cursor = a.length;

            @Override
            public boolean hasNext() {
                return cursor > 0;
            }

            @Override
            public char next() {
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
    public CharStream shuffle() {
        final char[] a = toArray();

        N.shuffle(a);

        return newStream(a, false);
    }

    @Override
    public Optional<Map<Percentage, Character>> distribution() {
        final char[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<CharSummaryStatistics, Optional<Map<Percentage, Character>>> summarize2() {
        final char[] a = sorted().toArray();

        final CharSummaryStatistics summaryStatistics = new CharSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Character>> distribution = a.length == 0 ? Optional.<Map<Percentage, Character>> empty()
                : Optional.of(N.distribution(a));

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

        final ObjCharConsumer<Joiner> accumulator = new ObjCharConsumer<Joiner>() {
            @Override
            public void accept(Joiner a, char t) {
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
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;
        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Stream<IndexedChar> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new CharFunction<IndexedChar>() {
            @Override
            public IndexedChar apply(char t) {
                return IndexedChar.of(idx.getAndIncrement(), t);
            }
        });
    }

    @Override
    public CharStream append(CharStream stream) {
        return CharStream.concat(this, stream);
    }

    @Override
    public CharStream merge(CharStream b, CharBiFunction<Nth> nextSelector) {
        return CharStream.merge(this, b, nextSelector);
    }

    @Override
    public CharStream zipWith(CharStream b, CharBiFunction<Character> zipFunction) {
        return CharStream.zip(this, b, zipFunction);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, CharTriFunction<Character> zipFunction) {
        return CharStream.zip(this, b, c, zipFunction);
    }

    @Override
    public CharStream cached() {
        return this.newStream(toArray(), sorted);
    }

    @Override
    public CharStream zipWith(CharStream b, char valueForNoneA, char valueForNoneB, CharBiFunction<Character> zipFunction) {
        return CharStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, char valueForNoneA, char valueForNoneB, char valueForNoneC, CharTriFunction<Character> zipFunction) {
        return CharStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }
}
