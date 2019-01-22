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

import com.landawn.abacus.exception.DuplicatedResultException;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharMatrix;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IndexedChar;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableChar;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.StringUtil.Strings;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharBiPredicate;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharTriFunction;
import com.landawn.abacus.util.function.Function;
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

    AbstractCharStream(final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);
    }

    @Override
    public CharStream flattMap(final CharFunction<char[]> mapper) {
        return flatMap(new CharFunction<CharStream>() {
            @Override
            public CharStream apply(char t) {
                return CharStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public <T> Stream<T> flattMapToObj(final CharFunction<? extends Collection<T>> mapper) {
        return flatMapToObj(new CharFunction<Stream<T>>() {
            @Override
            public Stream<T> apply(char t) {
                return Stream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public CharStream skip(final long n, final CharConsumer action) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        final CharPredicate filter = isParallel() ? new CharPredicate() {
            final AtomicLong cnt = new AtomicLong(n);

            @Override
            public boolean test(char value) {
                return cnt.getAndDecrement() > 0;
            }
        } : new CharPredicate() {
            final MutableLong cnt = MutableLong.of(n);

            @Override
            public boolean test(char value) {
                return cnt.getAndDecrement() > 0;
            }
        };

        return dropWhile(filter, action);
    }

    @Override
    public CharStream removeIf(final CharPredicate predicate) {
        N.checkArgNotNull(predicate);

        return filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return predicate.test(value) == false;
            }
        });
    }

    @Override
    public CharStream removeIf(final CharPredicate predicate, final CharConsumer action) {
        N.checkArgNotNull(predicate);
        N.checkArgNotNull(predicate);

        return filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                if (predicate.test(value)) {
                    action.accept(value);
                    return false;
                }

                return true;
            }
        });
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate, final CharConsumer action) {
        N.checkArgNotNull(predicate);
        N.checkArgNotNull(action);

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
    public CharStream step(final long step) {
        N.checkArgPositive(step, "step");

        if (step == 1) {
            return this;
        }

        final long skip = step - 1;
        final CharIteratorEx iter = this.iteratorEx();

        final CharIterator charIterator = new CharIteratorEx() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public char nextChar() {
                final char next = iter.nextChar();
                iter.skip(skip);
                return next;
            }
        };

        return newStream(charIterator, sorted);
    }

    @Override
    public Stream<CharStream> split(final int size) {
        return splitToList(size).map(new Function<CharList, CharStream>() {
            @Override
            public CharStream apply(CharList t) {
                return new ArrayCharStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<CharStream> split(final CharPredicate predicate) {
        return splitToList(predicate).map(new Function<CharList, CharStream>() {
            @Override
            public CharStream apply(CharList t) {
                return new ArrayCharStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public Stream<CharStream> sliding(final int windowSize, final int increment) {
        return slidingToList(windowSize, increment).map(new Function<CharList, CharStream>() {
            @Override
            public CharStream apply(CharList t) {
                return new ArrayCharStream(t.array(), 0, t.size(), sorted, null);
            }
        });
    }

    @Override
    public CharStream collapse(final CharBiPredicate collapsible, final CharBiFunction<Character> mergeFunction) {
        final CharIteratorEx iter = iteratorEx();

        return newStream(new CharIteratorEx() {
            private boolean hasNext = false;
            private char next = 0;

            @Override
            public boolean hasNext() {
                return hasNext || iter.hasNext();
            }

            @Override
            public char nextChar() {
                char res = hasNext ? next : (next = iter.nextChar());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.nextChar()))) {
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
    public CharStream scan(final CharBiFunction<Character> accumulator) {
        final CharIteratorEx iter = iteratorEx();

        return newStream(new CharIteratorEx() {
            private char res = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public char nextChar() {
                if (isFirst) {
                    isFirst = false;
                    return (res = iter.nextChar());
                } else {
                    return (res = accumulator.apply(res, iter.nextChar()));
                }
            }
        }, false);
    }

    @Override
    public CharStream scan(final char seed, final CharBiFunction<Character> accumulator) {
        final CharIteratorEx iter = iteratorEx();

        return newStream(new CharIteratorEx() {
            private char res = seed;

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public char nextChar() {
                return (res = accumulator.apply(res, iter.nextChar()));
            }
        }, false);
    }

    @Override
    public <K, V> Map<K, V> toMap(CharFunction<? extends K> keyExtractor, CharFunction<? extends V> valueMapper) {
        final Supplier<Map<K, V>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    @Override
    public <K, V, M extends Map<K, V>> M toMap(CharFunction<? extends K> keyExtractor, CharFunction<? extends V> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<V> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, V> Map<K, V> toMap(CharFunction<? extends K> keyExtractor, CharFunction<? extends V> valueMapper, BinaryOperator<V> mergeFunction) {
        final Supplier<Map<K, V>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(CharFunction<? extends K> classifier, Collector<Character, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public CharMatrix toMatrix() {
        return CharMatrix.of(toArray());
    }

    @Override
    public CharStream distinct() {
        final Set<Object> set = new HashSet<>();

        return newStream(this.sequential().filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return set.add(value);
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public OptionalChar first() {
        final CharIterator iter = this.iteratorEx();

        return iter.hasNext() ? OptionalChar.of(iter.nextChar()) : OptionalChar.empty();
    }

    @Override
    public OptionalChar last() {
        final CharIterator iter = this.iteratorEx();

        if (iter.hasNext() == false) {
            return OptionalChar.empty();
        }

        char next = iter.nextChar();

        while (iter.hasNext()) {
            next = iter.nextChar();
        }

        return OptionalChar.of(next);
    }

    @Override
    public OptionalChar onlyOne() throws DuplicatedResultException {
        final CharIterator iter = this.iteratorEx();

        final OptionalChar result = iter.hasNext() ? OptionalChar.of(iter.nextChar()) : OptionalChar.empty();

        if (result.isPresent() && iter.hasNext()) {
            throw new DuplicatedResultException("There are at least two elements: " + Strings.concat(result.get(), ", ", iter.nextChar()));
        }

        return result;
    }

    @Override
    public <E extends Exception> OptionalChar findAny(final Try.CharPredicate<E> predicate) throws E {
        return findFirst(predicate);
    }

    @Override
    public <E extends Exception, E2 extends Exception> OptionalChar findFirstOrLast(Try.CharPredicate<E> predicateForFirst,
            Try.CharPredicate<E> predicateForLast) throws E, E2 {
        final CharIteratorEx iter = iteratorEx();
        MutableChar last = null;
        char next = 0;

        while (iter.hasNext()) {
            next = iter.nextChar();

            if (predicateForFirst.test(next)) {
                return OptionalChar.of(next);
            } else if (predicateForLast.test(next)) {
                if (last == null) {
                    last = MutableChar.of(next);
                } else {
                    last.setValue(next);
                }
            }
        }

        return last == null ? OptionalChar.empty() : OptionalChar.of(last.value());
    }

    @Override
    public CharStream intersection(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public CharStream difference(final Collection<?> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).iteratorEx(), sorted);
    }

    @Override
    public CharStream symmetricDifference(final Collection<Character> c) {
        final Multiset<?> multiset = Multiset.from(c);

        return newStream(this.sequential().filter(new CharPredicate() {
            @Override
            public boolean test(char value) {
                return multiset.getAndRemove(value) < 1;
            }
        }).append(Stream.of(c).filter(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return multiset.getAndRemove(value) > 0;
            }
        }).mapToChar(ToCharFunction.UNBOX)).iteratorEx(), false);
    }

    @Override
    public Stream<CharStream> splitAt(final int n) {
        N.checkArgNotNegative(n, "n");

        return newStream(new ObjIteratorEx<CharStream>() {
            private CharStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public CharStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final CharIterator iter = AbstractCharStream.this.iteratorEx();
                    final CharList list = new CharList();

                    while (list.size() < n && iter.hasNext()) {
                        list.add(iter.nextChar());
                    }

                    a = new CharStream[] { new ArrayCharStream(list.array(), 0, list.size(), sorted, null), new IteratorCharStream(iter, sorted, null) };
                }
            }

        }, false, null);
    }

    @Override
    public Stream<CharStream> splitBy(final CharPredicate where) {
        N.checkArgNotNull(where);

        return newStream(new ObjIteratorEx<CharStream>() {
            private CharStream[] a = null;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                init();

                return cursor < 2;
            }

            @Override
            public CharStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            private void init() {
                if (a == null) {
                    final CharIterator iter = AbstractCharStream.this.iteratorEx();
                    final CharList list = new CharList();
                    char next = 0;
                    CharStream s = null;

                    while (iter.hasNext()) {
                        next = iter.nextChar();

                        if (where.test(next)) {
                            list.add(next);
                        } else {
                            s = CharStream.of(next);

                            break;
                        }
                    }

                    a = new CharStream[] { new ArrayCharStream(list.array(), 0, list.size(), sorted, null), new IteratorCharStream(iter, sorted, null) };

                    if (s != null) {
                        if (sorted) {
                            a[1] = new IteratorCharStream(a[1].prepend(s).iteratorEx(), sorted, null);
                        } else {
                            a[1] = a[1].prepend(s);
                        }
                    }
                }
            }

        }, false, null);
    }

    @Override
    public CharStream reversed() {
        return newStream(new CharIteratorEx() {
            private boolean initialized = false;
            private char[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public char nextChar() {
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
            public char[] toArray() {
                if (initialized == false) {
                    init();
                }

                final char[] a = new char[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractCharStream.this.toArray();
                    cursor = aar.length;
                }
            }
        }, false);
    }

    @Override
    public CharStream shuffled(final Random rnd) {
        return lazyLoad(new Function<char[], char[]>() {
            @Override
            public char[] apply(final char[] a) {
                N.shuffle(a, rnd);
                return a;
            }
        }, false);
    }

    @Override
    public CharStream rotated(final int distance) {
        return newStream(new CharIteratorEx() {
            private boolean initialized = false;
            private char[] aar;
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
            public char nextChar() {
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
            public char[] toArray() {
                if (initialized == false) {
                    init();
                }

                final char[] a = new char[len - cnt];

                for (int i = cnt; i < len; i++) {
                    a[i - cnt] = aar[(start + i) % len];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractCharStream.this.toArray();
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
    public CharStream sorted() {
        if (sorted) {
            return this;
        }

        return lazyLoad(new Function<char[], char[]>() {
            @Override
            public char[] apply(final char[] a) {
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
    public CharStream reverseSorted() {
        return newStream(new CharIteratorEx() {
            private boolean initialized = false;
            private char[] aar;
            private int cursor;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor > 0;
            }

            @Override
            public char nextChar() {
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
            public char[] toArray() {
                if (initialized == false) {
                    init();
                }

                final char[] a = new char[cursor];

                for (int i = 0; i < cursor; i++) {
                    a[i] = aar[cursor - i - 1];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = AbstractCharStream.this.toArray();

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

    private CharStream lazyLoad(final Function<char[], char[]> op, final boolean sorted) {
        return newStream(new CharIteratorEx() {
            private boolean initialized = false;
            private char[] aar;
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
            public char nextChar() {
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
            public char[] toArray() {
                if (initialized == false) {
                    init();
                }

                final char[] a = new char[len - cursor];

                for (int i = cursor; i < len; i++) {
                    a[i - cursor] = aar[i];
                }

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = op.apply(AbstractCharStream.this.toArray());
                    len = aar.length;
                }
            }
        }, sorted);
    }

    @Override
    public Optional<Map<Percentage, Character>> percentiles() {
        final char[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.percentiles(a));
    }

    @Override
    public Pair<CharSummaryStatistics, Optional<Map<Percentage, Character>>> summarizze() {
        final char[] a = sorted().toArray();

        if (N.isNullOrEmpty(a)) {
            return Pair.of(new CharSummaryStatistics(), Optional.<Map<Percentage, Character>> empty());
        } else {
            return Pair.of(new CharSummaryStatistics(a.length, sum(a), a[0], a[a.length - 1]), Optional.of(N.percentiles(a)));
        }
    }

    @Override
    public String join(final CharSequence delimiter) {
        return join(delimiter, "", "");
    }

    @Override
    public String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Joiner joiner = Joiner.with(delimiter, prefix, suffix).reuseCachedBuffer(true);
        final CharIteratorEx iter = this.iteratorEx();

        while (iter.hasNext()) {
            joiner.append(iter.nextChar());
        }

        return joiner.toString();
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator) {
        final BiConsumer<R, R> combiner = collectingCombiner;

        return collect(supplier, accumulator, combiner);
    }

    @Override
    public Pair<OptionalChar, CharStream> headAndTail() {
        return Pair.of(head(), tail());
    }

    //    @SuppressWarnings("deprecation")
    //    @Override
    //    public Pair<CharStream, OptionalChar> headAndTaill() {
    //        return Pair.of(headd(), taill());
    //    }

    @Override
    public Stream<IndexedChar> indexed() {
        final MutableLong idx = MutableLong.of(0);

        return newStream(this.sequential().mapToObj(new CharFunction<IndexedChar>() {
            @Override
            public IndexedChar apply(char t) {
                return IndexedChar.of(t, idx.getAndIncrement());
            }
        }).iterator(), true, INDEXED_CHAR_COMPARATOR);
    }

    @Override
    public CharStream append(CharStream stream) {
        return CharStream.concat(this, stream);
    }

    @Override
    public CharStream prepend(CharStream stream) {
        return CharStream.concat(stream, this);
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
    public CharStream zipWith(CharStream b, char valueForNoneA, char valueForNoneB, CharBiFunction<Character> zipFunction) {
        return CharStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, char valueForNoneA, char valueForNoneB, char valueForNoneC, CharTriFunction<Character> zipFunction) {
        return CharStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public CharStream cached() {
        return newStream(toArray(), sorted);
    }
}
