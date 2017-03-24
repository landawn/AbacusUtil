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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBinaryOperator;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharToIntFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class ArrayCharStream extends AbstractCharStream {
    final char[] elements;
    final int fromIndex;
    final int toIndex;

    ArrayCharStream(final char[] values) {
        this(values, 0, values.length);
    }

    ArrayCharStream(final char[] values, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayCharStream(final char[] values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayCharStream(final char[] values, final int fromIndex, final int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayCharStream(final char[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayCharStream(final char[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public CharStream filter(final CharPredicate predicate) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        }
                    } while (++cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public char next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && cursor < toIndex) {
                    if (predicate.test(elements[cursor])) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public char next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex) {
                    if (dropped == false) {
                        do {
                            if (predicate.test(elements[cursor]) == false) {
                                hasNext = true;
                                break;
                            }
                        } while (++cursor < toIndex);

                        dropped = true;
                    } else {
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public char next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream map(final CharUnaryOperator mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsChar(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public char[] toArray() {
                final char[] a = new char[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsChar(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final CharToIntFunction mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsInt(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsInt(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ImmutableIterator<U>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public U next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.apply(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) mapper.apply(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public CharStream flatMap(final CharFunction<? extends CharStream> mapper) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private int cursor = fromIndex;
            private CharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).charIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(final CharFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private int cursor = fromIndex;
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).intIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final CharFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private int cursor = fromIndex;
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public Stream<CharStream> split(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<CharStream>(new ImmutableIterator<CharStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayCharStream(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex), null, sorted);
            }
        }, closeHandlers);
    }

    @Override
    public Stream<CharList> split0(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<CharList>(new ImmutableIterator<CharList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new CharList(N.copyOfRange(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex)));
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<CharStream> split(final U identity, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<CharStream>(new ImmutableIterator<CharStream>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.apply(elements[from], identity);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], identity) == preCondition) {
                        cursor++;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return new ArrayCharStream(elements, from, cursor, null, sorted);
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<CharList> split0(final U identity, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<CharList>(new ImmutableIterator<CharList>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.apply(elements[from], identity);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], identity) == preCondition) {
                        cursor++;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return new CharList(N.copyOfRange(elements, from, cursor));
            }
        }, closeHandlers);
    }

    @Override
    public Stream<CharStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final CharStream[] a = new CharStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? CharStream.empty() : new ArrayCharStream(elements, fromIndex, middleIndex, null, sorted);
        a[1] = middleIndex == toIndex ? CharStream.empty() : new ArrayCharStream(elements, middleIndex, toIndex, null, sorted);

        return new ArrayStream<>(a, closeHandlers);
    }

    @Override
    public Stream<CharStream> splitBy(CharPredicate where) {
        N.requireNonNull(where);

        int n = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (where.test(elements[i])) {
                n++;
            } else {
                break;
            }
        }

        return splitAt(n);
    }

    @Override
    public Stream<CharStream> sliding(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<CharStream>(new ImmutableIterator<CharStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ArrayCharStream result = new ArrayCharStream(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex, null,
                        sorted);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<CharList> sliding0(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<CharList>(new ImmutableIterator<CharList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final CharList result = CharList.of(N.copyOfRange(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex));

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return this;
        }

        final char[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayCharStream(a, closeHandlers, true);
    }

    @Override
    public CharStream peek(final CharConsumer action) {
        return new IteratorCharStream(new ImmutableCharIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                action.accept(elements[cursor]);

                return elements[cursor++];
            }

            @Override
            public char[] toArray() {
                final char[] a = new char[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    action.accept(elements[cursor]);

                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public CharStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayCharStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
    }

    @Override
    public CharStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayCharStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayCharStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(CharConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public char[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public CharList toCharList() {
        return CharList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Character> toList() {
        final List<Character> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<Character> toList(Supplier<? extends List<Character>> supplier) {
        final List<Character> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Character> toSet() {
        final Set<Character> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Character> toSet(Supplier<? extends Set<Character>> supplier) {
        final Set<Character> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset() {
        final Multiset<Character> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset(Supplier<? extends Multiset<Character>> supplier) {
        final Multiset<Character> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset() {
        final LongMultiset<Character> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset(Supplier<? extends LongMultiset<Character>> supplier) {
        final LongMultiset<Character> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final CharFunction<? extends K> classifier, final Collector<Character, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Character> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = N.requireNonNull(classifier.apply(elements[i]), "element cannot be mapped to a null key");

            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, elements[i]);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        Collectors.replaceAll(intermediate, function);

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyMapper.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(CharFunction<? extends K> keyMapper, CharFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyMapper.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
    }

    @Override
    public OptionalChar first() {
        return fromIndex < toIndex ? OptionalChar.of(elements[fromIndex]) : OptionalChar.empty();
    }

    @Override
    public OptionalChar last() {
        return fromIndex < toIndex ? OptionalChar.of(elements[toIndex - 1]) : OptionalChar.empty();
    }

    @Override
    public char reduce(char identity, CharBinaryOperator op) {
        char result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsChar(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalChar reduce(CharBinaryOperator op) {
        if (fromIndex == toIndex) {
            return OptionalChar.empty();
        }

        char result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsChar(result, elements[i]);
        }

        return OptionalChar.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public char head() {
        if (fromIndex == toIndex) {
            throw new NoSuchElementException();
        }

        return elements[fromIndex];
    }

    @Override
    public CharStream tail() {
        if (fromIndex == toIndex) {
            throw new IllegalStateException();
        }

        return new ArrayCharStream(elements, fromIndex + 1, toIndex, closeHandlers, sorted);
    }

    @Override
    public CharStream head2() {
        if (fromIndex == toIndex) {
            throw new IllegalStateException();
        }

        return new ArrayCharStream(elements, fromIndex, toIndex - 1, closeHandlers, sorted);
    }

    @Override
    public char tail2() {
        if (fromIndex == toIndex) {
            throw new NoSuchElementException();
        }

        return elements[toIndex - 1];
    }

    @Override
    public OptionalChar min() {
        if (fromIndex == toIndex) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements[fromIndex]);
        }

        return OptionalChar.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalChar max() {
        if (fromIndex == toIndex) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements[toIndex - 1]);
        }

        return OptionalChar.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalChar kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements[toIndex - k]);
        }

        return OptionalChar.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public Long sum() {
        return N.sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public CharStream reverse() {
        return new IteratorCharStream(new ImmutableCharIterator() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public char next() {
                if (cursor <= fromIndex) {
                    throw new NoSuchElementException();
                }
                return elements[--cursor];
            }

            @Override
            public long count() {
                return cursor - fromIndex;
            }

            @Override
            public void skip(long n) {
                cursor = n < cursor - fromIndex ? cursor - (int) n : fromIndex;
            }

            @Override
            public char[] toArray() {
                final char[] a = new char[cursor - fromIndex];

                for (int i = 0, len = a.length; i < len; i++) {
                    a[i] = elements[cursor - i - 1];
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public CharSummaryStatistics summarize() {
        final CharSummaryStatistics result = new CharSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalChar findFirst(final CharPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalChar.of(elements[i]);
            }
        }

        return OptionalChar.empty();
    }

    @Override
    public OptionalChar findLast(final CharPredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalChar.of(elements[i]);
            }
        }

        return OptionalChar.empty();
    }

    @Override
    public IntStream asIntStream() {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Character> boxed() {
        return new IteratorStream<Character>(iterator(), closeHandlers, sorted, sorted ? CHAR_COMPARATOR : null);
    }

    @Override
    public CharStream cached() {
        return this;
    }

    @Override
    public ImmutableIterator<Character> iterator() {
        return new ImmutableIterator<Character>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Character next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) Character.valueOf(elements[cursor++]);
                }

                return a;
            }
        };
    }

    @Override
    public ImmutableCharIterator charIterator() {
        return new ImmutableCharIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public char[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public CharStream parallel(int maxThreadNum, com.landawn.abacus.util.stream.BaseStream.Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayCharStream(elements, fromIndex, toIndex, newCloseHandlers, sorted);
    }
}
