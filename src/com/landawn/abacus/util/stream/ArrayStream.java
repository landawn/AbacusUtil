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
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Executor;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class ArrayStream<T> extends AbstractStream<T> {
    final T[] elements;
    final int fromIndex;
    final int toIndex;

    ArrayStream(final T[] values) {
        this(values, 0, values.length);
    }

    ArrayStream(final T[] values, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayStream(final T[] values, final boolean sorted, final Comparator<? super T> comparator, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, comparator, closeHandlers);
    }

    ArrayStream(final T[] values, final int fromIndex, final int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayStream(final T[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, null, closeHandlers);
    }

    ArrayStream(final T[] values, final int fromIndex, final int toIndex, final boolean sorted, Comparator<? super T> comparator,
            final Collection<Runnable> closeHandlers) {
        super(sorted, comparator, closeHandlers);

        N.checkFromToIndex(fromIndex, toIndex, N.len(values));

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        return newStream(new ObjIteratorEx<T>() {
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
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, sorted, cmp);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        return newStream(new ObjIteratorEx<T>() {
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
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, sorted, cmp);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        return newStream(new ObjIteratorEx<T>() {
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
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, sorted, cmp);
    }

    @Override
    public Stream<T> step(final long step) {
        N.checkArgPositive(step, "step");

        if (step == 1 || fromIndex == toIndex) {
            return this;
        }

        return newStream(new ObjIteratorEx<T>() {
            private final int stepp = (int) N.min(step, Integer.MAX_VALUE);
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final T res = elements[cursor];
                cursor = cursor > toIndex - stepp ? toIndex : cursor + stepp;
                return res;
            }

            @Override
            public long count() {
                return (toIndex - cursor) % stepp == 0 ? (toIndex - cursor) / stepp : ((toIndex - cursor) / stepp) + 1;
            }

            @Override
            public void skip(long n) {
                if (n > 0) {
                    cursor = n <= (toIndex - cursor) / stepp ? cursor + (int) (n * stepp) : toIndex;
                }
            }

            @Override
            public <A> A[] toArray(A[] a) {
                final int len = (int) count();
                a = a.length >= len ? a : (A[]) N.newArray(a.getClass().getComponentType(), len);

                for (int i = 0; i < len; i++, cursor += stepp) {
                    a[i] = (A) elements[cursor];
                }

                return a;
            }
        }, sorted, cmp);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        return newStream(new ObjIteratorEx<R>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public R next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.apply(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) mapper.apply(elements[cursor++]);
                }

                return a;
            }
        }, false, null);
    }

    //    @Override
    //    public <R> Stream<R> biMap(final BiFunction<? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
    //        return newStream(new ObjIteratorEx<R>() {
    //            private final int atLeast = ignoreNotPaired ? 2 : 1;
    //            private int cursor = fromIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return toIndex - cursor >= atLeast;
    //            }
    //
    //            @Override
    //            public R next() {
    //                if (toIndex - cursor < atLeast) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++]);
    //            }
    //
    //            //            @Override
    //            //            public long count() {
    //            //                return (toIndex - cursor) / 2 + (ignoreNotPaired || (toIndex - cursor) % 2 == 0 ? 0 : 1);
    //            //            }
    //            //
    //            //            @Override
    //            //            public void skip(long n) {
    //            //                cursor = n < count() ? cursor + (int) n * 2 : toIndex;
    //            //            }
    //
    //            @Override
    //            public <A> A[] toArray(A[] a) {
    //                final int len = (int) count();
    //                a = a.length >= len ? a : (A[]) N.newArray(a.getClass().getComponentType(), len);
    //
    //                for (int i = 0, len2 = (toIndex - cursor) / 2; i < len2; i++) {
    //                    a[i] = (A) mapper.apply(elements[cursor++], elements[cursor++]);
    //                }
    //
    //                if (cursor < toIndex) {
    //                    a[len - 1] = (A) mapper.apply(elements[cursor++], null);
    //                }
    //
    //                return a;
    //            }
    //        }, closeHandlers);
    //    }
    //
    //    @Override
    //    public <R> Stream<R> triMap(final TriFunction<? super T, ? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
    //        return newStream(new ObjIteratorEx<R>() {
    //            private final int atLeast = ignoreNotPaired ? 3 : 1;
    //            private int cursor = fromIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return toIndex - cursor >= atLeast;
    //            }
    //
    //            @Override
    //            public R next() {
    //                if (toIndex - cursor < atLeast) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++], cursor == toIndex ? null : elements[cursor++]);
    //            }
    //
    //            //            @Override
    //            //            public long count() {
    //            //                return (toIndex - cursor) / 3 + (ignoreNotPaired || (toIndex - cursor) % 3 == 0 ? 0 : 1);
    //            //            }
    //            //
    //            //            @Override
    //            //            public void skip(long n) {
    //            //                cursor = n < count() ? cursor + (int) n * 3 : toIndex;
    //            //            }
    //
    //            @Override
    //            public <A> A[] toArray(A[] a) {
    //                final int len = (int) count();
    //                a = a.length >= len ? a : (A[]) N.newArray(a.getClass().getComponentType(), len);
    //
    //                for (int i = 0, len2 = (toIndex - cursor) / 3; i < len2; i++) {
    //                    a[i] = (A) mapper.apply(elements[cursor++], elements[cursor++], elements[cursor++]);
    //                }
    //
    //                if (cursor < toIndex) {
    //                    a[len - 1] = (A) mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++], null);
    //                }
    //
    //                return a;
    //            }
    //        }, closeHandlers);
    //    }

    @Override
    public <R> Stream<R> slidingMap(final BiFunction<? super T, ? super T, R> mapper, final int increment, final boolean ignoreNotPaired) {
        final int windowSize = 2;

        N.checkArgument(increment > 0, "'increment'=%s must not be less than 1", increment);

        return newStream(new ObjIteratorEx<R>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return ignoreNotPaired ? toIndex - cursor >= windowSize : cursor < toIndex;
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final R result = mapper.apply(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            //            @Override
            //            public long count() {
            //                if (toIndex - cursor == 0) {
            //                    return 0;
            //                } else if (toIndex - cursor <= windowSize) {
            //                    return 1;
            //                } else {
            //                    final long len = (toIndex - cursor) - windowSize;
            //                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
            //                }
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                if (n > 0) {
            //                    if (n >= count()) {
            //                        cursor = toIndex;
            //                    } else {
            //                        cursor += n * increment;
            //                    }
            //                }
            //            }

        }, false, null);
    }

    @Override
    public <R> Stream<R> slidingMap(final TriFunction<? super T, ? super T, ? super T, R> mapper, final int increment, final boolean ignoreNotPaired) {
        final int windowSize = 3;

        N.checkArgument(increment > 0, "'increment'=%s must not be less than 1", increment);

        return newStream(new ObjIteratorEx<R>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return ignoreNotPaired ? toIndex - cursor >= windowSize : cursor < toIndex;
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final R result = mapper.apply(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null,
                        cursor < toIndex - 2 ? elements[cursor + 2] : null);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            //            @Override
            //            public long count() {
            //                if (toIndex - cursor == 0) {
            //                    return 0;
            //                } else if (toIndex - cursor <= windowSize) {
            //                    return 1;
            //                } else {
            //                    final long len = (toIndex - cursor) - windowSize;
            //                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
            //                }
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                if (n > 0) {
            //                    if (n >= count()) {
            //                        cursor = toIndex;
            //                    } else {
            //                        cursor += n * increment;
            //                    }
            //                }
            //            }

        }, false, null);
    }

    @Override
    public Stream<T> mapFirst(final Function<? super T, ? extends T> mapperForFirst) {
        N.checkArgNotNull(mapperForFirst);

        if (fromIndex == toIndex) {
            return this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForFirst);
        } else {
            return newStream(new ObjIteratorEx<T>() {
                private int cursor = fromIndex;

                @Override
                public boolean hasNext() {
                    return cursor < toIndex;
                }

                @Override
                public T next() {
                    if (cursor >= toIndex) {
                        throw new NoSuchElementException();
                    }

                    if (cursor == fromIndex) {
                        return mapperForFirst.apply(elements[cursor++]);
                    } else {
                        return elements[cursor++];
                    }
                }

                //            @Override
                //            public long count() {
                //                return toIndex - cursor;
                //            }
                //
                //            @Override
                //            public void skip(long n) {
                //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                //            }

                @Override
                public long count() {
                    if (hasNext()) {
                        next();
                        return toIndex - cursor + 1;
                    }

                    return 0;
                }

                @Override
                public void skip(long n) {
                    if (hasNext()) {
                        next();
                        n -= 1;
                        cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                    }
                }

                @Override
                public <A> A[] toArray(A[] a) {
                    a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                    for (int i = 0, len = toIndex - cursor; i < len; i++) {
                        if (cursor == fromIndex) {
                            a[i] = (A) mapperForFirst.apply(elements[cursor++]);
                        } else {
                            a[i] = (A) elements[cursor++];
                        }
                    }

                    return a;
                }
            }, false, null);
        }
    }

    @Override
    public <R> Stream<R> mapFirstOrElse(final Function<? super T, ? extends R> mapperForFirst, final Function<? super T, ? extends R> mapperForElse) {
        N.checkArgNotNull(mapperForFirst);
        N.checkArgNotNull(mapperForElse);

        if (fromIndex == toIndex) {
            return (Stream<R>) this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForFirst);
        } else {
            return newStream(new ObjIteratorEx<R>() {
                private int cursor = fromIndex;

                @Override
                public boolean hasNext() {
                    return cursor < toIndex;
                }

                @Override
                public R next() {
                    if (cursor >= toIndex) {
                        throw new NoSuchElementException();
                    }

                    if (cursor == fromIndex) {
                        return mapperForFirst.apply(elements[cursor++]);
                    } else {
                        return mapperForElse.apply(elements[cursor++]);
                    }
                }

                //            @Override
                //            public long count() {
                //                return toIndex - cursor;
                //            }
                //
                //            @Override
                //            public void skip(long n) {
                //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                //            }

                @Override
                public <A> A[] toArray(A[] a) {
                    a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                    for (int i = 0, len = toIndex - cursor; i < len; i++) {
                        if (cursor == fromIndex) {
                            a[i] = (A) mapperForFirst.apply(elements[cursor++]);
                        } else {
                            a[i] = (A) mapperForElse.apply(elements[cursor++]);
                        }
                    }

                    return a;
                }
            }, false, null);
        }
    }

    @Override
    public Stream<T> mapLast(final Function<? super T, ? extends T> mapperForLast) {
        N.checkArgNotNull(mapperForLast);

        if (fromIndex == toIndex) {
            return this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForLast);
        } else {
            return newStream(new ObjIteratorEx<T>() {
                private int last = toIndex - 1;
                private int cursor = fromIndex;

                @Override
                public boolean hasNext() {
                    return cursor < toIndex;
                }

                @Override
                public T next() {
                    if (cursor >= toIndex) {
                        throw new NoSuchElementException();
                    }

                    if (cursor == last) {
                        return mapperForLast.apply(elements[cursor++]);
                    } else {
                        return elements[cursor++];
                    }
                }

                //            @Override
                //            public long count() {
                //                return toIndex - cursor;
                //            }
                //
                //            @Override
                //            public void skip(long n) {
                //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                //            }

                @Override
                public <A> A[] toArray(A[] a) {
                    a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                    for (int i = 0, len = toIndex - cursor; i < len; i++) {
                        if (cursor == last) {
                            a[i] = (A) mapperForLast.apply(elements[cursor++]);
                        } else {
                            a[i] = (A) elements[cursor++];
                        }
                    }

                    return a;
                }
            }, false, null);
        }
    }

    @Override
    public <R> Stream<R> mapLastOrElse(final Function<? super T, ? extends R> mapperForLast, final Function<? super T, ? extends R> mapperForElse) {
        N.checkArgNotNull(mapperForLast);
        N.checkArgNotNull(mapperForElse);

        return newStream(new ObjIteratorEx<R>() {
            private int last = toIndex - 1;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public R next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                if (cursor == last) {
                    return mapperForLast.apply(elements[cursor++]);
                } else {
                    return mapperForElse.apply(elements[cursor++]);
                }
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    if (cursor == last) {
                        a[i] = (A) mapperForLast.apply(elements[cursor++]);
                    } else {
                        a[i] = (A) mapperForElse.apply(elements[cursor++]);
                    }
                }

                return a;
            }
        }, false, null);
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        return newStream(new CharIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char nextChar() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsChar(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public char[] toArray() {
                final char[] a = new char[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsChar(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        return newStream(new ByteIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public byte nextByte() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsByte(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public byte[] toArray() {
                final byte[] a = new byte[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsByte(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        return newStream(new ShortIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsShort(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public short[] toArray() {
                final short[] a = new short[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsShort(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        return newStream(new IntIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int nextInt() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsInt(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsInt(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        return newStream(new LongIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long nextLong() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsLong(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public long[] toArray() {
                final long[] a = new long[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsLong(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        return newStream(new FloatIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float nextFloat() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsFloat(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public float[] toArray() {
                final float[] a = new float[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsFloat(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        return newStream(new DoubleIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsDouble(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public double[] toArray() {
                final double[] a = new double[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsDouble(elements[cursor++]);
                }

                return a;
            }
        }, false);
    }

    @Override
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
        final ObjIteratorEx<R> iter = new ObjIteratorEx<R>() {
            private int cursor = fromIndex;
            private Iterator<? extends R> cur = null;
            private Stream<? extends R> s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public R next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorStream<>(iter, newCloseHandlers);
    }

    @Override
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
        final CharIteratorEx iter = new CharIteratorEx() {
            private int cursor = fromIndex;
            private CharIterator cur = null;
            private CharStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char nextChar() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextChar();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorCharStream(iter, newCloseHandlers);
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        final ByteIteratorEx iter = new ByteIteratorEx() {
            private int cursor = fromIndex;
            private ByteIterator cur = null;
            private ByteStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public byte nextByte() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextByte();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorByteStream(iter, newCloseHandlers);
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        final ShortIteratorEx iter = new ShortIteratorEx() {
            private int cursor = fromIndex;
            private ShortIterator cur = null;
            private ShortStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short nextShort() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextShort();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorShortStream(iter, newCloseHandlers);
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        final IntIteratorEx iter = new IntIteratorEx() {
            private int cursor = fromIndex;
            private IntIterator cur = null;
            private IntStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int nextInt() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextInt();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorIntStream(iter, newCloseHandlers);
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        final LongIteratorEx iter = new LongIteratorEx() {
            private int cursor = fromIndex;
            private LongIterator cur = null;
            private LongStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long nextLong() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextLong();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorLongStream(iter, newCloseHandlers);
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        final FloatIteratorEx iter = new FloatIteratorEx() {
            private int cursor = fromIndex;
            private FloatIterator cur = null;
            private FloatStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float nextFloat() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextFloat();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorFloatStream(iter, newCloseHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        final DoubleIteratorEx iter = new DoubleIteratorEx() {
            private int cursor = fromIndex;
            private DoubleIterator cur = null;
            private DoubleStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements[cursor++]);

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Set<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double nextDouble() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextDouble();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorDoubleStream(iter, newCloseHandlers);
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<Stream<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayStream<>(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex), sorted, cmp, null);
            }

            @Override
            public long count() {
                final long len = toIndex - cursor;
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                final long len = toIndex - cursor;
                cursor = n <= len / size ? cursor + (int) n * size : toIndex;
            }
        }, false, null);
    }

    @Override
    public Stream<List<T>> splitToList(final int size) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<List<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return N.asList(N.copyOfRange(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex)));
            }

            @Override
            public long count() {
                final long len = toIndex - cursor;
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                final long len = toIndex - cursor;
                cursor = n <= len / size ? cursor + (int) n * size : toIndex;
            }
        }, false, null);
    }

    @Override
    public <C extends Collection<T>> Stream<C> split(final int size, final IntFunction<C> collectionSupplier) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<C>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public C next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final C result = collectionSupplier.apply(toIndex - cursor > size ? size : toIndex - cursor);

                for (int i = cursor, to = (cursor = size < toIndex - cursor ? cursor + size : toIndex); i < to; i++) {
                    result.add(elements[i]);
                }

                return result;
            }

            @Override
            public long count() {
                final long len = toIndex - cursor;
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                final long len = toIndex - cursor;
                cursor = n <= len / size ? cursor + (int) n * size : toIndex;
            }
        }, false, null);
    }

    @Override
    public Stream<Stream<T>> split(final Predicate<? super T> predicate) {
        return newStream(new ObjIteratorEx<Stream<T>>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from]);
                        cursor++;
                    } else if (predicate.test(elements[cursor]) == preCondition) {
                        cursor++;
                    } else {

                        break;
                    }
                }

                return new ArrayStream<>(elements, from, cursor, sorted, cmp, null);
            }
        }, false, null);
    }

    @Override
    public Stream<List<T>> splitToList(final Predicate<? super T> predicate) {
        return newStream(new ObjIteratorEx<List<T>>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from]);
                        cursor++;
                    } else if (predicate.test(elements[cursor]) == preCondition) {
                        cursor++;
                    } else {

                        break;
                    }
                }

                return N.asList(N.copyOfRange(elements, from, cursor));
            }
        }, false, null);
    }

    @Override
    public <C extends Collection<T>> Stream<C> split(final Predicate<? super T> predicate, final Supplier<C> collectionSupplier) {
        return newStream(new ObjIteratorEx<C>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public C next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final C result = collectionSupplier.get();

                while (cursor < toIndex) {
                    if (result.size() == 0) {
                        preCondition = predicate.test(elements[cursor]);
                        result.add(elements[cursor]);
                        cursor++;
                    } else if (predicate.test(elements[cursor]) == preCondition) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {

                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public <U> Stream<Stream<T>> split(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return newStream(new ObjIteratorEx<Stream<T>>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from], seed);
                        cursor++;
                    } else if (predicate.test(elements[cursor], seed) == preCondition) {
                        cursor++;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return new ArrayStream<>(elements, from, cursor, sorted, cmp, null);
            }
        }, false, null);
    }

    @Override
    public <U> Stream<List<T>> splitToList(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return newStream(new ObjIteratorEx<List<T>>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from], seed);
                        cursor++;
                    } else if (predicate.test(elements[cursor], seed) == preCondition) {
                        cursor++;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return N.asList(N.copyOfRange(elements, from, cursor));
            }
        }, false, null);
    }

    @Override
    public <U, C extends Collection<T>> Stream<C> split(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super U> seedUpdate,
            final Supplier<C> collectionSupplier) {
        return newStream(new ObjIteratorEx<C>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public C next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final C result = collectionSupplier.get();

                while (cursor < toIndex) {
                    if (result.size() == 0) {
                        preCondition = predicate.test(elements[cursor], seed);
                        result.add(elements[cursor]);
                        cursor++;
                    } else if (predicate.test(elements[cursor], seed) == preCondition) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public Stream<Stream<T>> splitAt(final int n) {
        N.checkArgNotNegative(n, "n");

        final Stream<T>[] a = new Stream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? (Stream<T>) Stream.empty() : new ArrayStream<>(elements, fromIndex, middleIndex, sorted, cmp, null);
        a[1] = middleIndex == toIndex ? (Stream<T>) Stream.empty() : new ArrayStream<>(elements, middleIndex, toIndex, sorted, cmp, null);

        return newStream(a, false, null);
    }

    @Override
    public Stream<Stream<T>> sliding(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<Stream<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final Stream<T> result = new ArrayStream<>(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex, sorted, cmp, null);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            @Override
            public long count() {
                if (toIndex - cursor == 0) {
                    return 0;
                } else if (toIndex - cursor <= windowSize) {
                    return 1;
                } else {
                    final long len = (toIndex - cursor) - windowSize;
                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
                }
            }

            @Override
            public void skip(long n) {
                if (n > 0) {
                    if (n >= count()) {
                        cursor = toIndex;
                    } else {
                        cursor += n * increment;
                    }
                }
            }
        }, false, null);
    }

    @Override
    public Stream<List<T>> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<List<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final List<T> result = N.asList(N.copyOfRange(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex));

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            @Override
            public long count() {
                if (toIndex - cursor == 0) {
                    return 0;
                } else if (toIndex - cursor <= windowSize) {
                    return 1;
                } else {
                    final long len = (toIndex - cursor) - windowSize;
                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
                }
            }

            @Override
            public void skip(long n) {
                if (n > 0) {
                    if (n >= count()) {
                        cursor = toIndex;
                    } else {
                        cursor += n * increment;
                    }
                }
            }

        }, false, null);
    }

    @Override
    public <C extends Collection<T>> Stream<C> sliding(final int windowSize, final int increment, final IntFunction<C> collectionSupplier) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<C>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public C next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final C result = collectionSupplier.apply(windowSize < toIndex - cursor ? windowSize : toIndex - cursor);

                for (int i = cursor, to = windowSize < toIndex - cursor ? cursor + windowSize : toIndex; i < to; i++) {
                    result.add(elements[i]);
                }

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            @Override
            public long count() {
                if (toIndex - cursor == 0) {
                    return 0;
                } else if (toIndex - cursor <= windowSize) {
                    return 1;
                } else {
                    final long len = (toIndex - cursor) - windowSize;
                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
                }
            }

            @Override
            public void skip(long n) {
                if (n > 0) {
                    if (n >= count()) {
                        cursor = toIndex;
                    } else {
                        cursor += n * increment;
                    }
                }
            }

        }, false, null);
    }

    @Override
    public Stream<T> top(final int n, final Comparator<? super T> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return newStream(elements, toIndex - n, toIndex, sorted, cmp);
        }

        return newStream(new ObjIteratorEx<T>() {
            private boolean initialized = false;
            private T[] aar;
            private int cursor = 0;
            private int to;

            @Override
            public boolean hasNext() {
                if (initialized == false) {
                    init();
                }

                return cursor < to;
            }

            @Override
            public T next() {
                if (initialized == false) {
                    init();
                }

                if (cursor >= to) {
                    throw new NoSuchElementException();
                }

                return aar[cursor++];
            }

            @Override
            public long count() {
                if (initialized == false) {
                    init();
                }

                return to - cursor;
            }

            @Override
            public void skip(long n) {
                if (initialized == false) {
                    init();
                }

                cursor = n > to - cursor ? to : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                if (initialized == false) {
                    init();
                }

                a = a.length >= (to - cursor) ? a : (A[]) N.newArray(a.getClass().getComponentType(), (to - cursor));

                N.copy(aar, cursor, a, 0, to - cursor);

                return a;
            }

            private void init() {
                if (initialized == false) {
                    initialized = true;
                    aar = (T[]) N.top(elements, fromIndex, toIndex, n, comparator).toArray();
                    to = aar.length;
                }
            }
        }, false, null);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        return newStream(new ObjIteratorEx<T>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                action.accept(elements[cursor]);

                return elements[cursor++];
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    action.accept(elements[cursor]);

                    a[i] = (A) elements[cursor++];
                }

                return a;
            }
        }, sorted, cmp);
    }

    @Override
    public Stream<T> limit(long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

        if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return newStream(elements, fromIndex, (int) (fromIndex + maxSize), sorted, cmp);
    }

    @Override
    public Stream<T> skip(long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return newStream(elements, toIndex, toIndex, sorted, cmp);
        } else {
            return newStream(elements, (int) (fromIndex + n), toIndex, sorted, cmp);
        }
    }

    @Override
    public <E extends Exception> void forEach(Try.Consumer<? super T, E> action) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public <E extends Exception> void forEachPair(final Try.BiConsumer<? super T, ? super T, E> action, final int increment) throws E {
        final int windowSize = 2;

        N.checkArgument(increment > 0, "'increment'=%s must not be less than 1", increment);

        int cursor = fromIndex;

        while (cursor < toIndex) {
            action.accept(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null);

            cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;
        }
    }

    @Override
    public <E extends Exception> void forEachTriple(final Try.TriConsumer<? super T, ? super T, ? super T, E> action, final int increment) throws E {
        final int windowSize = 3;

        N.checkArgument(increment > 0, "'increment'=%s must not be less than 1", increment);

        int cursor = fromIndex;

        while (cursor < toIndex) {
            action.accept(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null, cursor < toIndex - 2 ? elements[cursor + 2] : null);

            cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;
        }
    }

    @Override
    public Object[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    <A> A[] toArray(A[] a) {
        if (a.length < (toIndex - fromIndex)) {
            a = N.newArray(a.getClass().getComponentType(), toIndex - fromIndex);
        }

        N.copy(elements, fromIndex, a, 0, toIndex - fromIndex);

        return a;
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return toArray(generator.apply(toIndex - fromIndex));
    }

    @Override
    public List<T> toList() {
        // return N.asList(N.copyOfRange(elements, fromIndex, toIndex));

        if (fromIndex == 0 && toIndex == elements.length && elements.length > 9) {
            return new ArrayList<>(Arrays.asList(elements));
        }

        final List<T> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>(N.initHashCapacity(toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <C extends Collection<T>> C toCollection(Supplier<? extends C> supplier) {
        final C result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>(N.initHashCapacity(toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier) {
        final Multiset<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset() {
        final LongMultiset<T> result = new LongMultiset<>(N.initHashCapacity(toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier) {
        final LongMultiset<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, V, M extends Map<K, V>> M toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends V> valueMapper,
            BinaryOperator<V> mergeFunction, Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyExtractor.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final Function<A, D> downstreamFinisher = downstream.finisher();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = N.checkArgNotNull(classifier.apply(elements[i]), "element cannot be mapped to a null key");

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
                return (A) downstreamFinisher.apply(v);
            }
        };

        Collectors.replaceAll(intermediate, function);

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>, M extends Multimap<K, U, V>> M toMultimap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyExtractor.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
    }

    @Override
    public Optional<T> first() {
        return fromIndex < toIndex ? Optional.of(elements[fromIndex]) : Optional.<T> empty();
    }

    @Override
    public Optional<T> last() {
        return fromIndex < toIndex ? Optional.of(elements[toIndex - 1]) : Optional.<T> empty();
    }

    @Override
    public T reduce(T identity, BinaryOperator<T> accumulator) {
        T result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return result;
    }

    @Override
    public Optional<T> reduce(BinaryOperator<T> accumulator) {
        if (fromIndex == toIndex) {
            return Optional.empty();
        }

        T result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return Optional.of(result);
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {
        U result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(container, elements[i]);
        }

        return collector.finisher().apply(container);
    }

    @Override
    public Optional<T> head() {
        return fromIndex == toIndex ? Optional.<T> empty() : Optional.of(elements[fromIndex]);
    }

    @Override
    public Stream<T> tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return newStream(elements, fromIndex + 1, toIndex, sorted, cmp);
    }

    //    @Override
    //    public Stream<T> headd() {
    //        if (fromIndex == toIndex) {
    //            return this;
    //        }
    //
    //        return newStream(elements, fromIndex, toIndex - 1, sorted, cmp);
    //    }
    //
    //    @Override
    //    public Optional<T> taill() {
    //        return fromIndex == toIndex ? Optional.<T> empty() : Optional.of(elements[toIndex - 1]);
    //    }

    @Override
    public Stream<T> last(final int n) {
        N.checkArgNotNegative(n, "n");

        if (toIndex - fromIndex <= n) {
            return this;
        }

        return newStream(elements, toIndex - n, toIndex, sorted, cmp);
    }

    @Override
    public Stream<T> skipLast(int n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        return newStream(elements, fromIndex, N.max(fromIndex, toIndex - n), sorted, cmp);
    }

    @Override
    public Optional<T> min(Comparator<? super T> comparator) {
        if (fromIndex == toIndex) {
            return Optional.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return Optional.of(elements[fromIndex]);
        }

        return Optional.of(N.min(elements, fromIndex, toIndex, comparator));
    }

    @Override
    public Optional<T> max(Comparator<? super T> comparator) {
        if (fromIndex == toIndex) {
            return Optional.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return Optional.of(elements[toIndex - 1]);
        }

        return Optional.of(N.max(elements, fromIndex, toIndex, comparator));
    }

    @Override
    public Optional<T> kthLargest(int k, Comparator<? super T> comparator) {
        N.checkArgPositive(k, "k");

        if (k > toIndex - fromIndex) {
            return Optional.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return Optional.of(elements[toIndex - k]);
        }

        return Optional.of(N.kthLargest(elements, fromIndex, toIndex, k, comparator));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public Stream<T> reversed() {
        return newStream(new ObjIteratorEx<T>() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public T next() {
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
            public <A> A[] toArray(A[] a) {
                a = a.length >= cursor - fromIndex ? a : (A[]) N.newArray(a.getClass().getComponentType(), cursor - fromIndex);

                for (int i = 0, len = cursor - fromIndex; i < len; i++) {
                    a[i] = (A) elements[cursor - i - 1];
                }

                return a;
            }
        }, false, null);
    }

    @Override
    public Stream<T> rotated(final int distance) {
        if (distance == 0 || toIndex - fromIndex <= 1 || distance % (toIndex - fromIndex) == 0) {
            return this;
        }

        return newStream(new ObjIteratorEx<T>() {
            private final int len = toIndex - fromIndex;
            private int start;
            private int cnt = 0;

            {

                start = distance % len;

                if (start < 0) {
                    start += len;
                }

                start = len - start;
            }

            @Override
            public boolean hasNext() {
                return cnt < len;
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return elements[((start + cnt++) % len) + fromIndex];
            }

            @Override
            public long count() {
                return len - cnt;
            }

            @Override
            public void skip(long n) {
                cnt = n < len - cnt ? cnt + (int) n : len;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= len - cnt ? a : (A[]) N.newArray(a.getClass().getComponentType(), len - cnt);

                for (int i = cnt; i < len; i++) {
                    a[i - cnt] = (A) elements[((start + i) % len) + fromIndex];
                }

                return a;
            }
        }, false, null);
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.Predicate<? super T, E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.Predicate<? super T, E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.Predicate<? super T, E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> Optional<T> findFirst(final Try.Predicate<? super T, E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return Optional.of(elements[i]);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    @Override
    public <E extends Exception> Optional<T> findLast(final Try.Predicate<? super T, E> predicate) throws E {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return Optional.of(elements[i]);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    @Override
    public Stream<T> cached() {
        return this;
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
     * 
     * @param stream
     * @param queueSize Default value is 8
     * @return
     */
    @Override
    public Stream<T> queued(int queueSize) {
        return this;
    }

    @Override
    ObjIteratorEx<T> iteratorEx() {
        return ObjIteratorEx.of(elements, fromIndex, toIndex);
    }

    @Override
    public Stream<T> parallel(final int maxThreadNum, final Splitor splitor) {
        return new ParallelArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, maxThreadNum, checkSplitor(splitor), asyncExecutor(), closeHandlers);
    }

    @Override
    public Stream<T> parallel(final int maxThreadNum, final Executor executor) {
        return new ParallelArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, maxThreadNum, splitor(), createAsyncExecutor(executor), closeHandlers);
    }

    @Override
    public java.util.stream.Stream<T> toJdkStream() {
        java.util.stream.Stream<T> s = java.util.stream.Stream.of(elements);

        if (fromIndex > 0) {
            s = s.skip(fromIndex);
        }

        if (toIndex < elements.length) {
            s = s.limit(toIndex - fromIndex);
        }

        if (this.isParallel()) {
            s = s.parallel();
        }

        if (N.notNullOrEmpty(closeHandlers)) {
            s = s.onClose(() -> close(closeHandlers));
        }

        return s;
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, newCloseHandlers);
    }
}
