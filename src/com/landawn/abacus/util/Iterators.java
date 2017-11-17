/*
 * Copyright (c) 2017, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.TriFunction;

/**
 * 
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 */
public final class Iterators {
    private Iterators() {
        // singleton.
    }

    public static <T> List<T> toList(final Iterator<? extends T> iter) {
        if (iter == null) {
            return new ArrayList<>();
        }

        final List<T> result = new ArrayList<>();

        while (iter.hasNext()) {
            result.add(iter.next());
        }

        return result;
    }

    public static <T> Set<T> toSet(final Iterator<? extends T> iter) {
        if (iter == null) {
            return new HashSet<>();
        }

        final Set<T> result = new HashSet<>();

        while (iter.hasNext()) {
            result.add(iter.next());
        }

        return result;
    }

    public static <T, C extends Collection<T>> C toCollection(final Iterator<? extends T> iter, final Supplier<C> collectionFactory) {
        final C c = collectionFactory.get();

        if (iter == null) {
            return c;
        }

        while (iter.hasNext()) {
            c.add(iter.next());
        }

        return c;
    }

    public static <T, K> Map<K, T> toMap(final Iterator<? extends T> iter, final Function<? super T, K> keyExactor) {
        if (iter == null) {
            return new HashMap<>();
        }

        final Map<K, T> result = new HashMap<>();
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();
            result.put(keyExactor.apply(next), next);
        }

        return result;
    }

    public static <T> ObjIterator<T> repeat(final T e, final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (n == 0) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private int cnt = n;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public T next() {
                if (cnt <= 0) {
                    throw new NoSuchElementException();
                }

                cnt--;
                return e;
            }
        };
    };

    public static <T> ObjIterator<T> repeat(final Collection<T> c, final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (N.isNullOrEmpty(c) || n == 0) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private Iterator<T> iter = null;
            private int cnt = n;

            @Override
            public boolean hasNext() {
                return cnt > 0 || (iter != null && iter.hasNext());
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (iter == null || iter.hasNext() == false) {
                    iter = c.iterator();
                    cnt--;
                }

                return iter.next();
            }
        };
    };

    public static <T> ObjIterator<T> repeatToSize(final Collection<T> c, final int size) {
        N.checkArgument(size >= 0, "'size' can't be negative: %s", size);
        N.checkArgument(size == 0 || N.notNullOrEmpty(c), "Collection can't be empty or null when size > 0");

        if (N.isNullOrEmpty(c) || size == 0) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private Iterator<T> iter = null;
            private int cnt = size;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (iter == null || iter.hasNext() == false) {
                    iter = c.iterator();
                }

                cnt--;

                return iter.next();
            }
        };
    };

    public static <T> ObjIterator<T> nRepeat(final Collection<T> c, final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (N.isNullOrEmpty(c) || n == 0) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private Iterator<T> iter = null;
            private T next = null;
            private int cnt = n;

            @Override
            public boolean hasNext() {
                return cnt > 0 || (iter != null && iter.hasNext());
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (iter == null) {
                    iter = c.iterator();
                    next = iter.next();
                } else if (cnt <= 0) {
                    next = iter.next();
                    cnt = n;
                }

                cnt--;

                return next;
            }
        };
    };

    public static <T> ObjIterator<T> nRepeatToSize(final Collection<T> c, final int size) {
        N.checkArgument(size >= 0, "'size' can't be negative: %s", size);
        N.checkArgument(size == 0 || N.notNullOrEmpty(c), "Collection can't be empty or null when size > 0");

        if (N.isNullOrEmpty(c) || size == 0) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private final int n = size / c.size();
            private int mod = size % c.size();

            private Iterator<T> iter = null;
            private T next = null;
            private int cnt = mod-- > 0 ? n + 1 : n;

            @Override
            public boolean hasNext() {
                return cnt > 0 || ((n > 0 || mod > 0) && (iter != null && iter.hasNext()));
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (iter == null) {
                    iter = c.iterator();
                    next = iter.next();
                } else if (cnt <= 0) {
                    next = iter.next();
                    cnt = mod-- > 0 ? n + 1 : n;
                }

                cnt--;

                return next;
            }
        };
    };

    @SafeVarargs
    public static BooleanIterator concat(final boolean[]... a) {
        if (N.isNullOrEmpty(a)) {
            return BooleanIterator.EMPTY;
        }

        return new BooleanIterator() {
            private final Iterator<boolean[]> iter = Arrays.asList(a).iterator();
            private boolean[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public boolean nextBoolean() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static ShortIterator concat(final short[]... a) {
        if (N.isNullOrEmpty(a)) {
            return ShortIterator.EMPTY;
        }

        return new ShortIterator() {
            private final Iterator<short[]> iter = Arrays.asList(a).iterator();
            private short[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public short nextShort() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static ByteIterator concat(final byte[]... a) {
        if (N.isNullOrEmpty(a)) {
            return ByteIterator.EMPTY;
        }

        return new ByteIterator() {
            private final Iterator<byte[]> iter = Arrays.asList(a).iterator();
            private byte[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public byte nextByte() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static IntIterator concat(final int[]... a) {
        if (N.isNullOrEmpty(a)) {
            return IntIterator.EMPTY;
        }

        return new IntIterator() {
            private final Iterator<int[]> iter = Arrays.asList(a).iterator();
            private int[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public int nextInt() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static LongIterator concat(final long[]... a) {
        if (N.isNullOrEmpty(a)) {
            return LongIterator.EMPTY;
        }

        return new LongIterator() {
            private final Iterator<long[]> iter = Arrays.asList(a).iterator();
            private long[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public long nextLong() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static FloatIterator concat(final float[]... a) {
        if (N.isNullOrEmpty(a)) {
            return FloatIterator.EMPTY;
        }

        return new FloatIterator() {
            private final Iterator<float[]> iter = Arrays.asList(a).iterator();
            private float[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public float nextFloat() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static DoubleIterator concat(final double[]... a) {
        if (N.isNullOrEmpty(a)) {
            return DoubleIterator.EMPTY;
        }

        return new DoubleIterator() {
            private final Iterator<double[]> iter = Arrays.asList(a).iterator();
            private double[] cur;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                while ((N.isNullOrEmpty(cur) || cursor >= cur.length) && iter.hasNext()) {
                    cur = iter.next();
                    cursor = 0;
                }

                return cur != null && cursor < cur.length;
            }

            @Override
            public double nextDouble() {
                if ((cur == null || cursor >= cur.length) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur[cursor++];
            }
        };
    }

    @SafeVarargs
    public static BooleanIterator concat(final BooleanIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return BooleanIterator.EMPTY;
        }

        return new BooleanIterator() {
            private final Iterator<BooleanIterator> iter = Arrays.asList(a).iterator();
            private BooleanIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public boolean nextBoolean() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextBoolean();
            }
        };
    }

    @SafeVarargs
    public static CharIterator concat(final CharIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return CharIterator.EMPTY;
        }

        return new CharIterator() {
            private final Iterator<CharIterator> iter = Arrays.asList(a).iterator();
            private CharIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static ByteIterator concat(final ByteIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return ByteIterator.EMPTY;
        }

        return new ByteIterator() {
            private final Iterator<ByteIterator> iter = Arrays.asList(a).iterator();
            private ByteIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static ShortIterator concat(final ShortIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return ShortIterator.EMPTY;
        }

        return new ShortIterator() {
            private final Iterator<ShortIterator> iter = Arrays.asList(a).iterator();
            private ShortIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static IntIterator concat(final IntIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return IntIterator.EMPTY;
        }

        return new IntIterator() {
            private final Iterator<IntIterator> iter = Arrays.asList(a).iterator();
            private IntIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static LongIterator concat(final LongIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return LongIterator.EMPTY;
        }

        return new LongIterator() {
            private final Iterator<LongIterator> iter = Arrays.asList(a).iterator();
            private LongIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static FloatIterator concat(final FloatIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return FloatIterator.EMPTY;
        }

        return new FloatIterator() {
            private final Iterator<FloatIterator> iter = Arrays.asList(a).iterator();
            private FloatIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static DoubleIterator concat(final DoubleIterator... a) {
        if (N.isNullOrEmpty(a)) {
            return DoubleIterator.EMPTY;
        }

        return new DoubleIterator() {
            private final Iterator<DoubleIterator> iter = Arrays.asList(a).iterator();
            private DoubleIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    @SafeVarargs
    public static <T> ObjIterator<T> concat(final T[]... a) {
        if (N.isNullOrEmpty(a)) {
            return ObjIterator.empty();
        }

        final List<Iterator<T>> list = new ArrayList<>(a.length);

        for (T[] e : a) {
            if (N.notNullOrEmpty(e)) {
                list.add(ObjIterator.of(e));
            }
        }

        return concat(list);
    }

    @SafeVarargs
    public static <T> ObjIterator<T> concat(final Collection<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return ObjIterator.EMPTY;
        }

        final List<Iterator<? extends T>> list = new ArrayList<>(a.length);

        for (Collection<? extends T> e : a) {
            if (N.notNullOrEmpty(e)) {
                list.add(e.iterator());
            }
        }

        return concat(list);
    }

    @SafeVarargs
    public static <T> ObjIterator<T> concat(final Iterator<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return ObjIterator.EMPTY;
        }

        return concat(N.asList(a));
    }

    public static <T> ObjIterator<T> concat(final Collection<? extends Iterator<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private final Iterator<? extends Iterator<? extends T>> iter = c.iterator();
            private Iterator<? extends T> cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        };
    }

    public static <T> ObjIterator<T> merge(final Iterator<? extends T> a, final Iterator<? extends T> b,
            final BiFunction<? super T, ? super T, Nth> nextSelector) {

        return new ObjIterator<T>() {
            private final Iterator<? extends T> iterA = a == null ? ObjIterator.EMPTY : a;
            private final Iterator<? extends T> iterB = b == null ? ObjIterator.EMPTY : b;
            private T nextA = null;
            private T nextB = null;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return hasNextA || hasNextB || iterA.hasNext() || iterB.hasNext();
            }

            @Override
            public T next() {
                if (hasNextA) {
                    if (iterB.hasNext()) {
                        if (nextSelector.apply(nextA, (nextB = iterB.next())) == Nth.FIRST) {
                            hasNextA = false;
                            hasNextB = true;
                            return nextA;
                        } else {
                            return nextB;
                        }
                    } else {
                        hasNextA = false;
                        return nextA;
                    }
                } else if (hasNextB) {
                    if (iterA.hasNext()) {
                        if (nextSelector.apply((nextA = iterA.next()), nextB) == Nth.FIRST) {
                            return nextA;
                        } else {
                            hasNextA = true;
                            hasNextB = false;
                            return nextB;
                        }
                    } else {
                        hasNextB = false;
                        return nextB;
                    }
                } else if (iterA.hasNext()) {
                    if (iterB.hasNext()) {
                        if (nextSelector.apply((nextA = iterA.next()), (nextB = iterB.next())) == Nth.FIRST) {
                            hasNextB = true;
                            return nextA;
                        } else {
                            hasNextA = true;
                            return nextB;
                        }
                    } else {
                        return iterA.next();
                    }
                } else {
                    return iterB.next();
                }
            }
        };
    }

    public static <A, B, R> ObjIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final BiFunction<? super A, ? super B, R> zipFunction) {
        return new ObjIterator<R>() {
            private final Iterator<A> iterA = a == null ? ObjIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ObjIterator.<B> empty() : b;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() && iterB.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.next(), iterB.next());
            }
        };
    }

    public static <A, B, C, R> ObjIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final Iterator<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return new ObjIterator<R>() {
            private final Iterator<A> iterA = a == null ? ObjIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ObjIterator.<B> empty() : b;
            private final Iterator<C> iterC = c == null ? ObjIterator.<C> empty() : c;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() && iterB.hasNext() && iterC.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.next(), iterB.next(), iterC.next());
            }
        };
    }

    public static <A, B, R> ObjIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        return new ObjIterator<R>() {
            private final Iterator<A> iterA = a == null ? ObjIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ObjIterator.<B> empty() : b;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() || iterB.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.hasNext() ? iterB.next() : valueForNoneB);
            }
        };
    }

    public static <A, B, C, R> ObjIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final Iterator<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return new ObjIterator<R>() {
            private final Iterator<A> iterA = a == null ? ObjIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ObjIterator.<B> empty() : b;
            private final Iterator<C> iterC = c == null ? ObjIterator.<C> empty() : c;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() || iterB.hasNext() || iterC.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.hasNext() ? iterB.next() : valueForNoneB,
                        iterC.hasNext() ? iterC.next() : valueForNoneC);
            }
        };
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<List<L>, List<R>> unzip(final Iterator<? extends T> iter, final BiConsumer<? super T, Pair<L, R>> unzip) {
        final List<L> l = new ArrayList<L>();
        final List<R> r = new ArrayList<R>();
        final Pair<L, R> p = new Pair<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), p);

                l.add(p.left);
                r.add(p.right);
            }
        }

        return Pair.of(l, r);
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @param supplier
     * @return
     */
    public static <T, L, R, LC extends Collection<L>, RC extends Collection<R>> Pair<LC, RC> unzip(final Iterator<? extends T> iter,
            final BiConsumer<? super T, Pair<L, R>> unzip, final Supplier<? extends Collection<?>> supplier) {
        final LC l = (LC) supplier.get();
        final RC r = (RC) supplier.get();

        final Pair<L, R> p = new Pair<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), p);

                l.add(p.left);
                r.add(p.right);
            }
        }

        return Pair.of(l, r);
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, M, R> Triple<List<L>, List<M>, List<R>> unzip3(final Iterator<? extends T> iter, final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final List<L> l = new ArrayList<L>();
        final List<M> m = new ArrayList<M>();
        final List<R> r = new ArrayList<R>();
        final Triple<L, M, R> t = new Triple<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), t);

                l.add(t.left);
                m.add(t.middle);
                r.add(t.right);
            }
        }

        return Triple.of(l, m, r);
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @param supplier
     * @return
     */
    public static <T, L, M, R, LC extends Collection<L>, MC extends Collection<M>, RC extends Collection<R>> Triple<LC, MC, RC> unzip3(
            final Iterator<? extends T> iter, final BiConsumer<? super T, Triple<L, M, R>> unzip, final Supplier<? extends Collection<?>> supplier) {
        final LC l = (LC) supplier.get();
        final MC m = (MC) supplier.get();
        final RC r = (RC) supplier.get();
        final Triple<L, M, R> t = new Triple<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), t);

                l.add(t.left);
                m.add(t.middle);
                r.add(t.right);
            }
        }

        return Triple.of(l, m, r);
    }

    public static <T> ObjIterator<List<T>> split(final Iterator<? extends T> iter, final int size) {
        N.checkArgument(size > 0, "'size' must be greater than 0, can't be: %s", size);

        if (iter == null) {
            return ObjIterator.empty();
        }

        return new ObjIterator<List<T>>() {
            private final Iterator<? extends T> iterator = iter;

            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public List<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> next = new ArrayList<>(size);

                for (int i = 0; i < size && iterator.hasNext(); i++) {
                    next.add(iterator.next());
                }

                return next;
            }
        };
    }

    public static <T> ObjIterator<T> skipNull(final Iterator<T> iter) {
        if (iter == null) {
            return ObjIterator.empty();
        }

        return new ObjIterator<T>() {
            private final Iterator<T> iterator = iter;
            private T next;

            @Override
            public boolean hasNext() {
                if (next == null && iterator.hasNext()) {
                    next = iterator.next();

                    if (next == null) {
                        while (iterator.hasNext()) {
                            next = iterator.next();

                            if (next != null) {
                                break;
                            }
                        }
                    }
                }

                return next != null;
            }

            @Override
            public T next() {
                if (next == null && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final T result = next;
                next = null;
                return result;
            }
        };
    }
}
