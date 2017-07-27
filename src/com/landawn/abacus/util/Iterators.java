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

import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Supplier;

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

    public static <T> ImmutableIterator<T> repeat(final Collection<T> c, final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (N.isNullOrEmpty(c) || n == 0) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<T>() {
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

    public static <T> ImmutableIterator<T> repeatToSize(final Collection<T> c, final int size) {
        N.checkArgument(size >= 0, "'size' can't be negative: %s", size);
        N.checkArgument(size == 0 || N.notNullOrEmpty(c), "Collection can't be empty or null when size > 0");

        if (N.isNullOrEmpty(c) || size == 0) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<T>() {
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

    public static <T> ImmutableIterator<T> nRepeat(final Collection<T> c, final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (N.isNullOrEmpty(c) || n == 0) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<T>() {
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

    public static <T> ImmutableIterator<T> nRepeatToSize(final Collection<T> c, final int size) {
        N.checkArgument(size >= 0, "'size' can't be negative: %s", size);
        N.checkArgument(size == 0 || N.notNullOrEmpty(c), "Collection can't be empty or null when size > 0");

        if (N.isNullOrEmpty(c) || size == 0) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<T>() {
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
    public static <T> ImmutableIterator<T> concat(final T[]... a) {
        if (N.isNullOrEmpty(a)) {
            return ImmutableIterator.empty();
        }

        final List<Iterator<T>> list = new ArrayList<>(a.length);

        for (T[] e : a) {
            if (N.notNullOrEmpty(e)) {
                list.add(ImmutableIterator.of(e));
            }
        }

        return concat(list);
    }

    @SafeVarargs
    public static <T> ImmutableIterator<T> concat(final Collection<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return ImmutableIterator.empty();
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
    public static <T> ImmutableIterator<T> concat(final Iterator<? extends T>... a) {
        return concat(N.asList(a));
    }

    public static <T> ImmutableIterator<T> concat(final Collection<? extends Iterator<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<T>() {
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

    public static <T> ImmutableIterator<List<T>> split(final Iterator<? extends T> iter, final int size) {
        N.checkArgument(size > 0, "'size' must be greater than 0, can't be: %s", size);

        if (iter == null) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<List<T>>() {
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

    public static <T> ImmutableIterator<T> skipNull(final Iterator<T> iter) {
        if (iter == null) {
            return ImmutableIterator.empty();
        }

        return new ImmutableIterator<T>() {
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
