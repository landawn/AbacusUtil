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

package com.landawn.abacus.util;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;

/**
 * 
 * @param <T>
 * @since 0.8
 * 
 * @author haiyangl
 */
public class Builder<T> {
    final T value;

    Builder(T val) {
        N.requireNonNull(val);

        this.value = val;
    }

    public static final BooleanListBuilder of(BooleanList val) {
        return new BooleanListBuilder(val);
    }

    public static final CharListBuilder of(CharList val) {
        return new CharListBuilder(val);
    }

    public static final ByteListBuilder of(ByteList val) {
        return new ByteListBuilder(val);
    }

    public static final ShortListBuilder of(ShortList val) {
        return new ShortListBuilder(val);
    }

    public static final IntListBuilder of(IntList val) {
        return new IntListBuilder(val);
    }

    public static final LongListBuilder of(LongList val) {
        return new LongListBuilder(val);
    }

    public static final FloatListBuilder of(FloatList val) {
        return new FloatListBuilder(val);
    }

    public static final DoubleListBuilder of(DoubleList val) {
        return new DoubleListBuilder(val);
    }

    public static final <T, L extends List<T>> ListBuilder<T, L> of(L val) {
        return new ListBuilder<>(val);
    }

    public static final <T, C extends Collection<T>> CollectionBuilder<T, C> of(C val) {
        return new CollectionBuilder<>(val);
    }

    public static final <K, V, M extends Map<K, V>> MapBuilder<K, V, M> of(M val) {
        return new MapBuilder<>(val);
    }

    public static final <T> MultisetBuilder<T> of(Multiset<T> val) {
        return new MultisetBuilder<>(val);
    }

    public static final <T> LongMultisetBuilder<T> of(LongMultiset<T> val) {
        return new LongMultisetBuilder<>(val);
    }

    public static final <K, E, V extends Collection<E>, M extends Multimap<K, E, V>> MultimapBuilder<K, E, V, M> of(M val) {
        return new MultimapBuilder<>(val);
    }

    public static final <T> Builder<T> of(T val) {
        return new Builder<>(val);
    }

    //    public static <T> Builder<T> get(final Supplier<T> supplier) {
    //        return new Builder<>(supplier.get());
    //    }

    public Builder<T> accept(final Consumer<? super T> consumer) {
        consumer.accept(value);

        return this;
    }

    public <R> Builder<R> map(final Function<? super T, R> mapper) {
        return of(mapper.apply(value));
    }

    /**
     * 
     * @param predicate
     * @return <code>Optional</code> with the value if <code>predicate</code> returns true, 
     * otherwise, return an empty <code>Optional</code>
     */
    public Optional<T> filter(final Predicate<? super T> predicate) {
        return predicate.test(value) ? Optional.of(value) : Optional.<T> empty();
    }

    public T val() {
        return value;
    }

    public static final class BooleanListBuilder extends Builder<BooleanList> {
        BooleanListBuilder(BooleanList val) {
            super(val);
        }

        public BooleanListBuilder set(int index, boolean e) {
            value.set(index, e);

            return this;
        }

        public BooleanListBuilder add(boolean e) {
            value.add(e);

            return this;
        }

        public BooleanListBuilder add(int index, boolean e) {
            value.add(index, e);

            return this;
        }

        public BooleanListBuilder addAll(BooleanList c) {
            value.addAll(c);

            return this;
        }

        public BooleanListBuilder addAll(int index, BooleanList c) {
            value.addAll(index, c);

            return this;
        }

        public BooleanListBuilder remove(boolean e) {
            value.remove(e);

            return this;
        }

        //        public BooleanListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public BooleanListBuilder removeAll(BooleanList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class CharListBuilder extends Builder<CharList> {
        CharListBuilder(CharList val) {
            super(val);
        }

        public CharListBuilder set(int index, char e) {
            value.set(index, e);

            return this;
        }

        public CharListBuilder add(char e) {
            value.add(e);

            return this;
        }

        public CharListBuilder add(int index, char e) {
            value.add(index, e);

            return this;
        }

        public CharListBuilder addAll(CharList c) {
            value.addAll(c);

            return this;
        }

        public CharListBuilder addAll(int index, CharList c) {
            value.addAll(index, c);

            return this;
        }

        public CharListBuilder remove(char e) {
            value.remove(e);

            return this;
        }

        //        public CharListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public CharListBuilder removeAll(CharList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class ByteListBuilder extends Builder<ByteList> {
        ByteListBuilder(ByteList val) {
            super(val);
        }

        public ByteListBuilder set(int index, byte e) {
            value.set(index, e);

            return this;
        }

        public ByteListBuilder add(byte e) {
            value.add(e);

            return this;
        }

        public ByteListBuilder add(int index, byte e) {
            value.add(index, e);

            return this;
        }

        public ByteListBuilder addAll(ByteList c) {
            value.addAll(c);

            return this;
        }

        public ByteListBuilder addAll(int index, ByteList c) {
            value.addAll(index, c);

            return this;
        }

        public ByteListBuilder remove(byte e) {
            value.remove(e);

            return this;
        }

        //        public ByteListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public ByteListBuilder removeAll(ByteList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class ShortListBuilder extends Builder<ShortList> {
        ShortListBuilder(ShortList val) {
            super(val);
        }

        public ShortListBuilder set(int index, short e) {
            value.set(index, e);

            return this;
        }

        public ShortListBuilder add(short e) {
            value.add(e);

            return this;
        }

        public ShortListBuilder add(int index, short e) {
            value.add(index, e);

            return this;
        }

        public ShortListBuilder addAll(ShortList c) {
            value.addAll(c);

            return this;
        }

        public ShortListBuilder addAll(int index, ShortList c) {
            value.addAll(index, c);

            return this;
        }

        public ShortListBuilder remove(short e) {
            value.remove(e);

            return this;
        }

        //        public ShortListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public ShortListBuilder removeAll(ShortList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class IntListBuilder extends Builder<IntList> {
        IntListBuilder(IntList val) {
            super(val);
        }

        public IntListBuilder set(int index, int e) {
            value.set(index, e);

            return this;
        }

        public IntListBuilder add(int e) {
            value.add(e);

            return this;
        }

        public IntListBuilder add(int index, int e) {
            value.add(index, e);

            return this;
        }

        public IntListBuilder addAll(IntList c) {
            value.addAll(c);

            return this;
        }

        public IntListBuilder addAll(int index, IntList c) {
            value.addAll(index, c);

            return this;
        }

        public IntListBuilder remove(int e) {
            value.remove(e);

            return this;
        }

        //        public IntListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public IntListBuilder removeAll(IntList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class LongListBuilder extends Builder<LongList> {
        LongListBuilder(LongList val) {
            super(val);
        }

        public LongListBuilder set(int index, long e) {
            value.set(index, e);

            return this;
        }

        public LongListBuilder add(long e) {
            value.add(e);

            return this;
        }

        public LongListBuilder add(int index, long e) {
            value.add(index, e);

            return this;
        }

        public LongListBuilder addAll(LongList c) {
            value.addAll(c);

            return this;
        }

        public LongListBuilder addAll(int index, LongList c) {
            value.addAll(index, c);

            return this;
        }

        public LongListBuilder remove(long e) {
            value.remove(e);

            return this;
        }

        //        public LongListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public LongListBuilder removeAll(LongList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class FloatListBuilder extends Builder<FloatList> {
        FloatListBuilder(FloatList val) {
            super(val);
        }

        public FloatListBuilder set(int index, float e) {
            value.set(index, e);

            return this;
        }

        public FloatListBuilder add(float e) {
            value.add(e);

            return this;
        }

        public FloatListBuilder add(int index, float e) {
            value.add(index, e);

            return this;
        }

        public FloatListBuilder addAll(FloatList c) {
            value.addAll(c);

            return this;
        }

        public FloatListBuilder addAll(int index, FloatList c) {
            value.addAll(index, c);

            return this;
        }

        public FloatListBuilder remove(float e) {
            value.remove(e);

            return this;
        }

        //        public FloatListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public FloatListBuilder removeAll(FloatList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class DoubleListBuilder extends Builder<DoubleList> {
        DoubleListBuilder(DoubleList val) {
            super(val);
        }

        public DoubleListBuilder set(int index, double e) {
            value.set(index, e);

            return this;
        }

        public DoubleListBuilder add(double e) {
            value.add(e);

            return this;
        }

        public DoubleListBuilder add(int index, double e) {
            value.add(index, e);

            return this;
        }

        public DoubleListBuilder addAll(DoubleList c) {
            value.addAll(c);

            return this;
        }

        public DoubleListBuilder addAll(int index, DoubleList c) {
            value.addAll(index, c);

            return this;
        }

        public DoubleListBuilder remove(double e) {
            value.remove(e);

            return this;
        }

        //        public DoubleListBuilder removeAllOccurrences(double e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public DoubleListBuilder removeAll(DoubleList c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class ListBuilder<T, L extends List<T>> extends CollectionBuilder<T, L> {
        ListBuilder(L c) {
            super(c);
        }

        public ListBuilder<T, L> add(int index, T e) {
            value.add(index, e);

            return this;
        }

        public ListBuilder<T, L> addAll(int index, Collection<? extends T> c) {
            value.addAll(index, c);

            return this;
        }

        public ListBuilder<T, L> remove(int index) {
            value.remove(index);

            return this;
        }
    }

    public static class CollectionBuilder<T, C extends Collection<T>> extends Builder<C> {
        CollectionBuilder(C c) {
            super(c);
        }

        public CollectionBuilder<T, C> add(T e) {
            value.add(e);

            return this;
        }

        public CollectionBuilder<T, C> addAll(final Collection<? extends T> c) {
            value.addAll(c);

            return this;
        }

        public CollectionBuilder<T, C> remove(Object e) {
            value.remove(e);

            return this;
        }

        public CollectionBuilder<T, C> removeAll(Collection<?> c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class MultisetBuilder<T> extends Builder<Multiset<T>> {
        MultisetBuilder(Multiset<T> c) {
            super(c);
        }

        public MultisetBuilder<T> add(T e) {
            value.add(e);

            return this;
        }

        public MultisetBuilder<T> addAll(final Collection<? extends T> c) {
            value.addAll(c);

            return this;
        }

        public MultisetBuilder<T> addAll(final Map<? extends T, Integer> m) {
            value.addAll(m);

            return this;
        }

        public MultisetBuilder<T> addAll(final Multiset<? extends T> multiset) {
            value.addAll(multiset);

            return this;
        }

        public MultisetBuilder<T> remove(Object e) {
            value.remove(e);

            return this;
        }

        public MultisetBuilder<T> removeAll(Collection<?> c) {
            value.removeAll(c);

            return this;
        }

        public MultisetBuilder<T> removeAll(final Map<? extends T, Integer> m) {
            value.removeAll(m);

            return this;
        }

        public MultisetBuilder<T> removeAll(Multiset<? extends T> multiset) {
            value.removeAll(multiset);

            return this;
        }
    }

    public static final class LongMultisetBuilder<T> extends Builder<LongMultiset<T>> {
        LongMultisetBuilder(LongMultiset<T> c) {
            super(c);
        }

        public LongMultisetBuilder<T> add(T e) {
            value.add(e);

            return this;
        }

        public LongMultisetBuilder<T> addAll(final Collection<? extends T> c) {
            value.addAll(c);

            return this;
        }

        public LongMultisetBuilder<T> addAll(final Map<? extends T, Long> m) {
            value.addAll(m);

            return this;
        }

        public LongMultisetBuilder<T> addAll(final LongMultiset<? extends T> multiset) {
            value.addAll(multiset);

            return this;
        }

        public LongMultisetBuilder<T> remove(Object e) {
            value.remove(e);

            return this;
        }

        public LongMultisetBuilder<T> removeAll(Collection<?> c) {
            value.removeAll(c);

            return this;
        }

        public LongMultisetBuilder<T> removeAll(final Map<? extends T, Long> m) {
            value.removeAll(m);

            return this;
        }

        public LongMultisetBuilder<T> removeAll(LongMultiset<? extends T> multiset) {
            value.removeAll(multiset);

            return this;
        }
    }

    public static final class MapBuilder<K, V, M extends Map<K, V>> extends Builder<M> {
        MapBuilder(M m) {
            super(m);
        }

        public MapBuilder<K, V, M> put(K k, V v) {
            value.put(k, v);

            return this;
        }

        public MapBuilder<K, V, M> putAll(Map<? extends K, ? extends V> m) {
            value.putAll(m);

            return this;
        }

        public MapBuilder<K, V, M> remove(Object k) {
            value.remove(k);

            return this;
        }

        public MapBuilder<K, V, M> removeAll(Collection<?> c) {
            for (Object k : c) {
                value.remove(k);
            }

            return this;
        }
    }

    public static final class MultimapBuilder<K, E, V extends Collection<E>, M extends Multimap<K, E, V>> extends Builder<M> {
        MultimapBuilder(M m) {
            super(m);
        }

        public MultimapBuilder<K, E, V, M> put(K key, E e) {
            value.put(key, e);

            return this;
        }

        public MultimapBuilder<K, E, V, M> putAll(final K k, final Collection<? extends E> c) {
            value.putAll(k, c);

            return this;
        }

        public MultimapBuilder<K, E, V, M> putAll(Map<? extends K, ? extends E> m) {
            value.putAll(m);

            return this;
        }

        public MultimapBuilder<K, E, V, M> putAll(Multimap<? extends K, ? extends E, ? extends V> m) {
            value.putAll(m);

            return this;
        }

        public MultimapBuilder<K, E, V, M> remove(Object k, Object e) {
            value.remove(k, e);

            return this;
        }

        public MultimapBuilder<K, E, V, M> removeAll(K k) {
            value.removeAll(k);

            return this;
        }

        public MultimapBuilder<K, E, V, M> removeAll(Collection<? extends K> c) {
            for (Object k : c) {
                value.removeAll(k);
            }

            return this;
        }

        public MultimapBuilder<K, E, V, M> removeAll(Map<? extends K, ? extends E> m) {
            value.removeAll(m);

            return this;
        }

        public MultimapBuilder<K, E, V, M> removeAll(Multimap<? extends K, ? extends E, ? extends V> m) {
            value.removeAll(m);

            return this;
        }
    }

    public static final class X<T> extends Builder<T> {
        private X(T val) {
            super(val);
        }

        /**
         * 
         * @param output
         * @param e
         * @return return the specified {@code output}
         */
        public static <E, C extends Collection<E>> C add(final C output, final E e) {
            output.add(e);
            return output;
        }

        /**
         * 
         * @param output
         * @param e
         * @return return the specified {@code output}
         */
        public static <E, C extends Collection<E>> C addAll(final C output, final Collection<? extends E> c) {
            if (c == null || c.size() == 0) {
                return output;
            }

            output.addAll(c);
            return output;
        }

        /**
         * 
         * @param output
         * @param e
         * @return return the specified {@code output}
         */
        public static <E, C extends Collection<E>> C remove(final C output, final Object e) {
            if (output == null || output.size() == 0) {
                return output;
            }

            output.remove(e);
            return output;
        }

        /**
         * 
         * @param output
         * @param e
         * @return return the specified {@code output}
         */
        public static <E, C extends Collection<E>> C removeAll(final C output, final Collection<?> c) {
            if (output == null || output.size() == 0 || c == null || c.size() == 0) {
                return output;
            }

            output.removeAll(c);
            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @param value
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M put(final M output, K key, final V value) {
            output.put(key, value);
            return output;
        }

        /**
         * 
         * @param output
         * @param entryToAdd
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M put(final M output, final Map.Entry<? extends K, ? extends V> entryToAdd) {
            output.put(entryToAdd.getKey(), entryToAdd.getValue());
            return output;
        }

        /**
         * 
         * @param output
         * @param entriesToAdd
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M putAll(final M output, final Map<? extends K, ? extends V> entriesToAdd) {
            if (entriesToAdd == null || entriesToAdd.size() == 0) {
                return output;
            }

            output.putAll(entriesToAdd);
            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @param value
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M putIfAbsent(final M output, K key, final V value) {
            if (!output.containsKey(key)) {
                output.put(key, value);
            }

            return output;
        }

        /**
         * 
         * @param output
         * @param entryToAdd
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M putIfAbsent(final M output, final Map.Entry<? extends K, ? extends V> entryToAdd) {
            if (!output.containsKey(entryToAdd.getKey())) {
                output.put(entryToAdd.getKey(), entryToAdd.getValue());
            }
            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @param oldValue
         * @param newValue
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M replace(final M output, final K key, final V newValue) {
            if (output == null || output.size() == 0) {
                return output;
            }

            final V curValue = output.get(key);

            if ((curValue != null || output.containsKey(key))) {
                output.put(key, newValue);
            }

            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @param oldValue
         * @param newValue
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M replace(final M output, final K key, final V oldValue, final V newValue) {
            if (output == null || output.size() == 0) {
                return output;
            }

            final V curValue = output.get(key);

            if ((curValue != null || output.containsKey(key)) && N.equals(curValue, oldValue)) {
                output.put(key, newValue);
            }

            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M remove(final M output, final Object key) {
            if (output == null || output.size() == 0) {
                return output;
            }

            output.remove(key);
            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M remove(final M output, final Map.Entry<?, ?> entryToRemove) {
            if (output == null || output.size() == 0) {
                return output;
            }

            if (N.equals(output.get(entryToRemove.getKey()), entryToRemove.getValue())) {
                output.remove(entryToRemove.getKey());
            }

            return output;
        }

        /**
         * 
         * @param output
         * @param key
         * @param value
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M remove(final M output, final Object key, final Object value) {
            if (output == null || output.size() == 0) {
                return output;
            }

            if (N.equals(output.get(key), value)) {
                output.remove(key);
            }

            return output;
        }

        /**
         * 
         * @param output
         * @param keys
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M removeAll(final M output, final Collection<?> keys) {
            if (output == null || output.size() == 0 || keys == null || keys.size() == 0) {
                return output;
            }

            for (Object key : keys) {
                output.remove(key);
            }

            return output;
        }

        /**
         * 
         * @param output
         * @param entriesToRemove
         * @return
         * @return return the specified {@code output}
         */
        public static <K, V, M extends Map<K, V>> M removeAll(final M output, final Map<?, ?> entriesToRemove) {
            if (output == null || output.size() == 0 || entriesToRemove == null || entriesToRemove.size() == 0) {
                return output;
            }

            for (Map.Entry<?, ?> entry : entriesToRemove.entrySet()) {
                if (N.equals(output.get(entry.getKey()), entry.getValue())) {
                    output.remove(entry.getKey());
                }
            }

            return output;
        }

        public static void ifOrElse(boolean b, Runnable action, Runnable elseAction) {
            if (b) {
                action.run();
            } else {
                elseAction.run();
            }
        }
    }
}
