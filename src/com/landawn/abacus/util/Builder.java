package com.landawn.abacus.util;

import java.util.Collection;
import java.util.Map;

import com.landawn.abacus.util.function.Consumer;

public abstract class Builder<T> {
    final T value;

    Builder(T value) {
        this.value = value;
    }

    public static final BooleanListBuilder of(BooleanList l) {
        return new BooleanListBuilder(l);
    }

    public static final CharListBuilder of(CharList l) {
        return new CharListBuilder(l);
    }

    public static final ByteListBuilder of(ByteList l) {
        return new ByteListBuilder(l);
    }

    public static final ShortListBuilder of(ShortList l) {
        return new ShortListBuilder(l);
    }

    public static final IntListBuilder of(IntList l) {
        return new IntListBuilder(l);
    }

    public static final LongListBuilder of(LongList l) {
        return new LongListBuilder(l);
    }

    public static final FloatListBuilder of(FloatList l) {
        return new FloatListBuilder(l);
    }

    public static final DoubleListBuilder of(DoubleList l) {
        return new DoubleListBuilder(l);
    }

    public static final <T, L extends ObjectList<T>> ObjectListBuilder<T, L> of(L l) {
        return new ObjectListBuilder<>(l);
    }

    public static final <T, C extends Collection<T>> CollectionBuilder<T, C> of(C c) {
        return new CollectionBuilder<>(c);
    }

    public static final <K, V, M extends Map<K, V>> MapBuilder<K, V, M> of(M m) {
        return new MapBuilder<>(m);
    }

    public static final <T> MultisetBuilder<T> of(Multiset<T> m) {
        return new MultisetBuilder<>(m);
    }

    public static final <T> LongMultisetBuilder<T> of(LongMultiset<T> m) {
        return new LongMultisetBuilder<>(m);
    }

    public static final <K, E, V extends Collection<E>> MultimapBuilder<K, E, V> of(Multimap<K, E, V> m) {
        return new MultimapBuilder<>(m);
    }

    public static final <T> ObjectBuilder<T> of(T t) {
        return new ObjectBuilder<>(t);
    }

    //    public static <T> CollectionBuilder<T, List<T>> list() {
    //        final List<T> list = new ArrayList<>();
    //        return of(list);
    //    }
    //
    //    public static <T> CollectionBuilder<T, Set<T>> set() {
    //        final Set<T> set = new HashSet<T>();
    //        return of(set);
    //    }
    //
    //    public static <K, V, M extends Map<K, V>> MapBuilder<K, V, Map<K, V>> map() {
    //        final Map<K, V> map = new HashMap<K, V>();
    //        return of(map);
    //    }

    public T value() {
        return value;
    }

    public static final class BooleanListBuilder extends Builder<BooleanList> {
        BooleanListBuilder(BooleanList l) {
            super(l);
        }

        public BooleanListBuilder add(boolean e) {
            value.add(e);

            return this;
        }

        public BooleanListBuilder addAll(BooleanList c) {
            value.addAll(c);

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
        CharListBuilder(CharList l) {
            super(l);
        }

        public CharListBuilder add(char e) {
            value.add(e);

            return this;
        }

        public CharListBuilder addAll(CharList c) {
            value.addAll(c);

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
        ByteListBuilder(ByteList l) {
            super(l);
        }

        public ByteListBuilder add(byte e) {
            value.add(e);

            return this;
        }

        public ByteListBuilder addAll(ByteList c) {
            value.addAll(c);

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
        ShortListBuilder(ShortList l) {
            super(l);
        }

        public ShortListBuilder add(short e) {
            value.add(e);

            return this;
        }

        public ShortListBuilder addAll(ShortList c) {
            value.addAll(c);

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
        IntListBuilder(IntList l) {
            super(l);
        }

        public IntListBuilder add(int e) {
            value.add(e);

            return this;
        }

        public IntListBuilder addAll(IntList c) {
            value.addAll(c);

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
        LongListBuilder(LongList l) {
            super(l);
        }

        public LongListBuilder add(long e) {
            value.add(e);

            return this;
        }

        public LongListBuilder addAll(LongList c) {
            value.addAll(c);

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
        FloatListBuilder(FloatList l) {
            super(l);
        }

        public FloatListBuilder add(float e) {
            value.add(e);

            return this;
        }

        public FloatListBuilder addAll(FloatList c) {
            value.addAll(c);

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
        DoubleListBuilder(DoubleList l) {
            super(l);
        }

        public DoubleListBuilder add(double e) {
            value.add(e);

            return this;
        }

        public DoubleListBuilder addAll(DoubleList c) {
            value.addAll(c);

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

    public static final class ObjectListBuilder<T, L extends ObjectList<T>> extends Builder<L> {
        ObjectListBuilder(L l) {
            super(l);
        }

        public ObjectListBuilder<T, L> add(T e) {
            value.add(e);

            return this;
        }

        public ObjectListBuilder<T, L> addAll(ObjectList<T> c) {
            value.addAll(c);

            return this;
        }

        public ObjectListBuilder<T, L> remove(Object e) {
            value.remove(e);

            return this;
        }

        //        public ObjListBuilder<T, L> removeAllOccurrences(Object e) {
        //            value.removeAllOccurrences(e);
        //
        //            return this;
        //        }

        public ObjectListBuilder<T, L> removeAll(ObjectList<?> c) {
            value.removeAll(c);

            return this;
        }
    }

    public static final class CollectionBuilder<T, C extends Collection<T>> extends Builder<C> {
        CollectionBuilder(C c) {
            super(c);
        }

        public CollectionBuilder<T, C> add(T e) {
            value.add(e);

            return this;
        }

        public CollectionBuilder<T, C> addAll(Collection<? extends T> c) {
            @SuppressWarnings("rawtypes")
            final Collection tmp = c;

            value.addAll(tmp);

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

        public MultisetBuilder<T> addAll(Collection<? extends T> c) {
            @SuppressWarnings("rawtypes")
            final Collection tmp = c;

            value.addAll(tmp);

            return this;
        }

        public MultisetBuilder<T> addAll(final Map<? extends T, Integer> m) {
            value.addAll(m);

            return this;
        }

        public MultisetBuilder<T> addAll(Multiset<? extends T> multiset) {
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

        public LongMultisetBuilder<T> addAll(Collection<? extends T> c) {
            @SuppressWarnings("rawtypes")
            final Collection tmp = c;

            value.addAll(tmp);

            return this;
        }

        public LongMultisetBuilder<T> addAll(final Map<? extends T, Long> m) {
            value.addAll(m);

            return this;
        }

        public LongMultisetBuilder<T> addAll(LongMultiset<? extends T> multiset) {
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

    public static final class MultimapBuilder<K, E, V extends Collection<E>> extends Builder<Multimap<K, E, V>> {
        MultimapBuilder(Multimap<K, E, V> m) {
            super(m);
        }

        public MultimapBuilder<K, E, V> put(K key, E e) {
            value.put(key, e);

            return this;
        }

        public MultimapBuilder<K, E, V> putAll(final K k, final Collection<? extends E> c) {
            value.putAll(k, c);

            return this;
        }

        public MultimapBuilder<K, E, V> putAll(Map<? extends K, ? extends E> m) {
            value.putAll(m);

            return this;
        }

        public MultimapBuilder<K, E, V> putAll(Multimap<? extends K, ? extends E, ? extends V> m) {
            value.putAll(m);

            return this;
        }

        public MultimapBuilder<K, E, V> remove(Object k, Object e) {
            value.remove(k, e);

            return this;
        }

        public MultimapBuilder<K, E, V> removeAll(K k) {
            value.removeAll(k);

            return this;
        }

        public MultimapBuilder<K, E, V> removeAll(Collection<? extends K> c) {
            for (Object k : c) {
                value.removeAll(k);
            }

            return this;
        }

        public MultimapBuilder<K, E, V> removeAll(Map<? extends K, ? extends E> m) {
            value.removeAll(m);

            return this;
        }

        public MultimapBuilder<K, E, V> removeAll(Multimap<? extends K, ? extends E, ? extends V> m) {
            value.removeAll(m);

            return this;
        }
    }

    public static final class ObjectBuilder<T> extends Builder<T> {
        ObjectBuilder(T v) {
            super(v);
        }

        //        public ObjectBuilder<T> _(Consumer<? super T> op) {
        //            op.accept(value);
        //
        //            return this;
        //        }

        public ObjectBuilder<T> __(Consumer<? super T> op) {
            op.accept(value);

            return this;
        }
    }
}
