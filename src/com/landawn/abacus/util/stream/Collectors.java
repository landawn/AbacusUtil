/*
 * Copyright (c) 2012, 2013, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package com.landawn.abacus.util.stream;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.Deque;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.util.BiMap;
import com.landawn.abacus.util.BooleanList;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.StringJoiner;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleSupplier;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * Implementations of {@link Collector} that implement various useful reduction
 * operations, such as accumulating elements into collections, summarizing
 * elements according to various criteria, etc.
 *
 * <p>The following are examples of using the predefined collectors to perform
 * common mutable reduction tasks:
 *
 * <pre>{@code
 *     // Accumulate names into a List
 *     List<String> list = people.stream().map(Person::getName).collect(Collectors.toList());
 *
 *     // Accumulate names into a TreeSet
 *     Set<String> set = people.stream().map(Person::getName).collect(Collectors.toCollection(TreeSet::new));
 *
 *     // Convert elements to strings and concatenate them, separated by commas
 *     String joined = things.stream()
 *                           .map(Object::toString)
 *                           .collect(Collectors.joining(", "));
 *
 *     // Compute sum of salaries of employee
 *     int total = employees.stream()
 *                          .collect(Collectors.summingInt(Employee::getSalary)));
 *
 *     // Group employees by department
 *     Map<Department, List<Employee>> byDept
 *         = employees.stream()
 *                    .collect(Collectors.groupingBy(Employee::getDepartment));
 *
 *     // Compute sum of salaries by department
 *     Map<Department, Integer> totalByDept
 *         = employees.stream()
 *                    .collect(Collectors.groupingBy(Employee::getDepartment,
 *                                                   Collectors.summingInt(Employee::getSalary)));
 *
 *     // Partition students into passing and failing
 *     Map<Boolean, List<Student>> passingFailing =
 *         students.stream()
 *                 .collect(Collectors.partitioningBy(s -> s.getGrade() >= PASS_THRESHOLD));
 *
 * }</pre>
 *
 * @since 1.8
 */
public final class Collectors {

    static final Set<Collector.Characteristics> CH_CONCURRENT_ID = Collections
            .unmodifiableSet(EnumSet.of(Collector.Characteristics.CONCURRENT, Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
    static final Set<Collector.Characteristics> CH_CONCURRENT_NOID = Collections
            .unmodifiableSet(EnumSet.of(Collector.Characteristics.CONCURRENT, Collector.Characteristics.UNORDERED));
    static final Set<Collector.Characteristics> CH_ID = Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.IDENTITY_FINISH));
    static final Set<Collector.Characteristics> CH_UNORDERED_ID = Collections
            .unmodifiableSet(EnumSet.of(Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
    static final Set<Collector.Characteristics> CH_NOID = Collections.emptySet();

    private Collectors() {
    }

    /**
     * Returns a merge function, suitable for use in
     * {@link Map#merge(Object, Object, BiFunction) Map.merge()} or
     * {@link #toMap(Function, Function, BinaryOperator) toMap()}, which always
     * throws {@code IllegalStateException}.  This can be used to enforce the
     * assumption that the elements being collected are distinct.
     *
     * @param <T> the type of input arguments to the merge function
     * @return a merge function which always throw {@code IllegalStateException}
     */
    private static <T> BinaryOperator<T> throwingMerger() {
        return (u, v) -> {
            throw new IllegalStateException(String.format("Duplicate key %s", u));
        };
    }

    @SuppressWarnings("unchecked")
    private static <I, R> Function<I, R> castingIdentity() {
        return i -> (R) i;
    }

    /**
     * Simple implementation class for {@code Collector}.
     *
     * @param <T> the type of elements to be collected
     * @param <R> the type of the result
     */
    static class CollectorImpl<T, A, R> implements Collector<T, A, R> {
        private final Supplier<A> supplier;
        private final BiConsumer<A, T> accumulator;
        private final BinaryOperator<A> combiner;
        private final Function<A, R> finisher;
        private final Set<Characteristics> characteristics;

        CollectorImpl(Supplier<A> supplier, BiConsumer<A, T> accumulator, BinaryOperator<A> combiner, Function<A, R> finisher,
                Set<Characteristics> characteristics) {
            this.supplier = supplier;
            this.accumulator = accumulator;
            this.combiner = combiner;
            this.finisher = finisher;
            this.characteristics = characteristics;
        }

        CollectorImpl(Supplier<A> supplier, BiConsumer<A, T> accumulator, BinaryOperator<A> combiner, Set<Characteristics> characteristics) {
            this(supplier, accumulator, combiner, castingIdentity(), characteristics);
        }

        @Override
        public BiConsumer<A, T> accumulator() {
            return accumulator;
        }

        @Override
        public Supplier<A> supplier() {
            return supplier;
        }

        @Override
        public BinaryOperator<A> combiner() {
            return combiner;
        }

        @Override
        public Function<A, R> finisher() {
            return finisher;
        }

        @Override
        public Set<Characteristics> characteristics() {
            return characteristics;
        }
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new {@code Collection}, in encounter order.  The {@code Collection} is
     * created by the provided factory.
     *
     * @param <T> the type of the input elements
     * @param <C> the type of the resulting {@code Collection}
     * @param collectionFactory a {@code Supplier} which returns a new, empty
     * {@code Collection} of the appropriate type
     * @return a {@code Collector} which collects all the input elements into a
     * {@code Collection}, in encounter order
     */
    public static <T, C extends Collection<T>> Collector<T, ?, C> toCollection(Supplier<C> collectionFactory) {
        final BiConsumer<C, T> accumulator = new BiConsumer<C, T>() {
            @Override
            public void accept(C c, T t) {
                c.add(t);
            }
        };

        final BinaryOperator<C> combiner = new BinaryOperator<C>() {
            @Override
            public C apply(C a, C b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(collectionFactory, accumulator, combiner, collectionFactory.get() instanceof Set ? CH_UNORDERED_ID : CH_ID);
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new {@code List}. There are no guarantees on the type, mutability,
     * serializability, or thread-safety of the {@code List} returned; if more
     * control over the returned {@code List} is required, use {@link #toCollection(Supplier)}.
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} which collects all the input elements into a
     * {@code List}, in encounter order
     */
    public static <T> Collector<T, ?, List<T>> toList() {
        final Supplier<List<T>> supplier = new Supplier<List<T>>() {
            @Override
            public List<T> get() {
                return new ArrayList<T>();
            }
        };

        return toCollection(supplier);
    }

    public static <T> Collector<T, ?, LinkedList<T>> toLinkedList() {
        final Supplier<LinkedList<T>> supplier = new Supplier<LinkedList<T>>() {
            @Override
            public LinkedList<T> get() {
                return new LinkedList<T>();
            }
        };

        return toCollection(supplier);
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new {@code Set}. There are no guarantees on the type, mutability,
     * serializability, or thread-safety of the {@code Set} returned; if more
     * control over the returned {@code Set} is required, use
     * {@link #toCollection(Supplier)}.
     *
     * <p>This is an {@link Collector.Characteristics#UNORDERED unordered}
     * Collector.
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} which collects all the input elements into a
     * {@code Set}
     */
    public static <T> Collector<T, ?, Set<T>> toSet() {
        final Supplier<Set<T>> supplier = new Supplier<Set<T>>() {
            @Override
            public Set<T> get() {
                return new HashSet<T>();
            }
        };

        return toCollection(supplier);
    }

    public static <T> Collector<T, ?, LinkedHashSet<T>> toLinkedHashSet() {
        final Supplier<LinkedHashSet<T>> supplier = new Supplier<LinkedHashSet<T>>() {
            @Override
            public LinkedHashSet<T> get() {
                return new LinkedHashSet<T>();
            }
        };

        return toCollection(supplier);
    }

    public static <T> Collector<T, ?, Queue<T>> toQueue() {
        final Supplier<Queue<T>> supplier = new Supplier<Queue<T>>() {
            @Override
            public Queue<T> get() {
                return new LinkedList<T>();
            }
        };

        return toCollection(supplier);
    }

    public static <T> Collector<T, ?, Deque<T>> toDeque() {
        final Supplier<Deque<T>> supplier = new Supplier<Deque<T>>() {
            @Override
            public Deque<T> get() {
                return new ArrayDeque<T>();
            }
        };

        return toCollection(supplier);
    }

    public static <T> Collector<T, ?, Multiset<T>> toMultiset() {
        final Supplier<Multiset<T>> supplier = new Supplier<Multiset<T>>() {
            @Override
            public Multiset<T> get() {
                return new Multiset<T>();
            }
        };

        return toMultiset(supplier);
    }

    public static <T> Collector<T, ?, Multiset<T>> toMultiset(Supplier<Multiset<T>> supplier) {
        final BiConsumer<Multiset<T>, T> accumulator = new BiConsumer<Multiset<T>, T>() {
            @Override
            public void accept(Multiset<T> c, T t) {
                c.add(t);
            }
        };

        final BinaryOperator<Multiset<T>> combiner = new BinaryOperator<Multiset<T>>() {
            @Override
            public Multiset<T> apply(Multiset<T> a, Multiset<T> b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_UNORDERED_ID);
    }

    public static <T> Collector<T, ?, LongMultiset<T>> toLongMultiset() {
        final Supplier<LongMultiset<T>> supplier = new Supplier<LongMultiset<T>>() {
            @Override
            public LongMultiset<T> get() {
                return new LongMultiset<T>();
            }
        };

        return toLongMultiset(supplier);
    }

    public static <T> Collector<T, ?, LongMultiset<T>> toLongMultiset(Supplier<LongMultiset<T>> supplier) {
        final BiConsumer<LongMultiset<T>, T> accumulator = new BiConsumer<LongMultiset<T>, T>() {
            @Override
            public void accept(LongMultiset<T> c, T t) {
                c.add(t);
            }
        };

        final BinaryOperator<LongMultiset<T>> combiner = new BinaryOperator<LongMultiset<T>>() {
            @Override
            public LongMultiset<T> apply(LongMultiset<T> a, LongMultiset<T> b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_UNORDERED_ID);
    }

    public static <T> Collector<T, ?, ObjectList<T>> toObjectList(final Class<T> cls) {
        final Supplier<ObjectList<T>> supplier = new Supplier<ObjectList<T>>() {
            @Override
            public ObjectList<T> get() {
                return new ObjectList<T>((T[]) N.newArray(cls, 0));
            }
        };

        final BiConsumer<ObjectList<T>, T> accumulator = new BiConsumer<ObjectList<T>, T>() {
            @Override
            public void accept(ObjectList<T> c, T t) {
                c.add(t);
            }
        };

        final BinaryOperator<ObjectList<T>> combiner = new BinaryOperator<ObjectList<T>>() {
            @Override
            public ObjectList<T> apply(ObjectList<T> a, ObjectList<T> b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T> Collector<T, ?, Object[]> toArray() {
        return toArray(N.EMPTY_OBJECT_ARRAY, 0);
    }

    public static <T, A> Collector<T, ?, A[]> toArray(final Supplier<A[]> supplier) {
        return toArray(supplier.get(), 0);
    }

    static <T, A> Collector<T, ?, A[]> toArray(final A[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<ObjectList<A>> supplier = new Supplier<ObjectList<A>>() {
            @Override
            public ObjectList<A> get() {
                return new ObjectList<A>(array, fromIndex);
            }
        };

        final BiConsumer<ObjectList<A>, T> accumulator = new BiConsumer<ObjectList<A>, T>() {
            @Override
            public void accept(ObjectList<A> c, T t) {
                c.add((A) t);
            }
        };

        final BinaryOperator<ObjectList<A>> combiner = new BinaryOperator<ObjectList<A>>() {
            @Override
            public ObjectList<A> apply(ObjectList<A> a, ObjectList<A> b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<ObjectList<A>, A[]> finisher = new Function<ObjectList<A>, A[]>() {
            @Override
            public A[] apply(ObjectList<A> t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static Collector<Boolean, ?, BooleanList> toBooleanList() {
        final Supplier<BooleanList> supplier = new Supplier<BooleanList>() {
            @Override
            public BooleanList get() {
                return new BooleanList();
            }
        };

        final BiConsumer<BooleanList, Boolean> accumulator = new BiConsumer<BooleanList, Boolean>() {
            @Override
            public void accept(BooleanList c, Boolean t) {
                c.add(t);
            }
        };

        final BinaryOperator<BooleanList> combiner = new BinaryOperator<BooleanList>() {
            @Override
            public BooleanList apply(BooleanList a, BooleanList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static Collector<Boolean, ?, boolean[]> toBooleanArray() {
        return toBooleanArray(N.EMPTY_BOOLEAN_ARRAY, 0);
    }

    public static Collector<Boolean, ?, boolean[]> toBooleanArray(final Supplier<boolean[]> supplier) {
        return toBooleanArray(supplier.get(), 0);
    }

    static Collector<Boolean, ?, boolean[]> toBooleanArray(final boolean[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<BooleanList> supplier = new Supplier<BooleanList>() {
            @Override
            public BooleanList get() {
                return new BooleanList(array, fromIndex);
            }
        };

        final BiConsumer<BooleanList, Boolean> accumulator = new BiConsumer<BooleanList, Boolean>() {
            @Override
            public void accept(BooleanList c, Boolean t) {
                c.add(t);
            }
        };

        final BinaryOperator<BooleanList> combiner = new BinaryOperator<BooleanList>() {
            @Override
            public BooleanList apply(BooleanList a, BooleanList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<BooleanList, boolean[]> finisher = new Function<BooleanList, boolean[]>() {
            @Override
            public boolean[] apply(BooleanList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static Collector<Character, ?, CharList> toCharList() {
        final Supplier<CharList> supplier = new Supplier<CharList>() {
            @Override
            public CharList get() {
                return new CharList();
            }
        };

        final BiConsumer<CharList, Character> accumulator = new BiConsumer<CharList, Character>() {
            @Override
            public void accept(CharList c, Character t) {
                c.add(t);
            }
        };

        final BinaryOperator<CharList> combiner = new BinaryOperator<CharList>() {
            @Override
            public CharList apply(CharList a, CharList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static Collector<Character, ?, char[]> toCharArray() {
        return toCharArray(N.EMPTY_CHAR_ARRAY, 0);
    }

    public static Collector<Character, ?, char[]> toCharArray(final Supplier<char[]> supplier) {
        return toCharArray(supplier.get(), 0);
    }

    static Collector<Character, ?, char[]> toCharArray(final char[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<CharList> supplier = new Supplier<CharList>() {
            @Override
            public CharList get() {
                return new CharList(array, fromIndex);
            }
        };

        final BiConsumer<CharList, Character> accumulator = new BiConsumer<CharList, Character>() {
            @Override
            public void accept(CharList c, Character t) {
                c.add(t);
            }
        };

        final BinaryOperator<CharList> combiner = new BinaryOperator<CharList>() {
            @Override
            public CharList apply(CharList a, CharList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<CharList, char[]> finisher = new Function<CharList, char[]>() {
            @Override
            public char[] apply(CharList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T extends Number> Collector<T, ?, ByteList> toByteList() {
        final Supplier<ByteList> supplier = new Supplier<ByteList>() {
            @Override
            public ByteList get() {
                return new ByteList();
            }
        };

        final BiConsumer<ByteList, T> accumulator = new BiConsumer<ByteList, T>() {
            @Override
            public void accept(ByteList c, T t) {
                c.add(t.byteValue());
            }
        };

        final BinaryOperator<ByteList> combiner = new BinaryOperator<ByteList>() {
            @Override
            public ByteList apply(ByteList a, ByteList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T extends Number> Collector<T, ?, byte[]> toByteArray() {
        return toByteArray(N.EMPTY_BYTE_ARRAY, 0);
    }

    public static <T extends Number> Collector<T, ?, byte[]> toByteArray(final Supplier<byte[]> supplier) {
        return toByteArray(supplier.get(), 0);
    }

    static <T extends Number> Collector<T, ?, byte[]> toByteArray(final byte[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<ByteList> supplier = new Supplier<ByteList>() {
            @Override
            public ByteList get() {
                return new ByteList(array, fromIndex);
            }
        };

        final BiConsumer<ByteList, T> accumulator = new BiConsumer<ByteList, T>() {
            @Override
            public void accept(ByteList c, T t) {
                c.add(t.byteValue());
            }
        };

        final BinaryOperator<ByteList> combiner = new BinaryOperator<ByteList>() {
            @Override
            public ByteList apply(ByteList a, ByteList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<ByteList, byte[]> finisher = new Function<ByteList, byte[]>() {
            @Override
            public byte[] apply(ByteList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T extends Number> Collector<T, ?, ShortList> toShortList() {
        final Supplier<ShortList> supplier = new Supplier<ShortList>() {
            @Override
            public ShortList get() {
                return new ShortList();
            }
        };

        final BiConsumer<ShortList, T> accumulator = new BiConsumer<ShortList, T>() {
            @Override
            public void accept(ShortList c, T t) {
                c.add(t.shortValue());
            }
        };

        final BinaryOperator<ShortList> combiner = new BinaryOperator<ShortList>() {
            @Override
            public ShortList apply(ShortList a, ShortList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T extends Number> Collector<T, ?, short[]> toShortArray() {
        return toShortArray(N.EMPTY_SHORT_ARRAY, 0);
    }

    public static <T extends Number> Collector<T, ?, short[]> toShortArray(final Supplier<short[]> supplier) {
        return toShortArray(supplier.get(), 0);
    }

    static <T extends Number> Collector<T, ?, short[]> toShortArray(final short[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<ShortList> supplier = new Supplier<ShortList>() {
            @Override
            public ShortList get() {
                return new ShortList(array, fromIndex);
            }
        };

        final BiConsumer<ShortList, T> accumulator = new BiConsumer<ShortList, T>() {
            @Override
            public void accept(ShortList c, T t) {
                c.add(t.shortValue());
            }
        };

        final BinaryOperator<ShortList> combiner = new BinaryOperator<ShortList>() {
            @Override
            public ShortList apply(ShortList a, ShortList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<ShortList, short[]> finisher = new Function<ShortList, short[]>() {
            @Override
            public short[] apply(ShortList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T extends Number> Collector<T, ?, IntList> toIntList() {
        final Supplier<IntList> supplier = new Supplier<IntList>() {
            @Override
            public IntList get() {
                return new IntList();
            }
        };

        final BiConsumer<IntList, T> accumulator = new BiConsumer<IntList, T>() {
            @Override
            public void accept(IntList c, T t) {
                c.add(t.intValue());
            }
        };

        final BinaryOperator<IntList> combiner = new BinaryOperator<IntList>() {
            @Override
            public IntList apply(IntList a, IntList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T extends Number> Collector<T, ?, int[]> toIntArray() {
        return toIntArray(N.EMPTY_INT_ARRAY, 0);
    }

    public static <T extends Number> Collector<T, ?, int[]> toIntArray(final Supplier<int[]> supplier) {
        return toIntArray(supplier.get(), 0);
    }

    static <T extends Number> Collector<T, ?, int[]> toIntArray(final int[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<IntList> supplier = new Supplier<IntList>() {
            @Override
            public IntList get() {
                return new IntList(array, fromIndex);
            }
        };

        final BiConsumer<IntList, T> accumulator = new BiConsumer<IntList, T>() {
            @Override
            public void accept(IntList c, T t) {
                c.add(t.intValue());
            }
        };

        final BinaryOperator<IntList> combiner = new BinaryOperator<IntList>() {
            @Override
            public IntList apply(IntList a, IntList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<IntList, int[]> finisher = new Function<IntList, int[]>() {
            @Override
            public int[] apply(IntList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T extends Number> Collector<T, ?, LongList> toLongList() {
        final Supplier<LongList> supplier = new Supplier<LongList>() {
            @Override
            public LongList get() {
                return new LongList();
            }
        };

        final BiConsumer<LongList, T> accumulator = new BiConsumer<LongList, T>() {
            @Override
            public void accept(LongList c, T t) {
                c.add(t.longValue());
            }
        };

        final BinaryOperator<LongList> combiner = new BinaryOperator<LongList>() {
            @Override
            public LongList apply(LongList a, LongList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T extends Number> Collector<T, ?, long[]> toLongArray() {
        return toLongArray(N.EMPTY_LONG_ARRAY, 0);
    }

    public static <T extends Number> Collector<T, ?, long[]> toLongArray(final Supplier<long[]> supplier) {
        return toLongArray(supplier.get(), 0);
    }

    static <T extends Number> Collector<T, ?, long[]> toLongArray(final long[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<LongList> supplier = new Supplier<LongList>() {
            @Override
            public LongList get() {
                return new LongList(array, fromIndex);
            }
        };

        final BiConsumer<LongList, T> accumulator = new BiConsumer<LongList, T>() {
            @Override
            public void accept(LongList c, T t) {
                c.add(t.longValue());
            }
        };

        final BinaryOperator<LongList> combiner = new BinaryOperator<LongList>() {
            @Override
            public LongList apply(LongList a, LongList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<LongList, long[]> finisher = new Function<LongList, long[]>() {
            @Override
            public long[] apply(LongList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T extends Number> Collector<T, ?, FloatList> toFloatList() {
        final Supplier<FloatList> supplier = new Supplier<FloatList>() {
            @Override
            public FloatList get() {
                return new FloatList();
            }
        };

        final BiConsumer<FloatList, T> accumulator = new BiConsumer<FloatList, T>() {
            @Override
            public void accept(FloatList c, T t) {
                c.add(t.floatValue());
            }
        };

        final BinaryOperator<FloatList> combiner = new BinaryOperator<FloatList>() {
            @Override
            public FloatList apply(FloatList a, FloatList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T extends Number> Collector<T, ?, float[]> toFloatArray() {
        return toFloatArray(N.EMPTY_FLOAT_ARRAY, 0);
    }

    public static <T extends Number> Collector<T, ?, float[]> toFloatArray(final Supplier<float[]> supplier) {
        return toFloatArray(supplier.get(), 0);
    }

    static <T extends Number> Collector<T, ?, float[]> toFloatArray(final float[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<FloatList> supplier = new Supplier<FloatList>() {
            @Override
            public FloatList get() {
                return new FloatList(array, fromIndex);
            }
        };

        final BiConsumer<FloatList, T> accumulator = new BiConsumer<FloatList, T>() {
            @Override
            public void accept(FloatList c, T t) {
                c.add(t.floatValue());
            }
        };

        final BinaryOperator<FloatList> combiner = new BinaryOperator<FloatList>() {
            @Override
            public FloatList apply(FloatList a, FloatList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<FloatList, float[]> finisher = new Function<FloatList, float[]>() {
            @Override
            public float[] apply(FloatList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T extends Number> Collector<T, ?, DoubleList> toDoubleList() {
        final Supplier<DoubleList> supplier = new Supplier<DoubleList>() {
            @Override
            public DoubleList get() {
                return new DoubleList();
            }
        };

        final BiConsumer<DoubleList, T> accumulator = new BiConsumer<DoubleList, T>() {
            @Override
            public void accept(DoubleList c, T t) {
                c.add(t.doubleValue());
            }
        };

        final BinaryOperator<DoubleList> combiner = new BinaryOperator<DoubleList>() {
            @Override
            public DoubleList apply(DoubleList a, DoubleList b) {
                a.addAll(b);
                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T extends Number> Collector<T, ?, double[]> toDoubleArray() {
        return toDoubleArray(N.EMPTY_DOUBLE_ARRAY, 0);
    }

    public static <T extends Number> Collector<T, ?, double[]> toDoubleArray(final Supplier<double[]> supplier) {
        return toDoubleArray(supplier.get(), 0);
    }

    static <T extends Number> Collector<T, ?, double[]> toDoubleArray(final double[] array, final int fromIndex) {
        if (fromIndex < 0 || fromIndex > array.length) {
            throw new IllegalArgumentException("'fromIndex' can't be negative or bigger than array's length");
        }

        final Supplier<DoubleList> supplier = new Supplier<DoubleList>() {
            @Override
            public DoubleList get() {
                return new DoubleList(array, fromIndex);
            }
        };

        final BiConsumer<DoubleList, T> accumulator = new BiConsumer<DoubleList, T>() {
            @Override
            public void accept(DoubleList c, T t) {
                c.add(t.doubleValue());
            }
        };

        final BinaryOperator<DoubleList> combiner = new BinaryOperator<DoubleList>() {
            @Override
            public DoubleList apply(DoubleList a, DoubleList b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<DoubleList, double[]> finisher = new Function<DoubleList, double[]>() {
            @Override
            public double[] apply(DoubleList t) {
                return t.array() == array ? array : t.trimToSize().array();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that concatenates the input elements into a
     * {@code String}, in encounter order.
     *
     * @return a {@code Collector} that concatenates the input elements into a
     * {@code String}, in encounter order
     */
    public static Collector<CharSequence, ?, String> joining() {
        final Supplier<StringBuilder> supplier = new Supplier<StringBuilder>() {
            @Override
            public StringBuilder get() {
                return new StringBuilder();
            }
        };

        final BiConsumer<StringBuilder, CharSequence> accumulator = new BiConsumer<StringBuilder, CharSequence>() {
            @Override
            public void accept(StringBuilder a, CharSequence t) {
                a.append(t);
            }
        };

        final BinaryOperator<StringBuilder> combiner = new BinaryOperator<StringBuilder>() {
            @Override
            public StringBuilder apply(StringBuilder a, StringBuilder b) {
                a.append(b);
                return a;
            }
        };

        final Function<StringBuilder, String> finisher = new Function<StringBuilder, String>() {
            @Override
            public String apply(StringBuilder a) {
                return a.toString();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that concatenates the input elements,
     * separated by the specified delimiter, in encounter order.
     *
     * @param delimiter the delimiter to be used between each element
     * @return A {@code Collector} which concatenates CharSequence elements,
     * separated by the specified delimiter, in encounter order
     */
    public static Collector<CharSequence, ?, String> joining(CharSequence delimiter) {
        return joining(delimiter, "", "");
    }

    /**
     * Returns a {@code Collector} that concatenates the input elements,
     * separated by the specified delimiter, with the specified prefix and
     * suffix, in encounter order.
     *
     * @param delimiter the delimiter to be used between each element
     * @param  prefix the sequence of characters to be used at the beginning
     *                of the joined result
     * @param  suffix the sequence of characters to be used at the end
     *                of the joined result
     * @return A {@code Collector} which concatenates CharSequence elements,
     * separated by the specified delimiter, in encounter order
     */
    public static Collector<CharSequence, ?, String> joining(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        final Supplier<StringJoiner> supplier = new Supplier<StringJoiner>() {
            @Override
            public StringJoiner get() {
                return new StringJoiner(delimiter, prefix, suffix);
            }
        };

        final BiConsumer<StringJoiner, CharSequence> accumulator = new BiConsumer<StringJoiner, CharSequence>() {
            @Override
            public void accept(StringJoiner a, CharSequence t) {
                a.add(t);
            }
        };

        final BinaryOperator<StringJoiner> combiner = new BinaryOperator<StringJoiner>() {
            @Override
            public StringJoiner apply(StringJoiner a, StringJoiner b) {
                a.merge(b);
                return a;
            }
        };

        final Function<StringJoiner, String> finisher = new Function<StringJoiner, String>() {
            @Override
            public String apply(StringJoiner a) {
                return a.toString();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * {@code BinaryOperator<Map>} that merges the contents of its right
     * argument into its left argument, using the provided merge function to
     * handle duplicate keys.
     *
     * @param <K> type of the map keys
     * @param <V> type of the map values
     * @param <M> type of the map
     * @param mergeFunction A merge function suitable for
     * {@link Map#merge(Object, Object, BiFunction) Map.merge()}
     * @return a merge function for two maps
     */
    private static <K, V, M extends Map<K, V>> BinaryOperator<M> mapMerger(final BinaryOperator<V> mergeFunction) {
        Objects.requireNonNull(mergeFunction);

        return new BinaryOperator<M>() {
            @Override
            public M apply(M m1, M m2) {
                K key = null;
                V value = null;
                for (Map.Entry<K, V> e : m2.entrySet()) {
                    Objects.requireNonNull(e.getValue());
                    key = e.getKey();
                    value = e.getValue();
                    V oldValue = m1.get(key);
                    V newValue = (oldValue == null) ? value : mergeFunction.apply(oldValue, value);
                    if (newValue == null) {
                        m1.remove(key);
                    } else {
                        m1.put(key, newValue);
                    }
                }
                return m1;
            }
        };
    }

    private static <K, V, M extends ConcurrentMap<K, V>> BinaryOperator<M> mapMerger2(final BinaryOperator<V> mergeFunction) {
        Objects.requireNonNull(mergeFunction);

        return new BinaryOperator<M>() {
            @Override
            public M apply(M m1, M m2) {
                K key = null;
                V value = null;
                for (Map.Entry<K, V> e : m2.entrySet()) {
                    Objects.requireNonNull(e.getValue());
                    key = e.getKey();
                    value = e.getValue();
                    V oldValue = m1.get(key);
                    V newValue = (oldValue == null) ? value : mergeFunction.apply(oldValue, value);
                    if (newValue == null) {
                        m1.remove(key);
                    } else {
                        m1.put(key, newValue);
                    }
                }
                return m1;
            }
        };
    }

    private static <K, U, V extends Collection<U>> BinaryOperator<Multimap<K, U, V>> mapMerger3() {
        return new BinaryOperator<Multimap<K, U, V>>() {
            @Override
            public Multimap<K, U, V> apply(Multimap<K, U, V> m1, Multimap<K, U, V> m2) {
                K key = null;
                V value = null;
                for (Map.Entry<K, V> e : m2.entrySet()) {
                    Objects.requireNonNull(e.getValue());
                    key = e.getKey();
                    value = e.getValue();

                    if (N.notNullOrEmpty(value)) {
                        V oldValue = m1.get(key);

                        if (oldValue == null) {
                            m1.putAll(key, value);
                        } else {
                            oldValue.addAll(value);
                        }
                    }
                }
                return m1;
            }
        };
    }

    /**
     * Adapts a {@code Collector} accepting elements of type {@code U} to one
     * accepting elements of type {@code T} by applying a mapping function to
     * each input element before accumulation.
     *
     * @apiNote
     * The {@code mapping()} collectors are most useful when used in a
     * multi-level reduction, such as downstream of a {@code groupingBy} or
     * {@code partitioningBy}.  For example, given a stream of
     * {@code Person}, to accumulate the set of last names in each city:
     * <pre>{@code
     *     Map<City, Set<String>> lastNamesByCity
     *         = people.stream().collect(groupingBy(Person::getCity,
     *                                              mapping(Person::getLastName, toSet())));
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param <U> type of elements accepted by downstream collector
     * @param <A> intermediate accumulation type of the downstream collector
     * @param <R> result type of collector
     * @param mapper a function to be applied to the input elements
     * @param downstream a collector which will accept mapped values
     * @return a collector which applies the mapping function to the input
     * elements and provides the mapped results to the downstream collector
     */
    public static <T, U, A, R> Collector<T, ?, R> mapping(final Function<? super T, ? extends U> mapper, final Collector<? super U, A, R> downstream) {
        final BiConsumer<A, ? super U> downstreamAccumulator = downstream.accumulator();

        final BiConsumer<A, T> accumulator = new BiConsumer<A, T>() {
            @Override
            public void accept(A a, T t) {
                downstreamAccumulator.accept(a, mapper.apply(t));
            }
        };

        return new CollectorImpl<>(downstream.supplier(), accumulator, downstream.combiner(), downstream.finisher(), downstream.characteristics());
    }

    /**
     * Adapts a {@code Collector} to perform an additional finishing
     * transformation.  For example, one could adapt the {@link #toList()}
     * collector to always produce an immutable list with:
     * <pre>{@code
     *     List<String> people
     *         = people.stream().collect(collectingAndThen(toList(), Collections::unmodifiableList));
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param <A> intermediate accumulation type of the downstream collector
     * @param <R> result type of the downstream collector
     * @param <RR> result type of the resulting collector
     * @param downstream a collector
     * @param finisher a function to be applied to the final result of the downstream collector
     * @return a collector which performs the action of the downstream collector,
     * followed by an additional finishing step
     */
    public static <T, A, R, RR> Collector<T, A, RR> collectingAndThen(final Collector<T, A, R> downstream, final Function<R, RR> finisher) {
        Objects.requireNonNull(finisher);

        final Function<A, R> downstreamFinisher = downstream.finisher();

        final Function<A, RR> thenFinisher = new Function<A, RR>() {
            @Override
            public RR apply(A t) {
                return finisher.apply(downstreamFinisher.apply(t));
            }
        };

        Set<Collector.Characteristics> characteristics = downstream.characteristics();

        if (characteristics.contains(Collector.Characteristics.IDENTITY_FINISH)) {
            if (characteristics.size() == 1)
                characteristics = Collectors.CH_NOID;
            else {
                characteristics = EnumSet.copyOf(characteristics);
                characteristics.remove(Collector.Characteristics.IDENTITY_FINISH);
                characteristics = Collections.unmodifiableSet(characteristics);
            }
        }

        return new CollectorImpl<>(downstream.supplier(), downstream.accumulator(), downstream.combiner(), thenFinisher, characteristics);
    }

    /**
     * Returns a {@code Collector} accepting elements of type {@code T} that
     * counts the number of input elements.  If no elements are present, the
     * result is 0.
     *
     * @implSpec
     * This produces a result equivalent to:
     * <pre>{@code
     *     reducing(0L, e -> 1L, Long::sum)
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} that counts the input elements
     */
    public static <T> Collector<T, ?, Long> counting() {
        final Function<? super T, ? extends Long> mapper = new Function<T, Long>() {
            @Override
            public Long apply(T t) {
                return 1L;
            }
        };

        final BinaryOperator<Long> op = new BinaryOperator<Long>() {
            @Override
            public Long apply(Long a, Long b) {
                return a.longValue() + b.longValue();
            }
        };

        return reducing(0L, mapper, op);
    }

    /**
     * Returns a {@code Collector} that produces the minimal element according
     * to a given {@code Comparator}, described as an {@code Optional<T>}.
     *
     * @implSpec
     * This produces a result equivalent to:
     * <pre>{@code
     *     reducing(BinaryOperator.minBy(comparator))
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param comparator a {@code Comparator} for comparing elements
     * @return a {@code Collector} that produces the minimal value
     */
    public static <T> Collector<T, ?, Optional<T>> minBy(final Comparator<? super T> comparator) {
        Objects.requireNonNull(comparator);

        final BinaryOperator<T> op = new BinaryOperator<T>() {
            @Override
            public T apply(T a, T b) {
                return comparator.compare(a, b) <= 0 ? a : b;
            }
        };

        return reducing(op);
    }

    public static <T> Collector<T, ?, T> minByOrGet(final Comparator<? super T> comparator, final Supplier<? extends T> other) {
        Objects.requireNonNull(comparator);

        final BinaryOperator<T> op = new BinaryOperator<T>() {
            @Override
            public T apply(T a, T b) {
                return comparator.compare(a, b) <= 0 ? a : b;
            }
        };

        return reducingOrGet(op, other);
    }

    public static <T, X extends RuntimeException> Collector<T, ?, T> minByOrThrow(final Comparator<? super T> comparator,
            final Supplier<? extends X> exceptionSupplier) {
        Objects.requireNonNull(comparator);

        final BinaryOperator<T> op = new BinaryOperator<T>() {
            @Override
            public T apply(T a, T b) {
                return comparator.compare(a, b) <= 0 ? a : b;
            }
        };

        return reducingOrThrow(op, exceptionSupplier);
    }

    /**
     * Returns a {@code Collector} that produces the maximal element according
     * to a given {@code Comparator}, described as an {@code Optional<T>}.
     *
     * @implSpec
     * This produces a result equivalent to:
     * <pre>{@code
     *     reducing(BinaryOperator.maxBy(comparator))
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param comparator a {@code Comparator} for comparing elements
     * @return a {@code Collector} that produces the maximal value
     */
    public static <T> Collector<T, ?, Optional<T>> maxBy(final Comparator<? super T> comparator) {
        Objects.requireNonNull(comparator);

        final BinaryOperator<T> op = new BinaryOperator<T>() {
            @Override
            public T apply(T a, T b) {
                return comparator.compare(a, b) >= 0 ? a : b;
            }
        };

        return reducing(op);
    }

    public static <T> Collector<T, ?, T> maxByOrGet(final Comparator<? super T> comparator, final Supplier<? extends T> other) {
        Objects.requireNonNull(comparator);

        final BinaryOperator<T> op = new BinaryOperator<T>() {
            @Override
            public T apply(T a, T b) {
                return comparator.compare(a, b) >= 0 ? a : b;
            }
        };

        return reducingOrGet(op, other);
    }

    public static <T, X extends RuntimeException> Collector<T, ?, T> maxByOrThrow(final Comparator<? super T> comparator,
            final Supplier<? extends X> exceptionSupplier) {
        Objects.requireNonNull(comparator);

        final BinaryOperator<T> op = new BinaryOperator<T>() {
            @Override
            public T apply(T a, T b) {
                return comparator.compare(a, b) >= 0 ? a : b;
            }
        };

        return reducingOrThrow(op, exceptionSupplier);
    }

    /**
     * Returns a {@code Collector} that produces the sum of a integer-valued
     * function applied to the input elements.  If no elements are present,
     * the result is 0.
     *
     * @param <T> the type of the input elements
     * @param mapper a function extracting the property to be summed
     * @return a {@code Collector} that produces the sum of a derived property
     */
    public static <T> Collector<T, ?, Long> summingInt(final ToIntFunction<? super T> mapper) {
        final Supplier<long[]> supplier = new Supplier<long[]>() {
            @Override
            public long[] get() {
                return new long[1];
            }
        };

        final BiConsumer<long[], T> accumulator = new BiConsumer<long[], T>() {
            @Override
            public void accept(long[] a, T t) {
                a[0] += mapper.applyAsInt(t);
            }
        };

        final BinaryOperator<long[]> combiner = new BinaryOperator<long[]>() {
            @Override
            public long[] apply(long[] a, long[] b) {
                a[0] += b[0];
                return a;
            }
        };

        final Function<long[], Long> finisher = new Function<long[], Long>() {
            @Override
            public Long apply(long[] a) {
                return a[0];
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that produces the sum of a long-valued
     * function applied to the input elements.  If no elements are present,
     * the result is 0.
     *
     * @param <T> the type of the input elements
     * @param mapper a function extracting the property to be summed
     * @return a {@code Collector} that produces the sum of a derived property
     */
    public static <T> Collector<T, ?, Long> summingLong(final ToLongFunction<? super T> mapper) {
        final Supplier<long[]> supplier = new Supplier<long[]>() {
            @Override
            public long[] get() {
                return new long[1];
            }
        };

        final BiConsumer<long[], T> accumulator = new BiConsumer<long[], T>() {
            @Override
            public void accept(long[] a, T t) {
                a[0] += mapper.applyAsLong(t);
            }
        };

        final BinaryOperator<long[]> combiner = new BinaryOperator<long[]>() {
            @Override
            public long[] apply(long[] a, long[] b) {
                a[0] += b[0];
                return a;
            }
        };

        final Function<long[], Long> finisher = new Function<long[], Long>() {
            @Override
            public Long apply(long[] a) {
                return a[0];
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that produces the sum of a double-valued
     * function applied to the input elements.  If no elements are present,
     * the result is 0.
     *
     * <p>The sum returned can vary depending upon the order in which
     * values are recorded, due to accumulated rounding error in
     * addition of values of differing magnitudes. Values sorted by increasing
     * absolute magnitude tend to yield more accurate results.  If any recorded
     * value is a {@code NaN} or the sum is at any point a {@code NaN} then the
     * sum will be {@code NaN}.
     *
     * @param <T> the type of the input elements
     * @param mapper a function extracting the property to be summed
     * @return a {@code Collector} that produces the sum of a derived property
     */
    public static <T> Collector<T, ?, Double> summingDouble(final ToDoubleFunction<? super T> mapper) {
        /*
         * In the arrays allocated for the collect operation, index 0
         * holds the high-order bits of the running sum, index 1 holds
         * the low-order bits of the sum computed via compensated
         * summation, and index 2 holds the simple sum used to compute
         * the proper result if the stream contains infinite values of
         * the same sign.
         */
        //        return new CollectorImpl<>(
        //                () -> new double[3],
        //                (a, t) -> { sumWithCompensation(a, mapper.applyAsDouble(t));
        //                            a[2] += mapper.applyAsDouble(t);},
        //                (a, b) -> { sumWithCompensation(a, b[0]);
        //                            a[2] += b[2];
        //                            return sumWithCompensation(a, b[1]); },
        //                a -> computeFinalSum(a),
        //                CH_NOID);

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[3];
            }
        };

        final BiConsumer<double[], T> accumulator = new BiConsumer<double[], T>() {
            @Override
            public void accept(double[] a, T t) {
                final double d = mapper.applyAsDouble(t);
                sumWithCompensation(a, d);
                a[2] += d;
            }
        };

        final BinaryOperator<double[]> combiner = new BinaryOperator<double[]>() {
            @Override
            public double[] apply(double[] a, double[] b) {
                sumWithCompensation(a, b[0]);
                a[2] += b[2];
                return sumWithCompensation(a, b[1]);
            }
        };

        final Function<double[], Double> finisher = new Function<double[], Double>() {
            @Override
            public Double apply(double[] a) {
                return computeFinalSum(a);
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * Incorporate a new double value using Kahan summation /
     * compensation summation.
     *
     * High-order bits of the sum are in intermediateSum[0], low-order
     * bits of the sum are in intermediateSum[1], any additional
     * elements are application-specific.
     *
     * @param intermediateSum the high-order and low-order words of the intermediate sum
     * @param value the name value to be included in the running sum
     */
    static double[] sumWithCompensation(double[] intermediateSum, double value) {
        double tmp = value - intermediateSum[1];
        double sum = intermediateSum[0];
        double velvel = sum + tmp; // Little wolf of rounding error
        intermediateSum[1] = (velvel - sum) - tmp;
        intermediateSum[0] = velvel;
        return intermediateSum;
    }

    /**
     * If the compensated sum is spuriously NaN from accumulating one
     * or more same-signed infinite values, return the
     * correctly-signed infinity stored in the simple sum.
     */
    static double computeFinalSum(double[] summands) {
        // Better error bounds to add both terms as the final sum
        double tmp = summands[0] + summands[1];
        double simpleSum = summands[summands.length - 1];
        if (Double.isNaN(tmp) && Double.isInfinite(simpleSum))
            return simpleSum;
        else
            return tmp;
    }

    /**
     * Returns a {@code Collector} that produces the arithmetic mean of an integer-valued
     * function applied to the input elements.  If no elements are present,
     * the result is 0.
     *
     * @param <T> the type of the input elements
     * @param mapper a function extracting the property to be summed
     * @return a {@code Collector} that produces the sum of a derived property
     */
    public static <T> Collector<T, ?, Double> averagingInt(final ToIntFunction<? super T> mapper) {
        final Supplier<long[]> supplier = new Supplier<long[]>() {
            @Override
            public long[] get() {
                return new long[2];
            }
        };

        final BiConsumer<long[], T> accumulator = new BiConsumer<long[], T>() {
            @Override
            public void accept(long[] a, T t) {
                a[0] += mapper.applyAsInt(t);
                a[1]++;
            }
        };

        final BinaryOperator<long[]> combiner = new BinaryOperator<long[]>() {
            @Override
            public long[] apply(long[] a, long[] b) {
                a[0] += b[0];
                a[1] += b[1];
                return a;
            }
        };

        final Function<long[], Double> finisher = new Function<long[], Double>() {
            @Override
            public Double apply(long[] a) {
                return (a[1] == 0) ? 0d : (double) a[0] / a[1];
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T> Collector<T, ?, OptionalDouble> averagingInt2(final ToIntFunction<? super T> mapper) {
        final Collector<T, long[], Double> collector = (Collector<T, long[], Double>) averagingInt(mapper);

        final Function<long[], OptionalDouble> finisher = new Function<long[], OptionalDouble>() {
            @Override
            public OptionalDouble apply(long[] a) {
                if (a[1] == 0) {
                    return OptionalDouble.empty();
                } else {
                    return OptionalDouble.of((double) a[0] / a[1]);
                }
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T> Collector<T, ?, Double> averagingInt2OrGet(final ToIntFunction<? super T> mapper, final DoubleSupplier other) {
        final Collector<T, long[], OptionalDouble> collector = (Collector<T, long[], OptionalDouble>) averagingInt2(mapper);

        final Function<long[], Double> finisher = new Function<long[], Double>() {
            @Override
            public Double apply(long[] a) {
                return collector.finisher().apply(a).orGet(other);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T, X extends RuntimeException> Collector<T, ?, Double> averagingInt2OrThrow(final ToIntFunction<? super T> mapper,
            final Supplier<? extends X> exceptionSupplier) {
        final Collector<T, long[], OptionalDouble> collector = (Collector<T, long[], OptionalDouble>) averagingInt2(mapper);

        final Function<long[], Double> finisher = new Function<long[], Double>() {
            @Override
            public Double apply(long[] a) {
                return collector.finisher().apply(a).orThrow(exceptionSupplier);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that produces the arithmetic mean of a long-valued
     * function applied to the input elements.  If no elements are present,
     * the result is 0.
     *
     * @param <T> the type of the input elements
     * @param mapper a function extracting the property to be summed
     * @return a {@code Collector} that produces the sum of a derived property
     */
    public static <T> Collector<T, ?, Double> averagingLong(final ToLongFunction<? super T> mapper) {
        final Supplier<long[]> supplier = new Supplier<long[]>() {
            @Override
            public long[] get() {
                return new long[2];
            }
        };

        final BiConsumer<long[], T> accumulator = new BiConsumer<long[], T>() {
            @Override
            public void accept(long[] a, T t) {
                a[0] += mapper.applyAsLong(t);
                a[1]++;
            }
        };

        final BinaryOperator<long[]> combiner = new BinaryOperator<long[]>() {
            @Override
            public long[] apply(long[] a, long[] b) {
                a[0] += b[0];
                a[1] += b[1];
                return a;
            }
        };

        final Function<long[], Double> finisher = new Function<long[], Double>() {
            @Override
            public Double apply(long[] a) {
                return a[1] == 0 ? 0d : (double) a[0] / a[1];
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T> Collector<T, ?, OptionalDouble> averagingLong2(final ToLongFunction<? super T> mapper) {
        final Collector<T, long[], Double> collector = (Collector<T, long[], Double>) averagingLong(mapper);

        final Function<long[], OptionalDouble> finisher = new Function<long[], OptionalDouble>() {
            @Override
            public OptionalDouble apply(long[] a) {
                if (a[1] == 0) {
                    return OptionalDouble.empty();
                } else {
                    return OptionalDouble.of((double) a[0] / a[1]);
                }
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T> Collector<T, ?, Double> averagingLong2OrGet(final ToLongFunction<? super T> mapper, final DoubleSupplier other) {
        final Collector<T, long[], OptionalDouble> collector = (Collector<T, long[], OptionalDouble>) averagingLong2(mapper);

        final Function<long[], Double> finisher = new Function<long[], Double>() {
            @Override
            public Double apply(long[] a) {
                return collector.finisher().apply(a).orGet(other);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T, X extends RuntimeException> Collector<T, ?, Double> averagingLong2OrThrow(final ToLongFunction<? super T> mapper,
            final Supplier<? extends X> exceptionSupplier) {
        final Collector<T, long[], OptionalDouble> collector = (Collector<T, long[], OptionalDouble>) averagingLong2(mapper);

        final Function<long[], Double> finisher = new Function<long[], Double>() {
            @Override
            public Double apply(long[] a) {
                return collector.finisher().apply(a).orThrow(exceptionSupplier);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that produces the arithmetic mean of a double-valued
     * function applied to the input elements.  If no elements are present,
     * the result is 0.
     *
     * <p>The average returned can vary depending upon the order in which
     * values are recorded, due to accumulated rounding error in
     * addition of values of differing magnitudes. Values sorted by increasing
     * absolute magnitude tend to yield more accurate results.  If any recorded
     * value is a {@code NaN} or the sum is at any point a {@code NaN} then the
     * average will be {@code NaN}.
     *
     * @implNote The {@code double} format can represent all
     * consecutive integers in the range -2<sup>53</sup> to
     * 2<sup>53</sup>. If the pipeline has more than 2<sup>53</sup>
     * values, the divisor in the average computation will saturate at
     * 2<sup>53</sup>, leading to additional numerical errors.
     *
     * @param <T> the type of the input elements
     * @param mapper a function extracting the property to be summed
     * @return a {@code Collector} that produces the sum of a derived property
     */
    public static <T> Collector<T, ?, Double> averagingDouble(final ToDoubleFunction<? super T> mapper) {
        /*
         * In the arrays allocated for the collect operation, index 0
         * holds the high-order bits of the running sum, index 1 holds
         * the low-order bits of the sum computed via compensated
         * summation, and index 2 holds the number of values seen.
         */
        //        return new CollectorImpl<>(
        //                () -> new double[4],
        //                (a, t) -> { sumWithCompensation(a, mapper.applyAsDouble(t)); a[2]++; a[3]+= mapper.applyAsDouble(t);},
        //                (a, b) -> { sumWithCompensation(a, b[0]); sumWithCompensation(a, b[1]); a[2] += b[2]; a[3] += b[3]; return a; },
        //                a -> (a[2] == 0) ? 0.0d : (computeFinalSum(a) / a[2]),
        //                CH_NOID);

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[4];
            }
        };

        final BiConsumer<double[], T> accumulator = new BiConsumer<double[], T>() {
            @Override
            public void accept(double[] a, T t) {
                final double d = mapper.applyAsDouble(t);
                sumWithCompensation(a, d);
                a[2]++;
                a[3] += d;
            }
        };

        final BinaryOperator<double[]> combiner = new BinaryOperator<double[]>() {
            @Override
            public double[] apply(double[] a, double[] b) {
                sumWithCompensation(a, b[0]);
                sumWithCompensation(a, b[1]);
                a[2] += b[2];
                a[3] += b[3];
                return a;
            }
        };

        final Function<double[], Double> finisher = new Function<double[], Double>() {
            @Override
            public Double apply(double[] a) {
                return a[2] == 0 ? 0d : computeFinalSum(a) / a[2];
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <T> Collector<T, ?, OptionalDouble> averagingDouble2(final ToDoubleFunction<? super T> mapper) {
        final Collector<T, double[], Double> collector = (Collector<T, double[], Double>) averagingDouble(mapper);

        final Function<double[], OptionalDouble> finisher = new Function<double[], OptionalDouble>() {
            @Override
            public OptionalDouble apply(double[] a) {
                if (a[2] == 0) {
                    return OptionalDouble.empty();
                } else {
                    return OptionalDouble.of(computeFinalSum(a) / a[2]);
                }
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T> Collector<T, ?, Double> averagingDouble2OrGet(final ToDoubleFunction<? super T> mapper, final DoubleSupplier other) {
        final Collector<T, double[], OptionalDouble> collector = (Collector<T, double[], OptionalDouble>) averagingDouble2(mapper);

        final Function<double[], Double> finisher = new Function<double[], Double>() {
            @Override
            public Double apply(double[] a) {
                return collector.finisher().apply(a).orGet(other);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T, X extends RuntimeException> Collector<T, ?, Double> averagingDouble2OrThrow(final ToDoubleFunction<? super T> mapper,
            final Supplier<? extends X> exceptionSupplier) {
        final Collector<T, double[], OptionalDouble> collector = (Collector<T, double[], OptionalDouble>) averagingDouble2(mapper);

        final Function<double[], Double> finisher = new Function<double[], Double>() {
            @Override
            public Double apply(double[] a) {
                return collector.finisher().apply(a).orThrow(exceptionSupplier);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} which performs a reduction of its
     * input elements under a specified {@code BinaryOperator} using the
     * provided identity.
     *
     * @apiNote
     * The {@code reducing()} collectors are most useful when used in a
     * multi-level reduction, downstream of {@code groupingBy} or
     * {@code partitioningBy}.  To perform a simple reduction on a stream,
     * use {@link Stream#reduce(Object, BinaryOperator)}} instead.
     *
     * @param <T> element type for the input and output of the reduction
     * @param identity the identity value for the reduction (also, the value
     *                 that is returned when there are no input elements)
     * @param op a {@code BinaryOperator<T>} used to reduce the input elements
     * @return a {@code Collector} which implements the reduction operation
     *
     * @see #reducing(BinaryOperator)
     * @see #reducing(Object, Function, BinaryOperator)
     */
    public static <T> Collector<T, ?, T> reducing(final T identity, final BinaryOperator<T> op) {
        final BiConsumer<T[], T> accumulator = new BiConsumer<T[], T>() {
            @Override
            public void accept(T[] a, T t) {
                a[0] = op.apply(a[0], t);
            }
        };

        final BinaryOperator<T[]> combiner = new BinaryOperator<T[]>() {
            @Override
            public T[] apply(T[] a, T[] b) {
                a[0] = op.apply(a[0], b[0]);
                return a;
            }
        };

        final Function<T[], T> finisher = new Function<T[], T>() {
            @Override
            public T apply(T[] a) {
                return a[0];
            }
        };

        return new CollectorImpl<>(boxSupplier(identity), accumulator, combiner, finisher, CH_NOID);
    }

    @SuppressWarnings("unchecked")
    private static <T> Supplier<T[]> boxSupplier(final T identity) {
        return new Supplier<T[]>() {
            @Override
            public T[] get() {
                return (T[]) new Object[] { identity };
            }
        };
    }

    /**
     * Returns a {@code Collector} which performs a reduction of its
     * input elements under a specified {@code BinaryOperator}.  The result
     * is described as an {@code Optional<T>}.
     *
     * @apiNote
     * The {@code reducing()} collectors are most useful when used in a
     * multi-level reduction, downstream of {@code groupingBy} or
     * {@code partitioningBy}.  To perform a simple reduction on a stream,
     * use {@link Stream#reduce(BinaryOperator)} instead.
     *
     * <p>For example, given a stream of {@code Person}, to calculate tallest
     * person in each city:
     * <pre>{@code
     *     Comparator<Person> byHeight = Comparator.comparing(Person::getHeight);
     *     Map<City, Person> tallestByCity
     *         = people.stream().collect(groupingBy(Person::getCity, reducing(BinaryOperator.maxBy(byHeight))));
     * }</pre>
     *
     * @param <T> element type for the input and output of the reduction
     * @param op a {@code BinaryOperator<T>} used to reduce the input elements
     * @return a {@code Collector} which implements the reduction operation
     *
     * @see #reducing(Object, BinaryOperator)
     * @see #reducing(Object, Function, BinaryOperator)
     */
    public static <T> Collector<T, ?, Optional<T>> reducing(final BinaryOperator<T> op) {
        final Supplier<OptionalBox<T>> supplier = new Supplier<OptionalBox<T>>() {
            @Override
            public OptionalBox<T> get() {
                return new OptionalBox<T>(op);
            }
        };

        final BiConsumer<OptionalBox<T>, T> accumulator = new BiConsumer<OptionalBox<T>, T>() {
            @Override
            public void accept(OptionalBox<T> a, T t) {
                a.accept(t);
            }
        };

        final BinaryOperator<OptionalBox<T>> combiner = new BinaryOperator<OptionalBox<T>>() {
            @Override
            public OptionalBox<T> apply(OptionalBox<T> a, OptionalBox<T> b) {
                if (b.present) {
                    a.accept(b.value);
                }

                return a;
            }
        };

        final Function<OptionalBox<T>, Optional<T>> finisher = new Function<OptionalBox<T>, Optional<T>>() {
            @Override
            public Optional<T> apply(OptionalBox<T> a) {
                return Optional.ofNullable(a.value);
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    private static class OptionalBox<T> implements Consumer<T> {
        BinaryOperator<T> op = null;
        T value = null;
        boolean present = false;

        OptionalBox(final BinaryOperator<T> op) {
            this.op = op;
        }

        @Override
        public void accept(T t) {
            if (present) {
                value = op.apply(value, t);
            } else {
                value = t;
                present = true;
            }
        }
    }

    public static <T> Collector<T, ?, T> reducingOrGet(final BinaryOperator<T> op, final Supplier<? extends T> other) {
        final Collector<T, OptionalBox<T>, Optional<T>> collector = (Collector<T, OptionalBox<T>, Optional<T>>) reducing(op);

        final Function<OptionalBox<T>, T> finisher = new Function<OptionalBox<T>, T>() {
            @Override
            public T apply(OptionalBox<T> a) {
                return collector.finisher().apply(a).orGet(other);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T, X extends RuntimeException> Collector<T, ?, T> reducingOrThrow(final BinaryOperator<T> op,
            final Supplier<? extends X> exceptionSupplier) {
        final Collector<T, OptionalBox<T>, Optional<T>> collector = (Collector<T, OptionalBox<T>, Optional<T>>) reducing(op);

        final Function<OptionalBox<T>, T> finisher = new Function<OptionalBox<T>, T>() {
            @Override
            public T apply(OptionalBox<T> a) {
                return collector.finisher().apply(a).orThrow(exceptionSupplier);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} which performs a reduction of its
     * input elements under a specified mapping function and
     * {@code BinaryOperator}. This is a generalization of
     * {@link #reducing(Object, BinaryOperator)} which allows a transformation
     * of the elements before reduction.
     *
     * @apiNote
     * The {@code reducing()} collectors are most useful when used in a
     * multi-level reduction, downstream of {@code groupingBy} or
     * {@code partitioningBy}.  To perform a simple map-reduce on a stream,
     * use {@link Stream#map(Function)} and {@link Stream#reduce(Object, BinaryOperator)}
     * instead.
     *
     * <p>For example, given a stream of {@code Person}, to calculate the longest
     * last name of residents in each city:
     * <pre>{@code
     *     Comparator<String> byLength = Comparator.comparing(String::length);
     *     Map<City, String> longestLastNameByCity
     *         = people.stream().collect(groupingBy(Person::getCity,
     *                                              reducing(Person::getLastName, BinaryOperator.maxBy(byLength))));
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param <U> the type of the mapped values
     * @param identity the identity value for the reduction (also, the value
     *                 that is returned when there are no input elements)
     * @param mapper a mapping function to apply to each input value
     * @param op a {@code BinaryOperator<U>} used to reduce the mapped values
     * @return a {@code Collector} implementing the map-reduce operation
     *
     * @see #reducing(Object, BinaryOperator)
     * @see #reducing(BinaryOperator)
     */
    public static <T, U> Collector<T, ?, U> reducing(final U identity, final Function<? super T, ? extends U> mapper, final BinaryOperator<U> op) {
        final BiConsumer<U[], T> accumulator = new BiConsumer<U[], T>() {
            @Override
            public void accept(U[] a, T t) {
                a[0] = op.apply(a[0], mapper.apply(t));
            }
        };

        final BinaryOperator<U[]> combiner = new BinaryOperator<U[]>() {
            @Override
            public U[] apply(U[] a, U[] b) {
                a[0] = op.apply(a[0], b[0]);

                return a;
            }
        };

        final Function<U[], U> finisher = new Function<U[], U>() {
            @Override
            public U apply(U[] a) {
                return a[0];
            }
        };

        return new CollectorImpl<>(boxSupplier(identity), accumulator, combiner, finisher, CH_NOID);
    }

    public static <T, U> Collector<T, ?, Optional<U>> reducing(final Function<? super T, ? extends U> mapper, final BinaryOperator<U> op) {
        final Supplier<OptionalBox2<T, U>> supplier = new Supplier<OptionalBox2<T, U>>() {
            @Override
            public OptionalBox2<T, U> get() {
                return new OptionalBox2<T, U>(mapper, op);
            }
        };

        final BiConsumer<OptionalBox2<T, U>, T> accumulator = new BiConsumer<OptionalBox2<T, U>, T>() {
            @Override
            public void accept(OptionalBox2<T, U> a, T t) {
                a.accept(t);
            }
        };

        final BinaryOperator<OptionalBox2<T, U>> combiner = new BinaryOperator<OptionalBox2<T, U>>() {
            @Override
            public OptionalBox2<T, U> apply(OptionalBox2<T, U> a, OptionalBox2<T, U> b) {
                if (b.present) {
                    a.value = b.value;
                    a.present = true;
                }

                return a;
            }
        };

        final Function<OptionalBox2<T, U>, Optional<U>> finisher = new Function<OptionalBox2<T, U>, Optional<U>>() {
            @Override
            public Optional<U> apply(OptionalBox2<T, U> a) {
                return Optional.ofNullable(a.value);
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    private static class OptionalBox2<T, U> implements Consumer<T> {
        Function<? super T, ? extends U> mapper;
        BinaryOperator<U> op;
        U value = null;
        boolean present = false;

        OptionalBox2(final Function<? super T, ? extends U> mapper, final BinaryOperator<U> op) {
            this.mapper = mapper;
            this.op = op;
        }

        @Override
        public void accept(T t) {
            if (present) {
                value = op.apply(value, mapper.apply(t));
            } else {
                value = mapper.apply(t);
                present = true;
            }
        }
    }

    public static <T, U> Collector<T, ?, U> reducingOrGet(final Function<? super T, ? extends U> mapper, final BinaryOperator<U> op,
            final Supplier<? extends U> other) {
        final Collector<T, OptionalBox2<T, U>, Optional<U>> collector = (Collector<T, OptionalBox2<T, U>, Optional<U>>) reducing(mapper, op);

        final Function<OptionalBox2<T, U>, U> finisher = new Function<OptionalBox2<T, U>, U>() {
            @Override
            public U apply(OptionalBox2<T, U> a) {
                return collector.finisher().apply(a).orGet(other);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    public static <T, U, X extends RuntimeException> Collector<T, ?, U> reducingOrThrow(final Function<? super T, ? extends U> mapper,
            final BinaryOperator<U> op, final Supplier<? extends X> exceptionSupplier) {
        final Collector<T, OptionalBox2<T, U>, Optional<U>> collector = (Collector<T, OptionalBox2<T, U>, Optional<U>>) reducing(mapper, op);

        final Function<OptionalBox2<T, U>, U> finisher = new Function<OptionalBox2<T, U>, U>() {
            @Override
            public U apply(OptionalBox2<T, U> a) {
                return collector.finisher().apply(a).orThrow(exceptionSupplier);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    /**
     * Returns a {@code Collector} implementing a "group by" operation on
     * input elements of type {@code T}, grouping elements according to a
     * classification function, and returning the results in a {@code Map}.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The collector produces a {@code Map<K, List<T>>} whose keys are the
     * values resulting from applying the classification function to the input
     * elements, and whose corresponding values are {@code List}s containing the
     * input elements which map to the associated key under the classification
     * function.
     *
     * <p>There are no guarantees on the type, mutability, serializability, or
     * thread-safety of the {@code Map} or {@code List} objects returned.
     * @implSpec
     * This produces a result similar to:
     * <pre>{@code
     *     groupingBy(classifier, toList());
     * }</pre>
     *
     * @implNote
     * The returned {@code Collector} is not concurrent.  For parallel stream
     * pipelines, the {@code combiner} function operates by merging the keys
     * from one map into another, which can be an expensive operation.  If
     * preservation of the order in which elements appear in the resulting {@code Map}
     * collector is not required, using {@link #groupingByConcurrent(Function)}
     * may offer better parallel performance.
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param classifier the classifier function mapping input elements to keys
     * @return a {@code Collector} implementing the group-by operation
     *
     * @see #groupingBy(Function, Collector)
     * @see #groupingBy(Function, Collector, Supplier)
     * @see #groupingByConcurrent(Function)
     */
    public static <T, K> Collector<T, ?, Map<K, List<T>>> groupingBy(Function<? super T, ? extends K> classifier) {
        final Collector<? super T, ?, List<T>> downstream = toList();

        return groupingBy(classifier, downstream);
    }

    public static <T, K, M extends Map<K, List<T>>> Collector<T, ?, M> groupingBy(final Function<? super T, ? extends K> classifier,
            final Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = toList();

        return groupingBy(classifier, downstream, mapFactory);
    }

    /**
     * Returns a {@code Collector} implementing a cascaded "group by" operation
     * on input elements of type {@code T}, grouping elements according to a
     * classification function, and then performing a reduction operation on
     * the values associated with a given key using the specified downstream
     * {@code Collector}.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The downstream collector operates on elements of type {@code T} and
     * produces a result of type {@code D}. The resulting collector produces a
     * {@code Map<K, D>}.
     *
     * <p>There are no guarantees on the type, mutability,
     * serializability, or thread-safety of the {@code Map} returned.
     *
     * <p>For example, to compute the set of last names of people in each city:
     * <pre>{@code
     *     Map<City, Set<String>> namesByCity
     *         = people.stream().collect(groupingBy(Person::getCity,
     *                                              mapping(Person::getLastName, toSet())));
     * }</pre>
     *
     * @implNote
     * The returned {@code Collector} is not concurrent.  For parallel stream
     * pipelines, the {@code combiner} function operates by merging the keys
     * from one map into another, which can be an expensive operation.  If
     * preservation of the order in which elements are presented to the downstream
     * collector is not required, using {@link #groupingByConcurrent(Function, Collector)}
     * may offer better parallel performance.
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param classifier a classifier function mapping input elements to keys
     * @param downstream a {@code Collector} implementing the downstream reduction
     * @return a {@code Collector} implementing the cascaded group-by operation
     * @see #groupingBy(Function)
     *
     * @see #groupingBy(Function, Collector, Supplier)
     * @see #groupingByConcurrent(Function, Collector)
     */
    public static <T, K, A, D> Collector<T, ?, Map<K, D>> groupingBy(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        };

        return groupingBy(classifier, downstream, mapFactory);
    }

    /**
     * Returns a {@code Collector} implementing a cascaded "group by" operation
     * on input elements of type {@code T}, grouping elements according to a
     * classification function, and then performing a reduction operation on
     * the values associated with a given key using the specified downstream
     * {@code Collector}.  The {@code Map} produced by the Collector is created
     * with the supplied factory function.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The downstream collector operates on elements of type {@code T} and
     * produces a result of type {@code D}. The resulting collector produces a
     * {@code Map<K, D>}.
     *
     * <p>For example, to compute the set of last names of people in each city,
     * where the city names are sorted:
     * <pre>{@code
     *     Map<City, Set<String>> namesByCity
     *         = people.stream().collect(groupingBy(Person::getCity, TreeMap::new,
     *                                              mapping(Person::getLastName, toSet())));
     * }</pre>
     *
     * @implNote
     * The returned {@code Collector} is not concurrent.  For parallel stream
     * pipelines, the {@code combiner} function operates by merging the keys
     * from one map into another, which can be an expensive operation.  If
     * preservation of the order in which elements are presented to the downstream
     * collector is not required, using {@link #groupingByConcurrent(Function, Collector, Supplier)}
     * may offer better parallel performance.
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param <M> the type of the resulting {@code Map}
     * @param classifier a classifier function mapping input elements to keys
     * @param downstream a {@code Collector} implementing the downstream reduction
     * @param mapFactory a function which, when called, produces a new empty
     *                   {@code Map} of the desired type
     * @return a {@code Collector} implementing the cascaded group-by operation
     *
     * @see #groupingBy(Function, Collector)
     * @see #groupingBy(Function)
     * @see #groupingByConcurrent(Function, Collector, Supplier)
     */
    public static <T, K, D, A, M extends Map<K, D>> Collector<T, ?, M> groupingBy(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) {

        //        Supplier<A> downstreamSupplier = downstream.supplier();
        //        BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        //        BiConsumer<Map<K, A>, T> accumulator = (m, t) -> {
        //            K key = Objects.requireNonNull(classifier.apply(t), "element cannot be mapped to a null key");
        //            A container = m.computeIfAbsent(key, k -> downstreamSupplier.get());
        //            downstreamAccumulator.accept(container, t);
        //        };
        //        BinaryOperator<Map<K, A>> merger = Collectors.<K, A, Map<K, A>>mapMerger(downstream.combiner());
        //        @SuppressWarnings("unchecked")
        //        Supplier<Map<K, A>> mangledFactory = (Supplier<Map<K, A>>) mapFactory;
        //
        //        if (downstream.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)) {
        //            return new CollectorImpl<>(mangledFactory, accumulator, merger, CH_ID);
        //        }
        //        else {
        //            @SuppressWarnings("unchecked")
        //            Function<A, A> downstreamFinisher = (Function<A, A>) downstream.finisher();
        //            Function<Map<K, A>, M> finisher = intermediate -> {
        //                intermediate.replaceAll((k, v) -> downstreamFinisher.apply(v));
        //                @SuppressWarnings("unchecked")
        //                M castResult = (M) intermediate;
        //                return castResult;
        //            };
        //            return new CollectorImpl<>(mangledFactory, accumulator, merger, finisher, CH_NOID);
        //        }

        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();

        final Function<K, A> mappingFunction = new Function<K, A>() {
            @Override
            public A apply(K k) {
                return downstreamSupplier.get();
            }
        };

        final BiConsumer<Map<K, A>, T> accumulator = new BiConsumer<Map<K, A>, T>() {
            @Override
            public void accept(Map<K, A> m, T t) {
                K key = Objects.requireNonNull(classifier.apply(t), "element cannot be mapped to a null key");
                A container = computeIfAbsent(m, key, mappingFunction);
                downstreamAccumulator.accept(container, t);
            }
        };

        final BinaryOperator<Map<K, A>> combiner = mapMerger(downstream.combiner());
        @SuppressWarnings("unchecked")
        final Supplier<Map<K, A>> mangledFactory = (Supplier<Map<K, A>>) mapFactory;

        @SuppressWarnings("unchecked")
        final Function<A, A> downstreamFinisher = (Function<A, A>) downstream.finisher();

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return downstreamFinisher.apply(v);
            }
        };

        if (downstream.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)) {
            return new CollectorImpl<>(mangledFactory, accumulator, combiner, CH_ID);
        } else {
            final Function<Map<K, A>, M> finisher = new Function<Map<K, A>, M>() {
                @Override
                public M apply(Map<K, A> intermediate) {
                    replaceAll(intermediate, function);
                    @SuppressWarnings("unchecked")
                    M castResult = (M) intermediate;
                    return castResult;
                }
            };

            return new CollectorImpl<>(mangledFactory, accumulator, combiner, finisher, CH_NOID);
        }
    }

    /**
     * Returns a concurrent {@code Collector} implementing a "group by"
     * operation on input elements of type {@code T}, grouping elements
     * according to a classification function.
     *
     * <p>This is a {@link Collector.Characteristics#CONCURRENT concurrent} and
     * {@link Collector.Characteristics#UNORDERED unordered} Collector.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The collector produces a {@code ConcurrentMap<K, List<T>>} whose keys are the
     * values resulting from applying the classification function to the input
     * elements, and whose corresponding values are {@code List}s containing the
     * input elements which map to the associated key under the classification
     * function.
     *
     * <p>There are no guarantees on the type, mutability, or serializability
     * of the {@code Map} or {@code List} objects returned, or of the
     * thread-safety of the {@code List} objects returned.
     * @implSpec
     * This produces a result similar to:
     * <pre>{@code
     *     groupingByConcurrent(classifier, toList());
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param classifier a classifier function mapping input elements to keys
     * @return a concurrent, unordered {@code Collector} implementing the group-by operation
     *
     * @see #groupingBy(Function)
     * @see #groupingByConcurrent(Function, Collector)
     * @see #groupingByConcurrent(Function, Collector, Supplier)
     */
    public static <T, K> Collector<T, ?, ConcurrentMap<K, List<T>>> groupingByConcurrent(Function<? super T, ? extends K> classifier) {
        final Collector<? super T, ?, List<T>> downstream = toList();

        return groupingByConcurrent(classifier, downstream);
    }

    public static <T, K, M extends ConcurrentMap<K, List<T>>> Collector<T, ?, M> groupingByConcurrent(final Function<? super T, ? extends K> classifier,
            final Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = toList();

        return groupingByConcurrent(classifier, downstream, mapFactory);
    }

    /**
     * Returns a concurrent {@code Collector} implementing a cascaded "group by"
     * operation on input elements of type {@code T}, grouping elements
     * according to a classification function, and then performing a reduction
     * operation on the values associated with a given key using the specified
     * downstream {@code Collector}.
     *
     * <p>This is a {@link Collector.Characteristics#CONCURRENT concurrent} and
     * {@link Collector.Characteristics#UNORDERED unordered} Collector.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The downstream collector operates on elements of type {@code T} and
     * produces a result of type {@code D}. The resulting collector produces a
     * {@code Map<K, D>}.
     *
     * <p>For example, to compute the set of last names of people in each city,
     * where the city names are sorted:
     * <pre>{@code
     *     ConcurrentMap<City, Set<String>> namesByCity
     *         = people.stream().collect(groupingByConcurrent(Person::getCity,
     *                                                        mapping(Person::getLastName, toSet())));
     * }</pre>
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param classifier a classifier function mapping input elements to keys
     * @param downstream a {@code Collector} implementing the downstream reduction
     * @return a concurrent, unordered {@code Collector} implementing the cascaded group-by operation
     *
     * @see #groupingBy(Function, Collector)
     * @see #groupingByConcurrent(Function)
     * @see #groupingByConcurrent(Function, Collector, Supplier)
     */
    public static <T, K, A, D> Collector<T, ?, ConcurrentMap<K, D>> groupingByConcurrent(Function<? super T, ? extends K> classifier,
            Collector<? super T, A, D> downstream) {
        final Supplier<ConcurrentMap<K, D>> mapFactory = new Supplier<ConcurrentMap<K, D>>() {
            @Override
            public ConcurrentMap<K, D> get() {
                return new ConcurrentHashMap<>();
            }
        };

        return groupingByConcurrent(classifier, downstream, mapFactory);
    }

    /**
     * Returns a concurrent {@code Collector} implementing a cascaded "group by"
     * operation on input elements of type {@code T}, grouping elements
     * according to a classification function, and then performing a reduction
     * operation on the values associated with a given key using the specified
     * downstream {@code Collector}.  The {@code ConcurrentMap} produced by the
     * Collector is created with the supplied factory function.
     *
     * <p>This is a {@link Collector.Characteristics#CONCURRENT concurrent} and
     * {@link Collector.Characteristics#UNORDERED unordered} Collector.
     *
     * <p>The classification function maps elements to some key type {@code K}.
     * The downstream collector operates on elements of type {@code T} and
     * produces a result of type {@code D}. The resulting collector produces a
     * {@code Map<K, D>}.
     *
     * <p>For example, to compute the set of last names of people in each city,
     * where the city names are sorted:
     * <pre>{@code
     *     ConcurrentMap<City, Set<String>> namesByCity
     *         = people.stream().collect(groupingBy(Person::getCity, ConcurrentSkipListMap::new,
     *                                              mapping(Person::getLastName, toSet())));
     * }</pre>
     *
     *
     * @param <T> the type of the input elements
     * @param <K> the type of the keys
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param <M> the type of the resulting {@code ConcurrentMap}
     * @param classifier a classifier function mapping input elements to keys
     * @param downstream a {@code Collector} implementing the downstream reduction
     * @param mapFactory a function which, when called, produces a new empty
     *                   {@code ConcurrentMap} of the desired type
     * @return a concurrent, unordered {@code Collector} implementing the cascaded group-by operation
     *
     * @see #groupingByConcurrent(Function)
     * @see #groupingByConcurrent(Function, Collector)
     * @see #groupingBy(Function, Collector, Supplier)
     */
    public static <T, K, A, D, M extends ConcurrentMap<K, D>> Collector<T, ?, M> groupingByConcurrent(final Function<? super T, ? extends K> classifier,
            Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) {
        //        Supplier<A> downstreamSupplier = downstream.supplier();
        //        BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        //        BinaryOperator<ConcurrentMap<K, A>> merger = Collectors.<K, A, ConcurrentMap<K, A>> mapMerger(downstream.combiner());
        //        @SuppressWarnings("unchecked")
        //        Supplier<ConcurrentMap<K, A>> mangledFactory = (Supplier<ConcurrentMap<K, A>>) mapFactory;
        //        BiConsumer<ConcurrentMap<K, A>, T> accumulator;
        //        if (downstream.characteristics().contains(Collector.Characteristics.CONCURRENT)) {
        //            accumulator = (m, t) -> {
        //                K key = Objects.requireNonNull(classifier.apply(t), "element cannot be mapped to a null key");
        //                A resultContainer = m.computeIfAbsent(key, k -> downstreamSupplier.get());
        //                downstreamAccumulator.accept(resultContainer, t);
        //            };
        //        } else {
        //            accumulator = (m, t) -> {
        //                K key = Objects.requireNonNull(classifier.apply(t), "element cannot be mapped to a null key");
        //                A resultContainer = m.computeIfAbsent(key, k -> downstreamSupplier.get());
        //                synchronized (resultContainer) {
        //                    downstreamAccumulator.accept(resultContainer, t);
        //                }
        //            };
        //        }
        //
        //        if (downstream.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)) {
        //            return new CollectorImpl<>(mangledFactory, accumulator, merger, CH_CONCURRENT_ID);
        //        } else {
        //            @SuppressWarnings("unchecked")
        //            Function<A, A> downstreamFinisher = (Function<A, A>) downstream.finisher();
        //            Function<ConcurrentMap<K, A>, M> finisher = intermediate -> {
        //                intermediate.replaceAll((k, v) -> downstreamFinisher.apply(v));
        //                @SuppressWarnings("unchecked")
        //                M castResult = (M) intermediate;
        //                return castResult;
        //            };
        //            return new CollectorImpl<>(mangledFactory, accumulator, merger, finisher, CH_CONCURRENT_NOID);
        //        }

        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();

        final Function<K, A> mappingFunction = new Function<K, A>() {
            @Override
            public A apply(K k) {
                return downstreamSupplier.get();
            }
        };

        final BiConsumer<ConcurrentMap<K, A>, T> accumulator = new BiConsumer<ConcurrentMap<K, A>, T>() {
            @Override
            public void accept(ConcurrentMap<K, A> m, T t) {
                K key = Objects.requireNonNull(classifier.apply(t), "element cannot be mapped to a null key");
                A container = computeIfAbsent(m, key, mappingFunction);
                downstreamAccumulator.accept(container, t);
            }
        };

        final BinaryOperator<ConcurrentMap<K, A>> combiner = mapMerger(downstream.combiner());
        @SuppressWarnings("unchecked")
        final Supplier<ConcurrentMap<K, A>> mangledFactory = (Supplier<ConcurrentMap<K, A>>) mapFactory;

        @SuppressWarnings("unchecked")
        final Function<A, A> downstreamFinisher = (Function<A, A>) downstream.finisher();

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return downstreamFinisher.apply(v);
            }
        };

        if (downstream.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)) {
            return new CollectorImpl<>(mangledFactory, accumulator, combiner, CH_CONCURRENT_ID);
        } else {
            final Function<ConcurrentMap<K, A>, M> finisher = new Function<ConcurrentMap<K, A>, M>() {
                @Override
                public M apply(ConcurrentMap<K, A> intermediate) {
                    replaceAll(intermediate, function);
                    @SuppressWarnings("unchecked")
                    M castResult = (M) intermediate;
                    return castResult;
                }
            };

            return new CollectorImpl<>(mangledFactory, accumulator, combiner, finisher, CH_CONCURRENT_NOID);
        }
    }

    /**
     * Returns a {@code Collector} which partitions the input elements according
     * to a {@code Predicate}, and organizes them into a
     * {@code Map<Boolean, List<T>>}.
     *
     * There are no guarantees on the type, mutability,
     * serializability, or thread-safety of the {@code Map} returned.
     *
     * @param <T> the type of the input elements
     * @param predicate a predicate used for classifying input elements
     * @return a {@code Collector} implementing the partitioning operation
     *
     * @see #partitioningBy(Predicate, Collector)
     */
    public static <T> Collector<T, ?, Map<Boolean, List<T>>> partitioningBy(Predicate<? super T> predicate) {
        final Collector<? super T, ?, List<T>> downstream = toList();

        return partitioningBy(predicate, downstream);
    }

    /**
     * Returns a {@code Collector} which partitions the input elements according
     * to a {@code Predicate}, reduces the values in each partition according to
     * another {@code Collector}, and organizes them into a
     * {@code Map<Boolean, D>} whose values are the result of the downstream
     * reduction.
     *
     * <p>There are no guarantees on the type, mutability,
     * serializability, or thread-safety of the {@code Map} returned.
     *
     * @param <T> the type of the input elements
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param predicate a predicate used for classifying input elements
     * @param downstream a {@code Collector} implementing the downstream
     *                   reduction
     * @return a {@code Collector} implementing the cascaded partitioning
     *         operation
     *
     * @see #partitioningBy(Predicate)
     */
    public static <T, D, A> Collector<T, ?, Map<Boolean, D>> partitioningBy(final Predicate<? super T> predicate, final Collector<? super T, A, D> downstream) {
        //        BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        //        BiConsumer<Partition<A>, T> accumulator = (result, t) -> downstreamAccumulator.accept(predicate.test(t) ? result.forTrue : result.forFalse, t);
        //        BinaryOperator<A> op = downstream.combiner();
        //        BinaryOperator<Partition<A>> merger = (left, right) -> new Partition<>(op.apply(left.forTrue, right.forTrue), op.apply(left.forFalse, right.forFalse));
        //        Supplier<Partition<A>> supplier = () -> new Partition<>(downstream.supplier().get(), downstream.supplier().get());
        //        if (downstream.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)) {
        //            return new CollectorImpl<>(supplier, accumulator, merger, CH_ID);
        //        } else {
        //            Function<Partition<A>, Map<Boolean, D>> finisher = par -> new Partition<>(downstream.finisher().apply(par.forTrue),
        //                    downstream.finisher().apply(par.forFalse));
        //            return new CollectorImpl<>(supplier, accumulator, merger, finisher, CH_NOID);
        //        }

        final Supplier<Partition<A>> supplier = new Supplier<Partition<A>>() {
            @Override
            public Partition<A> get() {
                return new Partition<>(downstream.supplier().get(), downstream.supplier().get());
            }
        };

        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final BiConsumer<Partition<A>, T> accumulator = new BiConsumer<Partition<A>, T>() {
            @Override
            public void accept(Partition<A> a, T t) {
                downstreamAccumulator.accept(predicate.test(t) ? a.forTrue : a.forFalse, t);
            }
        };

        final BinaryOperator<A> op = downstream.combiner();
        final BinaryOperator<Partition<A>> combiner = new BinaryOperator<Partition<A>>() {
            @Override
            public Partition<A> apply(Partition<A> a, Partition<A> b) {
                return new Partition<>(op.apply(a.forTrue, b.forTrue), op.apply(a.forFalse, b.forFalse));
            }
        };

        if (downstream.characteristics().contains(Collector.Characteristics.IDENTITY_FINISH)) {
            return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
        } else {
            final Function<Partition<A>, Map<Boolean, D>> finisher = new Function<Partition<A>, Map<Boolean, D>>() {
                @Override
                public Map<Boolean, D> apply(Partition<A> a) {
                    return new Partition<>(downstream.finisher().apply(a.forTrue), downstream.finisher().apply(a.forFalse));
                }
            };

            return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
        }
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code Map} whose keys and values are the result of applying the provided
     * mapping functions to the input elements.
     *
     * <p>If the mapped keys contains duplicates (according to
     * {@link Object#equals(Object)}), an {@code IllegalStateException} is
     * thrown when the collection operation is performed.  If the mapped keys
     * may have duplicates, use {@link #toMap(Function, Function, BinaryOperator)}
     * instead.
     *
     * @apiNote
     * It is common for either the key or the value to be the input elements.
     * In this case, the utility method
     * {@link java.util.function.Function#identity()} may be helpful.
     * For example, the following produces a {@code Map} mapping
     * students to their grade point average:
     * <pre>{@code
     *     Map<Student, Double> studentToGPA
     *         students.stream().collect(toMap(Functions.identity(),
     *                                         student -> computeGPA(student)));
     * }</pre>
     * And the following produces a {@code Map} mapping a unique identifier to
     * students:
     * <pre>{@code
     *     Map<String, Student> studentIdToStudent
     *         students.stream().collect(toMap(Student::getId,
     *                                         Functions.identity());
     * }</pre>
     *
     * @implNote
     * The returned {@code Collector} is not concurrent.  For parallel stream
     * pipelines, the {@code combiner} function operates by merging the keys
     * from one map into another, which can be an expensive operation.  If it is
     * not required that results are inserted into the {@code Map} in encounter
     * order, using {@link #toConcurrentMap(Function, Function)}
     * may offer better parallel performance.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @return a {@code Collector} which collects elements into a {@code Map}
     * whose keys and values are the result of applying mapping functions to
     * the input elements
     *
     * @see #toMap(Function, Function, BinaryOperator)
     * @see #toMap(Function, Function, BinaryOperator, Supplier)
     * @see #toConcurrentMap(Function, Function)
     */
    public static <T, K, U> Collector<T, ?, Map<K, U>> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toMap(keyMapper, valueMapper, mergeFunction);
    }

    public static <T, K, U, M extends Map<K, U>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, final Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code Map} whose keys and values are the result of applying the provided
     * mapping functions to the input elements.
     *
     * <p>If the mapped
     * keys contains duplicates (according to {@link Object#equals(Object)}),
     * the value mapping function is applied to each equal element, and the
     * results are merged using the provided merging function.
     *
     * @apiNote
     * There are multiple ways to deal with collisions between multiple elements
     * mapping to the same key.  The other forms of {@code toMap} simply use
     * a merge function that throws unconditionally, but you can easily write
     * more flexible merge policies.  For example, if you have a stream
     * of {@code Person}, and you want to produce a "phone book" mapping name to
     * address, but it is possible that two persons have the same name, you can
     * do as follows to gracefully deals with these collisions, and produce a
     * {@code Map} mapping names to a concatenated list of addresses:
     * <pre>{@code
     *     Map<String, String> phoneBook
     *         people.stream().collect(toMap(Person::getName,
     *                                       Person::getAddress,
     *                                       (s, a) -> s + ", " + a));
     * }</pre>
     *
     * @implNote
     * The returned {@code Collector} is not concurrent.  For parallel stream
     * pipelines, the {@code combiner} function operates by merging the keys
     * from one map into another, which can be an expensive operation.  If it is
     * not required that results are merged into the {@code Map} in encounter
     * order, using {@link #toConcurrentMap(Function, Function, BinaryOperator)}
     * may offer better parallel performance.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, BiFunction)}
     * @return a {@code Collector} which collects elements into a {@code Map}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key and combining them
     * using the merge function
     *
     * @see #toMap(Function, Function)
     * @see #toMap(Function, Function, BinaryOperator, Supplier)
     * @see #toConcurrentMap(Function, Function, BinaryOperator)
     */
    public static <T, K, U> Collector<T, ?, Map<K, U>> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapSupplier = new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        };

        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    /**
     * Returns a {@code Collector} that accumulates elements into a
     * {@code Map} whose keys and values are the result of applying the provided
     * mapping functions to the input elements.
     *
     * <p>If the mapped
     * keys contains duplicates (according to {@link Object#equals(Object)}),
     * the value mapping function is applied to each equal element, and the
     * results are merged using the provided merging function.  The {@code Map}
     * is created by a provided supplier function.
     *
     * @implNote
     * The returned {@code Collector} is not concurrent.  For parallel stream
     * pipelines, the {@code combiner} function operates by merging the keys
     * from one map into another, which can be an expensive operation.  If it is
     * not required that results are merged into the {@code Map} in encounter
     * order, using {@link #toConcurrentMap(Function, Function, BinaryOperator, Supplier)}
     * may offer better parallel performance.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param <M> the type of the resulting {@code Map}
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, BiFunction)}
     * @param mapSupplier a function which returns a new, empty {@code Map} into
     *                    which the results will be inserted
     * @return a {@code Collector} which collects elements into a {@code Map}
     * whose keys are the result of applying a key mapping function to the input
     * elements, and whose values are the result of applying a value mapping
     * function to all input elements equal to the key and combining them
     * using the merge function
     *
     * @see #toMap(Function, Function)
     * @see #toMap(Function, Function, BinaryOperator)
     * @see #toConcurrentMap(Function, Function, BinaryOperator, Supplier)
     */
    public static <T, K, U, M extends Map<K, U>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        final BiConsumer<M, T> accumulator = new BiConsumer<M, T>() {
            @Override
            public void accept(M map, T element) {
                merge(map, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
            }
        };

        final BinaryOperator<M> combiner = (BinaryOperator<M>) mapMerger(mergeFunction);

        return new CollectorImpl<>(mapSupplier, accumulator, combiner, CH_ID);
    }

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see #toMap(Function, Function)
     */
    public static <T, K, U> Collector<T, ?, LinkedHashMap<K, U>> toLinkedHashMap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toLinkedHashMap(keyMapper, valueMapper, mergeFunction);
    }

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see #toMap(Function, Function, BinaryOperator)
     */
    public static <T, K, U> Collector<T, ?, LinkedHashMap<K, U>> toLinkedHashMap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<LinkedHashMap<K, U>> mapSupplier = new Supplier<LinkedHashMap<K, U>>() {
            @Override
            public LinkedHashMap<K, U> get() {
                return new LinkedHashMap<>();
            }
        };

        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    /**
     * Returns a concurrent {@code Collector} that accumulates elements into a
     * {@code ConcurrentMap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * <p>If the mapped keys contains duplicates (according to
     * {@link Object#equals(Object)}), an {@code IllegalStateException} is
     * thrown when the collection operation is performed.  If the mapped keys
     * may have duplicates, use
     * {@link #toConcurrentMap(Function, Function, BinaryOperator)} instead.
     *
     * @apiNote
     * It is common for either the key or the value to be the input elements.
     * In this case, the utility method
     * {@link java.util.function.Function#identity()} may be helpful.
     * For example, the following produces a {@code Map} mapping
     * students to their grade point average:
     * <pre>{@code
     *     Map<Student, Double> studentToGPA
     *         students.stream().collect(toMap(Functions.identity(),
     *                                         student -> computeGPA(student)));
     * }</pre>
     * And the following produces a {@code Map} mapping a unique identifier to
     * students:
     * <pre>{@code
     *     Map<String, Student> studentIdToStudent
     *         students.stream().collect(toConcurrentMap(Student::getId,
     *                                                   Functions.identity());
     * }</pre>
     *
     * <p>This is a {@link Collector.Characteristics#CONCURRENT concurrent} and
     * {@link Collector.Characteristics#UNORDERED unordered} Collector.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper the mapping function to produce keys
     * @param valueMapper the mapping function to produce values
     * @return a concurrent, unordered {@code Collector} which collects elements into a
     * {@code ConcurrentMap} whose keys are the result of applying a key mapping
     * function to the input elements, and whose values are the result of
     * applying a value mapping function to the input elements
     *
     * @see #toMap(Function, Function)
     * @see #toConcurrentMap(Function, Function, BinaryOperator)
     * @see #toConcurrentMap(Function, Function, BinaryOperator, Supplier)
     */
    public static <T, K, U> Collector<T, ?, ConcurrentMap<K, U>> toConcurrentMap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toConcurrentMap(keyMapper, valueMapper, mergeFunction);
    }

    public static <T, K, U, M extends ConcurrentMap<K, U>> Collector<T, ?, M> toConcurrentMap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toConcurrentMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    /**
     * Returns a concurrent {@code Collector} that accumulates elements into a
     * {@code ConcurrentMap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * <p>If the mapped keys contains duplicates (according to {@link Object#equals(Object)}),
     * the value mapping function is applied to each equal element, and the
     * results are merged using the provided merging function.
     *
     * @apiNote
     * There are multiple ways to deal with collisions between multiple elements
     * mapping to the same key.  The other forms of {@code toConcurrentMap} simply use
     * a merge function that throws unconditionally, but you can easily write
     * more flexible merge policies.  For example, if you have a stream
     * of {@code Person}, and you want to produce a "phone book" mapping name to
     * address, but it is possible that two persons have the same name, you can
     * do as follows to gracefully deals with these collisions, and produce a
     * {@code Map} mapping names to a concatenated list of addresses:
     * <pre>{@code
     *     Map<String, String> phoneBook
     *         people.stream().collect(toConcurrentMap(Person::getName,
     *                                                 Person::getAddress,
     *                                                 (s, a) -> s + ", " + a));
     * }</pre>
     *
     * <p>This is a {@link Collector.Characteristics#CONCURRENT concurrent} and
     * {@link Collector.Characteristics#UNORDERED unordered} Collector.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, BiFunction)}
     * @return a concurrent, unordered {@code Collector} which collects elements into a
     * {@code ConcurrentMap} whose keys are the result of applying a key mapping
     * function to the input elements, and whose values are the result of
     * applying a value mapping function to all input elements equal to the key
     * and combining them using the merge function
     *
     * @see #toConcurrentMap(Function, Function)
     * @see #toConcurrentMap(Function, Function, BinaryOperator, Supplier)
     * @see #toMap(Function, Function, BinaryOperator)
     */
    public static <T, K, U> Collector<T, ?, ConcurrentMap<K, U>> toConcurrentMap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<ConcurrentMap<K, U>> mapSupplier = new Supplier<ConcurrentMap<K, U>>() {
            @Override
            public ConcurrentMap<K, U> get() {
                return new ConcurrentHashMap<>();
            }
        };

        return toConcurrentMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    /**
     * Returns a concurrent {@code Collector} that accumulates elements into a
     * {@code ConcurrentMap} whose keys and values are the result of applying
     * the provided mapping functions to the input elements.
     *
     * <p>If the mapped keys contains duplicates (according to {@link Object#equals(Object)}),
     * the value mapping function is applied to each equal element, and the
     * results are merged using the provided merging function.  The
     * {@code ConcurrentMap} is created by a provided supplier function.
     *
     * <p>This is a {@link Collector.Characteristics#CONCURRENT concurrent} and
     * {@link Collector.Characteristics#UNORDERED unordered} Collector.
     *
     * @param <T> the type of the input elements
     * @param <K> the output type of the key mapping function
     * @param <U> the output type of the value mapping function
     * @param <M> the type of the resulting {@code ConcurrentMap}
     * @param keyMapper a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, BiFunction)}
     * @param mapSupplier a function which returns a new, empty {@code Map} into
     *                    which the results will be inserted
     * @return a concurrent, unordered {@code Collector} which collects elements into a
     * {@code ConcurrentMap} whose keys are the result of applying a key mapping
     * function to the input elements, and whose values are the result of
     * applying a value mapping function to all input elements equal to the key
     * and combining them using the merge function
     *
     * @see #toConcurrentMap(Function, Function)
     * @see #toConcurrentMap(Function, Function, BinaryOperator)
     * @see #toMap(Function, Function, BinaryOperator, Supplier)
     */
    public static <T, K, U, M extends ConcurrentMap<K, U>> Collector<T, ?, M> toConcurrentMap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier) {

        final BiConsumer<M, T> accumulator = new BiConsumer<M, T>() {
            @Override
            public void accept(M map, T element) {
                merge(map, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
            }
        };

        final BinaryOperator<M> combiner = (BinaryOperator<M>) mapMerger2(mergeFunction);

        return new CollectorImpl<T, M, M>(mapSupplier, accumulator, combiner, CH_CONCURRENT_ID);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toBiMap(keyMapper, valueMapper, mergeFunction);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, final Supplier<BiMap<K, U>> mapSupplier) {
        final BinaryOperator<U> mergeFunction = throwingMerger();

        return toBiMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Supplier<BiMap<K, U>> mapSupplier = new Supplier<BiMap<K, U>>() {
            @Override
            public BiMap<K, U> get() {
                return new BiMap<>();
            }
        };

        return toBiMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, final Supplier<BiMap<K, U>> mapSupplier) {
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    public static <T, K, U> Collector<T, ?, Multimap<K, U, List<U>>> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper) {
        final Supplier<Multimap<K, U, List<U>>> mapSupplier = new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        };

        return toMultimap(keyMapper, valueMapper, mapSupplier);
    }

    public static <T, K, U, V extends Collection<U>> Collector<T, ?, Multimap<K, U, V>> toMultimap(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapSupplier) {
        final BiConsumer<Multimap<K, U, V>, T> accumulator = new BiConsumer<Multimap<K, U, V>, T>() {
            @Override
            public void accept(Multimap<K, U, V> map, T element) {
                merge(map, keyMapper.apply(element), valueMapper.apply(element));
            }
        };

        final BinaryOperator<Multimap<K, U, V>> combiner = mapMerger3();

        return new CollectorImpl<>(mapSupplier, accumulator, combiner, CH_ID);
    }

    public static <T> Collector<T, ?, DataSet> toDataSet(final List<String> columnNames) {
        @SuppressWarnings("rawtypes")
        final Collector<T, List<T>, List<T>> collector = (Collector) toList();

        final Function<List<T>, DataSet> finisher = new Function<List<T>, DataSet>() {
            @Override
            public DataSet apply(List<T> t) {
                return N.newDataSet(columnNames, t);
            }
        };

        return new CollectorImpl<T, List<T>, DataSet>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    //    public static <T> Collector<T, ?, DataSet> toDataSet(final String entityName, final Class<?> entityClass, final List<String> columnNames) {
    //        @SuppressWarnings("rawtypes")
    //        final Collector<T, List<T>, List<T>> collector = (Collector) toList();
    //
    //        final Function<List<T>, DataSet> finisher = new Function<List<T>, DataSet>() {
    //            @Override
    //            public DataSet apply(List<T> t) {
    //                return N.newDataSet(entityName, entityClass, columnNames, t);
    //            }
    //        };
    //
    //        return new CollectorImpl<T, List<T>, DataSet>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher);
    //    }

    /**
     * Returns a {@code Collector} which applies an {@code int}-producing
     * mapping function to each input element, and returns summary statistics
     * for the resulting values.
     *
     * @param <T> the type of the input elements
     * @param mapper a mapping function to apply to each element
     * @return a {@code Collector} implementing the summary-statistics reduction
     *
     * @see #summarizingDouble(ToDoubleFunction)
     * @see #summarizingLong(ToLongFunction)
     */
    public static <T> Collector<T, ?, IntSummaryStatistics> summarizingInt(final ToIntFunction<? super T> mapper) {
        final Supplier<IntSummaryStatistics> supplier = new Supplier<IntSummaryStatistics>() {
            @Override
            public IntSummaryStatistics get() {
                return new IntSummaryStatistics();
            }
        };

        final BiConsumer<IntSummaryStatistics, T> accumulator = new BiConsumer<IntSummaryStatistics, T>() {
            @Override
            public void accept(IntSummaryStatistics a, T t) {
                a.accept(mapper.applyAsInt(t));
            }
        };

        final BinaryOperator<IntSummaryStatistics> combiner = new BinaryOperator<IntSummaryStatistics>() {
            @Override
            public IntSummaryStatistics apply(IntSummaryStatistics a, IntSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, IntSummaryStatistics, IntSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
    }

    /**
     * Returns a {@code Collector} which applies an {@code long}-producing
     * mapping function to each input element, and returns summary statistics
     * for the resulting values.
     *
     * @param <T> the type of the input elements
     * @param mapper the mapping function to apply to each element
     * @return a {@code Collector} implementing the summary-statistics reduction
     *
     * @see #summarizingDouble(ToDoubleFunction)
     * @see #summarizingInt(ToIntFunction)
     */
    public static <T> Collector<T, ?, LongSummaryStatistics> summarizingLong(final ToLongFunction<? super T> mapper) {
        final Supplier<LongSummaryStatistics> supplier = new Supplier<LongSummaryStatistics>() {
            @Override
            public LongSummaryStatistics get() {
                return new LongSummaryStatistics();
            }
        };

        final BiConsumer<LongSummaryStatistics, T> accumulator = new BiConsumer<LongSummaryStatistics, T>() {
            @Override
            public void accept(LongSummaryStatistics a, T t) {
                a.accept(mapper.applyAsLong(t));
            }
        };

        final BinaryOperator<LongSummaryStatistics> combiner = new BinaryOperator<LongSummaryStatistics>() {
            @Override
            public LongSummaryStatistics apply(LongSummaryStatistics a, LongSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, LongSummaryStatistics, LongSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
    }

    /**
     * Returns a {@code Collector} which applies an {@code double}-producing
     * mapping function to each input element, and returns summary statistics
     * for the resulting values.
     *
     * @param <T> the type of the input elements
     * @param mapper a mapping function to apply to each element
     * @return a {@code Collector} implementing the summary-statistics reduction
     *
     * @see #summarizingLong(ToLongFunction)
     * @see #summarizingInt(ToIntFunction)
     */
    public static <T> Collector<T, ?, DoubleSummaryStatistics> summarizingDouble(final ToDoubleFunction<? super T> mapper) {
        final Supplier<DoubleSummaryStatistics> supplier = new Supplier<DoubleSummaryStatistics>() {
            @Override
            public DoubleSummaryStatistics get() {
                return new DoubleSummaryStatistics();
            }
        };

        final BiConsumer<DoubleSummaryStatistics, T> accumulator = new BiConsumer<DoubleSummaryStatistics, T>() {
            @Override
            public void accept(DoubleSummaryStatistics a, T t) {
                a.accept(mapper.applyAsDouble(t));
            }
        };

        final BinaryOperator<DoubleSummaryStatistics> combiner = new BinaryOperator<DoubleSummaryStatistics>() {
            @Override
            public DoubleSummaryStatistics apply(DoubleSummaryStatistics a, DoubleSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, DoubleSummaryStatistics, DoubleSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
    }

    private static <K, V> void replaceAll(Map<K, V> map, BiFunction<? super K, ? super V, ? extends V> function) {
        Objects.requireNonNull(function);
        for (Map.Entry<K, V> entry : map.entrySet()) {
            K k;
            V v;
            try {
                k = entry.getKey();
                v = entry.getValue();
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }

            // ise thrown from function is not a cme.
            v = function.apply(k, v);

            try {
                entry.setValue(v);
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }
        }
    }

    private static <K, V> V computeIfAbsent(Map<K, V> map, K key, Function<? super K, ? extends V> mappingFunction) {
        Objects.requireNonNull(mappingFunction);
        V v = null;

        if ((v = map.get(key)) == null) {
            V newValue = null;
            if ((newValue = mappingFunction.apply(key)) != null) {
                map.put(key, newValue);
                return newValue;
            }
        }

        return v;
    }

    private static <K, V> V merge(Map<K, V> map, K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
        Objects.requireNonNull(remappingFunction);
        Objects.requireNonNull(value);

        V oldValue = map.get(key);
        V newValue = (oldValue == null) ? value : remappingFunction.apply(oldValue, value);
        if (newValue == null) {
            map.remove(key);
        } else {
            map.put(key, newValue);
        }

        return newValue;
    }

    private static <K, U, V extends Collection<U>> void merge(Multimap<K, U, V> map, K key, U value) {
        Objects.requireNonNull(value);

        V oldValue = map.get(key);

        if (oldValue == null) {
            map.put(key, value);
        } else {
            oldValue.add(value);
        }
    }

    /**
     * Implementation class used by partitioningBy.
     */
    private static final class Partition<T> extends AbstractMap<Boolean, T> implements Map<Boolean, T> {
        final T forTrue;
        final T forFalse;

        Partition(T forTrue, T forFalse) {
            this.forTrue = forTrue;
            this.forFalse = forFalse;
        }

        @Override
        public Set<Map.Entry<Boolean, T>> entrySet() {
            return new AbstractSet<Map.Entry<Boolean, T>>() {
                @Override
                public Iterator<Map.Entry<Boolean, T>> iterator() {
                    Map.Entry<Boolean, T> falseEntry = new SimpleImmutableEntry<>(false, forFalse);
                    Map.Entry<Boolean, T> trueEntry = new SimpleImmutableEntry<>(true, forTrue);
                    return Arrays.asList(falseEntry, trueEntry).iterator();
                }

                @Override
                public int size() {
                    return 2;
                }
            };
        }
    }
}
