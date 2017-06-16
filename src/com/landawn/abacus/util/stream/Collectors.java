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
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.BiMap;
import com.landawn.abacus.util.BooleanList;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.ImmutableList;
import com.landawn.abacus.util.ImmutableMap;
import com.landawn.abacus.util.ImmutableSet;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.Joiner;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleSupplier;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;

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
    static final Object NONE = new Object();

    static final Set<Collector.Characteristics> CH_CONCURRENT_ID = Collections
            .unmodifiableSet(EnumSet.of(Collector.Characteristics.CONCURRENT, Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
    static final Set<Collector.Characteristics> CH_CONCURRENT_NOID = Collections
            .unmodifiableSet(EnumSet.of(Collector.Characteristics.CONCURRENT, Collector.Characteristics.UNORDERED));
    static final Set<Collector.Characteristics> CH_ID = Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.IDENTITY_FINISH));
    static final Set<Collector.Characteristics> CH_UNORDERED_ID = Collections
            .unmodifiableSet(EnumSet.of(Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
    static final Set<Collector.Characteristics> CH_UNORDERED = Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.UNORDERED));
    static final Set<Collector.Characteristics> CH_NOID = Collections.emptySet();

    @SuppressWarnings("rawtypes")
    private static final BinaryOperator<Collection> ADD_ALL_MERGER = new BinaryOperator<Collection>() {
        @Override
        public Collection apply(Collection t, Collection u) {
            t.addAll(u);
            return t;
        }
    };

    private Collectors() {
    }

    /**
     * Simple implementation class for {@code Collector}.
     *
     * @param <T> the type of elements to be collected
     * @param <R> the type of the result
     */
    static class CollectorImpl<T, A, R> implements Collector<T, A, R> {
        private static final Function<Object, Object> IDENTITY_FINISHER = new Function<Object, Object>() {
            @Override
            public Object apply(Object t) {
                return t;
            }
        };

        private final Supplier<A> supplier;
        private final BiConsumer<A, T> accumulator;
        private final BinaryOperator<A> combiner;
        private final Function<A, R> finisher;
        private final Set<Characteristics> characteristics;

        CollectorImpl(Supplier<A> supplier, BiConsumer<A, T> accumulator, BinaryOperator<A> combiner, Set<Characteristics> characteristics) {
            this(supplier, accumulator, combiner, (Function<A, R>) IDENTITY_FINISHER, characteristics);
        }

        CollectorImpl(Supplier<A> supplier, BiConsumer<A, T> accumulator, BinaryOperator<A> combiner, Function<A, R> finisher,
                Set<Characteristics> characteristics) {
            this.supplier = supplier;
            this.accumulator = accumulator;
            this.combiner = combiner;
            this.finisher = finisher;
            this.characteristics = characteristics;
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
        public Set<com.landawn.abacus.util.stream.Collector.Characteristics> characteristics() {
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

    public static <T> Collector<T, ?, ImmutableList<T>> toImmutableList() {
        final Collector<T, ?, List<T>> downstream = toList();

        final Function<List<T>, ImmutableList<T>> finisher = new Function<List<T>, ImmutableList<T>>() {
            @Override
            public ImmutableList<T> apply(List<T> t) {
                return ImmutableList.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
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

    public static <T> Collector<T, ?, ImmutableSet<T>> toImmutableSet() {
        final Collector<T, ?, Set<T>> downstream = toSet();

        final Function<Set<T>, ImmutableSet<T>> finisher = new Function<Set<T>, ImmutableSet<T>>() {
            @Override
            public ImmutableSet<T> apply(Set<T> t) {
                return ImmutableSet.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
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

    public static <T> Collector<T, ?, Object[]> toArray() {
        return toArray(Fn.Suppliers.ofEmptyObjectArray());
    }

    public static <T, A> Collector<T, ?, A[]> toArray(final Supplier<A[]> arraySupplier) {
        final Supplier<List<A>> supplier = new Supplier<List<A>>() {
            @Override
            public List<A> get() {
                return new ArrayList<A>();
            }
        };

        final BiConsumer<List<A>, T> accumulator = new BiConsumer<List<A>, T>() {
            @Override
            public void accept(List<A> c, T t) {
                c.add((A) t);
            }
        };

        final BinaryOperator<List<A>> combiner = new BinaryOperator<List<A>>() {
            @Override
            public List<A> apply(List<A> a, List<A> b) {
                a.addAll(b);
                return a;
            }
        };

        final Function<List<A>, A[]> finisher = new Function<List<A>, A[]>() {
            @Override
            public A[] apply(List<A> t) {
                final A[] a = arraySupplier.get();

                if (a.length >= t.size()) {
                    return t.toArray(a);
                } else {
                    return t.toArray((A[]) Array.newInstance(a.getClass().getComponentType(), t.size()));
                }
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
     * 
     * @param n
     * @return
     * @throws UnsupportedOperationException it's used in parallel stream.
     */
    public static <T> Collector<T, ?, List<T>> last(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative");

        final Supplier<Deque<T>> supplier = new Supplier<Deque<T>>() {
            private volatile boolean isCalled = false;

            @Override
            public Deque<T> get() {
                if (isCalled) {
                    throw new UnsupportedOperationException("The 'last' Collector only can be used in sequential stream");
                }

                isCalled = true;

                return n <= 1024 ? new ArrayDeque<T>(n) : new LinkedList<T>();
            }
        };

        final BiConsumer<Deque<T>, T> accumulator = new BiConsumer<Deque<T>, T>() {
            @Override
            public void accept(Deque<T> dqueue, T t) {
                if (n > 0) {
                    if (dqueue.size() >= n) {
                        dqueue.pollFirst();
                    }

                    dqueue.offerLast(t);
                }
            }
        };

        final BinaryOperator<Deque<T>> combiner = new BinaryOperator<Deque<T>>() {
            @Override
            public Deque<T> apply(Deque<T> a, Deque<T> b) {
                if (N.notNullOrEmpty(a) && N.notNullOrEmpty(b)) {
                    throw new UnsupportedOperationException("The 'last' Collector only can be used in sequential stream");
                }

                while (b.size() < n && !a.isEmpty()) {
                    b.addFirst(a.pollLast());
                }

                return b;
            }
        };

        final Function<Deque<T>, List<T>> finisher = new Function<Deque<T>, List<T>>() {
            @Override
            public List<T> apply(Deque<T> dqueue) {
                return new ArrayList<>(dqueue);
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
        final Supplier<Joiner> supplier = new Supplier<Joiner>() {
            @Override
            public Joiner get() {
                return Joiner.with(delimiter, prefix, suffix);
            }
        };

        final BiConsumer<Joiner, CharSequence> accumulator = new BiConsumer<Joiner, CharSequence>() {
            @Override
            public void accept(Joiner a, CharSequence t) {
                a.add(t);
            }
        };

        final BinaryOperator<Joiner> combiner = new BinaryOperator<Joiner>() {
            @Override
            public Joiner apply(Joiner a, Joiner b) {
                a.merge(b);
                return a;
            }
        };

        final Function<Joiner, String> finisher = new Function<Joiner, String>() {
            @Override
            public String apply(Joiner a) {
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
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which filters input elements by the supplied
     * predicate, collecting them to the list.
     *
     * <p>
     * This method behaves like
     * {@code filtering(predicate, Collectors.toList())}.
     * 
     * <p>
     * There are no guarantees on the type, mutability, serializability, or
     * thread-safety of the {@code List} returned.
     * 
     * @param <T> the type of the input elements
     * @param predicate a filter function to be applied to the input elements
     * @return a collector which applies the predicate to the input elements and
     *         collects the elements for which predicate returned true to the
     *         {@code List}
     * @see #filtering(Predicate, Collector)
     * @since 0.6.0
     */
    public static <T> Collector<T, ?, List<T>> filtering(Predicate<? super T> predicate) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return filtering(predicate, downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which passes only those elements to the
     * specified downstream collector which match given predicate.
     *
     * <p>
     * This method returns a
     * <a href="package-summary.html#ShortCircuitReduction">short-circuiting
     * collector</a> if downstream collector is short-circuiting.
     * 
     * <p>
     * The operation performed by the returned collector is equivalent to
     * {@code stream.filter(predicate).collect(downstream)}. This collector is
     * mostly useful as a downstream collector in cascaded operation involving
     * {@link #pairing(Collector, Collector, BiFunction)} collector.
     *
     * <p>
     * This method is similar to {@code Collectors.filtering} method which
     * appears in JDK 9. However when downstream collector is
     * <a href="package-summary.html#ShortCircuitReduction">short-circuiting</a>
     * , this method will also return a short-circuiting collector.
     * 
     * @param <T> the type of the input elements
     * @param <A> intermediate accumulation type of the downstream collector
     * @param <R> result type of collector
     * @param predicate a filter function to be applied to the input elements
     * @param downstream a collector which will accept filtered values
     * @return a collector which applies the predicate to the input elements and
     *         provides the elements for which predicate returned true to the
     *         downstream collector
     * @see #pairing(Collector, Collector, BiFunction)
     * @since 0.4.0
     */
    public static <T, A, R> Collector<T, ?, R> filtering(final Predicate<? super T> predicate, final Collector<? super T, A, R> downstream) {
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();

        final BiConsumer<A, T> accumulator = new BiConsumer<A, T>() {
            @Override
            public void accept(A a, T t) {
                if (predicate.test(t)) {
                    downstreamAccumulator.accept(a, t);
                }
            }
        };

        return new CollectorImpl<>(downstream.supplier(), accumulator, downstream.combiner(), downstream.finisher(), downstream.characteristics());
    }

    public static <T, U> Collector<T, ?, List<U>> mapping(Function<? super T, ? extends U> mapper) {
        final Collector<? super U, ?, List<U>> downstream = Collectors.toList();

        return Collectors.mapping(mapper, downstream);
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

    public static <T, U> Collector<T, ?, List<U>> flatMapping(Function<? super T, ? extends Stream<? extends U>> mapper) {
        final Collector<? super U, ?, List<U>> downstream = Collectors.toList();

        return flatMapping(mapper, downstream);
    }

    public static <T, U, A, R> Collector<T, ?, R> flatMapping(final Function<? super T, ? extends Stream<? extends U>> mapper,
            Collector<? super U, A, R> downstream) {
        final BiConsumer<A, ? super U> downstreamAccumulator = downstream.accumulator();

        final BiConsumer<A, T> accumulator = new BiConsumer<A, T>() {
            @Override
            public void accept(final A a, final T t) {
                try (Stream<? extends U> stream = mapper.apply(t)) {
                    stream.forEach(new Consumer<U>() {
                        @Override
                        public void accept(U u) {
                            downstreamAccumulator.accept(a, u);
                        }
                    });
                }
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
     * Returns a {@code Collector} which collects into the {@link List} the
     * input elements for which given mapper function returns distinct results.
     *
     * <p>
     * For ordered source the order of collected elements is preserved. If the
     * same result is returned by mapper function for several elements, only the
     * first element is included into the resulting list.
     * 
     * <p>
     * There are no guarantees on the type, mutability, serializability, or
     * thread-safety of the {@code List} returned.
     * 
     * <p>
     * The operation performed by the returned collector is equivalent to
     * {@code stream.distinct(mapper).toList()}, but may work faster.
     * 
     * @param <T> the type of the input elements
     * @param mapper a function which classifies input elements.
     * @return a collector which collects distinct elements to the {@code List}.
     * @since 0.3.8
     */
    public static <T> Collector<T, ?, List<T>> distinctBy(final Function<? super T, ?> mapper) {
        final Supplier<Map<Object, T>> supplier = new Supplier<Map<Object, T>>() {
            @Override
            public Map<Object, T> get() {
                return new LinkedHashMap<>();
            }
        };

        final BiConsumer<Map<Object, T>, T> accumulator = new BiConsumer<Map<Object, T>, T>() {
            @Override
            public void accept(Map<Object, T> a, T t) {
                final Object key = mapper.apply(t);

                if (a.containsKey(key) == false) {
                    a.put(key, t);
                }
            }
        };

        final BinaryOperator<Map<Object, T>> combiner = new BinaryOperator<Map<Object, T>>() {
            @Override
            public Map<Object, T> apply(Map<Object, T> a, Map<Object, T> b) {

                for (Map.Entry<Object, T> entry : b.entrySet()) {
                    if (a.containsKey(entry.getKey()) == false) {
                        a.put(entry.getKey(), entry.getValue());
                    }
                }

                return a;
            }
        };

        final Function<Map<Object, T>, List<T>> finisher = new Function<Map<Object, T>, List<T>>() {
            @Override
            public List<T> apply(Map<Object, T> a) {
                return new ArrayList<>(a.values());
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which counts a number of distinct values the
     * mapper function returns for the stream elements.
     * 
     * <p>
     * The operation performed by the returned collector is equivalent to
     * {@code stream.map(mapper).distinct().count()}. This collector is mostly
     * useful as a downstream collector.
     * 
     * @param <T> the type of the input elements
     * @param mapper a function which classifies input elements.
     * @return a collector which counts a number of distinct classes the mapper
     *         function returns for the stream elements.
     */
    public static <T> Collector<T, ?, Integer> distinctCount(Function<? super T, ?> mapper) {
        final Collector<Object, ?, Set<Object>> downstream = Collectors.toSet();

        final Function<Set<Object>, Integer> finisher = new Function<Set<Object>, Integer>() {
            @Override
            public Integer apply(Set<Object> t) {
                return t.size();
            }
        };

        return Collectors.collectingAndThen(Collectors.mapping(mapper, downstream), finisher);
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

    public static <T> Collector<T, ?, Integer> countingInt() {
        final Function<? super T, ? extends Integer> mapper = new Function<T, Integer>() {
            @Override
            public Integer apply(T t) {
                return 1;
            }
        };

        final BinaryOperator<Integer> op = new BinaryOperator<Integer>() {
            @Override
            public Integer apply(Integer a, Integer b) {
                return a + b;
            }
        };

        return reducing(0, mapper, op);
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Collector<T, ?, NullabLe<T>> min() {
        return minBy(Fn.naturalOrder());
    }

    /**
     * Returns a {@code Collector} that produces the minimal element according
     * to a given {@code Comparator}, described as an {@code NullabLe<T>}.
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
    public static <T> Collector<T, ?, NullabLe<T>> minBy(final Comparator<? super T> comparator) {
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

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Collector<T, ?, NullabLe<T>> max() {
        return maxBy(Fn.naturalOrder());
    }

    /**
     * Returns a {@code Collector} that produces the maximal element according
     * to a given {@code Comparator}, described as an {@code NullabLe<T>}.
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
    public static <T> Collector<T, ?, NullabLe<T>> maxBy(final Comparator<? super T> comparator) {
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
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * Returns a {@code Collector} which aggregates the results of two supplied
     * collectors using the supplied finisher function.
     * 
     * <p>
     * This method returns a
     * <a href="package-summary.html#ShortCircuitReduction">short-circuiting
     * collector</a> if both downstream collectors are short-circuiting. The
     * collection might stop when both downstream collectors report that the
     * collection is complete.
     *
     * @param <T> the type of the input elements
     * @param <A1> the intermediate accumulation type of the first collector
     * @param <A2> the intermediate accumulation type of the second collector
     * @param <R1> the result type of the first collector
     * @param <R2> the result type of the second collector
     * @param <R> the final result type
     * @param c1 the first collector
     * @param c2 the second collector
     * @param finisher the function which merges two results into the single
     *        one.
     * @return a {@code Collector} which aggregates the results of two supplied
     *         collectors.
     */
    public static <T, A1, A2, R1, R2, R> Collector<T, ?, R> pairing(final Collector<? super T, A1, R1> c1, final Collector<? super T, A2, R2> c2,
            final BiFunction<? super R1, ? super R2, ? extends R> finisher) {
        final Supplier<A1> c1Supplier = c1.supplier();
        final Supplier<A2> c2Supplier = c2.supplier();
        final BiConsumer<A1, ? super T> c1Accumulator = c1.accumulator();
        final BiConsumer<A2, ? super T> c2Accumulator = c2.accumulator();
        final BinaryOperator<A1> c1Combiner = c1.combiner();
        final BinaryOperator<A2> c2combiner = c2.combiner();

        final Supplier<Pair<A1, A2>> supplier = new Supplier<Pair<A1, A2>>() {
            @Override
            public Pair<A1, A2> get() {
                return Pair.of(c1Supplier.get(), c2Supplier.get());
            }
        };

        final BiConsumer<Pair<A1, A2>, T> accumulator = new BiConsumer<Pair<A1, A2>, T>() {
            @Override
            public void accept(Pair<A1, A2> t, T u) {
                c1Accumulator.accept(t.left, u);
                c2Accumulator.accept(t.right, u);
            }
        };

        final BinaryOperator<Pair<A1, A2>> combiner = new BinaryOperator<Pair<A1, A2>>() {
            @Override
            public Pair<A1, A2> apply(Pair<A1, A2> t, Pair<A1, A2> u) {
                t.left = c1Combiner.apply(t.left, u.left);
                t.right = c2combiner.apply(t.right, u.right);

                return t;
            }
        };

        final Function<Pair<A1, A2>, R> resFinisher = new Function<Pair<A1, A2>, R>() {
            @Override
            public R apply(Pair<A1, A2> t) {
                final R1 r1 = c1.finisher().apply(t.left);
                final R2 r2 = c2.finisher().apply(t.right);

                return finisher.apply(r1, r2);
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, resFinisher, CH_NOID);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds the minimal and maximal element
     * according to the supplied comparator, then applies finisher function to
     * them producing the final result.
     * 
     * <p>
     * This collector produces stable result for ordered stream: if several
     * minimal or maximal elements appear, the collector always selects the
     * first encountered.
     * 
     * <p>
     * If there are no input elements, the finisher method is not called and
     * empty {@code Optional} is returned. Otherwise the finisher result is
     * wrapped into {@code Optional}.
     *
     * @param <T> the type of the input elements
     * @param <R> the type of the result wrapped into {@code Optional}
     * @param comparator comparator which is used to find minimal and maximal
     *        element
     * @param finisher a {@link BiFunction} which takes minimal and maximal
     *        element and produces the final result.
     * @return a {@code Collector} which finds minimal and maximal elements.
     */
    public static <T, R> Collector<T, ?, R> minMax(final Comparator<? super T> comparator,
            final BiFunction<? super NullabLe<T>, ? super NullabLe<T>, ? extends R> finisher) {
        return pairing(Collectors.minBy(comparator), Collectors.maxBy(comparator), finisher);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and bigger than any other element according to the
     * specified {@link Comparator}. The found elements are reduced using the
     * specified downstream {@code Collector}.
     *
     * @param <T> the type of the input elements
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param comparator a {@code Comparator} to compare the elements
     * @param downstream a {@code Collector} implementing the downstream
     *        reduction
     * @return a {@code Collector} which finds all the maximal elements.
     * @see #maxAll(Comparator)
     * @see #maxAll(Collector)
     * @see #maxAll()
     */
    public static <T, A, D> Collector<T, ?, D> maxAll(final Comparator<? super T> comparator, final Collector<? super T, A, D> downstream) {
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final BinaryOperator<A> downstreamCombiner = downstream.combiner();
        final MutableBoolean isCollection = MutableBoolean.of(false);

        final Supplier<Pair<A, T>> supplier = new Supplier<Pair<A, T>>() {
            @SuppressWarnings("rawtypes")
            @Override
            public Pair<A, T> get() {
                final A c = downstreamSupplier.get();

                if (c instanceof Collection && ((Collection) c).size() == 0) {
                    try {
                        ((Collection) c).clear();

                        isCollection.setTrue();
                    } catch (Throwable e) {
                        // ignore
                    }
                }

                return Pair.of(c, (T) none());
            }
        };

        final BiConsumer<Pair<A, T>, T> accumulator = new BiConsumer<Pair<A, T>, T>() {
            @SuppressWarnings("rawtypes")
            @Override
            public void accept(Pair<A, T> t, T u) {
                if (t.right == NONE) {
                    downstreamAccumulator.accept(t.left, u);
                    t.right = u;
                } else {
                    final int cmp = comparator.compare(u, t.right);

                    if (cmp > 0) {
                        if (isCollection.isTrue()) {

                            ((Collection) t.left).clear();
                        } else {
                            t.left = downstreamSupplier.get();
                        }

                        t.right = u;
                    }

                    if (cmp >= 0) {
                        downstreamAccumulator.accept(t.left, u);
                    }
                }
            }
        };

        final BinaryOperator<Pair<A, T>> combiner = new BinaryOperator<Pair<A, T>>() {
            @Override
            public Pair<A, T> apply(Pair<A, T> t, Pair<A, T> u) {
                if (u.right == NONE) {
                    return t;
                } else if (t.right == NONE) {
                    return u;
                }

                final int cmp = comparator.compare(t.right, u.right);

                if (cmp > 0) {
                    return t;
                } else if (cmp < 0) {
                    return u;
                }

                t.left = downstreamCombiner.apply(t.left, u.left);

                return t;
            }
        };

        final Function<Pair<A, T>, D> finisher = new Function<Pair<A, T>, D>() {
            @Override
            public D apply(Pair<A, T> t) {
                return downstream.finisher().apply(t.left);
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    @SuppressWarnings("unchecked")
    static <T> T none() {
        return (T) NONE;
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and bigger than any other element according to the
     * specified {@link Comparator}. The found elements are collected to
     * {@link List}.
     *
     * @param <T> the type of the input elements
     * @param comparator a {@code Comparator} to compare the elements
     * @return a {@code Collector} which finds all the maximal elements and
     *         collects them to the {@code List}.
     * @see #maxAll(Comparator, Collector)
     * @see #maxAll()
     */
    public static <T> Collector<T, ?, List<T>> maxAll(Comparator<? super T> comparator) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return maxAll(comparator, downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and bigger than any other element according to the natural
     * order. The found elements are reduced using the specified downstream
     * {@code Collector}.
     *
     * @param <T> the type of the input elements
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param downstream a {@code Collector} implementing the downstream
     *        reduction
     * @return a {@code Collector} which finds all the maximal elements.
     * @see #maxAll(Comparator, Collector)
     * @see #maxAll(Comparator)
     * @see #maxAll()
     */
    @SuppressWarnings("rawtypes")
    public static <T extends Comparable, A, D> Collector<T, ?, D> maxAll(Collector<T, A, D> downstream) {
        return maxAll(Fn.naturalOrder(), downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and bigger than any other element according to the natural
     * order. The found elements are collected to {@link List}.
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} which finds all the maximal elements and
     *         collects them to the {@code List}.
     * @see #maxAll(Comparator)
     * @see #maxAll(Collector)
     */
    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Collector<T, ?, List<T>> maxAll() {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return maxAll(Fn.naturalOrder(), downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and smaller than any other element according to the
     * specified {@link Comparator}. The found elements are reduced using the
     * specified downstream {@code Collector}.
     *
     * @param <T> the type of the input elements
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param comparator a {@code Comparator} to compare the elements
     * @param downstream a {@code Collector} implementing the downstream
     *        reduction
     * @return a {@code Collector} which finds all the minimal elements.
     * @see #minAll(Comparator)
     * @see #minAll(Collector)
     * @see #minAll()
     */
    public static <T, A, D> Collector<T, ?, D> minAll(Comparator<? super T> comparator, Collector<T, A, D> downstream) {
        return maxAll(Fn.reverseOrder(comparator), downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and smaller than any other element according to the
     * specified {@link Comparator}. The found elements are collected to
     * {@link List}.
     *
     * @param <T> the type of the input elements
     * @param comparator a {@code Comparator} to compare the elements
     * @return a {@code Collector} which finds all the minimal elements and
     *         collects them to the {@code List}.
     * @see #minAll(Comparator, Collector)
     * @see #minAll()
     */
    public static <T> Collector<T, ?, List<T>> minAll(Comparator<? super T> comparator) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return maxAll(Fn.reverseOrder(comparator), downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and smaller than any other element according to the natural
     * order. The found elements are reduced using the specified downstream
     * {@code Collector}.
     *
     * @param <T> the type of the input elements
     * @param <A> the intermediate accumulation type of the downstream collector
     * @param <D> the result type of the downstream reduction
     * @param downstream a {@code Collector} implementing the downstream
     *        reduction
     * @return a {@code Collector} which finds all the minimal elements.
     * @see #minAll(Comparator, Collector)
     * @see #minAll(Comparator)
     * @see #minAll()
     */
    @SuppressWarnings("rawtypes")
    public static <T extends Comparable, A, D> Collector<T, ?, D> minAll(Collector<T, A, D> downstream) {
        return maxAll(Fn.reverseOrder(), downstream);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which finds all the elements which are equal
     * to each other and smaller than any other element according to the natural
     * order. The found elements are collected to {@link List}.
     *
     * @param <T> the type of the input elements
     * @return a {@code Collector} which finds all the minimal elements and
     *         collects them to the {@code List}.
     * @see #minAll(Comparator)
     * @see #minAll(Collector)
     */
    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Collector<T, ?, List<T>> minAll() {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return maxAll(Fn.reverseOrder(), downstream);
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

    public static <T> Collector<T, ?, OptionalLong> summingInt2(final ToIntFunction<? super T> mapper) {
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

        final Function<long[], OptionalLong> finisher = new Function<long[], OptionalLong>() {
            @Override
            public OptionalLong apply(long[] a) {
                return a[1] == 0 ? OptionalLong.empty() : OptionalLong.of(a[0]);
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

    public static <T> Collector<T, ?, OptionalLong> summingLong2(final ToLongFunction<? super T> mapper) {
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

        final Function<long[], OptionalLong> finisher = new Function<long[], OptionalLong>() {
            @Override
            public OptionalLong apply(long[] a) {
                return a[1] == 0 ? OptionalLong.empty() : OptionalLong.of(a[0]);
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

    public static <T> Collector<T, ?, OptionalDouble> summingDouble2(final ToDoubleFunction<? super T> mapper) {
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
                return new double[4];
            }
        };

        final BiConsumer<double[], T> accumulator = new BiConsumer<double[], T>() {
            @Override
            public void accept(double[] a, T t) {
                final double d = mapper.applyAsDouble(t);
                sumWithCompensation(a, d);
                a[2] += d;

                a[3]++;
            }
        };

        final BinaryOperator<double[]> combiner = new BinaryOperator<double[]>() {
            @Override
            public double[] apply(double[] a, double[] b) {
                sumWithCompensation(a, b[0]);
                a[2] += b[2];
                a[3] += b[3];
                return sumWithCompensation(a, b[1]);
            }
        };

        final Function<double[], OptionalDouble> finisher = new Function<double[], OptionalDouble>() {
            @Override
            public OptionalDouble apply(double[] a) {
                return a[3] == 0 ? OptionalDouble.empty() : OptionalDouble.of(computeFinalSum(a));
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

    public static <T> Collector<T, ?, CharSummaryStatistics> summarizingChar(final ToCharFunction<? super T> mapper) {
        final Supplier<CharSummaryStatistics> supplier = new Supplier<CharSummaryStatistics>() {
            @Override
            public CharSummaryStatistics get() {
                return new CharSummaryStatistics();
            }
        };

        final BiConsumer<CharSummaryStatistics, T> accumulator = new BiConsumer<CharSummaryStatistics, T>() {
            @Override
            public void accept(CharSummaryStatistics a, T t) {
                a.accept(mapper.applyAsChar(t));
            }
        };

        final BinaryOperator<CharSummaryStatistics> combiner = new BinaryOperator<CharSummaryStatistics>() {
            @Override
            public CharSummaryStatistics apply(CharSummaryStatistics a, CharSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, CharSummaryStatistics, CharSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T> Collector<T, ?, ByteSummaryStatistics> summarizingByte(final ToByteFunction<? super T> mapper) {
        final Supplier<ByteSummaryStatistics> supplier = new Supplier<ByteSummaryStatistics>() {
            @Override
            public ByteSummaryStatistics get() {
                return new ByteSummaryStatistics();
            }
        };

        final BiConsumer<ByteSummaryStatistics, T> accumulator = new BiConsumer<ByteSummaryStatistics, T>() {
            @Override
            public void accept(ByteSummaryStatistics a, T t) {
                a.accept(mapper.applyAsByte(t));
            }
        };

        final BinaryOperator<ByteSummaryStatistics> combiner = new BinaryOperator<ByteSummaryStatistics>() {
            @Override
            public ByteSummaryStatistics apply(ByteSummaryStatistics a, ByteSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, ByteSummaryStatistics, ByteSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
    }

    public static <T> Collector<T, ?, ShortSummaryStatistics> summarizingShort(final ToShortFunction<? super T> mapper) {
        final Supplier<ShortSummaryStatistics> supplier = new Supplier<ShortSummaryStatistics>() {
            @Override
            public ShortSummaryStatistics get() {
                return new ShortSummaryStatistics();
            }
        };

        final BiConsumer<ShortSummaryStatistics, T> accumulator = new BiConsumer<ShortSummaryStatistics, T>() {
            @Override
            public void accept(ShortSummaryStatistics a, T t) {
                a.accept(mapper.applyAsShort(t));
            }
        };

        final BinaryOperator<ShortSummaryStatistics> combiner = new BinaryOperator<ShortSummaryStatistics>() {
            @Override
            public ShortSummaryStatistics apply(ShortSummaryStatistics a, ShortSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, ShortSummaryStatistics, ShortSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
    }

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

    public static <T> Collector<T, ?, FloatSummaryStatistics> summarizingFloat(final ToFloatFunction<? super T> mapper) {
        final Supplier<FloatSummaryStatistics> supplier = new Supplier<FloatSummaryStatistics>() {
            @Override
            public FloatSummaryStatistics get() {
                return new FloatSummaryStatistics();
            }
        };

        final BiConsumer<FloatSummaryStatistics, T> accumulator = new BiConsumer<FloatSummaryStatistics, T>() {
            @Override
            public void accept(FloatSummaryStatistics a, T t) {
                a.accept(mapper.applyAsFloat(t));
            }
        };

        final BinaryOperator<FloatSummaryStatistics> combiner = new BinaryOperator<FloatSummaryStatistics>() {
            @Override
            public FloatSummaryStatistics apply(FloatSummaryStatistics a, FloatSummaryStatistics b) {
                a.combine(b);
                return a;
            }
        };

        return new CollectorImpl<T, FloatSummaryStatistics, FloatSummaryStatistics>(supplier, accumulator, combiner, CH_ID);
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
     * is described as an {@code NullabLe<T>}.
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
    public static <T> Collector<T, ?, NullabLe<T>> reducing(final BinaryOperator<T> op) {
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

        final Function<OptionalBox<T>, NullabLe<T>> finisher = new Function<OptionalBox<T>, NullabLe<T>>() {
            @Override
            public NullabLe<T> apply(OptionalBox<T> a) {
                return a.present ? NullabLe.of(a.value) : (NullabLe<T>) NullabLe.empty();
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
        final Collector<T, OptionalBox<T>, NullabLe<T>> collector = (Collector<T, OptionalBox<T>, NullabLe<T>>) reducing(op);

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
        final Collector<T, OptionalBox<T>, NullabLe<T>> collector = (Collector<T, OptionalBox<T>, NullabLe<T>>) reducing(op);

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

    public static <T, U> Collector<T, ?, NullabLe<U>> reducing(final Function<? super T, ? extends U> mapper, final BinaryOperator<U> op) {
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

        final Function<OptionalBox2<T, U>, NullabLe<U>> finisher = new Function<OptionalBox2<T, U>, NullabLe<U>>() {
            @Override
            public NullabLe<U> apply(OptionalBox2<T, U> a) {
                return a.present ? NullabLe.of(a.value) : (NullabLe<U>) NullabLe.empty();
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
        final Collector<T, OptionalBox2<T, U>, NullabLe<U>> collector = (Collector<T, OptionalBox2<T, U>, NullabLe<U>>) reducing(mapper, op);

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
        final Collector<T, OptionalBox2<T, U>, NullabLe<U>> collector = (Collector<T, OptionalBox2<T, U>, NullabLe<U>>) reducing(mapper, op);

        final Function<OptionalBox2<T, U>, U> finisher = new Function<OptionalBox2<T, U>, U>() {
            @Override
            public U apply(OptionalBox2<T, U> a) {
                return collector.finisher().apply(a).orThrow(exceptionSupplier);
            }
        };

        return new CollectorImpl<>(collector.supplier(), collector.accumulator(), collector.combiner(), finisher, CH_NOID);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which computes a common prefix of input
     * {@code CharSequence} objects returning the result as {@code String}. For
     * empty input the empty {@code String} is returned.
     *
     * <p>
     * The returned {@code Collector} handles specially Unicode surrogate pairs:
     * the returned prefix may end with
     * <a href="http://www.unicode.org/glossary/#high_surrogate_code_unit">
     * Unicode high-surrogate code unit</a> only if it's not succeeded by
     * <a href="http://www.unicode.org/glossary/#low_surrogate_code_unit">
     * Unicode low-surrogate code unit</a> in any of the input sequences.
     * Normally the ending high-surrogate code unit is removed from the prefix.
     * 
     * <p>
     * This method returns a
     * <a href="package-summary.html#ShortCircuitReduction">short-circuiting
     * collector</a>: it may not process all the elements if the common prefix
     * is empty.
     * 
     * @return a {@code Collector} which computes a common prefix.
     * @since 0.5.0
     */
    public static Collector<CharSequence, ?, String> commonPrefix() {
        final Supplier<Pair<CharSequence, Integer>> supplier = new Supplier<Pair<CharSequence, Integer>>() {
            @Override
            public Pair<CharSequence, Integer> get() {
                return Pair.of(null, -1);
            }
        };

        final BiConsumer<Pair<CharSequence, Integer>, CharSequence> accumulator = new BiConsumer<Pair<CharSequence, Integer>, CharSequence>() {
            @Override
            public void accept(Pair<CharSequence, Integer> a, CharSequence t) {
                if (a.right == -1) {
                    a.left = t;
                    a.right = t.length();
                } else if (a.right > 0) {
                    if (t.length() < a.right) {
                        a.right = t.length();
                    }

                    for (int i = 0, to = a.right; i < to; i++) {
                        if (a.left.charAt(i) != t.charAt(i)) {
                            if (i > 0 && Character.isHighSurrogate(t.charAt(i - 1))
                                    && (Character.isLowSurrogate(t.charAt(i)) || Character.isLowSurrogate(a.left.charAt(i)))) {
                                i--;
                            }

                            a.right = i;

                            break;
                        }
                    }
                }
            }
        };

        final BinaryOperator<Pair<CharSequence, Integer>> combiner = new BinaryOperator<Pair<CharSequence, Integer>>() {
            @Override
            public Pair<CharSequence, Integer> apply(Pair<CharSequence, Integer> a, Pair<CharSequence, Integer> b) {
                if (a.right == -1) {
                    return b;
                }

                if (b.right != -1) {
                    accumulator.accept(a, b.left.subSequence(0, b.right));
                }

                return a;
            }
        };

        final Function<Pair<CharSequence, Integer>, String> finisher = new Function<Pair<CharSequence, Integer>, String>() {
            @Override
            public String apply(Pair<CharSequence, Integer> a) {
                return a.left == null ? "" : a.left.subSequence(0, a.right).toString();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_UNORDERED);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * Returns a {@code Collector} which computes a common suffix of input
     * {@code CharSequence} objects returning the result as {@code String}. For
     * empty input the empty {@code String} is returned.
     *
     * <p>
     * The returned {@code Collector} handles specially Unicode surrogate pairs:
     * the returned suffix may start with
     * <a href="http://www.unicode.org/glossary/#low_surrogate_code_unit">
     * Unicode low-surrogate code unit</a> only if it's not preceded by
     * <a href="http://www.unicode.org/glossary/#high_surrogate_code_unit">
     * Unicode high-surrogate code unit</a> in any of the input sequences.
     * Normally the starting low-surrogate code unit is removed from the suffix.
     * 
     * <p>
     * This method returns a
     * <a href="package-summary.html#ShortCircuitReduction">short-circuiting
     * collector</a>: it may not process all the elements if the common suffix
     * is empty.
     * 
     * @return a {@code Collector} which computes a common suffix.
     * @since 0.5.0
     */
    public static Collector<CharSequence, ?, String> commonSuffix() {
        final Supplier<Pair<CharSequence, Integer>> supplier = new Supplier<Pair<CharSequence, Integer>>() {
            @Override
            public Pair<CharSequence, Integer> get() {
                return Pair.of(null, -1);
            }
        };

        final BiConsumer<Pair<CharSequence, Integer>, CharSequence> accumulator = new BiConsumer<Pair<CharSequence, Integer>, CharSequence>() {
            @Override
            public void accept(Pair<CharSequence, Integer> a, CharSequence t) {
                if (a.right == -1) {
                    a.left = t;
                    a.right = t.length();
                } else if (a.right > 0) {
                    int alen = a.left.length();
                    int blen = t.length();

                    if (blen < a.right) {
                        a.right = blen;
                    }

                    for (int i = 0, to = a.right; i < to; i++) {
                        if (a.left.charAt(alen - 1 - i) != t.charAt(blen - 1 - i)) {
                            if (i > 0 && Character.isLowSurrogate(t.charAt(blen - i))
                                    && (Character.isHighSurrogate(t.charAt(blen - 1 - i)) || Character.isHighSurrogate(a.left.charAt(alen - 1 - i)))) {
                                i--;
                            }

                            a.right = i;

                            break;
                        }
                    }
                }
            }
        };

        final BinaryOperator<Pair<CharSequence, Integer>> combiner = new BinaryOperator<Pair<CharSequence, Integer>>() {
            @Override
            public Pair<CharSequence, Integer> apply(Pair<CharSequence, Integer> a, Pair<CharSequence, Integer> b) {
                if (a.right == -1) {
                    return b;
                }

                if (b.right != -1) {
                    accumulator.accept(a, b.left.subSequence(b.left.length() - b.right, b.left.length()));
                }

                return a;
            }
        };

        final Function<Pair<CharSequence, Integer>, String> finisher = new Function<Pair<CharSequence, Integer>, String>() {
            @Override
            public String apply(Pair<CharSequence, Integer> a) {
                return a.left == null ? "" : a.left.subSequence(a.left.length() - a.right, a.left.length()).toString();
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_UNORDERED);
    }

    /**
     * It's copied from StreamEx: https://github.com/amaembo/streamex
     * <br />
     * 
     * 
     * Returns a collector which collects input elements into {@code List}
     * removing the elements following their dominator element. The dominator
     * elements are defined according to given isDominator {@code BiPredicate}.
     * The isDominator relation must be transitive (if A dominates over B and B
     * dominates over C, then A also dominates over C).
     * 
     * <p>
     * This operation is similar to
     * {@code streamEx.collapse(isDominator).toList()}. The important difference
     * is that in this method {@code BiPredicate} accepts not the adjacent
     * stream elements, but the leftmost element of the series (current
     * dominator) and the current element.
     * 
     * <p>
     * For example, consider the stream of numbers:
     * 
     * <pre>{@code
     * StreamEx<Integer> stream = StreamEx.of(1, 5, 3, 4, 2, 7);
     * }</pre>
     * 
     * <p>
     * Using {@code stream.collapse((a, b) -> a >= b).toList()} you will get the
     * numbers which are bigger than their immediate predecessor (
     * {@code [1, 5, 4, 7]}), because (3, 4) pair is not collapsed. However
     * using {@code stream.collect(dominators((a, b) -> a >= b))} you will get
     * the numbers which are bigger than any predecessor ({@code [1, 5, 7]}) as
     * 5 is the dominator element for the subsequent 3, 4 and 2.
     * 
     * @param <T> type of the input elements.
     * @param isDominator a non-interfering, stateless, transitive
     *        {@code BiPredicate} which returns true if the first argument is
     *        the dominator for the second argument.
     * @return a collector which collects input element into {@code List}
     *         leaving only dominator elements. 
     * @throws UnsupportedOperationException it's used in parallel stream.
     * @since 0.5.1
     */
    public static <T> Collector<T, ?, List<T>> dominators(final BiPredicate<? super T, ? super T> isDominator) {
        final Supplier<List<T>> supplier = new Supplier<List<T>>() {
            private volatile boolean isCalled = false;

            @Override
            public List<T> get() {
                if (isCalled) {
                    throw new UnsupportedOperationException("The 'dominators' Collector only can be used in sequential stream");
                }

                isCalled = true;
                return new ArrayList<>();
            }
        };

        final BiConsumer<List<T>, T> accumulator = new BiConsumer<List<T>, T>() {
            @Override
            public void accept(List<T> a, T t) {
                if (a.isEmpty() || !isDominator.test(a.get(a.size() - 1), t))
                    a.add(t);
            }
        };

        final BinaryOperator<List<T>> combiner = new BinaryOperator<List<T>>() {
            @Override
            public List<T> apply(List<T> a, List<T> b) {
                if (N.notNullOrEmpty(a) && N.notNullOrEmpty(b)) {
                    throw new UnsupportedOperationException("The 'dominators' Collector only can be used in sequential streams");
                }

                if (a.isEmpty()) {
                    return b;
                }

                int i = 0, l = b.size();
                T last = a.get(a.size() - 1);

                while (i < l && isDominator.test(last, b.get(i))) {
                    i++;
                }

                if (i < l) {
                    a.addAll(b.subList(i, l));
                }

                return a;
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, CH_ID);
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
    public static <T, K, A, D, M extends Map<K, D>> Collector<T, ?, M> groupingBy(final Function<? super T, ? extends K> classifier,
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

        final BinaryOperator<Map<K, A>> combiner = Collectors.<K, A, Map<K, A>> mapMerger(downstream.combiner());
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

        final BinaryOperator<ConcurrentMap<K, A>> combiner = Collectors.<K, A, ConcurrentMap<K, A>> mapMerger(downstream.combiner());
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

        final Function<Partition<A>, Map<Boolean, D>> finisher = new Function<Partition<A>, Map<Boolean, D>>() {
            @Override
            public Map<Boolean, D> apply(Partition<A> a) {
                return new Partition<>(downstream.finisher().apply(a.forTrue), downstream.finisher().apply(a.forFalse));
            }
        };

        return new CollectorImpl<>(supplier, accumulator, combiner, finisher, CH_NOID);
    }

    public static <K, V> Collector<Map.Entry<K, V>, ?, Map<K, V>> toMap() {
        final Function<Map.Entry<K, V>, ? extends K> keyExtractor = new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Map.Entry<K, V> t) {
                return t.getKey();
            }
        };

        final Function<Map.Entry<K, V>, ? extends V> valueMapper = new Function<Map.Entry<K, V>, V>() {
            @Override
            public V apply(Map.Entry<K, V> t) {
                return t.getValue();
            }
        };

        return toMap(keyExtractor, valueMapper);
    }

    public static <K, V> Collector<Map.Entry<K, V>, ?, Map<K, V>> toMap(final BinaryOperator<V> mergeFunction) {
        return Collectors.toMap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Map.Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, V>() {
            @Override
            public V apply(Map.Entry<K, V> entry) {
                return entry.getValue();
            }
        }, mergeFunction);
    }

    public static <K, V, M extends Map<K, V>> Collector<Map.Entry<K, V>, ?, M> toMap(final Supplier<M> mapFactory) {
        final Function<Map.Entry<K, V>, ? extends K> keyExtractor = new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Map.Entry<K, V> t) {
                return t.getKey();
            }
        };

        final Function<Map.Entry<K, V>, ? extends V> valueMapper = new Function<Map.Entry<K, V>, V>() {
            @Override
            public V apply(Map.Entry<K, V> t) {
                return t.getValue();
            }
        };

        return toMap(keyExtractor, valueMapper, mapFactory);
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
     * @param keyExtractor a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @return a {@code Collector} which collects elements into a {@code Map}
     * whose keys and values are the result of applying mapping functions to
     * the input elements
     *
     * @see #toMap(Function, Function, BinaryOperator)
     * @see #toMap(Function, Function, BinaryOperator, Supplier)
     * @see #toConcurrentMap(Function, Function)
     */
    public static <T, K, U> Collector<T, ?, Map<K, U>> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction);
    }

    public static <T, K, U, M extends Map<K, U>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
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
     * @param keyExtractor a mapping function to produce keys
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
    public static <T, K, U> Collector<T, ?, Map<K, U>> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        };

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
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
     * @param keyExtractor a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, BiFunction)}
     * @param mapFactory a function which returns a new, empty {@code Map} into
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
    public static <T, K, U, M extends Map<K, U>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        final BiConsumer<M, T> accumulator = new BiConsumer<M, T>() {
            @Override
            public void accept(M map, T element) {
                merge(map, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
            }
        };

        final BinaryOperator<M> combiner = (BinaryOperator<M>) mapMerger(mergeFunction);

        return new CollectorImpl<>(mapFactory, accumulator, combiner, CH_ID);
    }

    public static <K, V> Collector<Map.Entry<K, V>, ?, Map<K, List<V>>> toMap2() {
        @SuppressWarnings("rawtypes")
        final BinaryOperator<List<V>> mergeFunction = (BinaryOperator) ADD_ALL_MERGER;

        return Collectors.toMap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Map.Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, List<V>>() {
            @Override
            public List<V> apply(Map.Entry<K, V> entry) {
                return N.asList(entry.getValue());
            }
        }, mergeFunction);
    }

    public static <K, V, M extends Map<K, List<V>>> Collector<Map.Entry<K, V>, ?, M> toMap2(final Supplier<M> mapFactory) {
        @SuppressWarnings("rawtypes")
        final BinaryOperator<List<V>> mergeFunction = (BinaryOperator) ADD_ALL_MERGER;

        return Collectors.toMap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, List<V>>() {
            @Override
            public List<V> apply(Entry<K, V> entry) {
                return N.asList(entry.getValue());
            }
        }, mergeFunction, mapFactory);
    }

    public static <T, K, V> Collector<T, ?, Map<K, List<V>>> toMap2(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends V> valueMapper) {
        @SuppressWarnings("rawtypes")
        final BinaryOperator<List<V>> mergeFunction = (BinaryOperator) ADD_ALL_MERGER;

        return Collectors.toMap(new Function<T, K>() {
            @Override
            public K apply(T t) {
                return keyExtractor.apply(t);
            }
        }, new Function<T, List<V>>() {
            @Override
            public List<V> apply(T t) {
                return N.asList(valueMapper.apply(t));
            }
        }, mergeFunction);
    }

    public static <T, K, V, M extends Map<K, List<V>>> Collector<T, ?, M> toMap2(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends V> valueMapper, final Supplier<M> mapFactory) {
        @SuppressWarnings("rawtypes")
        final BinaryOperator<List<V>> mergeFunction = (BinaryOperator) ADD_ALL_MERGER;

        return Collectors.toMap(new Function<T, K>() {
            @Override
            public K apply(T t) {
                return keyExtractor.apply(t);
            }
        }, new Function<T, List<V>>() {
            @Override
            public List<V> apply(T t) {
                return N.asList(valueMapper.apply(t));
            }
        }, mergeFunction, mapFactory);
    }

    public static <K, V> Collector<Map.Entry<K, V>, ?, ImmutableMap<K, V>> toImmutableMap() {
        final Collector<Map.Entry<K, V>, ?, Map<K, V>> downstream = toMap();

        final Function<Map<K, V>, ImmutableMap<K, V>> finisher = new Function<Map<K, V>, ImmutableMap<K, V>>() {
            @Override
            public ImmutableMap<K, V> apply(Map<K, V> t) {
                return ImmutableMap.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
    }

    public static <K, V> Collector<Map.Entry<K, V>, ?, ImmutableMap<K, V>> toImmutableMap(final BinaryOperator<V> mergeFunction) {
        final Collector<Map.Entry<K, V>, ?, Map<K, V>> downstream = toMap(mergeFunction);

        final Function<Map<K, V>, ImmutableMap<K, V>> finisher = new Function<Map<K, V>, ImmutableMap<K, V>>() {
            @Override
            public ImmutableMap<K, V> apply(Map<K, V> t) {
                return ImmutableMap.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
    }

    public static <T, K, U> Collector<T, ?, ImmutableMap<K, U>> toImmutableMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper) {
        final Collector<T, ?, Map<K, U>> downstream = toMap(keyExtractor, valueMapper);

        final Function<Map<K, U>, ImmutableMap<K, U>> finisher = new Function<Map<K, U>, ImmutableMap<K, U>>() {
            @Override
            public ImmutableMap<K, U> apply(Map<K, U> t) {
                return ImmutableMap.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
    }

    public static <T, K, U> Collector<T, ?, ImmutableMap<K, U>> toImmutableMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Collector<T, ?, Map<K, U>> downstream = toMap(keyExtractor, valueMapper, mergeFunction);

        final Function<Map<K, U>, ImmutableMap<K, U>> finisher = new Function<Map<K, U>, ImmutableMap<K, U>>() {
            @Override
            public ImmutableMap<K, U> apply(Map<K, U> t) {
                return ImmutableMap.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
    }

    public static <K, V> Collector<Map.Entry<K, V>, ?, ImmutableMap<K, List<V>>> toImmutableMap2() {
        final Collector<Map.Entry<K, V>, ?, Map<K, List<V>>> downstream = toMap2();

        final Function<Map<K, List<V>>, ImmutableMap<K, List<V>>> finisher = new Function<Map<K, List<V>>, ImmutableMap<K, List<V>>>() {
            @Override
            public ImmutableMap<K, List<V>> apply(Map<K, List<V>> t) {
                return ImmutableMap.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
    }

    public static <T, K, V> Collector<T, ?, ImmutableMap<K, List<V>>> toImmutableMap2(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends V> valueMapper) {
        final Collector<T, ?, Map<K, List<V>>> downstream = toMap2(keyExtractor, valueMapper);

        final Function<Map<K, List<V>>, ImmutableMap<K, List<V>>> finisher = new Function<Map<K, List<V>>, ImmutableMap<K, List<V>>>() {
            @Override
            public ImmutableMap<K, List<V>> apply(Map<K, List<V>> t) {
                return ImmutableMap.of(t);
            }
        };

        return collectingAndThen(downstream, finisher);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see #toMap(Function, Function)
     */
    public static <T, K, U> Collector<T, ?, LinkedHashMap<K, U>> toLinkedHashMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toLinkedHashMap(keyExtractor, valueMapper, mergeFunction);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see #toMap(Function, Function, BinaryOperator)
     */
    public static <T, K, U> Collector<T, ?, LinkedHashMap<K, U>> toLinkedHashMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<LinkedHashMap<K, U>> mapFactory = new Supplier<LinkedHashMap<K, U>>() {
            @Override
            public LinkedHashMap<K, U> get() {
                return new LinkedHashMap<>();
            }
        };

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
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
     * @param keyExtractor the mapping function to produce keys
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
    public static <T, K, U> Collector<T, ?, ConcurrentMap<K, U>> toConcurrentMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toConcurrentMap(keyExtractor, valueMapper, mergeFunction);
    }

    public static <T, K, U, M extends ConcurrentMap<K, U>> Collector<T, ?, M> toConcurrentMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toConcurrentMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
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
     * @param keyExtractor a mapping function to produce keys
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
    public static <T, K, U> Collector<T, ?, ConcurrentMap<K, U>> toConcurrentMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        final Supplier<ConcurrentMap<K, U>> mapFactory = new Supplier<ConcurrentMap<K, U>>() {
            @Override
            public ConcurrentMap<K, U> get() {
                return new ConcurrentHashMap<>();
            }
        };

        return toConcurrentMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
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
     * @param keyExtractor a mapping function to produce keys
     * @param valueMapper a mapping function to produce values
     * @param mergeFunction a merge function, used to resolve collisions between
     *                      values associated with the same key, as supplied
     *                      to {@link Map#merge(Object, Object, BiFunction)}
     * @param mapFactory a function which returns a new, empty {@code Map} into
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
    public static <T, K, U, M extends ConcurrentMap<K, U>> Collector<T, ?, M> toConcurrentMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {

        final BiConsumer<M, T> accumulator = new BiConsumer<M, T>() {
            @Override
            public void accept(M map, T element) {
                merge(map, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
            }
        };

        final BinaryOperator<M> combiner = (BinaryOperator<M>) mapMerger2(mergeFunction);

        return new CollectorImpl<T, M, M>(mapFactory, accumulator, combiner, CH_CONCURRENT_ID);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toBiMap(keyExtractor, valueMapper, mergeFunction);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final Supplier<BiMap<K, U>> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toBiMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Supplier<BiMap<K, U>> mapFactory = new Supplier<BiMap<K, U>>() {
            @Override
            public BiMap<K, U> get() {
                return new BiMap<>();
            }
        };

        return toBiMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public static <T, K, U> Collector<T, ?, BiMap<K, U>> toBiMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, final Supplier<BiMap<K, U>> mapFactory) {
        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public static <K, E> Collector<Map.Entry<? extends K, ? extends E>, ?, Multimap<K, E, List<E>>> toMultimap() {
        final Function<Map.Entry<? extends K, ? extends E>, ? extends K> keyExtractor = new Function<Map.Entry<? extends K, ? extends E>, K>() {
            @Override
            public K apply(Entry<? extends K, ? extends E> t) {
                return t.getKey();
            }
        };

        final Function<Map.Entry<? extends K, ? extends E>, ? extends E> valueMapper = new Function<Map.Entry<? extends K, ? extends E>, E>() {
            @Override
            public E apply(Entry<? extends K, ? extends E> t) {
                return t.getValue();
            }
        };

        return toMultimap(keyExtractor, valueMapper);
    }

    public static <K, E, V extends Collection<E>> Collector<Map.Entry<? extends K, ? extends E>, ?, Multimap<K, E, V>> toMultimap(
            final Supplier<Multimap<K, E, V>> mapFactory) {
        final Function<Map.Entry<? extends K, ? extends E>, ? extends K> keyExtractor = new Function<Map.Entry<? extends K, ? extends E>, K>() {
            @Override
            public K apply(Entry<? extends K, ? extends E> t) {
                return t.getKey();
            }
        };

        final Function<Map.Entry<? extends K, ? extends E>, ? extends E> valueMapper = new Function<Map.Entry<? extends K, ? extends E>, E>() {
            @Override
            public E apply(Entry<? extends K, ? extends E> t) {
                return t.getValue();
            }
        };

        return toMultimap(keyExtractor, valueMapper, mapFactory);
    }

    public static <T, K> Collector<T, ?, Multimap<K, T, List<T>>> toMultimap(Function<? super T, ? extends K> keyExtractor) {
        final Function<? super T, ? extends T> valueMapper = new Function<T, T>() {
            @Override
            public T apply(T t) {
                return t;
            }
        };

        return toMultimap(keyExtractor, valueMapper);
    }

    public static <T, K, V extends Collection<T>> Collector<T, ?, Multimap<K, T, V>> toMultimap(final Function<? super T, ? extends K> keyExtractor,
            final Supplier<Multimap<K, T, V>> mapFactory) {
        final Function<? super T, ? extends T> valueMapper = new Function<T, T>() {
            @Override
            public T apply(T t) {
                return t;
            }
        };

        return toMultimap(keyExtractor, valueMapper, mapFactory);
    }

    public static <T, K, U> Collector<T, ?, Multimap<K, U, List<U>>> toMultimap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper) {
        final Supplier<Multimap<K, U, List<U>>> mapFactory = new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        };

        return toMultimap(keyExtractor, valueMapper, mapFactory);
    }

    public static <T, K, U, V extends Collection<U>> Collector<T, ?, Multimap<K, U, V>> toMultimap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapFactory) {
        final BiConsumer<Multimap<K, U, V>, T> accumulator = new BiConsumer<Multimap<K, U, V>, T>() {
            @Override
            public void accept(Multimap<K, U, V> map, T element) {
                map.put(keyExtractor.apply(element), valueMapper.apply(element));
            }
        };

        final BinaryOperator<Multimap<K, U, V>> combiner = mapMerger3();

        return new CollectorImpl<>(mapFactory, accumulator, combiner, CH_ID);
    }

    public static <T> Collector<T, ?, DataSet> toDataSet() {
        return toDataSet(null);
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

    static <K, V> void replaceAll(Map<K, V> map, BiFunction<? super K, ? super V, ? extends V> function) {
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

    static <K, V> V merge(Map<K, V> map, K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
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
