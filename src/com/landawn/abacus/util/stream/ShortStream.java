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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.Set;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortBinaryOperator;
import com.landawn.abacus.util.function.ShortConsumer;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortNFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortSupplier;
import com.landawn.abacus.util.function.ShortToIntFunction;
import com.landawn.abacus.util.function.ShortTriFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * A sequence of primitive int-valued elements supporting sequential and parallel
 * aggregate operations.  This is the {@code int} primitive specialization of
 * {@link Stream}.
 *
 * <p>The following example illustrates an aggregate operation using
 * {@link Stream} and {@link ShortStream}, computing the sum of the weights of the
 * red widgets:
 *
 * <pre>{@code
 *     int sum = widgets.stream()
 *                      .filter(w -> w.getColor() == RED)
 *                      .mapToShort(w -> w.getWeight())
 *                      .sum();
 * }</pre>
 *
 * See the class documentation for {@link Stream} and the package documentation
 * for <a href="package-summary.html">java.util.stream</a> for additional
 * specification of streams, stream operations, stream pipelines, and
 * parallelism.
 *
 * @since 1.8
 * @see Stream
 * @see <a href="package-summary.html">java.util.stream</a>
 */
public abstract class ShortStream implements BaseStream<Short, ShortStream> {

    private static final ShortStream EMPTY = new ArrayShortStream(N.EMPTY_SHORT_ARRAY);

    /**
     * Returns a stream consisting of the elements of this stream that match
     * the given predicate.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param predicate a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to each element to determine if it
     *                  should be included
     * @return the new stream
     */
    public abstract ShortStream filter(final ShortPredicate predicate);

    /**
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract ShortStream filter(final ShortPredicate predicate, final long max);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    public abstract ShortStream takeWhile(final ShortPredicate predicate);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract ShortStream takeWhile(final ShortPredicate predicate, final long max);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    public abstract ShortStream dropWhile(final ShortPredicate predicate);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract ShortStream dropWhile(final ShortPredicate predicate, final long max);

    /**
     * Returns a stream consisting of the results of applying the given
     * function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract ShortStream map(ShortUnaryOperator mapper);

    /**
     * Returns a {@code IntStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract IntStream mapToInt(ShortToIntFunction mapper);

    /**
     * Returns an object-valued {@code Stream} consisting of the results of
     * applying the given function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">
     *     intermediate operation</a>.
     *
     * @param <U> the element type of the new stream
     * @param mapper a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract <U> Stream<U> mapToObj(ShortFunction<? extends U> mapper);

    /**
     * Returns a stream consisting of the results of replacing each element of
     * this stream with the contents of a mapped stream produced by applying
     * the provided mapping function to each element.  Each mapped stream is
     * {@link java.util.stream.BaseStream#close() closed} after its contents
     * have been placed into this stream.  (If a mapped stream is {@code null}
     * an empty stream is used, instead.)
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element which produces an
     *               {@code ShortStream} of new values
     * @return the new stream
     * @see Stream#flatMap(Function)
     */
    public abstract ShortStream flatMap(ShortFunction<? extends ShortStream> mapper);

    public abstract IntStream flatMapToInt(ShortFunction<? extends IntStream> mapper);

    public abstract <T> Stream<T> flatMapToObj(ShortFunction<? extends Stream<T>> mapper);

    /**
     * Returns Stream of ShortStream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller).
     * 
     * @param size
     * @return
     */
    public abstract Stream<ShortStream> split(int size);

    /**
     * Split the stream by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * final MutableInt border = MutableInt.of(5);
     * IntStream.of(1, 2, 3, 5, 7, 9, 10, 11, 19).split(e -> {
     *     if (e <= border.intValue()) {
     *         return true;
     *     } else {
     *         border.addAndGet(5);
     *         return false;
     *     }
     * }).map(s -> s.toArray()).forEach(N::println);
     * </code>
     * </pre>
     * 
     * This stream should be sorted by value which is used to verify the border.
     * This method only run sequentially, even in parallel stream.
     * 
     * @param predicate
     * @return
     */
    public abstract Stream<ShortStream> split(ShortPredicate predicate);

    /**
     * Returns a stream consisting of the distinct elements of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @return the new stream
     */
    public abstract ShortStream distinct();

    public abstract ShortStream top(int n);

    public abstract ShortStream top(final int n, Comparator<? super Short> comparator);

    /**
     * Returns a stream consisting of the elements of this stream in sorted
     * order.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @return the new stream
     */
    public abstract ShortStream sorted();

    /**
     * Returns a stream consisting of the elements of this stream, additionally
     * performing the provided action on each element as elements are consumed
     * from the resulting stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * <p>For parallel stream pipelines, the action may be called at
     * whatever time and in whatever thread the element is made available by the
     * upstream operation.  If the action modifies shared state,
     * it is responsible for providing the required synchronization.
     *
     * @apiNote This method exists mainly to support debugging, where you want
     * to see the elements as they flow past a certain point in a pipeline:
     * <pre>{@code
     *     ShortStream.of(1, 2, 3, 4)
     *         .filter(e -> e > 2)
     *         .peek(e -> System.out.println("Filtered value: " + e))
     *         .map(e -> e * e)
     *         .peek(e -> System.out.println("Mapped value: " + e))
     *         .sum();
     * }</pre>
     *
     * @param action a <a href="package-summary.html#NonShorterference">
     *               non-interfering</a> action to perform on the elements as
     *               they are consumed from the stream
     * @return the new stream
     */
    public abstract ShortStream peek(ShortConsumer action);

    /**
     * Returns a stream consisting of the elements of this stream, truncated
     * to be no longer than {@code maxSize} in length.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * stateful intermediate operation</a>.
     *
     * @apiNote
     * While {@code limit()} is generally a cheap operation on sequential
     * stream pipelines, it can be quite expensive on ordered parallel pipelines,
     * especially for large values of {@code maxSize}, since {@code limit(n)}
     * is constrained to return not just any <em>n</em> elements, but the
     * <em>first n</em> elements in the encounter order.  Using an unordered
     * stream source or removing the
     * ordering constraint with {@link #unordered()} may result in significant
     * speedups of {@code limit()} in parallel pipelines, if the semantics of
     * your situation permit.  If consistency with encounter order is required,
     * and you are experiencing poor performance or memory utilization with
     * {@code limit()} in parallel pipelines, switching to sequential execution
     * with {@link #sequential()} may improve performance.
     *
     * @param maxSize the number of elements the stream should be limited to
     * @return the new stream
     * @throws IllegalArgumentException if {@code maxSize} is negative
     */
    public abstract ShortStream limit(long maxSize);

    /**
     * Returns a stream consisting of the remaining elements of this stream
     * after discarding the first {@code n} elements of the stream.
     * If this stream contains fewer than {@code n} elements then an
     * empty stream will be returned.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @apiNote
     * While {@code skip()} is generally a cheap operation on sequential
     * stream pipelines, it can be quite expensive on ordered parallel pipelines,
     * especially for large values of {@code n}, since {@code skip(n)}
     * is constrained to skip not just any <em>n</em> elements, but the
     * <em>first n</em> elements in the encounter order.  Using an unordered
     * stream source or removing the
     * ordering constraint with {@link #unordered()} may result in significant
     * speedups of {@code skip()} in parallel pipelines, if the semantics of
     * your situation permit.  If consistency with encounter order is required,
     * and you are experiencing poor performance or memory utilization with
     * {@code skip()} in parallel pipelines, switching to sequential execution
     * with {@link #sequential()} may improve performance.
     *
     * @param n the number of leading elements to skip
     * @return the new stream
     * @throws IllegalArgumentException if {@code n} is negative
     */
    public abstract ShortStream skip(long n);

    /**
     * Performs an action for each element of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * <p>For parallel stream pipelines, this operation does <em>not</em>
     * guarantee to respect the encounter order of the stream, as doing so
     * would sacrifice the benefit of parallelism.  For any given element, the
     * action may be performed at whatever time and in whatever thread the
     * library chooses.  If the action accesses shared state, it is
     * responsible for providing the required synchronization.
     *
     * @param action a <a href="package-summary.html#NonShorterference">
     *               non-interfering</a> action to perform on the elements
     */
    public abstract void forEach(ShortConsumer action);

    //    /**
    //     * In parallel Streams, the elements after the first element which <code>action</code> returns false may be executed by action too.
    //     * 
    //     * @param action break if the action returns false.
    //     * @return false if it breaks, otherwise true.
    //     */
    //    public abstract boolean forEach2(ShortFunction<Boolean> action);

    /**
     * Returns an array containing the elements of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an array containing the elements of this stream
     */
    public abstract short[] toArray();

    public abstract ShortList toShortList();

    public abstract List<Short> toList();

    public abstract List<Short> toList(Supplier<? extends List<Short>> supplier);

    public abstract Set<Short> toSet();

    public abstract Set<Short> toSet(Supplier<? extends Set<Short>> supplier);

    public abstract Multiset<Short> toMultiset();

    public abstract Multiset<Short> toMultiset(Supplier<? extends Multiset<Short>> supplier);

    public abstract LongMultiset<Short> toLongMultiset();

    public abstract LongMultiset<Short> toLongMultiset(Supplier<? extends LongMultiset<Short>> supplier);

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public abstract <K> Map<K, List<Short>> toMap(ShortFunction<? extends K> classifier);

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public abstract <K, M extends Map<K, List<Short>>> M toMap(final ShortFunction<? extends K> classifier, final Supplier<M> mapFactory);

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public abstract <K, A, D> Map<K, D> toMap(final ShortFunction<? extends K> classifier, final Collector<Short, A, D> downstream);

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public abstract <K, D, A, M extends Map<K, D>> M toMap(final ShortFunction<? extends K> classifier, final Collector<Short, A, D> downstream,
            final Supplier<M> mapFactory);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public abstract <K, U> Map<K, U> toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, Supplier<M> mapSupplier);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public abstract <K, U> Map<K, U> toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public abstract <K, U> Multimap<K, U, List<U>> toMultimap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(ShortFunction<? extends K> keyMapper, ShortFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using the provided identity value and an
     * <a href="package-summary.html#Associativity">associative</a>
     * accumulation function, and returns the reduced value.  This is equivalent
     * to:
     * <pre>{@code
     *     int result = identity;
     *     for (int element : this stream)
     *         result = accumulator.applyAsShort(result, element)
     *     return result;
     * }</pre>
     *
     * but is not constrained to execute sequentially.
     *
     * <p>The {@code identity} value must be an identity for the accumulator
     * function. This means that for all {@code x},
     * {@code accumulator.apply(identity, x)} is equal to {@code x}.
     * The {@code accumulator} function must be an
     * <a href="package-summary.html#Associativity">associative</a> function.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote Sum, min, max, and average are all special cases of reduction.
     * Summing a stream of numbers can be expressed as:
     *
     * <pre>{@code
     *     int sum = integers.reduce(0, (a, b) -> a+b);
     * }</pre>
     *
     * or more compactly:
     *
     * <pre>{@code
     *     int sum = integers.reduce(0, Shorteger::sum);
     * }</pre>
     *
     * <p>While this may seem a more roundabout way to perform an aggregation
     * compared to simply mutating a running total in a loop, reduction
     * operations parallelize more gracefully, without needing additional
     * synchronization and with greatly reduced risk of data races.
     *
     * @param identity the identity value for the accumulating function
     * @param op an <a href="package-summary.html#Associativity">associative</a>,
     *           <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *           <a href="package-summary.html#Statelessness">stateless</a>
     *           function for combining two values
     * @return the result of the reduction
     * @see #sum()
     * @see #min()
     * @see #max()
     * @see #average()
     */
    public abstract short reduce(short identity, ShortBinaryOperator op);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using an
     * <a href="package-summary.html#Associativity">associative</a> accumulation
     * function, and returns an {@code OptionalShort} describing the reduced value,
     * if any. This is equivalent to:
     * <pre>{@code
     *     boolean foundAny = false;
     *     int result = null;
     *     for (int element : this stream) {
     *         if (!foundAny) {
     *             foundAny = true;
     *             result = element;
     *         }
     *         else
     *             result = accumulator.applyAsShort(result, element);
     *     }
     *     return foundAny ? OptionalShort.of(result) : OptionalShort.empty();
     * }</pre>
     *
     * but is not constrained to execute sequentially.
     *
     * <p>The {@code accumulator} function must be an
     * <a href="package-summary.html#Associativity">associative</a> function.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @param op an <a href="package-summary.html#Associativity">associative</a>,
     *           <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *           <a href="package-summary.html#Statelessness">stateless</a>
     *           function for combining two values
     * @return the result of the reduction
     * @see #reduce(int, ShortBinaryOperator)
     */
    public abstract OptionalShort reduce(ShortBinaryOperator op);

    /**
     * Performs a <a href="package-summary.html#MutableReduction">mutable
     * reduction</a> operation on the elements of this stream.  A mutable
     * reduction is one in which the reduced value is a mutable result container,
     * such as an {@code ArrayList}, and elements are incorporated by updating
     * the state of the result rather than by replacing the result.  This
     * produces a result equivalent to:
     * <pre>{@code
     *     R result = supplier.get();
     *     for (int element : this stream)
     *         accumulator.accept(result, element);
     *     return result;
     * }</pre>
     *
     * <p>Like {@link #reduce(int, ShortBinaryOperator)}, {@code collect} operations
     * can be parallelized without requiring additional synchronization.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @param <R> type of the result
     * @param supplier a function that creates a new result container. For a
     *                 parallel execution, this function may be called
     *                 multiple times and must return a fresh value each time.
     * @param accumulator an <a href="package-summary.html#Associativity">associative</a>,
     *                    <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *                    <a href="package-summary.html#Statelessness">stateless</a>
     *                    function for incorporating an additional element into a result
     * @param combiner an <a href="package-summary.html#Associativity">associative</a>,
     *                    <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *                    <a href="package-summary.html#Statelessness">stateless</a>
     *                    function for combining two values, which must be
     *                    compatible with the accumulator function
     * @return the result of the reduction
     * @see Stream#collect(Supplier, BiConsumer, BiConsumer)
     */
    public abstract <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator, BiConsumer<R, R> combiner);

    /**
     * 
     * @param supplier
     * @param accumulator
     * @return
     */
    public abstract <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator);

    /**
     * Returns an {@code OptionalShort} describing the minimum element of this
     * stream, or an empty optional if this stream is empty.  This is a special
     * case of a <a href="package-summary.html#Reduction">reduction</a>
     * and is equivalent to:
     * <pre>{@code
     *     return reduce(Shorteger::min);
     * }</pre>
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal operation</a>.
     *
     * @return an {@code OptionalShort} containing the minimum element of this
     * stream, or an empty {@code OptionalShort} if the stream is empty
     */
    public abstract OptionalShort min();

    /**
     * Returns an {@code OptionalShort} describing the maximum element of this
     * stream, or an empty optional if this stream is empty.  This is a special
     * case of a <a href="package-summary.html#Reduction">reduction</a>
     * and is equivalent to:
     * <pre>{@code
     *     return reduce(Shorteger::max);
     * }</pre>
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an {@code OptionalShort} containing the maximum element of this
     * stream, or an empty {@code OptionalShort} if the stream is empty
     */
    public abstract OptionalShort max();

    /**
     * 
     * @param k
     * @return OptionalByte.empty() if there is no element or count less than k, otherwise the kth largest element.
     */
    public abstract OptionalShort kthLargest(int k);

    public abstract Long sum();

    public abstract OptionalDouble average();

    /**
     * Returns the count of elements in this stream.  This is a special case of
     * a <a href="package-summary.html#Reduction">reduction</a> and is
     * equivalent to:
     * <pre>{@code
     *     return mapToLong(e -> 1L).sum();
     * }</pre>
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal operation</a>.
     *
     * @return the count of elements in this stream
     */
    public abstract long count();

    public abstract Optional<Map<Percentage, Short>> distribution();

    public abstract ShortSummaryStatistics summarize();

    public abstract Pair<ShortSummaryStatistics, Optional<Map<Percentage, Short>>> summarize2();

    /**
     * Returns whether any elements of this stream match the provided
     * predicate.  May not evaluate the predicate on all elements if not
     * necessary for determining the result.  If the stream is empty then
     * {@code false} is returned and the predicate is not evaluated.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * @apiNote
     * This method evaluates the <em>existential quantification</em> of the
     * predicate over the elements of the stream (for some x P(x)).
     *
     * @param predicate a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to elements of this stream
     * @return {@code true} if any elements of the stream match the provided
     * predicate, otherwise {@code false}
     */
    public abstract boolean anyMatch(ShortPredicate predicate);

    /**
     * Returns whether all elements of this stream match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result.  If the stream is empty then {@code true} is
     * returned and the predicate is not evaluated.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * @apiNote
     * This method evaluates the <em>universal quantification</em> of the
     * predicate over the elements of the stream (for all x P(x)).  If the
     * stream is empty, the quantification is said to be <em>vacuously
     * satisfied</em> and is always {@code true} (regardless of P(x)).
     *
     * @param predicate a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to elements of this stream
     * @return {@code true} if either all elements of the stream match the
     * provided predicate or the stream is empty, otherwise {@code false}
     */
    public abstract boolean allMatch(ShortPredicate predicate);

    /**
     * Returns whether no elements of this stream match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result.  If the stream is empty then {@code true} is
     * returned and the predicate is not evaluated.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * @apiNote
     * This method evaluates the <em>universal quantification</em> of the
     * negated predicate over the elements of the stream (for all x ~P(x)).  If
     * the stream is empty, the quantification is said to be vacuously satisfied
     * and is always {@code true}, regardless of P(x).
     *
     * @param predicate a <a href="package-summary.html#NonShorterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to elements of this stream
     * @return {@code true} if either no elements of the stream match the
     * provided predicate or the stream is empty, otherwise {@code false}
     */
    public abstract boolean noneMatch(ShortPredicate predicate);

    /**
     * Returns an {@link OptionalShort} describing the first element of this
     * stream, or an empty {@code OptionalShort} if the stream is empty.  If the
     * stream has no encounter order, then any element may be returned.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * @return an {@code OptionalShort} describing the first element of this stream,
     * or an empty {@code OptionalShort} if the stream is empty
     */
    // public abstract OptionalShort findFirst();

    public abstract OptionalShort findFirst(ShortPredicate predicate);

    // public abstract OptionalShort findLast();

    public abstract OptionalShort findLast(ShortPredicate predicate);

    /**
     * Returns an {@link OptionalShort} describing some element of the stream, or
     * an empty {@code OptionalShort} if the stream is empty.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * <p>The behavior of this operation is explicitly nondeterministic; it is
     * free to select any element in the stream.  This is to allow for maximal
     * performance in parallel operations; the cost is that multiple invocations
     * on the same source may not return the same result.  (If a stable result
     * is desired, use {@link #findFirst()} instead.)
     *
     * @return an {@code OptionalShort} describing some element of this stream, or
     * an empty {@code OptionalShort} if the stream is empty
     * @see #findFirst()
     */
    // public abstract OptionalShort findAny();

    public abstract OptionalShort findAny(ShortPredicate predicate);

    /**
     * @param c
     * @return
     * @see IntList#except(IntList)
     */
    public abstract ShortStream except(Collection<?> c);

    /**
     * @param c
     * @return
     * @see IntList#intersect(IntList)
     */
    public abstract ShortStream intersect(Collection<?> c);

    //    /**
    //     * Skill All the elements in the specified collection.
    //     * 
    //     * @param c
    //     * @return
    //     * @see IntList#intersect(IntList)
    //     */
    //    public abstract ShortStream exclude(Collection<?> c);

    /**
     * Append the specified stream to the tail of this stream.
     * @param stream
     * @return
     */
    @Override
    public abstract ShortStream append(ShortStream stream);

    /**
     * 
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public abstract ShortStream merge(final ShortStream b, final ShortBiFunction<Nth> nextSelector);

    /**
     * Returns a {@code LongStream} consisting of the elements of this stream,
     * converted to {@code long}.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @return a {@code LongStream} consisting of the elements of this stream,
     * converted to {@code long}
     */
    public abstract IntStream asIntStream();

    /**
     * Returns a {@code Stream} consisting of the elements of this stream,
     * each boxed to an {@code Shorteger}.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @return a {@code Stream} consistent of the elements of this stream,
     * each boxed to an {@code Shorteger}
     */
    public abstract Stream<Short> boxed();

    @Override
    public abstract ImmutableIterator<Short> iterator();

    public abstract ImmutableShortIterator shortIterator();

    // Static factories

    public static ShortStream empty() {
        return EMPTY;
    }

    public static ShortStream of(final short... a) {
        return N.isNullOrEmpty(a) ? empty() : new ArrayShortStream(a);
    }

    public static ShortStream of(final short[] a, final int startIndex, final int endIndex) {
        return N.isNullOrEmpty(a) && (startIndex == 0 && endIndex == 0) ? empty() : new ArrayShortStream(a, startIndex, endIndex);
    }

    public static ShortStream of(final ShortIterator iterator) {
        return iterator == null ? empty()
                : new IteratorShortStream(iterator instanceof ImmutableShortIterator ? (ImmutableShortIterator) iterator : new ImmutableShortIterator() {
                    @Override
                    public boolean hasNext() {
                        return iterator.hasNext();
                    }

                    @Override
                    public short next() {
                        return iterator.next();
                    }
                });
    }

    public static ShortStream from(final int... a) {
        return N.isNullOrEmpty(a) ? empty() : of(ShortList.from(a).trimToSize().array());
    }

    public static ShortStream from(final int[] a, final int startIndex, final int endIndex) {
        return N.isNullOrEmpty(a) && (startIndex == 0 && endIndex == 0) ? empty() : of(ShortList.from(a, startIndex, endIndex).trimToSize().array());
    }

    public static ShortStream range(final short startInclusive, final short endExclusive) {
        return of(Array.range(startInclusive, endExclusive));
    }

    public static ShortStream range(final short startInclusive, final short endExclusive, final short by) {
        return of(Array.range(startInclusive, endExclusive, by));
    }

    public static ShortStream rangeClosed(final short startInclusive, final short endInclusive) {
        return of(Array.rangeClosed(startInclusive, endInclusive));
    }

    public static ShortStream rangeClosed(final short startInclusive, final short endInclusive, final short by) {
        return of(Array.rangeClosed(startInclusive, endInclusive, by));
    }

    public static ShortStream repeat(short element, int n) {
        return of(Array.repeat(element, n));
    }

    public static ShortStream random() {
        return iterate(new ShortSupplier() {
            @Override
            public short getAsShort() {
                return (short) Stream.RAND.nextInt();
            }
        });
    }

    public static ShortStream iterate(final Supplier<Boolean> hasNext, final ShortSupplier next) {
        N.requireNonNull(hasNext);
        N.requireNonNull(next);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.get().booleanValue();
                }

                return hasNextVal;
            }

            @Override
            public short next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return next.getAsShort();
            }
        });
    }

    public static ShortStream iterate(final short seed, final Supplier<Boolean> hasNext, final ShortUnaryOperator f) {
        N.requireNonNull(hasNext);
        N.requireNonNull(f);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private short t = 0;
            private boolean isFirst = true;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.get().booleanValue();
                }

                return hasNextVal;
            }

            @Override
            public short next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;

                if (isFirst) {
                    isFirst = false;
                    t = seed;
                } else {
                    t = f.applyAsShort(t);
                }

                return t;
            }
        });
    }

    /**
     * 
     * @param seed
     * @param hasNext test if has next by hasNext.test(seed) for first time and hasNext.test(f.apply(previous)) for remaining.
     * @param f
     * @return
     */
    public static ShortStream iterate(final short seed, final ShortPredicate hasNext, final ShortUnaryOperator f) {
        N.requireNonNull(hasNext);
        N.requireNonNull(f);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private short t = 0;
            private short cur = 0;
            private boolean isFirst = true;
            private boolean noMoreVal = false;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false && noMoreVal == false) {
                    if (isFirst) {
                        isFirst = false;
                        hasNextVal = hasNext.test(cur = seed);
                    } else {
                        hasNextVal = hasNext.test(cur = f.applyAsShort(t));
                    }

                    if (hasNextVal == false) {
                        noMoreVal = true;
                    }
                }

                return hasNextVal;
            }

            @Override
            public short next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                t = cur;
                hasNextVal = false;
                return t;
            }
        });
    }

    public static ShortStream iterate(final short seed, final ShortUnaryOperator f) {
        N.requireNonNull(f);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private short t = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public short next() {
                if (isFirst) {
                    isFirst = false;
                    t = seed;
                } else {
                    t = f.applyAsShort(t);
                }

                return t;
            }
        });
    }

    public static ShortStream iterate(final ShortSupplier s) {
        N.requireNonNull(s);

        return new IteratorShortStream(new ImmutableShortIterator() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public short next() {
                return s.getAsShort();
            }
        });
    }

    public static ShortStream concat(final short[]... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorShortStream(new ImmutableShortIterator() {
            private final Iterator<short[]> iter = N.asList(a).iterator();
            private ImmutableShortIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = new ImmutableShortIterator() {
                        private final short[] cur = iter.next();
                        private int cursor = 0;

                        @Override
                        public boolean hasNext() {
                            return cursor < cur.length;
                        }

                        @Override
                        public short next() {
                            if (cursor >= cur.length) {
                                throw new NoSuchElementException();
                            }

                            return cur[cursor++];
                        }
                    };
                }
                return cur != null && cur.hasNext();
            }

            @Override
            public short next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        });
    }

    public static ShortStream concat(final ShortStream... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorShortStream(new ImmutableShortIterator() {
            private final Iterator<ShortStream> iter = N.asList(a).iterator();
            private ImmutableShortIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next().shortIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (ShortStream stream : a) {
                    try {
                        stream.close();
                    } catch (Throwable throwable) {
                        if (runtimeException == null) {
                            runtimeException = N.toRuntimeException(throwable);
                        } else {
                            runtimeException.addSuppressed(throwable);
                        }
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    public static ShortStream concat(final ShortIterator... a) {
        return N.isNullOrEmpty(a) ? empty() : concat(N.asList(a));
    }

    public static ShortStream concat(final Collection<? extends ShortIterator> c) {
        return N.isNullOrEmpty(c) ? empty() : new IteratorShortStream(new ImmutableShortIterator() {
            private final Iterator<? extends ShortIterator> iter = c.iterator();
            private ShortIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        });
    }

    /**
     * Zip together the "a" and "b" arrays until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ShortStream zip(final short[] a, final short[] b, final ShortBiFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ShortStream zip(final short[] a, final short[] b, final short[] c, final ShortTriFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, c, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ShortStream zip(final ShortStream a, final ShortStream b, final ShortBiFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ShortStream zip(final ShortStream a, final ShortStream b, final ShortStream c, final ShortTriFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, c, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ShortStream zip(final ShortIterator a, final ShortIterator b, final ShortBiFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ShortStream zip(final ShortIterator a, final ShortIterator b, final ShortIterator c, final ShortTriFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, c, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static ShortStream zip(final Collection<? extends ShortIterator> c, final ShortNFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(c, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param combiner
     * @return
     */
    public static ShortStream zip(final ShortStream a, final ShortStream b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, valueForNoneA, valueForNoneB, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param combiner
     * @return
     */
    public static ShortStream zip(final ShortStream a, final ShortStream b, final ShortStream c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param combiner
     * @return
     */
    public static ShortStream zip(final ShortIterator a, final ShortIterator b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, valueForNoneA, valueForNoneB, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param combiner
     * @return
     */
    public static ShortStream zip(final ShortIterator a, final ShortIterator b, final ShortIterator c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, combiner).mapToShort(mapper);
    }

    /**
     * Zip together the iterators until all of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param valuesForNone value to fill for any iterator runs out of values.
     * @param combiner
     * @return
     */
    public static ShortStream zip(final Collection<? extends ShortIterator> c, final short[] valuesForNone, final ShortNFunction<Short> combiner) {
        final ToShortFunction<Short> mapper = new ToShortFunction<Short>() {
            @Override
            public short applyAsShort(Short value) {
                return value.shortValue();
            }
        };

        return Stream.zip(c, valuesForNone, combiner).mapToShort(mapper);
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream merge(final short[] a, final short[] b, final ShortBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return of(b);
        } else if (N.isNullOrEmpty(b)) {
            return of(a);
        }

        return new IteratorShortStream(new ImmutableShortIterator() {
            private final int lenA = a.length;
            private final int lenB = b.length;
            private int cursorA = 0;
            private int cursorB = 0;

            @Override
            public boolean hasNext() {
                return cursorA < lenA || cursorB < lenB;
            }

            @Override
            public short next() {
                if (cursorA < lenA) {
                    if (cursorB < lenB) {
                        if (nextSelector.apply(a[cursorA], b[cursorB]) == Nth.FIRST) {
                            return a[cursorA++];
                        } else {
                            return b[cursorB++];
                        }
                    } else {
                        return a[cursorA++];
                    }
                } else if (cursorB < lenB) {
                    return b[cursorB++];
                } else {
                    throw new NoSuchElementException();
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream merge(final ShortStream a, final ShortStream b, final ShortBiFunction<Nth> nextSelector) {
        final ShortIterator iterA = a.shortIterator();
        final ShortIterator iterB = b.shortIterator();

        if (iterA.hasNext() == false) {
            return b;
        } else if (iterB.hasNext() == false) {
            return a;
        }

        return merge(iterA, iterB, nextSelector).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (ShortStream stream : N.asList(a, b)) {
                    try {
                        stream.close();
                    } catch (Throwable throwable) {
                        if (runtimeException == null) {
                            runtimeException = N.toRuntimeException(throwable);
                        } else {
                            runtimeException.addSuppressed(throwable);
                        }
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream merge(final ShortIterator a, final ShortIterator b, final ShortBiFunction<Nth> nextSelector) {
        if (a.hasNext() == false) {
            return of(b);
        } else if (b.hasNext() == false) {
            return of(a);
        }

        return new IteratorShortStream(new ImmutableShortIterator() {
            private short nextA = 0;
            private short nextB = 0;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || hasNextA || hasNextB;
            }

            @Override
            public short next() {
                if (hasNextA) {
                    if (b.hasNext()) {
                        if (nextSelector.apply(nextA, (nextB = b.next())) == Nth.FIRST) {
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
                    if (a.hasNext()) {
                        if (nextSelector.apply((nextA = a.next()), nextB) == Nth.FIRST) {
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
                } else if (a.hasNext()) {
                    if (b.hasNext()) {
                        if (nextSelector.apply((nextA = a.next()), (nextB = b.next())) == Nth.FIRST) {
                            hasNextB = true;
                            return nextA;
                        } else {
                            hasNextA = true;
                            return nextB;
                        }
                    } else {
                        return a.next();
                    }
                } else if (b.hasNext()) {
                    return b.next();
                } else {
                    throw new NoSuchElementException();
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream merge(final ShortStream[] a, final ShortBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return a[0];
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        final ShortIterator[] iters = new ShortIterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iters[i] = a[i].shortIterator();
        }

        return merge(iters, nextSelector).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (ShortStream stream : a) {
                    try {
                        stream.close();
                    } catch (Throwable throwable) {
                        if (runtimeException == null) {
                            runtimeException = N.toRuntimeException(throwable);
                        } else {
                            runtimeException.addSuppressed(throwable);
                        }
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream merge(final ShortIterator[] a, final ShortBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return of(a[0]);
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        return merge(Arrays.asList(a), nextSelector);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream merge(final Collection<? extends ShortIterator> c, final ShortBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return of(c.iterator().next());
        } else if (c.size() == 2) {
            final Iterator<? extends ShortIterator> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        }

        final Iterator<? extends ShortIterator> iter = c.iterator();
        ShortStream result = merge(iter.next(), iter.next(), nextSelector);

        while (iter.hasNext()) {
            result = merge(result.shortIterator(), iter.next(), nextSelector);
        }

        return result;
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream parallelMerge(final ShortStream[] a, final ShortBiFunction<Nth> nextSelector) {
        return parallelMerge(a, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static ShortStream parallelMerge(final ShortStream[] a, final ShortBiFunction<Nth> nextSelector, final int maxThreadNum) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("maxThreadNum can be less than 1");
        }

        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return a[0];
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        final ShortIterator[] iters = new ShortIterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iters[i] = a[i].shortIterator();
        }

        return parallelMerge(iters, nextSelector, maxThreadNum).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (ShortStream stream : a) {
                    try {
                        stream.close();
                    } catch (Throwable throwable) {
                        if (runtimeException == null) {
                            runtimeException = N.toRuntimeException(throwable);
                        } else {
                            runtimeException.addSuppressed(throwable);
                        }
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream parallelMerge(final ShortIterator[] a, final ShortBiFunction<Nth> nextSelector) {
        return parallelMerge(a, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static ShortStream parallelMerge(final ShortIterator[] a, final ShortBiFunction<Nth> nextSelector, final int maxThreadNum) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("maxThreadNum can be less than 1");
        }

        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return of(a[0]);
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        return parallelMerge(Arrays.asList(a), nextSelector, maxThreadNum);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ShortStream parallelMerge(final Collection<? extends ShortIterator> c, final ShortBiFunction<Nth> nextSelector) {
        return parallelMerge(c, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static ShortStream parallelMerge(final Collection<? extends ShortIterator> c, final ShortBiFunction<Nth> nextSelector, final int maxThreadNum) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("maxThreadNum can be less than 1");
        }

        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return of(c.iterator().next());
        } else if (c.size() == 2) {
            final Iterator<? extends ShortIterator> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        } else if (maxThreadNum <= 1) {
            return merge(c, nextSelector);
        }

        final Queue<ShortIterator> queue = new LinkedList<>();
        queue.addAll(c);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt cnt = MutableInt.of(c.size());
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(c.size() - 1);

        for (int i = 0, n = N.min(maxThreadNum, c.size() / 2 + 1); i < n; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    ShortIterator a = null;
                    ShortIterator b = null;
                    ShortIterator c = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (queue) {
                                if (cnt.intValue() > 2 && queue.size() > 1) {
                                    a = queue.poll();
                                    b = queue.poll();

                                    cnt.decrement();
                                } else {
                                    break;
                                }
                            }

                            c = ImmutableShortIterator.of(merge(a, b, nextSelector).toArray());

                            synchronized (queue) {
                                queue.offer(c);
                            }
                        }
                    } catch (Throwable e) {
                        Stream.setError(eHolder, e);
                    }
                }
            }));
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        // Should never happen.
        if (queue.size() != 2) {
            throw new AbacusException("Unknown error happened.");
        }

        return merge(queue.poll(), queue.poll(), nextSelector);
    }
}
