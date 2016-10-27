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
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
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
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoubleNFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleSupplier;
import com.landawn.abacus.util.function.DoubleToFloatFunction;
import com.landawn.abacus.util.function.DoubleToIntFunction;
import com.landawn.abacus.util.function.DoubleToLongFunction;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * A sequence of primitive double-valued elements supporting sequential and parallel
 * aggregate operations.  This is the {@code double} primitive specialization of
 * {@link Stream}.
 *
 * <p>The following example illustrates an aggregate operation using
 * {@link Stream} and {@link DoubleStream}, computing the sum of the weights of the
 * red widgets:
 *
 * <pre>{@code
 *     double sum = widgets.stream()
 *                         .filter(w -> w.getColor() == RED)
 *                         .mapToDouble(w -> w.getWeight())
 *                         .sum();
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
public abstract class DoubleStream implements BaseStream<Double, DoubleStream> {

    private static final DoubleStream EMPTY = new ArrayDoubleStream(N.EMPTY_DOUBLE_ARRAY);

    /**
     * Returns a stream consisting of the elements of this stream that match
     * the given predicate.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param predicate a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to each element to determine if it
     *                  should be included
     * @return the new stream
     */
    public abstract DoubleStream filter(final DoublePredicate predicate);

    /**
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract DoubleStream filter(final DoublePredicate predicate, final long max);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    public abstract DoubleStream takeWhile(final DoublePredicate predicate);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract DoubleStream takeWhile(final DoublePredicate predicate, final long max);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    public abstract DoubleStream dropWhile(final DoublePredicate predicate);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract DoubleStream dropWhile(final DoublePredicate predicate, final long max);

    /**
     * Returns a stream consisting of the results of applying the given
     * function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract DoubleStream map(DoubleUnaryOperator mapper);

    /**
     * Returns an {@code IntStream} consisting of the results of applying the
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
    public abstract IntStream mapToInt(DoubleToIntFunction mapper);

    /**
     * Returns a {@code LongStream} consisting of the results of applying the
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
    public abstract LongStream mapToLong(DoubleToLongFunction mapper);

    /**
     * Returns a {@code FloatStream} consisting of the results of applying the
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
    public abstract FloatStream mapToFloat(DoubleToFloatFunction mapper);

    /**
     * Returns an object-valued {@code Stream} consisting of the results of
     * applying the given function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">
     *     intermediate operation</a>.
     *
     * @param <U> the element type of the new stream
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract <U> Stream<U> mapToObj(DoubleFunction<? extends U> mapper);

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
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element which produces a
     *               {@code DoubleStream} of new values
     * @return the new stream
     * @see Stream#flatMap(Function)
     */
    public abstract DoubleStream flatMap(DoubleFunction<? extends DoubleStream> mapper);

    public abstract IntStream flatMapToInt(DoubleFunction<? extends IntStream> mapper);

    public abstract LongStream flatMapToLong(DoubleFunction<? extends LongStream> mapper);

    public abstract FloatStream flatMapToFloat(DoubleFunction<? extends FloatStream> mapper);

    public abstract <T> Stream<T> flatMapToObj(DoubleFunction<? extends Stream<T>> mapper);

    /**
     * Returns Stream of DoubleStream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller).
     * 
     * @param size
     * @return
     */
    public abstract Stream<DoubleStream> split(int size);

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
    public abstract Stream<DoubleStream> split(DoublePredicate predicate);

    /**
     * Returns a stream consisting of the distinct elements of this stream. The
     * elements are compared for equality according to
     * {@link java.lang.Double#compare(double, double)}.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @return the result stream
     */
    public abstract DoubleStream distinct();

    public abstract DoubleStream top(int n);

    public abstract DoubleStream top(final int n, Comparator<? super Double> comparator);

    /**
     * Returns a stream consisting of the elements of this stream in sorted
     * order. The elements are compared for equality according to
     * {@link java.lang.Double#compare(double, double)}.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @return the result stream
     */
    public abstract DoubleStream sorted();

    // public abstract DoubleStream parallelSorted();

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
     *     DoubleStream.of(1, 2, 3, 4)
     *         .filter(e -> e > 2)
     *         .peek(e -> System.out.println("Filtered value: " + e))
     *         .map(e -> e * e)
     *         .peek(e -> System.out.println("Mapped value: " + e))
     *         .sum();
     * }</pre>
     *
     * @param action a <a href="package-summary.html#NonInterference">
     *               non-interfering</a> action to perform on the elements as
     *               they are consumed from the stream
     * @return the new stream
     */
    public abstract DoubleStream peek(DoubleConsumer action);

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
    public abstract DoubleStream limit(long maxSize);

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
    public abstract DoubleStream skip(long n);

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
     * @param action a <a href="package-summary.html#NonInterference">
     *               non-interfering</a> action to perform on the elements
     */
    public abstract void forEach(DoubleConsumer action);

    //    /**
    //     * In parallel Streams, the elements after the first element which <code>action</code> returns false may be executed by action too.
    //     * 
    //     * @param action break if the action returns false.
    //     * @return false if it breaks, otherwise true.
    //     */
    //    public abstract boolean forEach2(DoubleFunction<Boolean> action);

    /**
     * Returns an array containing the elements of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an array containing the elements of this stream
     */
    public abstract double[] toArray();

    public abstract DoubleList toDoubleList();

    public abstract List<Double> toList();

    public abstract List<Double> toList(Supplier<? extends List<Double>> supplier);

    public abstract Set<Double> toSet();

    public abstract Set<Double> toSet(Supplier<? extends Set<Double>> supplier);

    public abstract Multiset<Double> toMultiset();

    public abstract Multiset<Double> toMultiset(Supplier<? extends Multiset<Double>> supplier);

    public abstract LongMultiset<Double> toLongMultiset();

    public abstract LongMultiset<Double> toLongMultiset(Supplier<? extends LongMultiset<Double>> supplier);

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public abstract <K> Map<K, List<Double>> toMap(DoubleFunction<? extends K> classifier);

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public abstract <K, M extends Map<K, List<Double>>> M toMap(final DoubleFunction<? extends K> classifier, final Supplier<M> mapFactory);

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public abstract <K, A, D> Map<K, D> toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream);

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public abstract <K, D, A, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public abstract <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper,
            Supplier<M> mapSupplier);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public abstract <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public abstract <K, U> Multimap<K, U, List<U>> toMultimap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using the provided identity value and an
     * <a href="package-summary.html#Associativity">associative</a>
     * accumulation function, and returns the reduced value.  This is equivalent
     * to:
     * <pre>{@code
     *     double result = identity;
     *     for (double element : this stream)
     *         result = accumulator.applyAsDouble(result, element)
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
    
     * <pre>{@code
     *     double sum = numbers.reduce(0, (a, b) -> a+b);
     * }</pre>
     *
     * or more compactly:
     *
     * <pre>{@code
     *     double sum = numbers.reduce(0, Double::sum);
     * }</pre>
     *
     * <p>While this may seem a more roundabout way to perform an aggregation
     * compared to simply mutating a running total in a loop, reduction
     * operations parallelize more gracefully, without needing additional
     * synchronization and with greatly reduced risk of data races.
     *
     * @param identity the identity value for the accumulating function
     * @param op an <a href="package-summary.html#Associativity">associative</a>,
     *           <a href="package-summary.html#NonInterference">non-interfering</a>,
     *           <a href="package-summary.html#Statelessness">stateless</a>
     *           function for combining two values
     * @return the result of the reduction
     * @see #sum()
     * @see #min()
     * @see #max()
     * @see #average()
     */
    public abstract double reduce(double identity, DoubleBinaryOperator op);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using an
     * <a href="package-summary.html#Associativity">associative</a> accumulation
     * function, and returns an {@code OptionalDouble} describing the reduced
     * value, if any. This is equivalent to:
     * <pre>{@code
     *     boolean foundAny = false;
     *     double result = null;
     *     for (double element : this stream) {
     *         if (!foundAny) {
     *             foundAny = true;
     *             result = element;
     *         }
     *         else
     *             result = accumulator.applyAsDouble(result, element);
     *     }
     *     return foundAny ? OptionalDouble.of(result) : OptionalDouble.empty();
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
     *           <a href="package-summary.html#NonInterference">non-interfering</a>,
     *           <a href="package-summary.html#Statelessness">stateless</a>
     *           function for combining two values
     * @return the result of the reduction
     * @see #reduce(double, DoubleBinaryOperator)
     */
    public abstract OptionalDouble reduce(DoubleBinaryOperator op);

    /**
     * Performs a <a href="package-summary.html#MutableReduction">mutable
     * reduction</a> operation on the elements of this stream.  A mutable
     * reduction is one in which the reduced value is a mutable result container,
     * such as an {@code ArrayList}, and elements are incorporated by updating
     * the state of the result rather than by replacing the result.  This
     * produces a result equivalent to:
     * <pre>{@code
     *     R result = supplier.get();
     *     for (double element : this stream)
     *         accumulator.accept(result, element);
     *     return result;
     * }</pre>
     *
     * <p>Like {@link #reduce(double, DoubleBinaryOperator)}, {@code collect}
     * operations can be parallelized without requiring additional
     * synchronization.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @param <R> type of the result
     * @param supplier a function that creates a new result container. For a
     *                 parallel execution, this function may be called
     *                 multiple times and must return a fresh value each time.
     * @param accumulator an <a href="package-summary.html#Associativity">associative</a>,
     *                    <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                    <a href="package-summary.html#Statelessness">stateless</a>
     *                    function for incorporating an additional element into a result
     * @param combiner an <a href="package-summary.html#Associativity">associative</a>,
     *                    <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                    <a href="package-summary.html#Statelessness">stateless</a>
     *                    function for combining two values, which must be
     *                    compatible with the accumulator function
     * @return the result of the reduction
     * @see Stream#collect(Supplier, BiConsumer, BiConsumer)
     */
    public abstract <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator, BiConsumer<R, R> combiner);

    /**
     * 
     * @param supplier
     * @param accumulator
     * @return
     */
    public abstract <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator);

    /**
     * Returns an {@code OptionalDouble} describing the minimum element of this
     * stream, or an empty OptionalDouble if this stream is empty.  The minimum
     * element will be {@code Double.NaN} if any stream element was NaN. Unlike
     * the numerical comparison operators, this method considers negative zero
     * to be strictly smaller than positive zero. This is a special case of a
     * <a href="package-summary.html#Reduction">reduction</a> and is
     * equivalent to:
     * <pre>{@code
     *     return reduce(Double::min);
     * }</pre>
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an {@code OptionalDouble} containing the minimum element of this
     * stream, or an empty optional if the stream is empty
     */
    public abstract OptionalDouble min();

    /**
     * Returns an {@code OptionalDouble} describing the maximum element of this
     * stream, or an empty OptionalDouble if this stream is empty.  The maximum
     * element will be {@code Double.NaN} if any stream element was NaN. Unlike
     * the numerical comparison operators, this method considers negative zero
     * to be strictly smaller than positive zero. This is a
     * special case of a
     * <a href="package-summary.html#Reduction">reduction</a> and is
     * equivalent to:
     * <pre>{@code
     *     return reduce(Double::max);
     * }</pre>
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an {@code OptionalDouble} containing the maximum element of this
     * stream, or an empty optional if the stream is empty
     */
    public abstract OptionalDouble max();

    /**
     * 
     * @param k
     * @return OptionalByte.empty() if there is no element or count less than k, otherwise the kth largest element.
     */
    public abstract OptionalDouble kthLargest(int k);

    /**
     * Returns the sum of elements in this stream.
     *
     * Summation is a special case of a <a
     * href="package-summary.html#Reduction">reduction</a>. If
     * floating-point summation were exact, this method would be
     * equivalent to:
     *
     * <pre>{@code
     *     return reduce(0, Double::sum);
     * }</pre>
     *
     * However, since floating-point summation is not exact, the above
     * code is not necessarily equivalent to the summation computation
     * done by this method.
     *
     * <p>If any stream element is a NaN or the sum is at any point a NaN
     * then the sum will be NaN.
     *
     * The value of a floating-point sum is a function both
     * of the input values as well as the order of addition
     * operations. The order of addition operations of this method is
     * intentionally not defined to allow for implementation
     * flexibility to improve the speed and accuracy of the computed
     * result.
     *
     * In particular, this method may be implemented using compensated
     * summation or other technique to reduce the error bound in the
     * numerical sum compared to a simple summation of {@code double}
     * values.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote Elements sorted by increasing absolute magnitude tend
     * to yield more accurate results.
     *
     * @return the sum of elements in this stream
     */
    public abstract Double sum();

    /**
     * Returns an {@code OptionalDouble} describing the arithmetic
     * mean of elements of this stream, or an empty optional if this
     * stream is empty.
     *
     * If any recorded value is a NaN or the sum is at any point a NaN
     * then the average will be NaN.
     *
     * <p>The average returned can vary depending upon the order in
     * which values are recorded.
     *
     * This method may be implemented using compensated summation or
     * other technique to reduce the error bound in the {@link #sum
     * numerical sum} used to compute the average.
     *
     *  <p>The average is a special case of a <a
     *  href="package-summary.html#Reduction">reduction</a>.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote Elements sorted by increasing absolute magnitude tend
     * to yield more accurate results.
     *
     * @return an {@code OptionalDouble} containing the average element of this
     * stream, or an empty optional if the stream is empty
     */
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

    public abstract Optional<Map<Percentage, Double>> distribution();

    public abstract DoubleSummaryStatistics summarize();

    public abstract Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarize2();

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
     * @param predicate a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to elements of this stream
     * @return {@code true} if any elements of the stream match the provided
     * predicate, otherwise {@code false}
     */
    public abstract boolean anyMatch(DoublePredicate predicate);

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
     * @param predicate a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to elements of this stream
     * @return {@code true} if either all elements of the stream match the
     * provided predicate or the stream is empty, otherwise {@code false}
     */
    public abstract boolean allMatch(DoublePredicate predicate);

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
     * @param predicate a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                  <a href="package-summary.html#Statelessness">stateless</a>
     *                  predicate to apply to elements of this stream
     * @return {@code true} if either no elements of the stream match the
     * provided predicate or the stream is empty, otherwise {@code false}
     */
    public abstract boolean noneMatch(DoublePredicate predicate);

    /**
     * Returns an {@link OptionalDouble} describing the first element of this
     * stream, or an empty {@code OptionalDouble} if the stream is empty.  If
     * the stream has no encounter order, then any element may be returned.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * @return an {@code OptionalDouble} describing the first element of this
     * stream, or an empty {@code OptionalDouble} if the stream is empty
     */
    // public abstract OptionalDouble findFirst();

    public abstract OptionalDouble findFirst(DoublePredicate predicate);

    // public abstract OptionalDouble findLast();

    public abstract OptionalDouble findLast(DoublePredicate predicate);

    /**
     * Returns an {@link OptionalDouble} describing some element of the stream,
     * or an empty {@code OptionalDouble} if the stream is empty.
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
     * @return an {@code OptionalDouble} describing some element of this stream,
     * or an empty {@code OptionalDouble} if the stream is empty
     * @see #findFirst()
     */
    // public abstract OptionalDouble findAny();

    public abstract OptionalDouble findAny(DoublePredicate predicate);

    /**
     * @param c
     * @return
     * @see IntList#except(IntList)
     */
    public abstract DoubleStream except(Collection<?> c);

    /**
     * @param c
     * @return
     * @see IntList#intersect(IntList)
     */
    public abstract DoubleStream intersect(Collection<?> c);

    //    /**
    //     * Skill All the elements in the specified collection.
    //     * 
    //     * @param c
    //     * @return
    //     * @see IntList#intersect(IntList)
    //     */
    //    public abstract DoubleStream exclude(Collection<?> c);

    /**
     * Append the specified stream to the tail of this stream.
     * @param stream
     * @return
     */
    @Override
    public abstract DoubleStream append(DoubleStream stream);

    /**
     * 
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public abstract DoubleStream merge(final DoubleStream b, final DoubleBiFunction<Nth> nextSelector);

    /**
     * Returns a {@code Stream} consisting of the elements of this stream,
     * boxed to {@code Double}.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @return a {@code Stream} consistent of the elements of this stream,
     * each boxed to a {@code Double}
     */
    public abstract Stream<Double> boxed();

    @Override
    public abstract ImmutableIterator<Double> iterator();

    public abstract ImmutableDoubleIterator doubleIterator();

    // Static factories

    public static DoubleStream empty() {
        return EMPTY;
    }

    public static DoubleStream of(final double... a) {
        return N.isNullOrEmpty(a) ? empty() : new ArrayDoubleStream(a);
    }

    public static DoubleStream of(final double[] a, final int startIndex, final int endIndex) {
        return N.isNullOrEmpty(a) && (startIndex == 0 && endIndex == 0) ? empty() : new ArrayDoubleStream(a, startIndex, endIndex);
    }

    public static DoubleStream of(final DoubleIterator iterator) {
        return iterator == null ? empty()
                : new IteratorDoubleStream(iterator instanceof ImmutableDoubleIterator ? (ImmutableDoubleIterator) iterator : new ImmutableDoubleIterator() {
                    @Override
                    public boolean hasNext() {
                        return iterator.hasNext();
                    }

                    @Override
                    public double next() {
                        return iterator.next();
                    }
                });
    }

    public static DoubleStream repeat(double element, int n) {
        return of(Array.repeat(element, n));
    }

    public static DoubleStream random() {
        return iterate(new DoubleSupplier() {
            @Override
            public double getAsDouble() {
                return Stream.RAND.nextDouble();
            }
        });
    }

    public static DoubleStream iterate(final Supplier<Boolean> hasNext, final DoubleSupplier next) {
        N.requireNonNull(hasNext);
        N.requireNonNull(next);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.get().booleanValue();
                }

                return hasNextVal;
            }

            @Override
            public double next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return next.getAsDouble();
            }
        });
    }

    public static DoubleStream iterate(final double seed, final Supplier<Boolean> hasNext, final DoubleUnaryOperator f) {
        N.requireNonNull(hasNext);
        N.requireNonNull(f);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private double t = 0;
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
            public double next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;

                if (isFirst) {
                    isFirst = false;
                    t = seed;
                } else {
                    t = f.applyAsDouble(t);
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
    public static DoubleStream iterate(final double seed, final DoublePredicate hasNext, final DoubleUnaryOperator f) {
        N.requireNonNull(hasNext);
        N.requireNonNull(f);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private double t = 0;
            private double cur = 0;
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
                        hasNextVal = hasNext.test(cur = f.applyAsDouble(t));
                    }

                    if (hasNextVal == false) {
                        noMoreVal = true;
                    }
                }

                return hasNextVal;
            }

            @Override
            public double next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                t = cur;
                hasNextVal = false;
                return t;
            }
        });
    }

    public static DoubleStream iterate(final double seed, final DoubleUnaryOperator f) {
        N.requireNonNull(f);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private double t = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public double next() {
                if (isFirst) {
                    isFirst = false;
                    t = seed;
                } else {
                    t = f.applyAsDouble(t);
                }

                return t;
            }
        });
    }

    public static DoubleStream iterate(final DoubleSupplier s) {
        N.requireNonNull(s);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public double next() {
                return s.getAsDouble();
            }
        });
    }

    public static DoubleStream concat(final double[]... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private final Iterator<double[]> iter = N.asList(a).iterator();
            private ImmutableDoubleIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = new ImmutableDoubleIterator() {
                        private final double[] cur = iter.next();
                        private int cursor = 0;

                        @Override
                        public boolean hasNext() {
                            return cursor < cur.length;
                        }

                        @Override
                        public double next() {
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
            public double next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        });
    }

    public static DoubleStream concat(final DoubleStream... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private final Iterator<DoubleStream> iter = N.asList(a).iterator();
            private ImmutableDoubleIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next().doubleIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (DoubleStream stream : a) {
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

    public static DoubleStream concat(final DoubleIterator... a) {
        return N.isNullOrEmpty(a) ? empty() : concat(N.asList(a));
    }

    public static DoubleStream concat(final Collection<? extends DoubleIterator> c) {
        return N.isNullOrEmpty(c) ? empty() : new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private final Iterator<? extends DoubleIterator> iter = c.iterator();
            private DoubleIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double next() {
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
    public static DoubleStream zip(final double[] a, final double[] b, final DoubleBiFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, combiner).mapToDouble(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final double[] a, final double[] b, final double[] c, final DoubleTriFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, c, combiner).mapToDouble(mapper);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final DoubleBiFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, combiner).mapToDouble(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final DoubleStream c, final DoubleTriFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, c, combiner).mapToDouble(mapper);
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final DoubleBiFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, combiner).mapToDouble(mapper);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final DoubleTriFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, c, combiner).mapToDouble(mapper);
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static DoubleStream zip(final Collection<? extends DoubleIterator> c, final DoubleNFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(c, combiner).mapToDouble(mapper);
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
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, valueForNoneA, valueForNoneB, combiner).mapToDouble(mapper);
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
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final DoubleStream c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, combiner).mapToDouble(mapper);
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
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, valueForNoneA, valueForNoneB, combiner).mapToDouble(mapper);
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
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final DoubleTriFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, combiner).mapToDouble(mapper);
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
    public static DoubleStream zip(final Collection<? extends DoubleIterator> c, final double[] valuesForNone, final DoubleNFunction<Double> combiner) {
        final ToDoubleFunction<Double> mapper = new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return value.doubleValue();
            }
        };

        return Stream.zip(c, valuesForNone, combiner).mapToDouble(mapper);
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream merge(final double[] a, final double[] b, final DoubleBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return of(b);
        } else if (N.isNullOrEmpty(b)) {
            return of(a);
        }

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private final int lenA = a.length;
            private final int lenB = b.length;
            private int cursorA = 0;
            private int cursorB = 0;

            @Override
            public boolean hasNext() {
                return cursorA < lenA || cursorB < lenB;
            }

            @Override
            public double next() {
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
    public static DoubleStream merge(final DoubleStream a, final DoubleStream b, final DoubleBiFunction<Nth> nextSelector) {
        final DoubleIterator iterA = a.doubleIterator();
        final DoubleIterator iterB = b.doubleIterator();

        if (iterA.hasNext() == false) {
            return b;
        } else if (iterB.hasNext() == false) {
            return a;
        }

        return merge(iterA, iterB, nextSelector).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (DoubleStream stream : N.asList(a, b)) {
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
    public static DoubleStream merge(final DoubleIterator a, final DoubleIterator b, final DoubleBiFunction<Nth> nextSelector) {
        if (a.hasNext() == false) {
            return of(b);
        } else if (b.hasNext() == false) {
            return of(a);
        }

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private double nextA = 0;
            private double nextB = 0;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || hasNextA || hasNextB;
            }

            @Override
            public double next() {
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
    public static DoubleStream merge(final DoubleStream[] a, final DoubleBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return a[0];
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        final DoubleIterator[] iters = new DoubleIterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iters[i] = a[i].doubleIterator();
        }

        return merge(iters, nextSelector).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (DoubleStream stream : a) {
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
    public static DoubleStream merge(final DoubleIterator[] a, final DoubleBiFunction<Nth> nextSelector) {
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
    public static DoubleStream merge(final Collection<? extends DoubleIterator> c, final DoubleBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return of(c.iterator().next());
        } else if (c.size() == 2) {
            final Iterator<? extends DoubleIterator> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        }

        final Iterator<? extends DoubleIterator> iter = c.iterator();
        DoubleStream result = merge(iter.next(), iter.next(), nextSelector);

        while (iter.hasNext()) {
            result = merge(result.doubleIterator(), iter.next(), nextSelector);
        }

        return result;
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream parallelMerge(final DoubleStream[] a, final DoubleBiFunction<Nth> nextSelector) {
        return parallelMerge(a, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static DoubleStream parallelMerge(final DoubleStream[] a, final DoubleBiFunction<Nth> nextSelector, final int maxThreadNum) {
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

        final DoubleIterator[] iters = new DoubleIterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iters[i] = a[i].doubleIterator();
        }

        return parallelMerge(iters, nextSelector, maxThreadNum).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (DoubleStream stream : a) {
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
    public static DoubleStream parallelMerge(final DoubleIterator[] a, final DoubleBiFunction<Nth> nextSelector) {
        return parallelMerge(a, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static DoubleStream parallelMerge(final DoubleIterator[] a, final DoubleBiFunction<Nth> nextSelector, final int maxThreadNum) {
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
    public static DoubleStream parallelMerge(final Collection<? extends DoubleIterator> c, final DoubleBiFunction<Nth> nextSelector) {
        return parallelMerge(c, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static DoubleStream parallelMerge(final Collection<? extends DoubleIterator> c, final DoubleBiFunction<Nth> nextSelector, final int maxThreadNum) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("maxThreadNum can be less than 1");
        }

        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return of(c.iterator().next());
        } else if (c.size() == 2) {
            final Iterator<? extends DoubleIterator> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        } else if (maxThreadNum <= 1) {
            return merge(c, nextSelector);
        }

        final Queue<DoubleIterator> queue = new LinkedList<>();
        queue.addAll(c);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt cnt = MutableInt.of(c.size());
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(c.size() - 1);

        for (int i = 0, n = N.min(maxThreadNum, c.size() / 2 + 1); i < n; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    DoubleIterator a = null;
                    DoubleIterator b = null;
                    DoubleIterator c = null;

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

                            c = ImmutableDoubleIterator.of(merge(a, b, nextSelector).toArray());

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
