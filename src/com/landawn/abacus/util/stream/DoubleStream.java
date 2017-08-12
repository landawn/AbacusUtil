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
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.PrimitiveIterator;
import java.util.Queue;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleMatrix;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedDouble;
import com.landawn.abacus.util.Multimap;
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
import com.landawn.abacus.util.function.DoubleBiPredicate;
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
public abstract class DoubleStream
        extends StreamBase<Double, double[], DoublePredicate, DoubleConsumer, DoubleList, OptionalDouble, IndexedDouble, DoubleStream> {

    private static final DoubleStream EMPTY = new ArrayDoubleStream(N.EMPTY_DOUBLE_ARRAY, null, true);

    DoubleStream(final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted, null);
    }

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
     * {@link java.util.stream.Baseclose() closed} after its contents
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
     * Merge series of adjacent elements which satisfy the given predicate using
     * the merger function and return a new stream.
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    public abstract DoubleStream collapse(final DoubleBiPredicate collapsible, final DoubleBiFunction<Double> mergeFunction);

    /**
     * Returns a {@code Stream} produced by iterative application of a accumulation function
     * to an initial element {@code identity} and next element of the current stream.
     * Produces a {@code Stream} consisting of {@code identity}, {@code acc(identity, value1)},
     * {@code acc(acc(identity, value1), value2)}, etc.
     *
     * <p>This is an intermediate operation.
     *
     * <p>Example:
     * <pre>
     * identity: 0
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [0, 1, 3, 6, 10, 15]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    public abstract DoubleStream scan(final DoubleBiFunction<Double> accumulator);

    /**
     * Returns a {@code Stream} produced by iterative application of a accumulation function
     * to an initial element {@code identity} and next element of the current stream.
     * Produces a {@code Stream} consisting of {@code identity}, {@code acc(identity, value1)},
     * {@code acc(acc(identity, value1), value2)}, etc.
     *
     * <p>This is an intermediate operation.
     *
     * <p>Example:
     * <pre>
     * seed:10
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [11, 13, 16, 20, 25]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param seed the initial value. it's only used once by <code>accumulator</code> to calculate the fist element in the returned stream. 
     * It will be ignored if this stream is empty and won't be the first element of the returned stream.
     * 
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    public abstract DoubleStream scan(final double seed, final DoubleBiFunction<Double> accumulator);

    /**
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param n
     * @return
     */
    public abstract DoubleStream top(int n);

    /**
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param n
     * @param comparator
     * @return
     */
    public abstract DoubleStream top(final int n, Comparator<? super Double> comparator);

    public abstract DoubleList toDoubleList();

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public abstract <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper,
            Supplier<M> mapFactory);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public abstract <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory);

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
    public abstract <K, A, D, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory);

    /**
     * 
     * @param keyExtractor
     * @return
     * @see Collectors#toMultimap(Function)
     */
    public abstract <K> Multimap<K, Double, List<Double>> toMultimap(DoubleFunction<? extends K> keyExtractor);

    /**
     * 
     * @param keyExtractor
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Supplier)
     */
    public abstract <K, V extends Collection<Double>> Multimap<K, Double, V> toMultimap(DoubleFunction<? extends K> keyExtractor,
            Supplier<Multimap<K, Double, V>> mapFactory);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public abstract <K, U> Multimap<K, U, List<U>> toMultimap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(DoubleFunction<? extends K> keyExtractor,
            DoubleFunction<? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapFactory);

    public abstract DoubleMatrix toMatrix();

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
     * Head and tail should be used by pair. If only one is called, should use first() or skip(1) instead.
     * Don't call any other methods with this stream after head() and tail() are called. 
     * 
     * @return
     */
    public abstract OptionalDouble head();

    /**
     * Head and tail should be used by pair. If only one is called, should use first() or skip(1) instead.
     * Don't call any other methods with this stream after head() and tail() are called. 
     * 
     * @return
     */
    public abstract DoubleStream tail();

    /**
     * Head2 and tail2 should be used by pair. 
     * Don't call any other methods with this stream after head2() and tail2() are called.
     * 
     * @return
     */
    public abstract DoubleStream head2();

    /**
     * Head2 and tail2 should be used by pair. 
     * Don't call any other methods with this stream after head2() and tail2() are called. 
     * 
     * @return
     */
    public abstract OptionalDouble tail2();

    public abstract Pair<OptionalDouble, DoubleStream> headAndTail();

    public abstract Pair<DoubleStream, OptionalDouble> headAndTail2();

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
    public abstract double sum();

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

    public abstract DoubleSummaryStatistics summarize();

    public abstract Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarize2();

    /**
     * 
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public abstract DoubleStream merge(final DoubleStream b, final DoubleBiFunction<Nth> nextSelector);

    public abstract DoubleStream zipWith(DoubleStream b, DoubleBiFunction<Double> zipFunction);

    public abstract DoubleStream zipWith(DoubleStream b, DoubleStream c, DoubleTriFunction<Double> zipFunction);

    public abstract DoubleStream zipWith(DoubleStream b, double valueForNoneA, double valueForNoneB, DoubleBiFunction<Double> zipFunction);

    public abstract DoubleStream zipWith(DoubleStream b, DoubleStream c, double valueForNoneA, double valueForNoneB, double valueForNoneC,
            DoubleTriFunction<Double> zipFunction);

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
    public DoubleIterator iterator() {
        return exIterator();
    }

    abstract ExDoubleIterator exIterator();

    public <SS> SS __(Function<? super DoubleStream, SS> transfer) {
        return transfer.apply(this);
    }

    public static DoubleStream empty() {
        return EMPTY;
    }

    @SafeVarargs
    public static DoubleStream of(final double... a) {
        return N.isNullOrEmpty(a) ? empty() : new ArrayDoubleStream(a);
    }

    public static DoubleStream of(final double[] a, final int startIndex, final int endIndex) {
        return N.isNullOrEmpty(a) && (startIndex == 0 && endIndex == 0) ? empty() : new ArrayDoubleStream(a, startIndex, endIndex);
    }

    public static DoubleStream of(final double[][] a) {
        return N.isNullOrEmpty(a) ? empty() : Stream.of(a).flatMapToDouble(new Function<double[], DoubleStream>() {
            @Override
            public DoubleStream apply(double[] t) {
                return DoubleStream.of(t);
            }
        });
    }

    public static DoubleStream of(final double[][][] a) {
        return N.isNullOrEmpty(a) ? empty() : Stream.of(a).flatMapToDouble(new Function<double[][], DoubleStream>() {
            @Override
            public DoubleStream apply(double[][] t) {
                return DoubleStream.of(t);
            }
        });
    }

    public static DoubleStream of(final DoubleIterator iterator) {
        return iterator == null ? empty() : new IteratorDoubleStream(iterator);
    }

    public static DoubleStream of(final java.util.stream.DoubleStream stream) {
        return of(new ExDoubleIterator() {
            private PrimitiveIterator.OfDouble iter = null;

            @Override
            public boolean hasNext() {
                if (iter == null) {
                    iter = stream.iterator();
                }

                return iter.hasNext();
            }

            @Override
            public double nextDouble() {
                if (iter == null) {
                    iter = stream.iterator();
                }

                return iter.nextDouble();
            }

            @Override
            public long count() {
                final long result = stream.count();
                iter = null;
                return result;
            }

            @Override
            public void skip(long n) {
                stream.skip(n);
                iter = null;
            }

            @Override
            public double[] toArray() {
                final double[] result = stream.toArray();
                iter = null;
                return result;
            }
        });
    }

    @SafeVarargs
    public static DoubleStream from(final float... a) {
        return N.isNullOrEmpty(a) ? empty() : from(a, 0, a.length);
    }

    public static DoubleStream from(final float[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return empty();
        }

        return new IteratorDoubleStream(new ExDoubleIterator() {
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

                return a[cursor++];
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
            public double[] toArray() {
                final double[] result = new double[toIndex - cursor];

                for (int i = cursor; i < toIndex; i++) {
                    result[i - cursor] = a[i];
                }

                return result;
            }
        });
    }

    public static DoubleStream repeat(final double element, final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative: " + n);
        } else if (n == 0) {
            return empty();
        }

        return new IteratorDoubleStream(new ExDoubleIterator() {
            private long cnt = n;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public double nextDouble() {
                if (cnt-- <= 0) {
                    throw new NoSuchElementException();
                }

                return element;
            }

            @Override
            public void skip(long n) {
                cnt = n >= cnt ? 0 : cnt - (int) n;
            }

            @Override
            public long count() {
                return cnt;
            }

            @Override
            public double[] toArray() {
                final double[] result = new double[(int) cnt];

                for (int i = 0; i < cnt; i++) {
                    result[i] = element;
                }

                cnt = 0;

                return result;
            }
        });
    }

    public static DoubleStream random() {
        return generate(new DoubleSupplier() {
            @Override
            public double getAsDouble() {
                return RAND.nextDouble();
            }
        });
    }

    public static DoubleStream iterate(final Supplier<Boolean> hasNext, final DoubleSupplier next) {
        N.requireNonNull(hasNext);
        N.requireNonNull(next);

        return new IteratorDoubleStream(new ExDoubleIterator() {
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.get().booleanValue();
                }

                return hasNextVal;
            }

            @Override
            public double nextDouble() {
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

        return new IteratorDoubleStream(new ExDoubleIterator() {
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
            public double nextDouble() {
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

        return new IteratorDoubleStream(new ExDoubleIterator() {
            private double t = 0;
            private double cur = 0;
            private boolean isFirst = true;
            private boolean hasMore = true;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false && hasMore) {
                    if (isFirst) {
                        isFirst = false;
                        hasNextVal = hasNext.test(cur = seed);
                    } else {
                        hasNextVal = hasNext.test(cur = f.applyAsDouble(t));
                    }

                    if (hasNextVal == false) {
                        hasMore = false;
                    }
                }

                return hasNextVal;
            }

            @Override
            public double nextDouble() {
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

        return new IteratorDoubleStream(new ExDoubleIterator() {
            private double t = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public double nextDouble() {
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

    public static DoubleStream generate(final DoubleSupplier s) {
        N.requireNonNull(s);

        return new IteratorDoubleStream(new ExDoubleIterator() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public double nextDouble() {
                return s.getAsDouble();
            }
        });
    }

    @SafeVarargs
    public static DoubleStream concat(final double[]... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorDoubleStream(new ExDoubleIterator() {
            private final Iterator<double[]> iter = N.asList(a).iterator();
            private DoubleIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = ExDoubleIterator.of(iter.next());
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
        });
    }

    @SafeVarargs
    public static DoubleStream concat(final DoubleIterator... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorDoubleStream(new ExDoubleIterator() {
            private final Iterator<? extends DoubleIterator> iter = N.asList(a).iterator();
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
        });
    }

    @SafeVarargs
    public static DoubleStream concat(final DoubleStream... a) {
        return N.isNullOrEmpty(a) ? empty() : concat(N.asList(a));
    }

    public static DoubleStream concat(final Collection<? extends DoubleStream> c) {
        return N.isNullOrEmpty(c) ? empty() : new IteratorDoubleStream(new ExDoubleIterator() {
            private final Iterator<? extends DoubleStream> iter = c.iterator();
            private DoubleIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next().exIterator();
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
        }).onClose(newCloseHandler(c));
    }

    /**
     * Zip together the "a" and "b" arrays until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final double[] a, final double[] b, final DoubleBiFunction<Double> zipFunction) {
        return Stream.zip(a, b, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final double[] a, final double[] b, final double[] c, final DoubleTriFunction<Double> zipFunction) {
        return Stream.zip(a, b, c, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final DoubleBiFunction<Double> zipFunction) {
        return Stream.zip(a, b, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final DoubleTriFunction<Double> zipFunction) {
        return Stream.zip(a, b, c, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final DoubleBiFunction<Double> zipFunction) {
        return Stream.zip(a, b, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final DoubleStream c, final DoubleTriFunction<Double> zipFunction) {
        return Stream.zip(a, b, c, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param c
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final Collection<? extends DoubleStream> c, final DoubleNFunction<Double> zipFunction) {
        return Stream.zip(c, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        return Stream.zip(a, b, valueForNoneA, valueForNoneB, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        return Stream.zip(a, b, valueForNoneA, valueForNoneB, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        return Stream.zip(a, b, valueForNoneA, valueForNoneB, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final DoubleStream a, final DoubleStream b, final DoubleStream c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
    }

    /**
     * Zip together the iterators until all of them runs out of values.
     * Each array of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param c
     * @param valuesForNone value to fill for any iterator runs out of values.
     * @param zipFunction
     * @return
     */
    public static DoubleStream zip(final Collection<? extends DoubleStream> c, final double[] valuesForNone, final DoubleNFunction<Double> zipFunction) {
        return Stream.zip(c, valuesForNone, zipFunction).mapToDouble(ToDoubleFunction.UNBOX);
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

        return new IteratorDoubleStream(new ExDoubleIterator() {
            private final int lenA = a.length;
            private final int lenB = b.length;
            private int cursorA = 0;
            private int cursorB = 0;

            @Override
            public boolean hasNext() {
                return cursorA < lenA || cursorB < lenB;
            }

            @Override
            public double nextDouble() {
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
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream merge(final double[] a, final double[] b, final double[] c, final DoubleBiFunction<Nth> nextSelector) {
        return merge(merge(a, b, nextSelector).exIterator(), DoubleStream.of(c).exIterator(), nextSelector);
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

        return new IteratorDoubleStream(new ExDoubleIterator() {
            private double nextA = 0;
            private double nextB = 0;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || hasNextA || hasNextB;
            }

            @Override
            public double nextDouble() {
                if (hasNextA) {
                    if (b.hasNext()) {
                        if (nextSelector.apply(nextA, (nextB = b.nextDouble())) == Nth.FIRST) {
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
                        if (nextSelector.apply((nextA = a.nextDouble()), nextB) == Nth.FIRST) {
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
                        if (nextSelector.apply((nextA = a.nextDouble()), (nextB = b.nextDouble())) == Nth.FIRST) {
                            hasNextB = true;
                            return nextA;
                        } else {
                            hasNextA = true;
                            return nextB;
                        }
                    } else {
                        return a.nextDouble();
                    }
                } else if (b.hasNext()) {
                    return b.nextDouble();
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
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream merge(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final DoubleBiFunction<Nth> nextSelector) {
        return merge(merge(a, b, nextSelector).exIterator(), c, nextSelector);
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream merge(final DoubleStream a, final DoubleStream b, final DoubleBiFunction<Nth> nextSelector) {
        return merge(a.exIterator(), b.exIterator(), nextSelector).onClose(newCloseHandler(N.asList(a, b)));
    }

    /**
     * 
     * @param a
     * @param b
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream merge(final DoubleStream a, final DoubleStream b, final DoubleStream c, final DoubleBiFunction<Nth> nextSelector) {
        return merge(N.asList(a, b, c), nextSelector);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream merge(final Collection<? extends DoubleStream> c, final DoubleBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return c.iterator().next();
        } else if (c.size() == 2) {
            final Iterator<? extends DoubleStream> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        }

        final Iterator<? extends DoubleStream> iter = c.iterator();
        DoubleStream result = merge(iter.next().exIterator(), iter.next().exIterator(), nextSelector);

        while (iter.hasNext()) {
            result = merge(result.exIterator(), iter.next().exIterator(), nextSelector);
        }

        return result.onClose(newCloseHandler(c));
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static DoubleStream parallelMerge(final Collection<? extends DoubleStream> c, final DoubleBiFunction<Nth> nextSelector) {
        return parallelMerge(c, nextSelector, DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static DoubleStream parallelMerge(final Collection<? extends DoubleStream> c, final DoubleBiFunction<Nth> nextSelector, final int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return c.iterator().next();
        } else if (c.size() == 2) {
            final Iterator<? extends DoubleStream> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        } else if (maxThreadNum <= 1) {
            return merge(c, nextSelector);
        }

        final Queue<DoubleIterator> queue = N.newLinkedList();

        for (DoubleStream e : c) {
            queue.add(e.exIterator());
        }

        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt cnt = MutableInt.of(c.size());
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(c.size() - 1);

        for (int i = 0, n = N.min(maxThreadNum, c.size() / 2 + 1); i < n; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
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

                            c = ExDoubleIterator.of(merge(a, b, nextSelector).toArray());

                            synchronized (queue) {
                                queue.offer(c);
                            }
                        }
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }
                }
            }));
        }

        complete(futureList, eHolder);

        // Should never happen.
        if (queue.size() != 2) {
            throw new AbacusException("Unknown error happened.");
        }

        return merge(queue.poll(), queue.poll(), nextSelector).onClose(newCloseHandler(c));
    }

    public static abstract class DoubleStreamEx extends DoubleStream {
        private DoubleStreamEx(Collection<Runnable> closeHandlers, boolean sorted) {
            super(closeHandlers, sorted);
            // Factory class.
        }
    }
}
