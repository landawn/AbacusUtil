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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * Base interface for streams, which are sequences of elements supporting
 * sequential and parallel aggregate operations.  The following example
 * illustrates an aggregate operation using the stream types {@link Stream}
 * and {@link IntStream}, computing the sum of the weights of the red widgets:
 *
 * <pre>{@code
 *     int sum = widgets.stream()
 *                      .filter(w -> w.getColor() == RED)
 *                      .mapToInt(w -> w.getWeight())
 *                      .sum();
 * }</pre>
 *
 * See the class documentation for {@link Stream} and the package documentation
 * for <a href="package-summary.html">java.util.stream</a> for additional
 * specification of streams, stream operations, stream pipelines, and
 * parallelism, which governs the behavior of all stream types.
 *
 * @param <T> the type of the stream elements
 * @param <A> the type of array
 * @param <P> the type of predicate
 * @param <C> the type of consumer
 * @param <PL> the type of PrimitiveList/ObjectList
 * @param <OT> the type of Optional
 * @param <IT> the type of Indexed
 * @param <S> the type of of the stream implementing {@code BaseStream}
 * @since 1.8
 * @see Stream
 * @see IntStream
 * @see LongStream
 * @see DoubleStream
 * @see <a href="package-summary.html">java.util.stream</a>
 */
public interface BaseStream<T, A, P, C, PL, OT, IT, S extends BaseStream<T, A, P, C, PL, OT, IT, S>> extends AutoCloseable {
    // public static final int MAX_THREAD_POOL_SIZE = 8192;
    public static final int MAX_THREAD_POOL_SIZE = Integer.MAX_VALUE;
    public static final int MAX_THREAD_NUM_PER_OPERATION = 1024;

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
    S filter(P predicate);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    S takeWhile(P predicate);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    S dropWhile(P predicate);

    /**
     * Take away and consume the specified <code>n</code> elements.
     * 
     * @param n
     * @param action
     * @return
     * @see #dropWhile(C)
     */
    S drop(long n, C action);

    /**
     * Take away and consume elements while <code>predicate</code> returns true.
     * 
     * @param predicate
     * @param action
     * @return
     * @see  #dropWhile(C)
     */
    S dropWhile(P predicate, C action);

    /**
     * Returns Stream of ByteStream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller).
     * 
     * @param size
     * @return
     */
    Stream<S> split(int size);

    /**
     * Returns Stream of Stream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller).
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param size
     * @return
     */
    public abstract Stream<PL> split0(int size);

    /**
     * Split the stream by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * Stream.of(1, 2, 3, 5, 7, 9, 10, 11, 19).split2(MutableInt.of(5), (e, b) -> e <= b.intValue(), b -> b.addAndGet(5)).forEach(N::println);
     * </code>
     * </pre>
     * 
     * This stream should be sorted by value which is used to verify the border.
     * This method only run sequentially, even in parallel stream.
     * 
     * @param identity
     * @param predicate
     * @param identityUpdate
     * @return
     */
    <U> Stream<S> split(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate);

    /**
     * Split the stream by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * Stream.of(1, 2, 3, 5, 7, 9, 10, 11, 19).split2(MutableInt.of(5), (e, b) -> e <= b.intValue(), b -> b.addAndGet(5)).forEach(N::println);
     * </code>
     * </pre>
     * 
     * This stream should be sorted by value which is used to verify the border.
     * This method only run sequentially, even in parallel stream.
     * 
     * @param identity
     * @param predicate
     * @param identityUpdate
     * @return
     */
    <U> Stream<PL> split0(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate);

    /**
     * Split the stream into two pieces at <code>where</code>
     * 
     * @param where
     * @return
     */
    Stream<S> splitAt(int where);

    /**
     * Split the stream into two pieces at <code>where</code>
     * 
     * @param where
     * @return
     */
    Stream<S> splitBy(P where);

    /**
     * 
     * @param windowSize
     * @return
     * @see #sliding(int, int)
     */
    Stream<S> sliding(int windowSize);

    /**
     * 
     * @param windowSize
     * @return
     * @see #sliding(int, int)
     */
    Stream<PL> sliding0(int windowSize);

    /**
     * <code>Stream.of(1, 2, 3, 4, 5, 6, 7, 8).sliding(3, 1).forEach(Stream::println)</code>
     * <br /> output: <br />
     * [1, 2, 3] <br />
     * [2, 3, 4] <br />
     * [3, 4, 5] <br />
     * [4, 5, 6] <br />
     * [5, 6, 7] <br />
     * [6, 7, 8] <br />
     * 
     * <br>============================================================================</br>
     * <code>Stream.of(1, 2, 3, 4, 5, 6, 7, 8).sliding(3, 3).forEach(Stream::println)</code>
     * <br /> output: <br />
     * [1, 2, 3] <br />
     * [4, 5, 6] <br />
     * [7, 8] <br />
     * 
     * <br>============================================================================</br>
     * <code>Stream.of(1, 2, 3, 4, 5, 6, 7, 5).sliding(3, 5).forEach(Stream::println)</code>
     * <br /> output: <br />
     * [1, 2, 3] <br />
     * [6, 7, 8] <br />
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param windowSize
     * @param increment
     * @return
     */
    Stream<S> sliding(int windowSize, int increment);

    /**
     * 
     * @param windowSize
     * @param increment
     * @return
     * @see #sliding(int, int)
     */
    Stream<PL> sliding0(int windowSize, int increment);

    /**
     * @param c
     * @return
     * @see IntList#except(IntList)
     */
    S except(Collection<?> c);

    /**
     * @param c
     * @return
     * @see IntList#intersect(IntList)
     */
    S intersect(Collection<?> c);

    /**
     * @param c
     * @return
     * @see IntList#xor(IntList)
     */
    S xor(Collection<T> c);

    /**
     * 
     * @return
     */
    Optional<Map<Percentage, T>> distribution();

    /**
     * 
     * <br />
     * This method only run sequentially, even in parallel stream and all elements will be loaded to memory.
     * 
     * @return
     */
    S reverse();

    /**
     * 
     * <br />
     * This method only run sequentially, even in parallel stream and all elements will be loaded to memory.
     * 
     * @return
     */
    S shuffle();

    /**
     * 
     * <br />
     * This method only run sequentially, even in parallel stream and all elements will be loaded to memory.
     * 
     * @return
     */
    S rotate(int distance);

    /**
     * Returns a stream consisting of the distinct elements of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @return the new stream
     */
    S distinct();

    /**
     * Returns a stream consisting of the elements of this stream in sorted
     * order.
     *
     * <br />
     * All elements will be loaded to memory.
     *
     * @return the new stream
     */
    S sorted();

    S append(S s);

    S prepend(S stream);

    /**
     * <br />
     * This method only run sequentially, even in parallel stream and all elements will be loaded to memory.
     * 
     * @return
     */
    S cached();

    /**
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @return
     */
    Stream<IT> indexed();

    String join(CharSequence delimiter);

    String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix);

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
    S skip(long n);

    /**
     * Returns a stream consisting of the elements of this stream, truncated
     * to be no longer than {@code maxSize} in length.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">byte-circuiting
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
    S limit(long maxSize);

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
    long count();

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
     *     Stream.of("one", "two", "three", "four")
     *         .filter(e -> e.length() > 3)
     *         .peek(e -> System.out.println("Filtered value: " + e))
     *         .map(String::toUpperCase)
     *         .peek(e -> System.out.println("Mapped value: " + e))
     *         .collect(Collectors.toList());
     * }</pre>
     *
     * @param action a <a href="package-summary.html#NonInterference">
     *                 non-interfering</a> action to perform on the elements as
     *                 they are consumed from the stream
     * @return this stream or a new stream with same elements.
     */
    S peek(C action);

    /**
     * Performs an action for each element of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * <p>The behavior of this operation is explicitly nondeterministic.
     * For parallel stream pipelines, this operation does <em>not</em>
     * guarantee to respect the encounter order of the stream, as doing so
     * would sacrifice the benefit of parallelism.  For any given element, the
     * action may be performed at whatever time and in whatever thread the
     * library chooses.  If the action accesses shared state, it is
     * responsible for providing the required synchronization.
     *
     * @param action a <a href="package-summary.html#NonInterference">
     *               non-interfering</a> action to perform on the elements
     */
    void forEach(C action);

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
    boolean anyMatch(P predicate);

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
    boolean allMatch(P predicate);

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
    boolean noneMatch(P predicate);

    /**
     * Returns an {@link Optional} describing the first element of this stream,
     * or an empty {@code Optional} if the stream is empty.  If the stream has
     * no encounter order, then any element may be returned.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">short-circuiting
     * terminal operation</a>.
     *
     * @return an {@code Optional} describing the first element of this stream,
     * or an empty {@code Optional} if the stream is empty
     */
    OT findFirst(P predicate);

    /**
     * Sometimes, <code>stream.reverse().findFirst(predicate)</code> has better performance than <code>stream.findLast(predicate)</code>.
     * 
     * @param predicate
     * @return
     */
    OT findLast(P predicate);

    /**
     * Returns an {@link Optional} describing some element of the stream, or an
     * empty {@code Optional} if the stream is empty.
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
     * @return an {@code Optional} describing some element of this stream, or an
     * empty {@code Optional} if the stream is empty
     * @see #findFirst()
     */
    OT findAny(P predicate);

    OT first();

    OT last();

    /**
     * Returns an array containing the elements of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an array containing the elements of this stream
     */
    A toArray();

    List<T> toList();

    List<T> toList(Supplier<? extends List<T>> supplier);

    Set<T> toSet();

    Set<T> toSet(Supplier<? extends Set<T>> supplier);

    Multiset<T> toMultiset();

    Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier);

    LongMultiset<T> toLongMultiset();

    LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier);

    /**
     * Returns an iterator for the elements of this stream.
     *
     * @return the element iterator for this stream
     */
    Iterator<T> iterator();

    void println();

    /**
     * Returns whether this stream, if a terminal operation were to be executed,
     * would execute in parallel.  Calling this method after invoking an
     * terminal stream operation method may yield unpredictable results.
     *
     * @return {@code true} if this stream would execute in parallel if executed
     */
    boolean isParallel();

    /**
     * Returns an equivalent stream that is sequential. May return
     * itself, either because the stream was already sequential, or because
     * the underlying stream state was modified to be sequential.
     *
     * @return a sequential stream
     */
    S sequential();

    /**
     * Returns an equivalent stream that is parallel. May return
     * itself if the stream was already parallel. Any parallel should be closed by try-catch or call tried before last step.
     *
     * @return a parallel stream
     * @see #parallel(int, Splitor)
     */
    S parallel();

    /**
     * Returns an equivalent stream that is parallel. May return
     * itself if the stream was already parallel with the same <code>maxThreadNum</code> as the specified one.
     * 
     * @param maxThreadNum
     * @return
     * @see #parallel(int, Splitor)
     */
    S parallel(int maxThreadNum);

    /**
     * Returns an equivalent stream that is parallel. May return
     * itself if the stream was already parallel with the same <code>splitor</code> as the specified one.
     * 
     * @param splitor
     * @return
     * @see #parallel(int, Splitor)
     */
    S parallel(Splitor splitor);

    /**
     * Returns an equivalent stream that is parallel. May return itself if the stream was already parallel with the same <code>maxThreadNum</code> and <code>splitor</code> as the specified ones.
     * 
     * <br></br>
     * When to use parallel Streams? 
     * <ul>
     * <li>First of all, do NOT and should NOT use parallel Streams if you don't have any problem with sequential Streams, because using parallel Streams has extra cost.</li>
     * <li>Consider using parallel Streams only when <a href="http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html">N(the number of elements) * Q(cost per element of F, the per-element function (usually a lambda)) is big enough(e.g. IO involved. Network: DB/web service request..., Reading/Writing file...)</a>.</li>
     * <li>It's easy to test out the differences of performance by sequential Streams and parallel Streams with <a href="http://www.landawn.com/api-docs/com/landawn/abacus/util/Profiler.html">Profiler</a>:</li>
     * </ul>
     * <pre>
     * <code>
     * Profiler.run(1, 1, 3, "sequential", () -> Stream.of(list).operation(F)...).printResult();
     * Profiler.run(1, 1, 3, "parallel", () -> Stream.of(list).parallel().operation(F)...).printResult();
     * </code>
     * </pre>
     * 
     * Here is a sample performance test with computer: CPU Intel i7-3520M 4-cores 2.9 GHz, JDK 1.8.0_101, Windows 7:
     * 
     * <pre>
     * <code>
     * public void test_perf() {
     *     final String[] strs = new String[10_000];
     *     N.fill(strs, N.uuid());
     * 
     *     final int m = 1;
     *     final Function<String, Long> mapper = str -> {
     *         long result = 0;
     *         for (int i = 0; i < m; i++) {
     *             result += N.sum(str.toCharArray()) + 1;
     *         }
     *         return result;
     *     };
     * 
     *     final MutableLong sum = MutableLong.of(0);
     * 
     *     for (int i = 0, len = strs.length; i < len; i++) {
     *         sum.add(mapper.apply(strs[i]));
     *     }
     * 
     *     final int threadNum = 1, loopNum = 100, roundNum = 3;
     * 
     *     Profiler.run(threadNum, loopNum, roundNum, "For Loop", () -> {
     *         long result = 0;
     *         for (int i = 0, len = strs.length; i < len; i++) {
     *             result += mapper.apply(strs[i]);
     *         }
     *         assertEquals(sum.longValue(), result);
     *     }).printResult();
     * 
     *     Profiler.run(threadNum, loopNum, roundNum, "JDK Sequential",
     *             () -> assertEquals(sum.longValue(), java.util.stream.Stream.of(strs).map(mapper).mapToLong(e -> e).sum())).printResult();
     * 
     *     Profiler.run(threadNum, loopNum, roundNum, "JDK Parallel",
     *             () -> assertEquals(sum.longValue(), java.util.stream.Stream.of(strs).parallel().map(mapper).mapToLong(e -> e).sum())).printResult();
     * 
     *     Profiler.run(threadNum, loopNum, roundNum, "Abcus Sequential",
     *             () -> assertEquals(sum.longValue(), Stream.of(strs).map(mapper).mapToLong(e -> e).sum().longValue())).printResult();
     * 
     *     Profiler.run(threadNum, loopNum, roundNum, "Abcus Parallel",
     *             () -> assertEquals(sum.longValue(), Stream.of(strs).parallel().map(mapper).mapToLong(e -> e).sum().longValue())).printResult();
     * }
     * </code>
     * </pre>
     * <b>And test result</b>: <i>Unit is milliseconds. N(the number of elements) is 10_000, Q(cost per element of F, the per-element function (usually a lambda), here is <code>mapper</code>) is calculated by: value of 'For loop' / N(10_000).</i>
     * <table>
     * <tr><th></th><th>  m = 1  </th><th>m = 10</th><th>m = 50</th><th>m = 100</th><th>m = 500</th><th>m = 1000</th></tr>
     * <tr><td>   Q   </td><td>0.00002</td><td>0.0002</td><td>0.001</td><td>0.002</td><td>0.01</td><td>0.02</td></tr>
     * <tr><td>For Loop</td><td>0.23</td><td>2.3</td><td>11</td><td>22</td><td>110</td><td>219</td></tr>
     * <tr><td>JDK Sequential</td><td>0.28</td><td>2.3</td><td>11</td><td>22</td><td>114</td><td>212</td></tr>
     * <tr><td>JDK Parallel</td><td>0.22</td><td>1.3</td><td>6</td><td>12</td><td>66</td><td>122</td></tr>
     * <tr><td>Abcus Sequential</td><td>0.3</td><td>2</td><td>11</td><td>22</td><td>112</td><td>212</td></tr>
     * <tr><td>Abcus Parallel</td><td>11</td><td>11</td><td>11</td><td>16</td><td>77</td><td>128</td></tr>
     * </table>
     *  
     * <b>Comparison:</b>
     * <ul>
     * <li>Again, do NOT and should NOT use parallel Streams if you don't have any problem with sequential Streams, because using parallel Streams has extra cost.</li>
     * <li>Again, consider using parallel Streams only when <a href="http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html">N(the number of elements) * Q(cost per element of F, the per-element function (usually a lambda)) is big enough</a>.</li>
     * <li>The implementation of parallel Streams in Abacus is more than 10 times, slower than parallel Streams in JDK when Q is tiny(here is less than 0.0002 milliseconds by the test): </li>
     * <ul>
     *      <li>The implementation of parallel Streams in JDK 8 still can beat the sequential/for loop when Q is tiny(Here is 0.00002 milliseconds by the test). 
     *      That's amazing, considering the extra cost brought by parallel computation. It's well done.</li>      
     *      <li>The implementation of parallel Streams in Abacus is pretty simple and straight forward. 
     *      The extra cost(starting threads/synchronization/queue...) brought by parallel Streams in Abacus is too bigger to tiny Q(Here is less than 0.001 milliseconds by the test).
     *      But it starts to be faster than sequential Streams when Q is big enough(Here is 0.001 milliseconds by the test) and starts to catch the parallel Streams in JDK when Q is bigger(Here is 0.01 milliseconds by the test).</li>
     *      <li>Consider using the parallel Streams in Abacus when Q is big enough, specially when IO involved in F. 
     *      Because one IO operation(e.g. DB/web service request..., Reading/Writing file...) usually takes 1 to 1000 milliseconds, or even longer.
     *      By the parallel Streams APIs in Abacus, it's very simple to specify max thread numbers. Sometimes, it's much faster to execute IO/Network requests with a bit more threads.
     *      It's fair to say that the parallel Streams in Abacus is high efficient, may same as or faster than the parallel Streams in JDK when Q is big enough, except F is heavy cpu-used operation. 
     *      Most of the times, the Q is big enough to consider using parallel Stream is because IO/Network is involved in F.</li>
     * </ul>
     * <li>JDK 7 is supported by the Streams in Abacus. It's perfect to work with <a href="https://github.com/orfjackal/retrolambda">retrolambda</a> on Android</li>
     * <li>All primitive types are supported by Stream APIs in Abacus except boolean</li>
     * </ul>
     * 
     * <br></br>
     * A bit more about Lambdas/Stream APIs, you may heard that Lambdas/Stream APIs is <a href="http://blog.takipi.com/benchmark-how-java-8-lambdas-and-streams-can-make-your-code-5-times-slower/">5 time slower than imperative programming</a>. 
     * It's true when Q and F is VERY, VERY tiny, like <code>f = (int a, int b) -> a + b;</code>.
     * But if we look into the samples in the article and think about it: it just takes less than 1 milliseconds to get the max value in 100k numbers.
     * There is potential performance issue only if the "get the max value in 100K numbers" call many, many times in your API or single request. 
     * Otherwise, the difference between 0.1 milliseconds to 0.5 milliseconds can be totally ignored. 
     * Usually we meet performance issue only if Q and F is big enough. However, the performance of Lambdas/Streams APIs is closed to for loop when Q and F is big enough.
     * No matter in which scenario, We don't need and should not concern the performance of Lambdas/Stream APIs.
     * 
     * <br></br>
     * Although it's is parallel Streams, it doesn't means all the methods are executed in parallel. 
     * Because the sequential way is as fast, or even faster than the parallel way for some methods, or is pretty difficult, if not possible, to implement the method by parallel approach.
     * Here are the methods which are executed sequentially even in parallel Streams.  
     * <br></br>
     * <i>split/splitAt/splitBy/sliding, distinct, toArray, toObjectList, toList, toSet, toMultiset, toLongMultiset, kthLargest, 
     * count, except(Collection c), intersect(Collection c), forEach(identity, accumulator, predicate)</i>
     * 
     * @param maxThreadNum Default value is the number of cpu-cores. Steps/operations will be executed sequentially if <code>maxThreadNum</code> is 1.
     * @param splitor The target array is split by ranges for multiple threads if splitor is <code>splitor.ARRAY</code> and target stream composed by array. It looks like:
     *        
     * <pre><code>
     * for (int i = 0; i < maxThreadNum; i++) {
     *     final int sliceIndex = i;
     * 
     *     futureList.add(asyncExecutor.execute(new Runnable() {
     *         public void run() {
     *             int cursor = fromIndex + sliceIndex * sliceSize;
     *             final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
     *             while (cursor < to) {
     *                 action.accept(elements[cursor++]);
     *             }
     *        }
     *    }));
     * }
     * </code></pre>
     *        Otherwise, each thread will get the elements from the target array/iterator in the stream one by one with the target array/iterator synchronized. It looks like:
     * <pre><code>
     * for (int i = 0; i < maxThreadNum; i++) {
     *     futureList.add(asyncExecutor.execute(new Runnable() {
     *         public void run() {
     *             T next = null;
     * 
     *             while (true) {
     *                 synchronized (elements) {
     *                     if (cursor.intValue() < toIndex) {
     *                         next = elements[cursor.getAndIncrement()];
     *                     } else {
     *                         break;
     *                     }
     *                 }
     * 
     *                 action.accept(next);
     *             }
     *         }
     *     }));
     * }
     * </code></pre>       
     *        Using <code>splitor.ARRAY</code> only when F (the per-element function (usually a lambda)) is very tiny and the cost of synchronization on the target array/iterator is too big to it.
     *        For the F involving IO or taking 'long' to complete, choose <code>splitor.ITERATOR</code>. Default value is <code>splitor.ITERATOR</code>.
     * @return
     * @see Nth
     * @see com.landawn.abacus.util.Profiler#run(int, int, int, String, Runnable)
     * @see <a href="https://www.infoq.com/presentations/parallel-java-se-8#downloadPdf">Understanding Parallel Stream Performance in Java SE 8</a>
     * @see <a href="http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html">When to use parallel Streams</a>
     */
    S parallel(int maxThreadNum, Splitor splitor);

    /**
     * Return the underlying <code>maxThreadNum</code> if the stream is parallel, otherwise <code>1</code> is returned.
     * 
     * @return
     */
    int maxThreadNum();

    /**
     * Returns a parallel stream with the specified <code>maxThreadNum</code> . Or return
     * itself, either because the stream was already parallel with same <code>maxThreadNum</code>, or because
     * it's a sequential stream.
     * 
     * @param maxThreadNum
     * @return
     */
    S maxThreadNum(int maxThreadNum);

    /**
     * Return the underlying <code>splitor</code> if the stream is parallel, otherwise the default value <code>splitor.ITERATOR</code> is returned.
     * 
     * @return
     */
    Splitor splitor();

    /**
     * Returns a parallel stream with the specified <code>splitor</code> . Or return
     * itself, either because the stream was already parallel with same <code>splitor</code>, or because
     * it's a sequential stream.
     * 
     * @param splitor
     * @return
     */
    S splitor(Splitor splitor);

    Try<S> tried();

    /**
     * Returns an equivalent stream with an additional close handler.  Close
     * handlers are run when the {@link #close()} method
     * is called on the stream, and are executed in the order they were
     * added.  All close handlers are run, even if earlier close handlers throw
     * exceptions.  If any close handler throws an exception, the first
     * exception thrown will be relayed to the caller of {@code close()}, with
     * any remaining exceptions added to that exception as suppressed exceptions
     * (unless one of the remaining exceptions is the same exception as the
     * first exception, since an exception cannot suppress itself.)  May
     * return itself.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param closeHandler A task to execute when the stream is closed
     * @return a stream with a handler that is run if the stream is closed
     */
    S onClose(Runnable closeHandler);

    /**
     * Closes this stream, causing all close handlers for this stream pipeline
     * to be called.
     *
     * @see AutoCloseable#close()
     */
    @Override
    void close();

    public static enum Splitor {
        ARRAY, ITERATOR;
    }
}
