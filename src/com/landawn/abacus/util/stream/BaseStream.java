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

import java.util.Iterator;

import com.landawn.abacus.util.Nth;

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
 * @param <S> the type of of the stream implementing {@code BaseStream}
 * @since 1.8
 * @see Stream
 * @see IntStream
 * @see LongStream
 * @see DoubleStream
 * @see <a href="package-summary.html">java.util.stream</a>
 */
public interface BaseStream<T, S extends BaseStream<T, S>> extends AutoCloseable {

    S append(S s);

    //    /**
    //     * Append the specified Iterator to the tail of this stream.
    //     * @param iter
    //     * @return
    //     */
    //    S append(Iterator<? extends T> iter);

    /**
     * Returns an iterator for the elements of this stream.
     *
     * @return the element iterator for this stream
     */
    Iterator<T> iterator();

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
     * itself if the stream was already parallel
     *
     * @return a parallel stream
     */
    S parallel();

    /**
     * Returns an equivalent stream that is parallel. May return
     * itself if the stream was already parallel with the same <code>maxThreadNum</code> as the specified one.
     * 
     * @param maxThreadNum
     * @return
     * @see #parallel(int, Splitter)
     */
    S parallel(int maxThreadNum);

    /**
     * Returns an equivalent stream that is parallel. May return
     * itself if the stream was already parallel with the same <code>splitter</code> as the specified one.
     * 
     * @param splitter
     * @return
     * @see #parallel(int, Splitter)
     */
    S parallel(Splitter splitter);

    /**
     * Returns an equivalent stream that is parallel. May return itself if the stream was already parallel with the same <code>maxThreadNum</code> and <code>splitter</code> as the specified ones.
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
     *     Profiler.run(1, 100, 3, "For loop", () -> {
     *         long sum = 0;
     *         for (int i = 0, len = strs.length; i < len; i++) {
     *             sum += mapper.apply(strs[i]);
     *         }
     *         N.println(sum);
     *     }).printResult();
     * 
     *     Profiler.run(1, 100, 3, "JDK sequential", () -> java.util.stream.Stream.of(strs).map(mapper).mapToLong(e -> e).sum()).printResult();
     * 
     *     Profiler.run(1, 100, 3, "JDK parallel", () -> java.util.stream.Stream.of(strs).parallel().map(mapper).mapToLong(e -> e).sum()).printResult();
     * 
     *     Profiler.run(1, 100, 3, "Abcus parallel", () -> Stream.of(strs).parallel().map(mapper).mapToLong(e -> e).sum()).printResult();
     * 
     *     Profiler.run(1, 100, 3, "Abcus sequential", () -> Stream.of(strs).map(mapper).mapToLong(e -> e).sum()).printResult();
     * }
     * </code>
     * </pre>
     * <b>And test result</b>: <i>Unit is milliseconds. N(the number of elements) is 10_000, Q(cost per element of F, the per-element function (usually a lambda), here is <code>mapper</code>) is calculated by: value of 'For loop' / N(10_000).</i>
     * <table>
     * <tr><th></th><th>  m = 1  </th><th>m = 10</th><th>m = 100</th><th>m = 500</th><th>m = 1000</th><th>m = 5000</th><th>m = 10000</th></tr>
     * <tr><td>   Q   </td><td>0.00002</td><td>0.0002</td><td>0.002</td><td>0.001</td><td>0.02</td><td>0.01</td><td>0.2</td></tr>
     * <tr><td>For loop</td><td>0.2</td><td>2.05</td><td>22</td><td>107</td><td>222</td><td>1105</td><td>2186</td></tr>
     * <tr><td>JDK sequential</td><td>0.2</td><td>2.2</td><td>21</td><td>106</td><td>214</td><td>1104</td><td>2180</td></tr>
     * <tr><td>JDK parallel</td><td>0.2</td><td>1.3</td><td>11</td><td>52</td><td>121</td><td>615</td><td>1348</td></tr>
     * <tr><td>Abcus parallel</td><td>77</td><td>93</td><td>86</td><td>93</td><td>172</td><td>720</td><td>1387</td></tr>
     * <tr><td>Abcus sequential</td><td>0.3</td><td>2.3</td><td>22</td><td>105</td><td>216</td><td>1094</td><td>2173</td></tr>
     * </table>
     *  
     * <b>Comparison:</b>
     * <ul>
     * <li>Again, do NOT and should NOT use parallel Streams if you don't have any problem with sequential Streams, because using parallel Streams has extra cost.</li>
     * <li>Again, consider using parallel Streams only when <a href="http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html">N(the number of elements) * Q(cost per element of F, the per-element function (usually a lambda)) is big enough</a>.</li>
     * <li>Choose JDK Streams, both sequential and parallel, over the implementation in Abacus if APIs meet the requirements</li>
     * <li>Consider using Abacus Streams when the APIs not exist in JDK Streams</li>
     * <li>The implementation of parallel Streams in Abacus is more than 10 times, even 100 times slower than parallel Streams in JDK when Q is tiny(here is less than 0.002 milliseconds by the test): </li>
     * <ul>
     *      <li>The implementation of parallel Streams in JDK 8 still can beat the sequential/for loop when Q is tiny(Here is 0.00002 milliseconds by the test). 
     *      That's amazing, considering the extra cost brought by parallel computation. It's well done.</li>      
     *      <li>The implementation of parallel Streams in Abacus is pretty simple and straight forward. 
     *      The extra cost(starting threads/synchronization/queue...) brought by parallel Streams in Abacus is too bigger to tiny Q(Here is less than 0.02 milliseconds by the test).
     *      But it starts to be faster than sequential Streams when Q is big enough(Here is 0.001 by the test) and starts to catch the parallel Streams in JDK when Q is bigger(Here is 0.2 by the test).</li>
     *      <li>Consider using the parallel Streams in Abacus when Q is big enough, specially when IO involved in F. 
     *      Because one IO operation(e.g. DB/web service request..., Reading/Writing file...) usually takes 1 to 1000 milliseconds, or even longer.
     *      By the parallel Streams APIs in Abacus, it's very simple to specify max thread numbers. Sometimes, it's much faster to execute IO/Network requests with a bit more threads.
     *      It's fair to say that the parallel Streams in Abacus is high efficient, may same as or faster than the parallel Streams in JDK when Q is big enough, except F is heavy cpu-used operation. 
     *      But the reason, most of the times, that Q is big enough to consider using parallel Stream is that IO/Network is involved in F.</li>
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
     * <i>split, distinct, toArray, toObjectList, toList, toSet, toMultiset, toLongMultiset, kthLargest, count, except(Collection c), intersect(Collection c), breakWhileNull, breakWhileError</i>
     * 
     * @param maxThreadNum Default value is the number of cpu-cores. Steps/operations will be executed sequentially if <code>maxThreadNum</code> is 1.
     * @param splitter The target array is split by ranges for multiple threads if splitter is <code>Splitter.ARRAY</code> and target stream composed by array. It looks like:
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
     *        Using <code>Splitter.ARRAY</code> only when F (the per-element function (usually a lambda)) is very tiny and the cost of synchronization on the target array/iterator is too big to it.
     *        For the F involving IO or taking 'long' to complete, choose <code>Splitter.ITERATOR</code>. Default value is <code>Splitter.ITERATOR</code>.
     * @return
     * @see Nth
     * @see com.landawn.abacus.util.Profiler#run(int, int, int, String, Runnable)
     * @see <a href="https://www.infoq.com/presentations/parallel-java-se-8#downloadPdf">Understanding Parallel Stream Performance in Java SE 8</a>
     * @see <a href="http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html">When to use parallel Streams</a>
     */
    S parallel(int maxThreadNum, Splitter splitter);

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
     * Return the underlying <code>splitter</code> if the stream is parallel, otherwise the default value <code>Splitter.ITERATOR</code> is returned.
     * 
     * @return
     */
    Splitter splitter();

    /**
     * Returns a parallel stream with the specified <code>splitter</code> . Or return
     * itself, either because the stream was already parallel with same <code>splitter</code>, or because
     * it's a sequential stream.
     * 
     * @param splitter
     * @return
     */
    S splitter(Splitter splitter);

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

    public static enum Splitter {
        ARRAY, ITERATOR;
    }
}
