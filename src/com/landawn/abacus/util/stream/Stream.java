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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.lang.reflect.Field;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.SecureRandom;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
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
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.AbacusIOException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.AsyncExecutor;
import com.landawn.abacus.util.BiMap;
import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LineIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.RowIterator;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteNFunction;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharNFunction;
import com.landawn.abacus.util.function.CharTriFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleNFunction;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatNFunction;
import com.landawn.abacus.util.function.FloatTriFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntBiFunction;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntNFunction;
import com.landawn.abacus.util.function.IntTriFunction;
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongNFunction;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.NFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortNFunction;
import com.landawn.abacus.util.function.ShortTriFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.UnaryOperator;
import com.landawn.abacus.util.stream.AbstractStream.LocalLinkedHashSet;
import com.landawn.abacus.util.stream.ImmutableIterator.QueuedIterator;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * A sequence of elements supporting sequential and parallel aggregate
 * operations.  The following example illustrates an aggregate operation using
 * {@link Stream} and {@link IntStream}:
 *
 * <pre>{@code
 *     int sum = widgets.stream()
 *                      .filter(w -> w.getColor() == RED)
 *                      .mapToInt(w -> w.getWeight())
 *                      .sum();
 * }</pre>
 *
 * In this example, {@code widgets} is a {@code Collection<Widget>}.  We create
 * a stream of {@code Widget} objects via {@link Collection#stream Collection.stream()},
 * filter it to produce a stream containing only the red widgets, and then
 * transform it into a stream of {@code int} values representing the weight of
 * each red widget. Then this stream is summed to produce a total weight.
 *
 * <p>In addition to {@code Stream}, which is a stream of object references,
 * there are primitive specializations for {@link IntStream}, {@link LongStream},
 * and {@link DoubleStream}, all of which are referred to as "streams" and
 * conform to the characteristics and restrictions described here.
 *
 * <p>To perform a computation, stream
 * <a href="package-summary.html#StreamOps">operations</a> are composed into a
 * <em>stream pipeline</em>.  A stream pipeline consists of a source (which
 * might be an array, a collection, a generator function, an I/O channel,
 * etc), zero or more <em>intermediate operations</em> (which transform a
 * stream into another stream, such as {@link Stream#filter(Predicate)}), and a
 * <em>terminal operation</em> (which produces a result or side-effect, such
 * as {@link Stream#count()} or {@link Stream#forEach(Consumer)}).
 * Streams are lazy; computation on the source data is only performed when the
 * terminal operation is initiated, and source elements are consumed only
 * as needed.
 *
 * <p>Collections and streams, while bearing some superficial similarities,
 * have different goals.  Collections are primarily concerned with the efficient
 * management of, and access to, their elements.  By contrast, streams do not
 * provide a means to directly access or manipulate their elements, and are
 * instead concerned with declaratively describing their source and the
 * computational operations which will be performed in aggregate on that source.
 * However, if the provided stream operations do not offer the desired
 * functionality, the {@link #iterator()} and {@link #spliterator()} operations
 * can be used to perform a controlled traversal.
 *
 * <p>A stream pipeline, like the "widgets" example above, can be viewed as
 * a <em>query</em> on the stream source.  Unless the source was explicitly
 * designed for concurrent modification (such as a {@link ConcurrentHashMap}),
 * unpredictable or erroneous behavior may result from modifying the stream
 * source while it is being queried.
 *
 * <p>Most stream operations accept parameters that describe user-specified
 * behavior, such as the lambda expression {@code w -> w.getWeight()} passed to
 * {@code mapToInt} in the example above.  To preserve correct behavior,
 * these <em>behavioral parameters</em>:
 * <ul>
 * <li>must be <a href="package-summary.html#NonInterference">non-interfering</a>
 * (they do not modify the stream source); and</li>
 * <li>in most cases must be <a href="package-summary.html#Statelessness">stateless</a>
 * (their result should not depend on any state that might change during execution
 * of the stream pipeline).</li>
 * </ul>
 *
 * <p>Such parameters are always instances of a
 * <a href="../function/package-summary.html">functional interface</a> such
 * as {@link java.util.function.Function}, and are often lambda expressions or
 * method references.  Unless otherwise specified these parameters must be
 * <em>non-null</em>.
 *
 * <p>A stream should be operated on (invoking an intermediate or terminal stream
 * operation) only once.  This rules out, for example, "forked" streams, where
 * the same source feeds two or more pipelines, or multiple traversals of the
 * same stream.  A stream implementation may throw {@link IllegalStateException}
 * if it detects that the stream is being reused. However, since some stream
 * operations may return their receiver rather than a new stream object, it may
 * not be possible to detect reuse in all cases.
 *
 * <p>Streams have a {@link #close()} method and implement {@link AutoCloseable},
 * but nearly all stream instances do not actually need to be closed after use.
 * Generally, only streams whose source is an IO channel (such as those returned
 * by {@link Files#lines(Path, Charset)}) will require closing.  Most streams
 * are backed by collections, arrays, or generating functions, which require no
 * special resource management.  (If a stream does require closing, it can be
 * declared as a resource in a {@code try}-with-resources statement.)
 *
 * <p>Stream pipelines may execute either sequentially or in
 * <a href="package-summary.html#Parallelism">parallel</a>.  This
 * execution mode is a property of the stream.  Streams are created
 * with an initial choice of sequential or parallel execution.  (For example,
 * {@link Collection#stream() Collection.stream()} creates a sequential stream,
 * and {@link Collection#parallelStream() Collection.parallelStream()} creates
 * a parallel one.)  This choice of execution mode may be modified by the
 * {@link #sequential()} or {@link #parallel()} methods, and may be queried with
 * the {@link #isParallel()} method.
 *
 * @param <T> the type of the stream elements
 * @since 1.8
 * @see IntStream
 * @see LongStream
 * @see DoubleStream
 * @see <a href="package-summary.html">java.util.stream</a>
 */
public abstract class Stream<T> implements BaseStream<T, Stream<T>> {
    static final Logger logger = LoggerFactory.getLogger(Stream.class);

    static final Object NONE = new Object();

    static final int THREAD_POOL_SIZE = 512;

    static final AsyncExecutor asyncExecutor = new AsyncExecutor(Stream.THREAD_POOL_SIZE, 300L, TimeUnit.SECONDS);

    static final int DEFAULT_MAX_THREAD_NUM = N.CPU_CORES;

    static final int DEFAULT_READING_THREAD_NUM = 8;

    static final int DEFAULT_QUEUE_SIZE = 16;

    static final Splitter DEFAULT_SPILTTER = Splitter.ITERATOR;

    static final Random RAND = new SecureRandom();

    @SuppressWarnings("rawtypes")
    private static final Stream EMPTY = new ArrayStream(N.EMPTY_OBJECT_ARRAY);

    static final Comparator<Character> CHAR_COMPARATOR = new Comparator<Character>() {
        @Override
        public int compare(Character o1, Character o2) {
            return Character.compare(o1, o2);
        }
    };

    static final Comparator<Byte> BYTE_COMPARATOR = new Comparator<Byte>() {
        @Override
        public int compare(Byte o1, Byte o2) {
            return Byte.compare(o1, o2);
        }
    };

    static final Comparator<Short> SHORT_COMPARATOR = new Comparator<Short>() {
        @Override
        public int compare(Short o1, Short o2) {
            return Short.compare(o1, o2);
        }
    };

    static final Comparator<Integer> INT_COMPARATOR = new Comparator<Integer>() {
        @Override
        public int compare(Integer o1, Integer o2) {
            return Integer.compare(o1, o2);
        }
    };

    static final Comparator<Long> LONG_COMPARATOR = new Comparator<Long>() {
        @Override
        public int compare(Long o1, Long o2) {
            return Long.compare(o1, o2);
        }
    };

    static final Comparator<Float> FLOAT_COMPARATOR = new Comparator<Float>() {
        @Override
        public int compare(Float o1, Float o2) {
            return Float.compare(o1, o2);
        }
    };

    static final Comparator<Double> DOUBLE_COMPARATOR = new Comparator<Double>() {
        @Override
        public int compare(Double o1, Double o2) {
            return Double.compare(o1, o2);
        }
    };

    @SuppressWarnings("rawtypes")
    static final Comparator OBJECT_COMPARATOR = new Comparator<Comparable>() {
        @Override
        public int compare(final Comparable a, final Comparable b) {
            return a == null ? (b == null ? 0 : -1) : (b == null ? 1 : a.compareTo(b));
        }
    };

    static final BiMap<Class<?>, Comparator<?>> defaultComparator = new BiMap<>();
    static {
        defaultComparator.put(char.class, CHAR_COMPARATOR);
        defaultComparator.put(byte.class, BYTE_COMPARATOR);
        defaultComparator.put(short.class, SHORT_COMPARATOR);
        defaultComparator.put(int.class, INT_COMPARATOR);
        defaultComparator.put(long.class, LONG_COMPARATOR);
        defaultComparator.put(float.class, FLOAT_COMPARATOR);
        defaultComparator.put(double.class, DOUBLE_COMPARATOR);
        defaultComparator.put(Object.class, OBJECT_COMPARATOR);
    }

    static final Field listElementDataField;
    static final Field listSizeField;
    static volatile boolean isListElementDataFieldGettable = true;
    static volatile boolean isListElementDataFieldSettable = true;

    static {
        Field tmp = null;

        try {
            tmp = ArrayList.class.getDeclaredField("elementData");
        } catch (Exception e) {
            // ignore.
        }

        listElementDataField = tmp != null && tmp.getType().equals(Object[].class) ? tmp : null;

        if (listElementDataField != null) {
            listElementDataField.setAccessible(true);
        }

        tmp = null;

        try {
            tmp = ArrayList.class.getDeclaredField("size");
        } catch (Exception e) {
            // ignore.
        }

        listSizeField = tmp != null && tmp.getType().equals(int.class) ? tmp : null;

        if (listSizeField != null) {
            listSizeField.setAccessible(true);
        }
    }

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
    public abstract Stream<T> filter(final Predicate<? super T> predicate);

    /**
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract Stream<T> filter(final Predicate<? super T> predicate, final long max);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    public abstract Stream<T> takeWhile(final Predicate<? super T> predicate);

    /**
     * Keep the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns false, any element y behind x: <code>predicate.text(y)</code> should returns false.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract Stream<T> takeWhile(final Predicate<? super T> predicate, final long max);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @return
     */
    public abstract Stream<T> dropWhile(final Predicate<? super T> predicate);

    /**
     * Remove the elements until the given predicate returns false. The stream should be sorted, which means if x is the first element: <code>predicate.text(x)</code> returns true, any element y behind x: <code>predicate.text(y)</code> should returns true.
     * 
     * In parallel Streams, the elements after the first element which <code>predicate</code> returns false may be tested by predicate too.
     * 
     * @param predicate
     * @param max the maximum elements number to the new Stream.
     * @return
     */
    public abstract Stream<T> dropWhile(final Predicate<? super T> predicate, final long max);

    /**
     * Returns a stream consisting of the results of applying the given
     * function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param <R> The element type of the new stream
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract <R> Stream<R> map(Function<? super T, ? extends R> mapper);

    public abstract CharStream mapToChar(ToCharFunction<? super T> mapper);

    public abstract ByteStream mapToByte(ToByteFunction<? super T> mapper);

    public abstract ShortStream mapToShort(ToShortFunction<? super T> mapper);

    /**
     * Returns an {@code IntStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an <a href="package-summary.html#StreamOps">
     *     intermediate operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element
     * @return the new stream
     */
    public abstract IntStream mapToInt(ToIntFunction<? super T> mapper);

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
    public abstract LongStream mapToLong(ToLongFunction<? super T> mapper);

    public abstract FloatStream mapToFloat(ToFloatFunction<? super T> mapper);

    /**
     * Returns a {@code DoubleStream} consisting of the results of applying the
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
    public abstract DoubleStream mapToDouble(ToDoubleFunction<? super T> mapper);

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
     * @apiNote
     * The {@code flatMap()} operation has the effect of applying a one-to-many
     * transformation to the elements of the stream, and then flattening the
     * resulting elements into a new stream.
     *
     * <p><b>Examples.</b>
     *
     * <p>If {@code orders} is a stream of purchase orders, and each purchase
     * order contains a collection of line items, then the following produces a
     * stream containing all the line items in all the orders:
     * <pre>{@code
     *     orders.flatMap(order -> order.getLineItems().stream())...
     * }</pre>
     *
     * <p>If {@code path} is the path to a file, then the following produces a
     * stream of the {@code words} contained in that file:
     * <pre>{@code
     *     Stream<String> lines = Files.lines(path, StandardCharsets.UTF_8);
     *     Stream<String> words = lines.flatMap(line -> Stream.of(line.split(" +")));
     * }</pre>
     * The {@code mapper} function passed to {@code flatMap} splits a line,
     * using a simple regular expression, into an array of words, and then
     * creates a stream of words from that array.
     *
     * @param <R> The element type of the new stream
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element which produces a stream
     *               of new values
     * @return the new stream
     */
    public abstract <R> Stream<R> flatMap(Function<? super T, ? extends Stream<? extends R>> mapper);

    public abstract <R> Stream<R> flatMap2(Function<? super T, ? extends R[]> mapper);

    public abstract <R> Stream<R> flatMap3(Function<? super T, ? extends Collection<? extends R>> mapper);

    public abstract CharStream flatMapToChar(Function<? super T, ? extends CharStream> mapper);

    public abstract CharStream flatMapToChar2(Function<? super T, char[]> mapper);

    public abstract CharStream flatMapToChar3(Function<? super T, ? extends Collection<Character>> mapper);

    public abstract ByteStream flatMapToByte(Function<? super T, ? extends ByteStream> mapper);

    public abstract ByteStream flatMapToByte2(Function<? super T, byte[]> mapper);

    public abstract ByteStream flatMapToByte3(Function<? super T, ? extends Collection<Byte>> mapper);

    public abstract ShortStream flatMapToShort(Function<? super T, ? extends ShortStream> mapper);

    public abstract ShortStream flatMapToShort2(Function<? super T, short[]> mapper);

    public abstract ShortStream flatMapToShort3(Function<? super T, ? extends Collection<Short>> mapper);

    /**
     * Returns an {@code IntStream} consisting of the results of replacing each
     * element of this stream with the contents of a mapped stream produced by
     * applying the provided mapping function to each element.  Each mapped
     * stream is {@link java.util.stream.BaseStream#close() closed} after its
     * contents have been placed into this stream.  (If a mapped stream is
     * {@code null} an empty stream is used, instead.)
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element which produces a stream
     *               of new values
     * @return the new stream
     * @see #flatMap(Function)
     */
    public abstract IntStream flatMapToInt(Function<? super T, ? extends IntStream> mapper);

    public abstract IntStream flatMapToInt2(Function<? super T, int[]> mapper);

    public abstract IntStream flatMapToInt3(Function<? super T, ? extends Collection<Integer>> mapper);

    /**
     * Returns an {@code LongStream} consisting of the results of replacing each
     * element of this stream with the contents of a mapped stream produced by
     * applying the provided mapping function to each element.  Each mapped
     * stream is {@link java.util.stream.BaseStream#close() closed} after its
     * contents have been placed into this stream.  (If a mapped stream is
     * {@code null} an empty stream is used, instead.)
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element which produces a stream
     *               of new values
     * @return the new stream
     * @see #flatMap(Function)
     */
    public abstract LongStream flatMapToLong(Function<? super T, ? extends LongStream> mapper);

    public abstract LongStream flatMapToLong2(Function<? super T, long[]> mapper);

    public abstract LongStream flatMapToLong3(Function<? super T, ? extends Collection<Long>> mapper);

    public abstract FloatStream flatMapToFloat(Function<? super T, ? extends FloatStream> mapper);

    public abstract FloatStream flatMapToFloat2(Function<? super T, float[]> mapper);

    public abstract FloatStream flatMapToFloat3(Function<? super T, ? extends Collection<Float>> mapper);

    /**
     * Returns an {@code DoubleStream} consisting of the results of replacing
     * each element of this stream with the contents of a mapped stream produced
     * by applying the provided mapping function to each element.  Each mapped
     * stream is {@link java.util.stream.BaseStream#close() closed} after its
     * contents have placed been into this stream.  (If a mapped stream is
     * {@code null} an empty stream is used, instead.)
     *
     * <p>This is an <a href="package-summary.html#StreamOps">intermediate
     * operation</a>.
     *
     * @param mapper a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *               <a href="package-summary.html#Statelessness">stateless</a>
     *               function to apply to each element which produces a stream
     *               of new values
     * @return the new stream
     * @see #flatMap(Function)
     */
    public abstract DoubleStream flatMapToDouble(Function<? super T, ? extends DoubleStream> mapper);

    public abstract DoubleStream flatMapToDouble2(Function<? super T, double[]> mapper);

    public abstract DoubleStream flatMapToDouble3(Function<? super T, ? extends Collection<Double>> mapper);

    public abstract <K> Stream<Map.Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier);

    public abstract <K> Stream<Map.Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier, final Supplier<Map<K, List<T>>> mapFactory);

    public abstract <K, A, D> Stream<Map.Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream);

    public abstract <K, D, A> Stream<Map.Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<Map<K, D>> mapFactory);

    public abstract <K, U> Stream<Map.Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper,
            final Function<? super T, ? extends U> valueMapper);

    public abstract <K, U> Stream<Map.Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            final Supplier<Map<K, U>> mapFactory);

    public abstract <K, U> Stream<Map.Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction);

    public abstract <K, U> Stream<Map.Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<Map<K, U>> mapFactory);

    /**
     * Returns Stream of Stream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller). 
     * This method only run sequentially, even in parallel stream.
     * 
     * @param size
     * @return
     */
    public abstract Stream<Stream<T>> split(int size);

    /**
     * Returns Stream of Stream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller).
     * This method only run sequentially, even in parallel stream.
     * 
     * @param size
     * @return
     */
    public abstract Stream<List<T>> splitIntoList(int size);

    /**
     * Returns Stream of Stream with consecutive sub sequences of the elements, each of the same size (the final sequence may be smaller).
     * This method only run sequentially, even in parallel stream.
     * 
     * @param size
     * @return
     */
    public abstract Stream<Set<T>> splitIntoSet(int size);

    /**
     * Split the stream by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * final MutableInt border = MutableInt.of(5);
     * Stream.of(1, 2, 3, 5, 7, 9, 10, 11, 19).splitIntoList(e -> {
     *     if (e <= border.intValue()) {
     *         return true;
     *     } else {
     *         border.addAndGet(5);
     *         return false;
     *     }
     * }).forEach(N::println);
     * </code>
     * </pre>
     * 
     * This stream should be sorted by value which is used to verify the border.
     * This method only run sequentially, even in parallel stream.
     * 
     * @param predicate
     * @return
     */
    public abstract Stream<Stream<T>> split(Predicate<? super T> predicate);

    /**
     * Split the stream by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * final MutableInt border = MutableInt.of(5);
     * Stream.of(1, 2, 3, 5, 7, 9, 10, 11, 19).splitIntoList(e -> {
     *     if (e <= border.intValue()) {
     *         return true;
     *     } else {
     *         border.addAndGet(5);
     *         return false;
     *     }
     * }).forEach(N::println);
     * </code>
     * </pre>
     * 
     * This stream should be sorted by value which is used to verify the border.
     * This method only run sequentially, even in parallel stream.
     * 
     * @param predicate
     * @return
     */
    public abstract Stream<List<T>> splitIntoList(Predicate<? super T> predicate);

    /**
     * Split the stream by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * final MutableInt border = MutableInt.of(5);
     * Stream.of(1, 2, 3, 5, 7, 9, 10, 11, 19).splitIntoSet(e -> {
     *     if (e <= border.intValue()) {
     *         return true;
     *     } else {
     *         border.addAndGet(5);
     *         return false;
     *     }
     * }).forEach(N::println);
     * </code>
     * </pre>
     * 
     * This stream should be sorted by value which is used to verify the border.
     * This method only run sequentially, even in parallel stream.
     * 
     * @param predicate
     * @return
     */
    public abstract Stream<Set<T>> splitIntoSet(Predicate<? super T> predicate);

    /**
     * Returns a stream consisting of the distinct elements (according to
     * {@link Object#equals(Object)}) of this stream.
     *
     * <p>For ordered streams, the selection of distinct elements is stable
     * (for duplicated elements, the element appearing first in the encounter
     * order is preserved.)  For unordered streams, no stability guarantees
     * are made.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @apiNote
     * Preserving stability for {@code distinct()} in parallel pipelines is
     * relatively expensive (requires that the operation act as a full barrier,
     * with substantial buffering overhead), and stability is often not needed.
     * Using an unordered stream source
     * or removing the ordering constraint with {@link #unordered()} may result
     * in significantly more efficient execution for {@code distinct()} in parallel
     * pipelines, if the semantics of your situation permit.  If consistency
     * with encounter order is required, and you are experiencing poor performance
     * or memory utilization with {@code distinct()} in parallel pipelines,
     * switching to sequential execution with {@link #sequential()} may improve
     * performance.
     *
     * @return the new stream
     */
    public abstract Stream<T> distinct();

    public abstract Stream<T> distinct(Comparator<? super T> comparator);

    /**
     * Distinct by the value mapped from <code>keyMapper</code>
     * 
     * @param keyMapper
     * @return
     */
    public abstract Stream<T> distinct(Function<? super T, ?> keyMapper);

    public abstract Stream<T> top(int n);

    public abstract Stream<T> top(int n, Comparator<? super T> comparator);

    /**
     * Returns a stream consisting of the elements of this stream, sorted
     * according to natural order.  If the elements of this stream are not
     * {@code Comparable}, a {@code java.lang.ClassCastException} may be thrown
     * when the terminal operation is executed.
     *
     * <p>For ordered streams, the sort is stable.  For unordered streams, no
     * stability guarantees are made.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @return the new stream
     */
    public abstract Stream<T> sorted();

    /**
     * Returns a stream consisting of the elements of this stream, sorted
     * according to the provided {@code Comparator}.
     *
     * <p>For ordered streams, the sort is stable.  For unordered streams, no
     * stability guarantees are made.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">stateful
     * intermediate operation</a>.
     *
     * @param comparator a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                   <a href="package-summary.html#Statelessness">stateless</a>
     *                   {@code Comparator} to be used to compare stream elements
     * @return the new stream
     */
    public abstract Stream<T> sorted(Comparator<? super T> comparator);

    //    public abstract Stream<T> parallelSorted();
    //
    //    public abstract Stream<T> parallelSorted(Comparator<? super T> comparator);

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
    public abstract Stream<T> peek(Consumer<? super T> action);

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
    public abstract Stream<T> limit(long maxSize);

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
    public abstract Stream<T> skip(long n);

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
    public abstract void forEach(Consumer<? super T> action);

    /**
     * Execute <code>accumulator</code> on each element till <code>till</code> returns true.
     * 
     * @param identity
     * @param accumulator
     * @param till break if the <code>till</code> returns true.
     * @return
     */
    public abstract <U> U forEach(final U identity, BiFunction<U, ? super T, U> accumulator, final Predicate<? super U> till);

    //    /**
    //     * In parallel Streams, the elements after the first element which <code>action</code> returns false may be executed by action too.
    //     * 
    //     * @param action break if the action returns false.
    //     * @return false if it breaks, otherwise true.
    //     */
    //    public abstract boolean forEach2(Function<? super T, Boolean> action);

    /**
     * Returns an array containing the elements of this stream.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @return an array containing the elements of this stream
     */
    public abstract Object[] toArray();

    /**
     * Returns an array containing the elements of this stream, using the
     * provided {@code generator} function to allocate the returned array, as
     * well as any additional arrays that might be required for a partitioned
     * execution or for resizing.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote
     * The generator function takes an integer, which is the size of the
     * desired array, and produces an array of the desired size.  This can be
     * concisely expressed with an array constructor reference:
     * <pre>{@code
     *     Person[] men = people.stream()
     *                          .filter(p -> p.getGender() == MALE)
     *                          .toArray(Person[]::new);
     * }</pre>
     *
     * @param <A> the element type of the resulting array
     * @param generator a function which produces a new array of the desired
     *                  type and the provided length
     * @return an array containing the elements in this stream
     * @throws ArrayStoreException if the runtime type of the array returned
     * from the array generator is not a supertype of the runtime type of every
     * element in this stream
     */
    public abstract <A> A[] toArray(IntFunction<A[]> generator);

    public abstract <A> ObjectList<A> toObjectList(Class<A> cls);

    public abstract List<T> toList();

    public abstract List<T> toList(Supplier<? extends List<T>> supplier);

    public abstract Set<T> toSet();

    public abstract Set<T> toSet(Supplier<? extends Set<T>> supplier);

    public abstract Multiset<T> toMultiset();

    public abstract Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier);

    public abstract LongMultiset<T> toLongMultiset();

    public abstract LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier);

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public abstract <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier);

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public abstract <K, M extends Map<K, List<T>>> M toMap(final Function<? super T, ? extends K> classifier, final Supplier<M> mapFactory);

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public abstract <K, A, D> Map<K, D> toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream);

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public abstract <K, D, A, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public abstract <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapSupplier);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public abstract <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mergeFunction
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public abstract <K, U> Multimap<K, U, List<U>> toMultimap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper);

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @param mapSupplier
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapSupplier);

    /**
     * 
     * @param columnNames it can be null or empty if this is Map or entity stream.
     * @return
     */
    public abstract DataSet toDataSet(final List<String> columnNames);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using the provided identity value and an
     * <a href="package-summary.html#Associativity">associative</a>
     * accumulation function, and returns the reduced value.  This is equivalent
     * to:
     * <pre>{@code
     *     T result = identity;
     *     for (T element : this stream)
     *         result = accumulator.apply(result, element)
     *     return result;
     * }</pre>
     *
     * but is not constrained to execute sequentially.
     *
     * <p>The {@code identity} value must be an identity for the accumulator
     * function. This means that for all {@code t},
     * {@code accumulator.apply(identity, t)} is equal to {@code t}.
     * The {@code accumulator} function must be an
     * <a href="package-summary.html#Associativity">associative</a> function.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote Sum, min, max, average, and string concatenation are all special
     * cases of reduction. Summing a stream of numbers can be expressed as:
     *
     * <pre>{@code
     *     Integer sum = integers.reduce(0, (a, b) -> a+b);
     * }</pre>
     *
     * or:
     *
     * <pre>{@code
     *     Integer sum = integers.reduce(0, Integer::sum);
     * }</pre>
     *
     * <p>While this may seem a more roundabout way to perform an aggregation
     * compared to simply mutating a running total in a loop, reduction
     * operations parallelize more gracefully, without needing additional
     * synchronization and with greatly reduced risk of data races.
     *
     * @param identity the identity value for the accumulating function
     * @param accumulator an <a href="package-summary.html#Associativity">associative</a>,
     *                    <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                    <a href="package-summary.html#Statelessness">stateless</a>
     *                    function for combining two values
     * @return the result of the reduction
     */
    public abstract T reduce(T identity, BinaryOperator<T> accumulator);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using an
     * <a href="package-summary.html#Associativity">associative</a> accumulation
     * function, and returns an {@code Optional} describing the reduced value,
     * if any. This is equivalent to:
     * <pre>{@code
     *     boolean foundAny = false;
     *     T result = null;
     *     for (T element : this stream) {
     *         if (!foundAny) {
     *             foundAny = true;
     *             result = element;
     *         }
     *         else
     *             result = accumulator.apply(result, element);
     *     }
     *     return foundAny ? OptionalNullable.of(result) : OptionalNullable.empty();
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
     * @param accumulator an <a href="package-summary.html#Associativity">associative</a>,
     *                    <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                    <a href="package-summary.html#Statelessness">stateless</a>
     *                    function for combining two values
     * @return an {@link Optional} describing the result of the reduction
     * @throws NullPointerException if the result of the reduction is null
     * @see #reduce(Object, BinaryOperator)
     * @see #min(Comparator)
     * @see #max(Comparator)
     */
    public abstract OptionalNullable<T> reduce(BinaryOperator<T> accumulator);

    /**
     * Performs a <a href="package-summary.html#Reduction">reduction</a> on the
     * elements of this stream, using the provided identity, accumulation and
     * combining functions.  This is equivalent to:
     * <pre>{@code
     *     U result = identity;
     *     for (T element : this stream)
     *         result = accumulator.apply(result, element)
     *     return result;
     * }</pre>
     *
     * but is not constrained to execute sequentially.
     *
     * <p>The {@code identity} value must be an identity for the combiner
     * function.  This means that for all {@code u}, {@code combiner(identity, u)}
     * is equal to {@code u}.  Additionally, the {@code combiner} function
     * must be compatible with the {@code accumulator} function; for all
     * {@code u} and {@code t}, the following must hold:
     * <pre>{@code
     *     combiner.apply(u, accumulator.apply(identity, t)) == accumulator.apply(u, t)
     * }</pre>
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote Many reductions using this form can be represented more simply
     * by an explicit combination of {@code map} and {@code reduce} operations.
     * The {@code accumulator} function acts as a fused mapper and accumulator,
     * which can sometimes be more efficient than separate mapping and reduction,
     * such as when knowing the previously reduced value allows you to avoid
     * some computation.
     *
     * @param <U> The type of the result
     * @param identity the identity value for the combiner function
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
     * @see #reduce(BinaryOperator)
     * @see #reduce(Object, BinaryOperator)
     */
    public abstract <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner);

    /**
     * This method is always executed sequentially, even in parallel stream.
     * 
     * @param identity
     * @param accumulator
     * @return
     */
    public abstract <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator);

    /**
     * Performs a <a href="package-summary.html#MutableReduction">mutable
     * reduction</a> operation on the elements of this stream.  A mutable
     * reduction is one in which the reduced value is a mutable result container,
     * such as an {@code ArrayList}, and elements are incorporated by updating
     * the state of the result rather than by replacing the result.  This
     * produces a result equivalent to:
     * <pre>{@code
     *     R result = supplier.get();
     *     for (T element : this stream)
     *         accumulator.accept(result, element);
     *     return result;
     * }</pre>
     *
     * <p>Like {@link #reduce(Object, BinaryOperator)}, {@code collect} operations
     * can be parallelized without requiring additional synchronization.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @apiNote There are many existing classes in the JDK whose signatures are
     * well-suited for use with method references as arguments to {@code collect()}.
     * For example, the following will accumulate strings into an {@code ArrayList}:
     * <pre>{@code
     *     List<String> asList = stringStream.collect(ArrayList::new, ArrayList::add,
     *                                                ArrayList::addAll);
     * }</pre>
     *
     * <p>The following will take a stream of strings and concatenates them into a
     * single string:
     * <pre>{@code
     *     String concat = stringStream.collect(StringBuilder::new, StringBuilder::append,
     *                                          StringBuilder::append)
     *                                 .toString();
     * }</pre>
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
     */
    public abstract <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner);

    /**
     * This method is always executed sequentially, even in parallel stream.
     * 
     * @param supplier
     * @param accumulator
     * @return
     */
    public abstract <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator);

    /**
     * Performs a <a href="package-summary.html#MutableReduction">mutable
     * reduction</a> operation on the elements of this stream using a
     * {@code Collector}.  A {@code Collector}
     * encapsulates the functions used as arguments to
     * {@link #collect(Supplier, BiConsumer, BiConsumer)}, allowing for reuse of
     * collection strategies and composition of collect operations such as
     * multiple-level grouping or partitioning.
     *
     * <p>If the stream is parallel, and the {@code Collector}
     * is {@link Collector.Characteristics#CONCURRENT concurrent}, and
     * either the stream is unordered or the collector is
     * {@link Collector.Characteristics#UNORDERED unordered},
     * then a concurrent reduction will be performed (see {@link Collector} for
     * details on concurrent reduction.)
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * <p>When executed in parallel, multiple intermediate results may be
     * instantiated, populated, and merged so as to maintain isolation of
     * mutable data structures.  Therefore, even when executed in parallel
     * with non-thread-safe data structures (such as {@code ArrayList}), no
     * additional synchronization is needed for a parallel reduction.
     *
     * @apiNote
     * The following will accumulate strings into an ArrayList:
     * <pre>{@code
     *     List<String> asList = stringStream.collect(Collectors.toList());
     * }</pre>
     *
     * <p>The following will classify {@code Person} objects by city:
     * <pre>{@code
     *     Map<String, List<Person>> peopleByCity
     *         = personStream.collect(Collectors.groupingBy(Person::getCity));
     * }</pre>
     *
     * <p>The following will classify {@code Person} objects by state and city,
     * cascading two {@code Collector}s together:
     * <pre>{@code
     *     Map<String, Map<String, List<Person>>> peopleByStateAndCity
     *         = personStream.collect(Collectors.groupingBy(Person::getState,
     *                                                      Collectors.groupingBy(Person::getCity)));
     * }</pre>
     *
     * @param <R> the type of the result
     * @param <A> the intermediate accumulation type of the {@code Collector}
     * @param collector the {@code Collector} describing the reduction
     * @return the result of the reduction
     * @see #collect(Supplier, BiConsumer, BiConsumer)
     * @see Collectors
     */
    public abstract <R, A> R collect(Collector<? super T, A, R> collector);

    /**
     * Returns the minimum element of this stream according to the provided
     * {@code Comparator}.  This is a special case of a
     * <a href="package-summary.html#Reduction">reduction</a>.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal operation</a>.
     *
     * @param comparator a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                   <a href="package-summary.html#Statelessness">stateless</a>
     *                   {@code Comparator} to compare elements of this stream
     * @return an {@code Optional} describing the minimum element of this stream,
     * or an empty {@code Optional} if the stream is empty
     * @throws NullPointerException if the minimum element is null
     */
    public abstract OptionalNullable<T> min(Comparator<? super T> comparator);

    /**
     * Returns the maximum element of this stream according to the provided
     * {@code Comparator}.  This is a special case of a
     * <a href="package-summary.html#Reduction">reduction</a>.
     *
     * <p>This is a <a href="package-summary.html#StreamOps">terminal
     * operation</a>.
     *
     * @param comparator a <a href="package-summary.html#NonInterference">non-interfering</a>,
     *                   <a href="package-summary.html#Statelessness">stateless</a>
     *                   {@code Comparator} to compare elements of this stream
     * @return an {@code Optional} describing the maximum element of this stream,
     * or an empty {@code Optional} if the stream is empty
     * @throws NullPointerException if the maximum element is null
     */
    public abstract OptionalNullable<T> max(Comparator<? super T> comparator);

    /**
     * 
     * @param k
     * @param comparator
     * @return OptionalNullable.empty() if there is no element or count less than k, otherwise the kth largest element.
     */
    public abstract OptionalNullable<T> kthLargest(int k, Comparator<? super T> comparator);

    public abstract Long sumInt(ToIntFunction<? super T> mapper);

    public abstract Long sumLong(ToLongFunction<? super T> mapper);

    public abstract Double sumDouble(ToDoubleFunction<? super T> mapper);

    public abstract OptionalDouble averageInt(ToIntFunction<? super T> mapper);

    public abstract OptionalDouble averageLong(ToLongFunction<? super T> mapper);

    public abstract OptionalDouble averageDouble(ToDoubleFunction<? super T> mapper);

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

    public abstract CharSummaryStatistics summarizeChar(ToCharFunction<? super T> mapper);

    public abstract ByteSummaryStatistics summarizeByte(ToByteFunction<? super T> mapper);

    public abstract ShortSummaryStatistics summarizeShort(ToShortFunction<? super T> mapper);

    public abstract IntSummaryStatistics summarizeInt(ToIntFunction<? super T> mapper);

    public abstract LongSummaryStatistics summarizeLong(ToLongFunction<? super T> mapper);

    public abstract FloatSummaryStatistics summarizeFloat(ToFloatFunction<? super T> mapper);

    public abstract DoubleSummaryStatistics summarizeDouble(ToDoubleFunction<? super T> mapper);

    public abstract String join(CharSequence delimiter);

    public abstract String join(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix);

    public abstract Optional<Map<String, T>> distribution();

    public abstract Optional<Map<String, T>> distribution(Comparator<? super T> comparator);

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
    public abstract boolean anyMatch(Predicate<? super T> predicate);

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
    public abstract boolean allMatch(Predicate<? super T> predicate);

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
    public abstract boolean noneMatch(Predicate<? super T> predicate);

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
     * @throws NullPointerException if the element selected is null
     */
    // public abstract OptionalNullable<T> findFirst();

    public abstract OptionalNullable<T> findFirst(Predicate<? super T> predicate);

    // public abstract OptionalNullable<T> findLast();

    public abstract OptionalNullable<T> findLast(Predicate<? super T> predicate);

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
     * @throws NullPointerException if the element selected is null
     * @see #findFirst()
     */
    // public abstract OptionalNullable<T> findAny();

    public abstract OptionalNullable<T> findAny(Predicate<? super T> predicate);

    /**
     * @param c
     * @return
     * @see IntList#except(IntList)
     */
    public abstract Stream<T> except(Collection<?> c);

    /**
     * Except with the specified Collection by the values mapped by <code>mapper</code>
     * 
     * @param mapper
     * @param c
     * @return
     * @see IntList#except(IntList)
     */
    public abstract Stream<T> except(Function<? super T, ?> mapper, Collection<?> c);

    /**
     * @param c
     * @return
     * @see IntList#intersect(IntList)
     */
    public abstract Stream<T> intersect(Collection<?> c);

    /**
     * Intersect with the specified Collection by the values mapped by <code>mapper</code>
     * 
     * @param mapper
     * @param c
     * @return
     * @see IntList#intersect(IntList)
     */
    public abstract Stream<T> intersect(Function<? super T, ?> mapper, Collection<?> c);

    //    /**
    //     * Skip all the elements in the specified collection.
    //     * 
    //     * @param c
    //     * @return
    //     */
    //    public abstract Stream<T> exclude(Collection<?> c);
    //
    //    /**
    //     * Skip all from the specified Collection by the values mapped by <code>mapper</code>
    //     * 
    //     * @param mapper
    //     * @param c
    //     * @return
    //     */
    //    public abstract Stream<T> exclude(Function<? super T, ?> mapper, Collection<?> c);

    //    /**
    //     * Skip all the <code>null</code> elements in the stream.
    //     * 
    //     * @return
    //     */
    //    public abstract Stream<T> skipNull();
    //
    //    /**
    //     * Break the iteration of the Stream if null element occurs
    //     *  
    //     * @return
    //     * @see NullBreakIterator
    //     */
    //    public abstract Stream<T> breakWhileNull();
    //
    //    /**
    //     * Break the iteration of the Stream if errors occurs
    //     * 
    //     * @return
    //     * @see ErrorBreakIterator
    //     */
    //    public abstract Stream<T> breakWhileError();
    //
    //    /**
    //     * Break the iteration of the Stream if error still occurs after retry specified <code>maxRetries</code> times.
    //     * 
    //     * @param maxRetries Default value is 0, no retry.
    //     * @param retryInterval
    //     * @return
    //     * @see ErrorBreakIterator
    //     */
    //    public abstract Stream<T> breakWhileError(int maxRetries, long retryInterval);

    public abstract Stream<T> queued();

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
     * 
     * @param queueSize Default value is 8
     * @return
     */
    public abstract Stream<T> queued(int queueSize);

    /**
     * Append the specified stream to the tail of this stream.
     * @param stream
     * @return
     */
    @Override
    public abstract Stream<T> append(final Stream<T> stream);

    /**
     * 
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public abstract Stream<T> merge(final Stream<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector);

    /**
     * Returns a reusable stream which can be repeatedly used.
     * 
     * @param generator
     * @return
     */
    public abstract Stream<T> cached(IntFunction<T[]> generator);

    public abstract long persist(File file, Function<? super T, String> toLine);

    public abstract long persist(OutputStream os, Function<? super T, String> toLine);

    public abstract long persist(Writer writer, Function<? super T, String> toLine);

    public abstract long persist(final Connection conn, final String insertSQL, final int batchSize, final int batchInterval,
            final BiConsumer<? super PreparedStatement, ? super T> stmtSetter);

    public abstract long persist(final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final BiConsumer<? super PreparedStatement, ? super T> stmtSetter);

    @Override
    public abstract ImmutableIterator<T> iterator();

    // Static factories

    // Static factories

    public static <T> Stream<T> empty() {
        return EMPTY;
    }

    public static <T> Stream<T> of(final T... a) {
        return N.isNullOrEmpty(a) ? (Stream<T>) empty() : of(a, 0, a.length);
    }

    /**
     * Returns a sequential, stateful and immutable <code>Stream</code>.
     *
     * @param a
     * @param startIndex
     * @param endIndex
     * @return
     */
    public static <T> Stream<T> of(final T[] a, final int startIndex, final int endIndex) {
        return N.isNullOrEmpty(a) && (startIndex == 0 && endIndex == 0) ? (Stream<T>) empty() : new ArrayStream<T>(a, startIndex, endIndex);
    }

    /**
     * Returns a sequential, stateful and immutable <code>Stream</code>.
     *
     * @param c
     * @return
     */
    public static <T> Stream<T> of(final Collection<? extends T> c) {
        return N.isNullOrEmpty(c) ? (Stream<T>) empty() : of(c, 0, c.size());
    }

    /**
     * Returns a sequential, stateful and immutable <code>Stream</code>.
     * 
     * @param c
     * @param startIndex
     * @param endIndex
     * @return
     */
    public static <T> Stream<T> of(final Collection<? extends T> c, int startIndex, int endIndex) {
        if (N.isNullOrEmpty(c) && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        if (startIndex < 0 || endIndex < startIndex || endIndex > c.size()) {
            throw new IllegalArgumentException("startIndex(" + startIndex + ") or endIndex(" + endIndex + ") is invalid");
        }

        // return new CollectionStream<T>(c);
        // return new ArrayStream<T>((T[]) c.toArray()); // faster

        if (isListElementDataFieldGettable && listElementDataField != null && c instanceof ArrayList) {
            T[] array = null;

            try {
                array = (T[]) listElementDataField.get(c);
            } catch (Exception e) {
                // ignore;
                isListElementDataFieldGettable = false;
            }

            if (array != null) {
                return of(array, startIndex, endIndex);
            }
        }

        if (startIndex == 0 && endIndex == c.size()) {
            // return (c.size() > 10 && (c.size() < 1000 || (c.size() < 100000 && c instanceof ArrayList))) ? streamOf((T[]) c.toArray()) : c.stream();
            return of(c.iterator());
        } else {
            return of(c.iterator(), startIndex, endIndex);
        }
    }

    /**
     * Returns a sequential, stateful and immutable <code>Stream</code>.
     *
     * @param iterator
     * @return
     */
    public static <T> Stream<T> of(final Iterator<? extends T> iterator) {
        return iterator == null ? (Stream<T>) empty() : new IteratorStream<T>(iterator);
    }

    /**
     * Returns a sequential, stateful and immutable <code>Stream</code>.
     * 
     * @param c
     * @param startIndex
     * @param endIndex
     * @return
     */
    static <T> Stream<T> of(final Iterator<? extends T> iterator, int startIndex, int endIndex) {
        if (iterator == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        if (startIndex < 0 || endIndex < startIndex) {
            throw new IllegalArgumentException("startIndex(" + startIndex + ") or endIndex(" + endIndex + ") is invalid");
        }

        final Stream<T> stream = of(iterator);
        return stream.skip(startIndex).limit(endIndex - startIndex);
    }

    /**
     * <p> The returned stream encapsulates a {@link Reader}.  If timely
     * disposal of file system resources is required, the try-with-resources
     * construct should be used to ensure that the stream's
     * {@link Stream#close close} method is invoked after the stream operations
     * are completed.
     * 
     * @param file
     * @return
     */
    static Stream<String> of(File file) {
        BufferedReader br = null;

        try {
            br = new BufferedReader(new FileReader(file));
            final BufferedReader tmp = br;

            return of(br).onClose(new Runnable() {
                @Override
                public void run() {
                    IOUtil.close(tmp);
                }
            });
        } catch (IOException e) {
            IOUtil.close(br);
            throw new AbacusIOException(e);
        }
    }

    /**
     * It's user's responsibility to close the input <code>reader</code> after the stream is finished.
     * 
     * @param reader
     * @return
     */
    public static Stream<String> of(final Reader reader) {
        N.requireNonNull(reader);

        return of(new LineIterator(reader));
    }

    static Stream<String> of(final Reader reader, int startIndex, int endIndex) {
        N.requireNonNull(reader);

        return of(new LineIterator(reader), startIndex, endIndex);
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @return
     */
    public static Stream<Object[]> of(final ResultSet resultSet) {
        N.requireNonNull(resultSet);

        return of(new RowIterator(resultSet));
    }

    static Stream<Object[]> of(final ResultSet resultSet, int startIndex, int endIndex) {
        N.requireNonNull(resultSet);

        return of(new RowIterator(resultSet), startIndex, endIndex);
    }

    public static Stream<Character> from(char... a) {
        return CharStream.of(a).boxed();
    }

    public static Stream<Character> from(char[] a, int fromIndex, int toIndex) {
        return CharStream.of(a, fromIndex, toIndex).boxed();
    }

    public static Stream<Byte> from(byte... a) {
        return ByteStream.of(a).boxed();
    }

    public static Stream<Byte> from(byte[] a, int fromIndex, int toIndex) {
        return ByteStream.of(a, fromIndex, toIndex).boxed();
    }

    public static Stream<Short> from(short... a) {
        return ShortStream.of(a).boxed();
    }

    public static Stream<Short> from(short[] a, int fromIndex, int toIndex) {
        return ShortStream.of(a, fromIndex, toIndex).boxed();
    }

    public static Stream<Integer> from(int... a) {
        return IntStream.of(a).boxed();
    }

    public static Stream<Integer> from(int[] a, int fromIndex, int toIndex) {
        return IntStream.of(a, fromIndex, toIndex).boxed();
    }

    public static Stream<Long> from(long... a) {
        return LongStream.of(a).boxed();
    }

    public static Stream<Long> from(long[] a, int fromIndex, int toIndex) {
        return LongStream.of(a, fromIndex, toIndex).boxed();
    }

    public static Stream<Float> from(float... a) {
        return FloatStream.of(a).boxed();
    }

    public static Stream<Float> from(float[] a, int fromIndex, int toIndex) {
        return FloatStream.of(a, fromIndex, toIndex).boxed();
    }

    public static Stream<Double> from(double... a) {
        return DoubleStream.of(a).boxed();
    }

    public static Stream<Double> from(double[] a, int fromIndex, int toIndex) {
        return DoubleStream.of(a, fromIndex, toIndex).boxed();
    }

    public static <T> Stream<T> repeat(T element, int n) {
        return of(Array.repeat(element, n));
    }

    public static <T> Stream<T> iterate(final Supplier<Boolean> hasNext, final Supplier<? extends T> next) {
        N.requireNonNull(hasNext);
        N.requireNonNull(next);

        return of(new ImmutableIterator<T>() {
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.get().booleanValue();
                }

                return hasNextVal;
            }

            @Override
            public T next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return next.get();
            }
        });
    }

    /**
     * Returns a sequential ordered {@code Stream} produced by iterative
     * application of a function {@code f} to an initial element {@code seed},
     * producing a {@code Stream} consisting of {@code seed}, {@code f(seed)},
     * {@code f(f(seed))}, etc.
     *
     * <p>The first element (position {@code 0}) in the {@code Stream} will be
     * the provided {@code seed}.  For {@code n > 0}, the element at position
     * {@code n}, will be the result of applying the function {@code f} to the
     * element at position {@code n - 1}.
     * 
     * @param seed
     * @param hasNext
     * @param f
     * @return
     */
    public static <T> Stream<T> iterate(final T seed, final Supplier<Boolean> hasNext, final UnaryOperator<T> f) {
        N.requireNonNull(hasNext);
        N.requireNonNull(f);

        return of(new ImmutableIterator<T>() {
            private T t = (T) Stream.NONE;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.get().booleanValue();
                }

                return hasNextVal;
            }

            @Override
            public T next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return t = (t == Stream.NONE) ? seed : f.apply(t);
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
    public static <T> Stream<T> iterate(final T seed, final Predicate<T> hasNext, final UnaryOperator<T> f) {
        N.requireNonNull(hasNext);
        N.requireNonNull(f);

        return of(new ImmutableIterator<T>() {
            private T t = (T) Stream.NONE;
            private T cur = (T) Stream.NONE;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false && cur == Stream.NONE) {
                    hasNextVal = hasNext.test((cur = (t == Stream.NONE ? seed : f.apply(t))));
                }

                return hasNextVal;
            }

            @Override
            public T next() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                t = cur;
                cur = (T) Stream.NONE;
                hasNextVal = false;
                return t;
            }
        });
    }

    public static <T> Stream<T> iterate(final T seed, final UnaryOperator<T> f) {
        N.requireNonNull(f);

        return of(new ImmutableIterator<T>() {
            private T t = (T) Stream.NONE;

            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public T next() {
                return t = t == Stream.NONE ? seed : f.apply(t);
            }
        });
    }

    public static <T> Stream<T> iterate(final Supplier<T> s) {
        N.requireNonNull(s);

        return of(new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public T next() {
                return s.get();
            }
        });
    }

    //    public static <T> Stream<T> queued(Stream<? extends T> stream) {
    //        return queued(stream, DEFAULT_QUEUE_SIZE);
    //    }
    //
    //    /**
    //     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
    //     * 
    //     * @param stream
    //     * @param queueSize Default value is 8
    //     * @return
    //     */
    //    public static <T> Stream<T> queued(Stream<? extends T> stream, int queueSize) {
    //        if (stream.iterator() instanceof QueuedIterator && ((QueuedIterator<? extends T>) stream.iterator()).max() >= queueSize) {
    //            return (Stream<T>) stream;
    //        } else {
    //            return parallelConcat(N.asArray(stream), 1, queueSize);
    //        }
    //    }
    //
    //    public static <T> Stream<T> queued(Iterator<? extends T> iterator) {
    //        return queued(iterator, DEFAULT_QUEUE_SIZE);
    //    }
    //
    //    /**
    //     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
    //     * 
    //     * @param iterator
    //     * @param queueSize Default value is 8
    //     * @return
    //     */
    //    public static <T> Stream<T> queued(Iterator<? extends T> iterator, int queueSize) {
    //        if (iterator instanceof QueuedIterator && ((QueuedIterator<? extends T>) iterator).max() >= queueSize) {
    //            return of(iterator);
    //        } else {
    //            return parallelConcat(N.asArray(iterator), 1, queueSize);
    //        }
    //    }

    public static <T> Stream<T> concat(final T[]... a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        final Iterator<? extends T>[] iter = new Iterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iter[i] = ImmutableIterator.of(a[i]);
        }

        return concat(iter);
    }

    //    static <T> Stream<T> concat(Collection<? extends T>... a) {
    //        final List<Iterator<? extends T>> iterators = new ArrayList<>(a.length);
    //
    //        for (int i = 0, len = a.length; i < len; i++) {
    //            iterators.add(a[i].iterator());
    //        }
    //
    //        return concat(iterators);
    //    }

    public static <T> Stream<T> concat(final Stream<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        final Iterator<? extends T>[] iter = new Iterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iter[i] = a[i].iterator();
        }

        return concat(iter).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (Stream<? extends T> stream : a) {
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

    public static <T> Stream<T> concat(final Iterator<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        return concat(N.asList(a));
    }

    public static <T> Stream<T> concat(final Collection<? extends Iterator<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return of(new ImmutableIterator<T>() {
            private final Iterator<? extends Iterator<? extends T>> iterators = c.iterator();
            private Iterator<? extends T> cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iterators.hasNext()) {
                    cur = iterators.next();
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
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Stream<? extends T>... a) {
        return parallelConcat(a, DEFAULT_READING_THREAD_NUM, calculateQueueSize(a.length));
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterators in parallel.
     * 
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param readThreadNum - count of threads used to read elements from iterator to queue. Default value is min(8, a.length)
     * @param queueSize Default value is N.min(128, a.length * 16)
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Stream<? extends T>[] a, final int readThreadNum, final int queueSize) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        final Iterator<? extends T>[] iter = new Iterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iter[i] = a[i].iterator();
        }

        return parallelConcat(N.asList(iter), readThreadNum, queueSize).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (Stream<? extends T> stream : a) {
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
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Iterator<? extends T>... a) {
        return parallelConcat(a, DEFAULT_READING_THREAD_NUM, calculateQueueSize(a.length));
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterators in parallel.
     * 
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param readThreadNum - count of threads used to read elements from iterator to queue. Default value is min(8, a.length)
     * @param queueSize Default value is N.min(128, a.length * 16)
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Iterator<? extends T>[] a, final int readThreadNum, final int queueSize) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        return parallelConcat(N.asList(a), readThreadNum, queueSize);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param c
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Collection<? extends Iterator<? extends T>> c) {
        return parallelConcat(c, DEFAULT_READING_THREAD_NUM);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param c
     * @param readThreadNum
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Collection<? extends Iterator<? extends T>> c, final int readThreadNum) {
        return parallelConcat(c, readThreadNum, calculateQueueSize(c.size()));
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterators in parallel.
     * 
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelConcat(a,b, ...)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param readThreadNum - count of threads used to read elements from iterator to queue. Default value is min(8, c.size())
     * @param queueSize Default value is N.min(128, c.size() * 16)
     * @return
     */
    public static <T> Stream<T> parallelConcat(final Collection<? extends Iterator<? extends T>> c, final int readThreadNum, final int queueSize) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final AsyncExecutor asyncExecutor = new AsyncExecutor(N.min(readThreadNum, c.size()), 300L, TimeUnit.SECONDS);

        return parallelConcat(c, queueSize, asyncExecutor);
    }

    static <T> Stream<T> parallelConcat(final Collection<? extends Iterator<? extends T>> c, final int queueSize, final AsyncExecutor asyncExecutor) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final AtomicInteger threadCounter = new AtomicInteger(c.size());
        final ArrayBlockingQueue<T> queue = new ArrayBlockingQueue<T>(queueSize);
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);

        for (Iterator<? extends T> e : c) {
            final Iterator<? extends T> iter = e;

            asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        T next = null;

                        while (onGoing.booleanValue() && iter.hasNext()) {
                            next = iter.next();

                            if (next == null) {
                                next = (T) NONE;
                            }

                            if (queue.offer(next) == false) {
                                while (onGoing.booleanValue()) {
                                    if (queue.offer(next, 100, TimeUnit.MILLISECONDS)) {
                                        break;
                                    }
                                }
                            }
                        }
                    } catch (Throwable e) {
                        setError(errorHolder, e, onGoing);
                    } finally {
                        threadCounter.decrementAndGet();
                    }
                }
            });
        }

        return of(new QueuedIterator<T>(queueSize) {
            T next = null;

            @Override
            public boolean hasNext() {
                try {
                    if (next == null && (next = queue.poll()) == null) {
                        while (onGoing.booleanValue() && (threadCounter.get() > 0 || queue.size() > 0)) { // (queue.size() > 0 || counter.get() > 0) is wrong. has to check counter first
                            if ((next = queue.poll(100, TimeUnit.MILLISECONDS)) != null) {
                                break;
                            }
                        }
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                }

                if (errorHolder.value() != null) {
                    throwError(errorHolder, onGoing);
                }

                return next != null;
            }

            @Override
            public T next() {
                if (next == null && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                T result = next == NONE ? null : next;
                next = null;
                return result;
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
            }
        });
    }

    static <T> Stream<T> parallelConcat(final Collection<? extends Iterator<? extends T>> c, final AsyncExecutor asyncExecutor) {
        return parallelConcat(c, calculateQueueSize(c.size()), asyncExecutor);
    }

    /**
     * Zip together the "a" and "b" arrays until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final char[] a, final char[] b, final CharBiFunction<R> combiner) {
        return zip(ImmutableCharIterator.of(a), ImmutableCharIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final char[] a, final char[] b, final char[] c, final CharTriFunction<R> combiner) {
        return zip(ImmutableCharIterator.of(a), ImmutableCharIterator.of(b), ImmutableCharIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final CharStream a, final CharStream b, final CharBiFunction<R> combiner) {
        return zip(a.charIterator(), b.charIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final CharStream a, final CharStream b, final CharStream c, final CharTriFunction<R> combiner) {
        return zip(a.charIterator(), b.charIterator(), c.charIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final CharIterator a, final CharIterator b, final CharBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final CharIterator a, final CharIterator b, final CharIterator c, final CharTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends CharIterator> c, final CharNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (CharIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final char[] args = new char[len];
                int idx = 0;

                for (CharIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final CharStream a, final CharStream b, final char valueForNoneA, final char valueForNoneB,
            final CharBiFunction<R> combiner) {
        return zip(a.charIterator(), b.charIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final CharStream a, final CharStream b, final CharStream c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final CharTriFunction<R> combiner) {
        return zip(a.charIterator(), b.charIterator(), c.charIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final CharIterator a, final CharIterator b, final char valueForNoneA, final char valueForNoneB,
            final CharBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final CharIterator a, final CharIterator b, final CharIterator c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final CharTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends CharIterator> c, final char[] valuesForNone, final CharNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (CharIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final char[] args = new char[len];
                int idx = 0;
                boolean hasNext = false;

                for (CharIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <R> Stream<R> zip(final byte[] a, final byte[] b, final ByteBiFunction<R> combiner) {
        return zip(ImmutableByteIterator.of(a), ImmutableByteIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final byte[] a, final byte[] b, final byte[] c, final ByteTriFunction<R> combiner) {
        return zip(ImmutableByteIterator.of(a), ImmutableByteIterator.of(b), ImmutableByteIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ByteStream a, final ByteStream b, final ByteBiFunction<R> combiner) {
        return zip(a.byteIterator(), b.byteIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ByteStream a, final ByteStream b, final ByteStream c, final ByteTriFunction<R> combiner) {
        return zip(a.byteIterator(), b.byteIterator(), c.byteIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ByteIterator a, final ByteIterator b, final ByteBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ByteIterator a, final ByteIterator b, final ByteIterator c, final ByteTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends ByteIterator> c, final ByteNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (ByteIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final byte[] args = new byte[len];
                int idx = 0;

                for (ByteIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final ByteStream a, final ByteStream b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<R> combiner) {
        return zip(a.byteIterator(), b.byteIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final ByteStream a, final ByteStream b, final ByteStream c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<R> combiner) {
        return zip(a.byteIterator(), b.byteIterator(), c.byteIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final ByteIterator a, final ByteIterator b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final ByteIterator a, final ByteIterator b, final ByteIterator c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends ByteIterator> c, final byte[] valuesForNone, final ByteNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (ByteIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final byte[] args = new byte[len];
                int idx = 0;
                boolean hasNext = false;

                for (ByteIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <R> Stream<R> zip(final short[] a, final short[] b, final ShortBiFunction<R> combiner) {
        return zip(ImmutableShortIterator.of(a), ImmutableShortIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final short[] a, final short[] b, final short[] c, final ShortTriFunction<R> combiner) {
        return zip(ImmutableShortIterator.of(a), ImmutableShortIterator.of(b), ImmutableShortIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ShortStream a, final ShortStream b, final ShortBiFunction<R> combiner) {
        return zip(a.shortIterator(), b.shortIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ShortStream a, final ShortStream b, final ShortStream c, final ShortTriFunction<R> combiner) {
        return zip(a.shortIterator(), b.shortIterator(), c.shortIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ShortIterator a, final ShortIterator b, final ShortBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final ShortIterator a, final ShortIterator b, final ShortIterator c, final ShortTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends ShortIterator> c, final ShortNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (ShortIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final short[] args = new short[len];
                int idx = 0;

                for (ShortIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final ShortStream a, final ShortStream b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<R> combiner) {
        return zip(a.shortIterator(), b.shortIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final ShortStream a, final ShortStream b, final ShortStream c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<R> combiner) {
        return zip(a.shortIterator(), b.shortIterator(), c.shortIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final ShortIterator a, final ShortIterator b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final ShortIterator a, final ShortIterator b, final ShortIterator c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends ShortIterator> c, final short[] valuesForNone, final ShortNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (ShortIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final short[] args = new short[len];
                int idx = 0;
                boolean hasNext = false;

                for (ShortIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <R> Stream<R> zip(final int[] a, final int[] b, final IntBiFunction<R> combiner) {
        return zip(ImmutableIntIterator.of(a), ImmutableIntIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final int[] a, final int[] b, final int[] c, final IntTriFunction<R> combiner) {
        return zip(ImmutableIntIterator.of(a), ImmutableIntIterator.of(b), ImmutableIntIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final IntStream a, final IntStream b, final IntBiFunction<R> combiner) {
        return zip(a.intIterator(), b.intIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final IntStream a, final IntStream b, final IntStream c, final IntTriFunction<R> combiner) {
        return zip(a.intIterator(), b.intIterator(), c.intIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final IntIterator a, final IntIterator b, final IntBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final IntIterator a, final IntIterator b, final IntIterator c, final IntTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends IntIterator> c, final IntNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (IntIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final int[] args = new int[len];
                int idx = 0;

                for (IntIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final IntStream a, final IntStream b, final int valueForNoneA, final int valueForNoneB, final IntBiFunction<R> combiner) {
        return zip(a.intIterator(), b.intIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final IntStream a, final IntStream b, final IntStream c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final IntTriFunction<R> combiner) {
        return zip(a.intIterator(), b.intIterator(), c.intIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final IntIterator a, final IntIterator b, final int valueForNoneA, final int valueForNoneB,
            final IntBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final IntIterator a, final IntIterator b, final IntIterator c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final IntTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends IntIterator> c, final int[] valuesForNone, final IntNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (IntIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final int[] args = new int[len];
                int idx = 0;
                boolean hasNext = false;

                for (IntIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <R> Stream<R> zip(final long[] a, final long[] b, final LongBiFunction<R> combiner) {
        return zip(ImmutableLongIterator.of(a), ImmutableLongIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final long[] a, final long[] b, final long[] c, final LongTriFunction<R> combiner) {
        return zip(ImmutableLongIterator.of(a), ImmutableLongIterator.of(b), ImmutableLongIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final LongStream a, final LongStream b, final LongBiFunction<R> combiner) {
        return zip(a.longIterator(), b.longIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final LongStream a, final LongStream b, final LongStream c, final LongTriFunction<R> combiner) {
        return zip(a.longIterator(), b.longIterator(), c.longIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final LongIterator a, final LongIterator b, final LongBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final LongIterator a, final LongIterator b, final LongIterator c, final LongTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends LongIterator> c, final LongNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (LongIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final long[] args = new long[len];
                int idx = 0;

                for (LongIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final LongStream a, final LongStream b, final long valueForNoneA, final long valueForNoneB,
            final LongBiFunction<R> combiner) {
        return zip(a.longIterator(), b.longIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final LongStream a, final LongStream b, final LongStream c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final LongTriFunction<R> combiner) {
        return zip(a.longIterator(), b.longIterator(), c.longIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final LongIterator a, final LongIterator b, final long valueForNoneA, final long valueForNoneB,
            final LongBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final LongIterator a, final LongIterator b, final LongIterator c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final LongTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends LongIterator> c, final long[] valuesForNone, final LongNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (LongIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final long[] args = new long[len];
                int idx = 0;
                boolean hasNext = false;

                for (LongIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <R> Stream<R> zip(final float[] a, final float[] b, final FloatBiFunction<R> combiner) {
        return zip(ImmutableFloatIterator.of(a), ImmutableFloatIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final float[] a, final float[] b, final float[] c, final FloatTriFunction<R> combiner) {
        return zip(ImmutableFloatIterator.of(a), ImmutableFloatIterator.of(b), ImmutableFloatIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final FloatStream a, final FloatStream b, final FloatBiFunction<R> combiner) {
        return zip(a.floatIterator(), b.floatIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final FloatStream a, final FloatStream b, final FloatStream c, final FloatTriFunction<R> combiner) {
        return zip(a.floatIterator(), b.floatIterator(), c.floatIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final FloatIterator a, final FloatIterator b, final FloatBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final FloatIterator a, final FloatIterator b, final FloatIterator c, final FloatTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends FloatIterator> c, final FloatNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (FloatIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final float[] args = new float[len];
                int idx = 0;

                for (FloatIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final FloatStream a, final FloatStream b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<R> combiner) {
        return zip(a.floatIterator(), b.floatIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final FloatStream a, final FloatStream b, final FloatStream c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final FloatTriFunction<R> combiner) {
        return zip(a.floatIterator(), b.floatIterator(), c.floatIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final FloatIterator a, final FloatIterator b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final FloatIterator a, final FloatIterator b, final FloatIterator c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final FloatTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends FloatIterator> c, final float[] valuesForNone, final FloatNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (FloatIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final float[] args = new float[len];
                int idx = 0;
                boolean hasNext = false;

                for (FloatIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <R> Stream<R> zip(final double[] a, final double[] b, final DoubleBiFunction<R> combiner) {
        return zip(ImmutableDoubleIterator.of(a), ImmutableDoubleIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final double[] a, final double[] b, final double[] c, final DoubleTriFunction<R> combiner) {
        return zip(ImmutableDoubleIterator.of(a), ImmutableDoubleIterator.of(b), ImmutableDoubleIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final DoubleStream a, final DoubleStream b, final DoubleBiFunction<R> combiner) {
        return zip(a.doubleIterator(), b.doubleIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final DoubleStream a, final DoubleStream b, final DoubleStream c, final DoubleTriFunction<R> combiner) {
        return zip(a.doubleIterator(), b.doubleIterator(), c.doubleIterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final DoubleIterator a, final DoubleIterator b, final DoubleBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <R> Stream<R> zip(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final DoubleTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends DoubleIterator> c, final DoubleNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (DoubleIterator e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final double[] args = new double[len];
                int idx = 0;

                for (DoubleIterator e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <R> Stream<R> zip(final DoubleStream a, final DoubleStream b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<R> combiner) {
        return zip(a.doubleIterator(), b.doubleIterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final DoubleStream a, final DoubleStream b, final DoubleStream c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<R> combiner) {
        return zip(a.doubleIterator(), b.doubleIterator(), c.doubleIterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <R> Stream<R> zip(final DoubleIterator a, final DoubleIterator b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <R> Stream<R> zip(final DoubleIterator a, final DoubleIterator b, final DoubleIterator c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final DoubleTriFunction<R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends DoubleIterator> c, final double[] valuesForNone, final DoubleNFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (DoubleIterator e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final double[] args = new double[len];
                int idx = 0;
                boolean hasNext = false;

                for (DoubleIterator e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
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
    public static <A, B, R> Stream<R> zip(final A[] a, final B[] b, final BiFunction<? super A, ? super B, R> combiner) {
        return zip(ImmutableIterator.of(a), ImmutableIterator.of(b), combiner);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <A, B, C, R> Stream<R> zip(final A[] a, final B[] b, final C[] c, final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return zip(ImmutableIterator.of(a), ImmutableIterator.of(b), ImmutableIterator.of(c), combiner);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <A, B, R> Stream<R> zip(final Stream<A> a, final Stream<B> b, final BiFunction<? super A, ? super B, R> combiner) {
        return zip(a.iterator(), b.iterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <A, B, C, R> Stream<R> zip(final Stream<A> a, final Stream<B> b, final Stream<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return zip(a.iterator(), b.iterator(), c.iterator(), combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <A, B, R> Stream<R> zip(final Iterator<? extends A> a, final Iterator<? extends B> b, final BiFunction<? super A, ? super B, R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next());
            }
        });
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied combiner function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static <A, B, C, R> Stream<R> zip(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c,
            final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() && b.hasNext() && c.hasNext();
            }

            @Override
            public R next() {
                return combiner.apply(a.next(), b.next(), c.next());
            }
        });
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied combiner function.
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> zip(final Collection<? extends Iterator<?>> c, final NFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {

            @Override
            public boolean hasNext() {
                for (Iterator<?> e : c) {
                    if (e.hasNext() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public R next() {
                final Object[] args = new Object[len];
                int idx = 0;

                for (Iterator<?> e : c) {
                    args[idx++] = e.next();
                }

                return combiner.apply(args);
            }
        });
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
    public static <A, B, R> Stream<R> zip(final Stream<A> a, final Stream<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> combiner) {
        return zip(a.iterator(), b.iterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <A, B, C, R> Stream<R> zip(final Stream<A> a, final Stream<B> b, final Stream<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return zip(a.iterator(), b.iterator(), c.iterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
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
    public static <A, B, R> Stream<R> zip(final Iterator<? extends A> a, final Iterator<? extends B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB);
            }
        });
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
    public static <A, B, C, R> Stream<R> zip(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || c.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(a.hasNext() ? a.next() : valueForNoneA, b.hasNext() ? b.next() : valueForNoneB, c.hasNext() ? c.next() : valueForNoneC);
            }
        });
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
    public static <R> Stream<R> zip(final Collection<? extends Iterator<?>> c, final Object[] valuesForNone, final NFunction<R> combiner) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            @Override
            public boolean hasNext() {
                for (Iterator<?> e : c) {
                    if (e.hasNext()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public R next() {
                final Object[] args = new Object[len];
                int idx = 0;
                boolean hasNext = false;

                for (Iterator<?> e : c) {
                    if (e.hasNext()) {
                        hasNext = true;
                        args[idx] = e.next();
                    } else {
                        args[idx] = valuesForNone[idx];
                    }
                    idx++;
                }

                if (hasNext == false) {
                    throw new NoSuchElementException();
                }

                return combiner.apply(args);
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param combiner
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final BiFunction<? super A, ? super B, R> combiner) {
        return parallelZip(a, b, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final BiFunction<? super A, ? super B, R> combiner,
            final int queueSize) {
        return parallelZip(a.iterator(), b.iterator(), combiner, queueSize).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    public static <A, B, C, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final Stream<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return parallelZip(a, b, c, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, C, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final Stream<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> combiner, final int queueSize) {
        return parallelZip(a.iterator(), b.iterator(), c.iterator(), combiner, queueSize).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param combiner
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b,
            final BiFunction<? super A, ? super B, R> combiner) {
        return parallelZip(a, b, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b,
            final BiFunction<? super A, ? super B, R> combiner, final int queueSize) {
        final AsyncExecutor asyncExecutor = new AsyncExecutor(2, 300L, TimeUnit.SECONDS);
        final AtomicInteger threadCounterA = new AtomicInteger(1);
        final AtomicInteger threadCounterB = new AtomicInteger(1);
        final BlockingQueue<A> queueA = new ArrayBlockingQueue<>(queueSize);
        final BlockingQueue<B> queueB = new ArrayBlockingQueue<>(queueSize);
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);

        readToQueue(a, b, asyncExecutor, threadCounterA, threadCounterB, queueA, queueB, errorHolder, onGoing);

        return of(new QueuedIterator<R>(queueSize) {
            A nextA = null;
            B nextB = null;

            @Override
            public boolean hasNext() {
                try {
                    while (nextA == null && onGoing.booleanValue() && (threadCounterA.get() > 0 || queueA.size() > 0)) { // (threadCounterA.get() > 0 || queueA.size() > 0) is wrong. has to check counter first
                        nextA = queueA.poll(100, TimeUnit.MILLISECONDS);
                    }

                    if (nextA == null) {
                        onGoing.setFalse();

                        return false;
                    }

                    while (nextB == null && onGoing.booleanValue() && (threadCounterB.get() > 0 || queueB.size() > 0)) { // (threadCounterB.get() > 0 || queueB.size() > 0) is wrong. has to check counter first
                        nextB = queueB.poll(100, TimeUnit.MILLISECONDS);
                    }

                    if (nextB == null) {
                        onGoing.setFalse();

                        return false;
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                }

                if (errorHolder.value() != null) {
                    throwError(errorHolder, onGoing);
                }

                return true;
            }

            @Override
            public R next() {
                if ((nextA == null || nextB == null) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                boolean isOK = false;

                try {
                    final R result = combiner.apply(nextA == NONE ? null : nextA, nextB == NONE ? null : nextB);
                    nextA = null;
                    nextB = null;
                    isOK = true;
                    return result;
                } finally {
                    // error happened
                    if (isOK == false) {
                        onGoing.setFalse();
                    }
                }
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
            }
        });
    }

    public static <A, B, C, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c,
            final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return parallelZip(a, b, c, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, C, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c,
            final TriFunction<? super A, ? super B, ? super C, R> combiner, final int queueSize) {
        final AsyncExecutor asyncExecutor = new AsyncExecutor(3, 300L, TimeUnit.SECONDS);
        final AtomicInteger threadCounterA = new AtomicInteger(1);
        final AtomicInteger threadCounterB = new AtomicInteger(1);
        final AtomicInteger threadCounterC = new AtomicInteger(1);
        final BlockingQueue<A> queueA = new ArrayBlockingQueue<>(queueSize);
        final BlockingQueue<B> queueB = new ArrayBlockingQueue<>(queueSize);
        final BlockingQueue<C> queueC = new ArrayBlockingQueue<>(queueSize);
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);

        readToQueue(a, b, c, asyncExecutor, threadCounterA, threadCounterB, threadCounterC, queueA, queueB, queueC, errorHolder, onGoing);

        return of(new QueuedIterator<R>(queueSize) {
            A nextA = null;
            B nextB = null;
            C nextC = null;

            @Override
            public boolean hasNext() {
                try {
                    while (nextA == null && onGoing.booleanValue() && (threadCounterA.get() > 0 || queueA.size() > 0)) { // (threadCounterA.get() > 0 || queueA.size() > 0) is wrong. has to check counter first
                        nextA = queueA.poll(100, TimeUnit.MILLISECONDS);
                    }

                    if (nextA == null) {
                        onGoing.setFalse();

                        return false;
                    }

                    while (nextB == null && onGoing.booleanValue() && (threadCounterB.get() > 0 || queueB.size() > 0)) { // (threadCounterB.get() > 0 || queueB.size() > 0) is wrong. has to check counter first
                        nextB = queueB.poll(100, TimeUnit.MILLISECONDS);
                    }

                    if (nextB == null) {
                        onGoing.setFalse();

                        return false;
                    }

                    while (nextC == null && onGoing.booleanValue() && (threadCounterC.get() > 0 || queueC.size() > 0)) { // (threadCounterC.get() > 0 || queueC.size() > 0) is wrong. has to check counter first
                        nextC = queueC.poll(100, TimeUnit.MILLISECONDS);
                    }

                    if (nextC == null) {
                        onGoing.setFalse();

                        return false;
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                }

                if (errorHolder.value() != null) {
                    throwError(errorHolder, onGoing);
                }

                return true;
            }

            @Override
            public R next() {
                if ((nextA == null || nextB == null || nextC == null) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                boolean isOK = false;

                try {
                    final R result = combiner.apply(nextA == NONE ? null : nextA, nextB == NONE ? null : nextB, nextC == NONE ? null : nextC);
                    nextA = null;
                    nextB = null;
                    nextC = null;
                    isOK = true;
                    return result;
                } finally {
                    // error happened
                    if (isOK == false) {
                        onGoing.setFalse();
                    }
                }
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param c
     * @param combiner
     * @return
     */
    public static <R> Stream<R> parallelZip(final Collection<? extends Iterator<?>> c, final NFunction<R> combiner) {
        return parallelZip(c, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <R> Stream<R> parallelZip(final Collection<? extends Iterator<?>> c, final NFunction<R> combiner, final int queueSize) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        final int len = c.size();
        final AsyncExecutor asyncExecutor = new AsyncExecutor(len, 300L, TimeUnit.SECONDS);
        final AtomicInteger[] counters = new AtomicInteger[len];
        final BlockingQueue<Object>[] queues = new ArrayBlockingQueue[len];
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);

        readToQueue(c, queueSize, asyncExecutor, counters, queues, errorHolder, onGoing);

        return of(new QueuedIterator<R>(queueSize) {
            Object[] next = null;

            @Override
            public boolean hasNext() {
                if (next == null) {
                    next = new Object[len];
                }

                for (int i = 0; i < len; i++) {
                    try {
                        while (next[i] == null && onGoing.booleanValue() && (counters[i].get() > 0 || queues[i].size() > 0)) { // (counters[i].get() > 0 || queues[i].size() > 0) is wrong. has to check counter first
                            next[i] = queues[i].poll(100, TimeUnit.MILLISECONDS);
                        }

                        if (next[i] == null) {
                            onGoing.setFalse();

                            return false;
                        }
                    } catch (Throwable e) {
                        setError(errorHolder, e, onGoing);
                    }

                    if (errorHolder.value() != null) {
                        throwError(errorHolder, onGoing);
                    }
                }

                return true;
            }

            @Override
            public R next() {
                if (next == null) {
                    if (hasNext() == false) {
                        throw new NoSuchElementException();
                    }
                } else {
                    for (int i = 0; i < len; i++) {
                        if (next[i] == null) {
                            if (hasNext() == false) {
                                throw new NoSuchElementException();
                            } else {
                                break;
                            }
                        }
                    }
                }

                for (int i = 0; i < len; i++) {
                    if (next[i] == NONE) {
                        next[i] = null;
                    }
                }

                boolean isOK = false;

                try {
                    R result = combiner.apply(next);
                    next = null;
                    isOK = true;
                    return result;
                } finally {
                    // error happened
                    if (isOK == false) {
                        onGoing.setFalse();
                    }
                }
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param valueForNoneA
     * @param valueForNoneB
     * @param combiner
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> combiner) {
        return parallelZip(a, b, valueForNoneA, valueForNoneB, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param valueForNoneA
     * @param valueForNoneB
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> combiner, final int queueSize) {
        return parallelZip(a.iterator(), b.iterator(), valueForNoneA, valueForNoneB, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA
     * @param valueForNoneB
     * @param valueForNoneC
     * @param combiner
     * @return
     */
    public static <A, B, C, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final Stream<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return parallelZip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA
     * @param valueForNoneB
     * @param valueForNoneC
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, C, R> Stream<R> parallelZip(final Stream<A> a, final Stream<B> b, final Stream<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> combiner, final int queueSize) {
        return parallelZip(a.iterator(), b.iterator(), c.iterator(), valueForNoneA, valueForNoneB, valueForNoneC, combiner).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                try {
                    a.close();
                } catch (Throwable throwable) {
                    runtimeException = N.toRuntimeException(throwable);
                }

                try {
                    b.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                try {
                    c.close();
                } catch (Throwable throwable) {
                    if (runtimeException == null) {
                        runtimeException = N.toRuntimeException(throwable);
                    } else {
                        runtimeException.addSuppressed(throwable);
                    }
                }

                if (runtimeException != null) {
                    throw runtimeException;
                }
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param valueForNoneA
     * @param valueForNoneB
     * @param combiner
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> combiner) {
        return parallelZip(a, b, valueForNoneA, valueForNoneB, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param valueForNoneA
     * @param valueForNoneB
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> combiner, final int queueSize) {
        final AsyncExecutor asyncExecutor = new AsyncExecutor(2, 300L, TimeUnit.SECONDS);
        final AtomicInteger threadCounterA = new AtomicInteger(1);
        final AtomicInteger threadCounterB = new AtomicInteger(1);
        final BlockingQueue<A> queueA = new ArrayBlockingQueue<>(queueSize);
        final BlockingQueue<B> queueB = new ArrayBlockingQueue<>(queueSize);
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);

        readToQueue(a, b, asyncExecutor, threadCounterA, threadCounterB, queueA, queueB, errorHolder, onGoing);

        return of(new QueuedIterator<R>(queueSize) {
            A nextA = null;
            B nextB = null;

            @Override
            public boolean hasNext() {
                try {
                    while (nextA == null && onGoing.booleanValue() && (threadCounterA.get() > 0 || queueA.size() > 0)) { // (threadCounterA.get() > 0 || queueA.size() > 0) is wrong. has to check counter first
                        nextA = queueA.poll(100, TimeUnit.MILLISECONDS);
                    }

                    while (nextB == null && onGoing.booleanValue() && (threadCounterB.get() > 0 || queueB.size() > 0)) { // (threadCounterB.get() > 0 || queueB.size() > 0) is wrong. has to check counter first
                        nextB = queueB.poll(100, TimeUnit.MILLISECONDS);
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                }

                if (errorHolder.value() != null) {
                    throwError(errorHolder, onGoing);
                }

                if (nextA != null || nextB != null) {
                    return true;
                } else {
                    onGoing.setFalse();
                    return false;
                }
            }

            @Override
            public R next() {
                if ((nextA == null && nextB == null) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                nextA = nextA == NONE ? null : (nextA == null ? valueForNoneA : nextA);
                nextB = nextB == NONE ? null : (nextB == null ? valueForNoneB : nextB);
                boolean isOK = false;

                try {
                    final R result = combiner.apply(nextA, nextB);
                    nextA = null;
                    nextB = null;
                    isOK = true;
                    return result;
                } finally {
                    // error happened
                    if (isOK == false) {
                        onGoing.setFalse();
                    }
                }
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA
     * @param valueForNoneB
     * @param valueForNoneC
     * @param combiner
     * @return
     */
    public static <A, B, C, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c,
            final A valueForNoneA, final B valueForNoneB, final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> combiner) {
        return parallelZip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA
     * @param valueForNoneB
     * @param valueForNoneC
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <A, B, C, R> Stream<R> parallelZip(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c,
            final A valueForNoneA, final B valueForNoneB, final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> combiner,
            final int queueSize) {
        final AsyncExecutor asyncExecutor = new AsyncExecutor(3, 300L, TimeUnit.SECONDS);
        final AtomicInteger threadCounterA = new AtomicInteger(1);
        final AtomicInteger threadCounterB = new AtomicInteger(1);
        final AtomicInteger threadCounterC = new AtomicInteger(1);
        final BlockingQueue<A> queueA = new ArrayBlockingQueue<>(queueSize);
        final BlockingQueue<B> queueB = new ArrayBlockingQueue<>(queueSize);
        final BlockingQueue<C> queueC = new ArrayBlockingQueue<>(queueSize);
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);

        readToQueue(a, b, c, asyncExecutor, threadCounterA, threadCounterB, threadCounterC, queueA, queueB, queueC, errorHolder, onGoing);

        return of(new QueuedIterator<R>(queueSize) {
            A nextA = null;
            B nextB = null;
            C nextC = null;

            @Override
            public boolean hasNext() {
                try {
                    while (nextA == null && onGoing.booleanValue() && (threadCounterA.get() > 0 || queueA.size() > 0)) { // (threadCounterA.get() > 0 || queueA.size() > 0) is wrong. has to check counter first
                        nextA = queueA.poll(100, TimeUnit.MILLISECONDS);
                    }

                    while (nextB == null && onGoing.booleanValue() && (threadCounterB.get() > 0 || queueB.size() > 0)) { // (threadCounterB.get() > 0 || queueB.size() > 0) is wrong. has to check counter first
                        nextB = queueB.poll(100, TimeUnit.MILLISECONDS);
                    }

                    while (nextC == null && onGoing.booleanValue() && (threadCounterC.get() > 0 || queueC.size() > 0)) { // (threadCounterC.get() > 0 || queueC.size() > 0) is wrong. has to check counter first
                        nextC = queueC.poll(100, TimeUnit.MILLISECONDS);
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                }

                if (errorHolder.value() != null) {
                    throwError(errorHolder, onGoing);
                }

                if (nextA != null || nextB != null || nextC != null) {
                    return true;
                } else {
                    onGoing.setFalse();

                    return false;
                }
            }

            @Override
            public R next() {
                if ((nextA == null && nextB == null && nextC == null) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                nextA = nextA == NONE ? null : (nextA == null ? valueForNoneA : nextA);
                nextB = nextB == NONE ? null : (nextB == null ? valueForNoneB : nextB);
                nextC = nextC == NONE ? null : (nextC == null ? valueForNoneC : nextC);
                boolean isOK = false;

                try {
                    final R result = combiner.apply(nextA, nextB, nextC);
                    nextA = null;
                    nextB = null;
                    nextC = null;
                    isOK = true;
                    return result;
                } finally {
                    // error happened
                    if (isOK == false) {
                        onGoing.setFalse();
                    }
                }
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
            }
        });
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param c
     * @param valuesForNone
     * @param combiner
     * @return
     */
    public static <R> Stream<R> parallelZip(final Collection<? extends Iterator<?>> c, final Object[] valuesForNone, final NFunction<R> combiner) {
        return parallelZip(c, valuesForNone, combiner, DEFAULT_QUEUE_SIZE);
    }

    /**
     * Put the stream in try-catch to stop the back-end reading thread if error happens
     * <br />
     * <code>
     * try (Stream<Integer> stream = Stream.parallelZip(a, b, combiner)) {
     *            stream.forEach(N::println);
     *        }
     * </code>
     * 
     * @param c
     * @param valuesForNone
     * @param combiner
     * @param queueSize for each iterator. Default value is 8
     * @return
     */
    public static <R> Stream<R> parallelZip(final Collection<? extends Iterator<?>> c, final Object[] valuesForNone, final NFunction<R> combiner,
            final int queueSize) {
        if (N.isNullOrEmpty(c)) {
            return Stream.empty();
        }

        if (c.size() != valuesForNone.length) {
            throw new IllegalArgumentException("The size of 'valuesForNone' must be same as the size of the collection of iterators");
        }

        final int len = c.size();
        final AsyncExecutor asyncExecutor = new AsyncExecutor(len, 300L, TimeUnit.SECONDS);
        final Holder<Throwable> errorHolder = new Holder<>();
        final MutableBoolean onGoing = MutableBoolean.of(true);
        final AtomicInteger[] counters = new AtomicInteger[len];
        final BlockingQueue<Object>[] queues = new ArrayBlockingQueue[len];

        readToQueue(c, queueSize, asyncExecutor, counters, queues, errorHolder, onGoing);

        return of(new QueuedIterator<R>(queueSize) {
            Object[] next = null;

            @Override
            public boolean hasNext() {
                if (next == null) {
                    next = new Object[len];
                }

                for (int i = 0; i < len; i++) {
                    try {
                        while (next[i] == null && onGoing.booleanValue() && (counters[i].get() > 0 || queues[i].size() > 0)) { // (counters[i].get() > 0 || queues[i].size() > 0) is wrong. has to check counter first
                            next[i] = queues[i].poll(100, TimeUnit.MILLISECONDS);
                        }
                    } catch (Throwable e) {
                        setError(errorHolder, e, onGoing);
                    }

                    if (errorHolder.value() != null) {
                        throwError(errorHolder, onGoing);
                    }
                }

                for (int i = 0; i < len; i++) {
                    if (next[i] != null) {
                        return true;
                    }
                }

                onGoing.setFalse();
                return false;
            }

            @Override
            public R next() {
                if (next == null) {
                    if (hasNext() == false) {
                        throw new NoSuchElementException();
                    }
                } else {
                    for (int i = 0; i < len; i++) {
                        if (next[i] == null) {
                            if (hasNext() == false) {
                                throw new NoSuchElementException();
                            } else {
                                break;
                            }
                        }
                    }
                }

                for (int i = 0; i < len; i++) {
                    next[i] = next[i] == NONE ? null : (next[i] == null ? valuesForNone[i] : next[i]);
                }

                boolean isOK = false;
                try {
                    R result = combiner.apply(next);
                    next = null;
                    isOK = true;
                    return result;
                } finally {
                    // error happened
                    if (isOK == false) {
                        onGoing.setFalse();
                    }
                }
            }
        }).onClose(new Runnable() {
            @Override
            public void run() {
                onGoing.setFalse();
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
    public static <T> Stream<T> merge(final T[] a, final T[] b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return of(b);
        } else if (N.isNullOrEmpty(b)) {
            return of(a);
        }

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private final int lenA = a.length;
            private final int lenB = b.length;
            private int cursorA = 0;
            private int cursorB = 0;

            @Override
            public boolean hasNext() {
                return cursorA < lenA || cursorB < lenB;
            }

            @Override
            public T next() {
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
    public static <T> Stream<T> merge(final Stream<? extends T> a, final Stream<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        final Iterator<? extends T> iterA = a.iterator();
        final Iterator<? extends T> iterB = b.iterator();

        if (iterA.hasNext() == false) {
            return (Stream<T>) b;
        } else if (iterB.hasNext() == false) {
            return (Stream<T>) a;
        }

        return merge(iterA, iterB, nextSelector).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (Stream<? extends T> stream : N.asList(a, b)) {
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
    public static <T> Stream<T> merge(final Iterator<? extends T> a, final Iterator<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        if (a.hasNext() == false) {
            return of(b);
        } else if (b.hasNext() == false) {
            return of(a);
        }

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private T nextA = null;
            private T nextB = null;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || hasNextA || hasNextB;
            }

            @Override
            public T next() {
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
    public static <T> Stream<T> merge(final Stream<? extends T>[] a, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return (Stream<T>) a[0];
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        final Iterator<? extends T>[] iters = new Iterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iters[i] = a[i].iterator();
        }

        return merge(iters, nextSelector).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (Stream<? extends T> stream : a) {
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
    public static <T> Stream<T> merge(final Iterator<? extends T>[] a, final BiFunction<? super T, ? super T, Nth> nextSelector) {
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
    public static <T> Stream<T> merge(final Collection<? extends Iterator<? extends T>> c, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return of(c.iterator().next());
        } else if (c.size() == 2) {
            final Iterator<? extends Iterator<? extends T>> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        }

        final Iterator<? extends Iterator<? extends T>> iter = c.iterator();
        Stream<T> result = merge(iter.next(), iter.next(), nextSelector);

        while (iter.hasNext()) {
            result = merge(result.iterator(), iter.next(), nextSelector);
        }

        return result;
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static <T> Stream<T> parallelMerge(final Stream<? extends T>[] a, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return parallelMerge(a, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static <T> Stream<T> parallelMerge(final Stream<? extends T>[] a, final BiFunction<? super T, ? super T, Nth> nextSelector, final int maxThreadNum) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("maxThreadNum can be less than 1");
        }

        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return (Stream<T>) a[0];
        } else if (a.length == 2) {
            return merge(a[0], a[1], nextSelector);
        }

        final Iterator<? extends T>[] iters = new Iterator[a.length];

        for (int i = 0, len = a.length; i < len; i++) {
            iters[i] = a[i].iterator();
        }

        return parallelMerge(iters, nextSelector, maxThreadNum).onClose(new Runnable() {
            @Override
            public void run() {
                RuntimeException runtimeException = null;

                for (Stream<? extends T> stream : a) {
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
    public static <T> Stream<T> parallelMerge(final Iterator<? extends T>[] a, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return parallelMerge(a, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param a
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static <T> Stream<T> parallelMerge(final Iterator<? extends T>[] a, final BiFunction<? super T, ? super T, Nth> nextSelector,
            final int maxThreadNum) {
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
    public static <T> Stream<T> parallelMerge(final Collection<? extends Iterator<? extends T>> c, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return parallelMerge(c, nextSelector, Stream.DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static <T> Stream<T> parallelMerge(final Collection<? extends Iterator<? extends T>> c, final BiFunction<? super T, ? super T, Nth> nextSelector,
            final int maxThreadNum) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("maxThreadNum can be less than 1");
        }

        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return of(c.iterator().next());
        } else if (c.size() == 2) {
            final Iterator<? extends Iterator<? extends T>> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        } else if (maxThreadNum <= 1) {
            return merge(c, nextSelector);
        }

        final Queue<Iterator<? extends T>> queue = new LinkedList<>();
        queue.addAll(c);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt cnt = MutableInt.of(c.size());
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(c.size() - 1);

        for (int i = 0, n = N.min(maxThreadNum, c.size() / 2 + 1); i < n; i++) {
            futureList.add(Stream.asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    Iterator<? extends T> a = null;
                    Iterator<? extends T> b = null;
                    Iterator<? extends T> c = null;

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

                            c = (Iterator<? extends T>) ImmutableIterator.of(merge(a, b, nextSelector).toArray());

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

        return Stream.of(queue.poll());
    }

    private static <B, A> void readToQueue(final Iterator<? extends A> a, final Iterator<? extends B> b, final AsyncExecutor asyncExecutor,
            final AtomicInteger threadCounterA, final AtomicInteger threadCounterB, final BlockingQueue<A> queueA, final BlockingQueue<B> queueB,
            final Holder<Throwable> errorHolder, final MutableBoolean onGoing) {
        asyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    A nextA = null;

                    while (onGoing.booleanValue() && a.hasNext()) {
                        nextA = a.next();

                        if (nextA == null) {
                            nextA = (A) NONE;
                        }

                        while (onGoing.booleanValue() && queueA.offer(nextA, 100, TimeUnit.MILLISECONDS) == false) {
                            // continue
                        }
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                } finally {
                    threadCounterA.decrementAndGet();
                }
            }
        });

        asyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    B nextB = null;

                    while (onGoing.booleanValue() && b.hasNext()) {
                        nextB = b.next();

                        if (nextB == null) {
                            nextB = (B) NONE;
                        }

                        while (onGoing.booleanValue() && queueB.offer(nextB, 100, TimeUnit.MILLISECONDS) == false) {
                            // continue
                        }
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                } finally {
                    threadCounterB.decrementAndGet();
                }
            }
        });
    }

    private static <B, C, A> void readToQueue(final Iterator<? extends A> a, final Iterator<? extends B> b, final Iterator<? extends C> c,
            final AsyncExecutor asyncExecutor, final AtomicInteger threadCounterA, final AtomicInteger threadCounterB, final AtomicInteger threadCounterC,
            final BlockingQueue<A> queueA, final BlockingQueue<B> queueB, final BlockingQueue<C> queueC, final Holder<Throwable> errorHolder,
            final MutableBoolean onGoing) {
        asyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    A nextA = null;

                    while (onGoing.booleanValue() && a.hasNext()) {
                        nextA = a.next();

                        if (nextA == null) {
                            nextA = (A) NONE;
                        }

                        while (onGoing.booleanValue() && queueA.offer(nextA, 100, TimeUnit.MILLISECONDS) == false) {
                            // continue
                        }
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                } finally {
                    threadCounterA.decrementAndGet();
                }
            }
        });

        asyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    B nextB = null;

                    while (onGoing.booleanValue() && b.hasNext()) {
                        nextB = b.next();

                        if (nextB == null) {
                            nextB = (B) NONE;
                        }

                        while (onGoing.booleanValue() && queueB.offer(nextB, 100, TimeUnit.MILLISECONDS) == false) {
                            // continue
                        }
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                } finally {
                    threadCounterB.decrementAndGet();
                }
            }
        });

        asyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    C nextC = null;

                    while (onGoing.booleanValue() && c.hasNext()) {
                        nextC = c.next();

                        if (nextC == null) {
                            nextC = (C) NONE;
                        }

                        while (onGoing.booleanValue() && queueC.offer(nextC, 100, TimeUnit.MILLISECONDS) == false) {
                            // continue
                        }
                    }
                } catch (Throwable e) {
                    setError(errorHolder, e, onGoing);
                } finally {
                    threadCounterC.decrementAndGet();
                }
            }
        });
    }

    private static void readToQueue(final Collection<? extends Iterator<?>> c, final int queueSize, final AsyncExecutor asyncExecutor,
            final AtomicInteger[] counters, final BlockingQueue<Object>[] queues, final Holder<Throwable> errorHolder, final MutableBoolean onGoing) {
        int idx = 0;

        for (Iterator<?> e : c) {
            counters[idx] = new AtomicInteger(1);
            queues[idx] = new ArrayBlockingQueue<>(queueSize);

            final Iterator<?> iter = e;
            final AtomicInteger count = counters[idx];
            final BlockingQueue<Object> queue = queues[idx];

            asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        Object next = null;

                        while (onGoing.booleanValue() && iter.hasNext()) {
                            next = iter.next();

                            if (next == null) {
                                next = NONE;
                            }

                            while (onGoing.booleanValue() && queue.offer(next, 100, TimeUnit.MILLISECONDS) == false) {
                                // continue
                            }
                        }
                    } catch (Throwable e) {
                        setError(errorHolder, e, onGoing);
                    } finally {
                        count.decrementAndGet();
                    }
                }
            });

            idx++;
        }
    }

    static void setError(final Holder<Throwable> errorHolder, Throwable e, final MutableBoolean onGoing) {
        onGoing.setFalse();

        synchronized (errorHolder) {
            if (errorHolder.value() == null) {
                errorHolder.setValue(e);
            } else {
                errorHolder.value().addSuppressed(e);
            }
        }
    }

    static void throwError(final Holder<Throwable> errorHolder, final MutableBoolean onGoing) {
        onGoing.setFalse();

        throw N.toRuntimeException(errorHolder.value());
    }

    static void setError(final Holder<Throwable> errorHolder, Throwable e) {
        synchronized (errorHolder) {
            if (errorHolder.value() == null) {
                errorHolder.setValue(e);
            } else {
                errorHolder.value().addSuppressed(e);
            }
        }
    }

    static void throwError(final Holder<Throwable> errorHolder) {
        throw N.toRuntimeException(errorHolder.value());
    }

    static int calculateQueueSize(int len) {
        return N.min(128, len * DEFAULT_QUEUE_SIZE);
    }

    static boolean isSameComparator(Comparator<?> a, Comparator<?> b) {
        if (a == b) {
            return true;
        } else if (a == null) {
            return defaultComparator.containsValue(b);
        } else if (b == null) {
            return defaultComparator.containsValue(a);
        }

        return false;
    }

    static void checkIndex(int fromIndex, int toIndex, int length) {
        if (fromIndex < 0 || toIndex < fromIndex || toIndex > length) {
            throw new IllegalArgumentException("Invalid fromIndex(" + fromIndex + ") or toIndex(" + toIndex + ")");
        }
    }

    static int toInt(long max) {
        return max > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) max;
    }

    static ImmutableCharIterator charIterator(final ImmutableIterator<Character> iter) {
        final ImmutableCharIterator charIter = new ImmutableCharIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public char next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return charIter;
    }

    static ImmutableByteIterator byteIterator(final ImmutableIterator<Byte> iter) {
        final ImmutableByteIterator byteIter = new ImmutableByteIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public byte next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return byteIter;
    }

    static ImmutableShortIterator shortIterator(final ImmutableIterator<Short> iter) {
        final ImmutableShortIterator shortIter = new ImmutableShortIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public short next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return shortIter;
    }

    static ImmutableIntIterator intIterator(final ImmutableIterator<Integer> iter) {
        final ImmutableIntIterator intIter = new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public int next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return intIter;
    }

    static ImmutableLongIterator longIterator(final ImmutableIterator<Long> iter) {
        final ImmutableLongIterator longIter = new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public long next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return longIter;
    }

    static ImmutableFloatIterator floatIterator(final ImmutableIterator<Float> iter) {
        final ImmutableFloatIterator floatIter = new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public float next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return floatIter;
    }

    static ImmutableDoubleIterator doubleIterator(final ImmutableIterator<Double> iter) {
        final ImmutableDoubleIterator doubleIter = new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public double next() {
                return iter.next();
            }

            @Override
            public long count() {
                return iter.count();
            }

            @Override
            public void skip(long n) {
                iter.skip(n);
            }
        };

        return doubleIter;
    }

    static void close(Collection<Runnable> closeHandlers) {
        if (N.notNullOrEmpty(closeHandlers)) {
            Throwable ex = null;

            for (Runnable closeHandler : closeHandlers) {
                try {
                    closeHandler.run();
                } catch (Throwable e) {
                    if (ex == null) {
                        ex = e;
                    } else {
                        ex.addSuppressed(e);
                    }
                }
            }

            if (ex != null) {
                throw N.toRuntimeException(ex);
            }
        }
    }

    static Set<Runnable> mergeCloseHandlers(final BaseStream<?, ?> stream, Set<Runnable> closeHandlers) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>();

        if (N.notNullOrEmpty(closeHandlers)) {
            newCloseHandlers.addAll(closeHandlers);
        }

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                stream.close();
            }
        });

        return newCloseHandlers;
    }
}
