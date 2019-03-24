/*
 * Copyright (C) 2019 HaiYang Li
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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Executor;

import com.landawn.abacus.exception.DuplicatedResultException;
import com.landawn.abacus.exception.UncheckedSQLException;
import com.landawn.abacus.util.Fn.Factory;
import com.landawn.abacus.util.Fn.Suppliers;
import com.landawn.abacus.util.JdbcUtil.BiRecordGetter;
import com.landawn.abacus.util.StringUtil.Strings;
import com.landawn.abacus.util.u.Optional;
import com.landawn.abacus.util.u.OptionalDouble;
import com.landawn.abacus.util.u.OptionalInt;
import com.landawn.abacus.util.u.OptionalLong;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.ObjIteratorEx;
import com.landawn.abacus.util.stream.Stream;

/**
 * The Stream will be automatically closed after execution(A terminal method is executed/triggered).
 * 
 * @since 1.3
 * 
 * @author Haiyang Li
 */
public class ExceptionalStream<T, E extends Exception> implements AutoCloseable {
    private final ExceptionalIterator<T, E> elements;
    private final boolean sorted;
    private final Comparator<? super T> comparator;
    private final Deque<Try.Runnable<? extends E>> closeHandlers;
    private boolean isClosed = false;

    ExceptionalStream(final ExceptionalIterator<T, E> iter, final Deque<Try.Runnable<? extends E>> closeHandlers) {
        this(iter, false, null, closeHandlers);
    }

    ExceptionalStream(final ExceptionalIterator<T, E> iter, final boolean sorted, final Comparator<? super T> comparator,
            final Deque<Try.Runnable<? extends E>> closeHandlers) {
        this.elements = iter;
        this.sorted = sorted;
        this.comparator = comparator;
        this.closeHandlers = closeHandlers;
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> empty() {
        return new ExceptionalStream<>(ExceptionalIterator.EMPTY, null);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> just(final T e) {
        return of(e);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> ofNullable(final T e) {
        if (e == null) {
            return empty();
        }

        return of(e);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        final int len = N.len(a);

        return newStream(new ExceptionalIterator<T, E>() {
            private int position = 0;

            @Override
            public boolean hasNext() throws E {
                return position < len;
            }

            @Override
            public T next() throws E {
                if (position >= len) {
                    throw new NoSuchElementException();
                }

                return a[position++];
            }

            @Override
            public long count() throws E {
                return len - position;
            }

            @Override
            public void skip(long n) throws E {
                N.checkArgNotNegative(n, "n");

                if (n > len - position) {
                    position = len;
                }

                position += n;
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return of(c.iterator());
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Iterator<? extends T> iter) {
        if (iter == null) {
            return empty();
        }

        return newStream(new ExceptionalIterator<T, E>() {
            @Override
            public boolean hasNext() throws E {
                return iter.hasNext();
            }

            @Override
            public T next() throws E {
                return iter.next();
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Iterable<? extends T> iterable) {
        if (iterable == null) {
            return empty();
        }

        return of(iterable.iterator());
    }

    public static <K, V, E extends Exception> ExceptionalStream<Map.Entry<K, V>, E> of(final Map<K, V> m) {
        if (m == null) {
            return empty();
        }

        return of(m.entrySet());
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Stream<? extends T> stream) {
        if (stream == null) {
            return empty();
        }

        final ExceptionalIterator<T, E> iter = new ExceptionalIterator<T, E>() {
            private Stream<? extends T> s = stream;
            private Iterator<? extends T> iter = null;
            private boolean isInitialized = false;

            @Override
            public boolean hasNext() throws E {
                if (isInitialized == false) {
                    init();
                }

                return iter.hasNext();
            }

            @Override
            public T next() throws E {
                if (isInitialized == false) {
                    init();
                }

                return iter.next();
            }

            @Override
            public void skip(long n) throws E {
                N.checkArgNotNegative(n, "n");

                if (iter == null) {
                    s = s.skip(n);
                } else {
                    super.skip(n);
                }
            }

            @Override
            public long count() throws E {
                if (iter == null) {
                    return s.count();
                } else {
                    return super.count();
                }
            }

            @Override
            public void close() throws E {
                s.close();
            }

            private void init() {
                if (isInitialized == false) {
                    isInitialized = true;
                    iter = stream.iterator();
                }
            }
        };

        return newStream(iter).onClose(new Try.Runnable<E>() {
            @Override
            public void run() throws E {
                iter.close();
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Collection<? extends T> c, final Class<E> exceptionType) {
        return of(c);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Iterator<? extends T> iter, final Class<E> exceptionType) {
        return of(iter);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Iterable<? extends T> iterable, final Class<E> exceptionType) {
        return of(iterable);
    }

    public static <K, V, E extends Exception> ExceptionalStream<Map.Entry<K, V>, E> of(final Map<K, V> m, final Class<E> exceptionType) {
        return of(m);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Stream<? extends T> stream, final Class<E> exceptionType) {
        return of(stream);
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> iterate(final Try.BooleanSupplier<? extends E> hasNext,
            final Try.Supplier<? extends T, E> next) {
        N.checkArgNotNull(hasNext, "hasNext");
        N.checkArgNotNull(next, "next");

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() throws E {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.getAsBoolean();
                }

                return hasNextVal;
            }

            @Override
            public T next() throws E {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return next.get();
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> iterate(final T seed, final Try.BooleanSupplier<? extends E> hasNext,
            final Try.UnaryOperator<T, ? extends E> f) {
        N.checkArgNotNull(hasNext, "hasNext");
        N.checkArgNotNull(f, "f");

        return newStream(new ExceptionalIterator<T, E>() {
            private final T NONE = (T) N.NULL_MASK;
            private T t = NONE;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() throws E {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.getAsBoolean();
                }

                return hasNextVal;
            }

            @Override
            public T next() throws E {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return t = (t == NONE) ? seed : f.apply(t);
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> iterate(final T seed, final Try.Predicate<T, ? extends E> hasNext,
            final Try.UnaryOperator<T, ? extends E> f) {
        N.checkArgNotNull(hasNext, "hasNext");
        N.checkArgNotNull(f, "f");

        return newStream(new ExceptionalIterator<T, E>() {
            private final T NONE = (T) N.NULL_MASK;
            private T t = NONE;
            private T cur = NONE;
            private boolean hasMore = true;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() throws E {
                if (hasNextVal == false && hasMore) {
                    hasNextVal = hasNext.test((cur = (t == NONE ? seed : f.apply(t))));

                    if (hasNextVal == false) {
                        hasMore = false;
                    }
                }

                return hasNextVal;
            }

            @Override
            public T next() throws E {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                t = cur;
                cur = NONE;
                hasNextVal = false;
                return t;
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> iterate(final T seed, final Try.UnaryOperator<T, ? extends E> f) {
        N.checkArgNotNull(f, "f");

        return newStream(new ExceptionalIterator<T, E>() {
            private final T NONE = (T) N.NULL_MASK;
            private T t = NONE;

            @Override
            public boolean hasNext() throws E {
                return true;
            }

            @Override
            public T next() throws E {
                return t = t == NONE ? seed : f.apply(t);
            }
        });
    }

    public static ExceptionalStream<String, IOException> lines(final File file) {
        return lines(file, Charsets.UTF_8);
    }

    public static ExceptionalStream<String, IOException> lines(final File file, final Charset charset) {
        N.checkArgNotNull(file, "file");

        final ExceptionalIterator<String, IOException> iter = createLazyLineIterator(file, null, charset, null, true);

        return newStream(iter).onClose(new Try.Runnable<IOException>() {
            @Override
            public void run() throws IOException {
                iter.close();
            }
        });
    }

    public static ExceptionalStream<String, IOException> lines(final Path path) {
        return lines(path, Charsets.UTF_8);
    }

    public static ExceptionalStream<String, IOException> lines(final Path path, final Charset charset) {
        N.checkArgNotNull(path, "path");

        final ExceptionalIterator<String, IOException> iter = createLazyLineIterator(null, path, charset, null, true);

        return newStream(iter).onClose(new Try.Runnable<IOException>() {
            @Override
            public void run() throws IOException {
                iter.close();
            }
        });
    }

    public static ExceptionalStream<String, IOException> lines(final Reader reader) {
        N.checkArgNotNull(reader, "reader");

        return newStream(createLazyLineIterator(null, null, Charsets.UTF_8, reader, false));
    }

    private static ExceptionalIterator<String, IOException> createLazyLineIterator(final File file, final Path path, final Charset charset, final Reader reader,
            final boolean closeReader) {
        return ExceptionalIterator.of(new Try.Supplier<ExceptionalIterator<String, IOException>, IOException>() {
            private ExceptionalIterator<String, IOException> lazyIter = null;

            @Override
            public synchronized ExceptionalIterator<String, IOException> get() {
                if (lazyIter == null) {
                    lazyIter = new ExceptionalIterator<String, IOException>() {
                        private BufferedReader bufferedReader;

                        {
                            if (reader != null) {
                                bufferedReader = reader instanceof BufferedReader ? ((BufferedReader) reader) : new BufferedReader(reader);
                            } else if (file != null) {
                                bufferedReader = IOUtil.newBufferedReader(file, charset == null ? Charsets.UTF_8 : charset);
                            } else {
                                bufferedReader = IOUtil.newBufferedReader(path, charset == null ? Charsets.UTF_8 : charset);
                            }
                        }

                        private String cachedLine;
                        private boolean finished = false;

                        @Override
                        public boolean hasNext() throws IOException {
                            if (this.cachedLine != null) {
                                return true;
                            } else if (this.finished) {
                                return false;
                            } else {
                                this.cachedLine = this.bufferedReader.readLine();
                                if (this.cachedLine == null) {
                                    this.finished = true;
                                    return false;
                                } else {
                                    return true;
                                }
                            }
                        }

                        @Override
                        public String next() throws IOException {
                            if (!this.hasNext()) {
                                throw new NoSuchElementException("No more lines");
                            } else {
                                String res = this.cachedLine;
                                this.cachedLine = null;
                                return res;
                            }
                        }

                        @Override
                        public void close() throws IOException {
                            if (closeReader) {
                                IOUtil.close(reader);
                            }
                        }
                    };
                }

                return lazyIter;
            }
        });
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @return
     */
    public static ExceptionalStream<Object[], SQLException> rows(final ResultSet resultSet) {
        return rows(Object[].class, resultSet);
    }

    /**
     * 
     * @param resultSet
     * @param closeResultSet
     * @return
     * @deprecated
     */
    @Deprecated
    static ExceptionalStream<Object[], SQLException> rows(final ResultSet resultSet, final boolean closeResultSet) {
        return rows(Object[].class, resultSet, closeResultSet);
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param targetClass Array/List/Map or Entity with getter/setter methods.
     * @param resultSet
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> rows(final Class<T> targetClass, final ResultSet resultSet) {
        N.checkArgNotNull(targetClass, "targetClass");
        N.checkArgNotNull(resultSet, "resultSet");

        final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter = new Try.BiFunction<ResultSet, List<String>, T, SQLException>() {
            private final BiRecordGetter<T, RuntimeException> biRecordGetter = BiRecordGetter.to(targetClass);

            @Override
            public T apply(ResultSet resultSet, List<String> columnLabels) throws SQLException {
                return biRecordGetter.apply(resultSet, columnLabels);
            }
        };

        return rows(resultSet, recordGetter);
    }

    /**
     * 
     * @param targetClass Array/List/Map or Entity with getter/setter methods.
     * @param resultSet
     * @param closeResultSet
     * @return
     * @deprecated
     */
    @Deprecated
    static <T> ExceptionalStream<T, SQLException> rows(final Class<T> targetClass, final ResultSet resultSet, final boolean closeResultSet) {
        N.checkArgNotNull(targetClass, "targetClass");
        N.checkArgNotNull(resultSet, "resultSet");

        if (closeResultSet) {
            return rows(targetClass, resultSet).onClose(new Try.Runnable<SQLException>() {
                @Override
                public void run() throws SQLException {
                    JdbcUtil.closeQuietly(resultSet);
                }
            });
        } else {
            return rows(targetClass, resultSet);
        }
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @param recordGetter
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> rows(final ResultSet resultSet, final Try.Function<ResultSet, T, SQLException> recordGetter) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNull(recordGetter, "recordGetter");

        final ExceptionalIterator<T, SQLException> iter = new ExceptionalIterator<T, SQLException>() {
            private boolean hasNext;

            @Override
            public boolean hasNext() throws SQLException {
                if (hasNext == false) {
                    hasNext = resultSet.next();
                }

                return hasNext;
            }

            @Override
            public T next() throws SQLException {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return recordGetter.apply(resultSet);
            }

            @Override
            public void skip(long n) throws SQLException {
                N.checkArgNotNegative(n, "n");

                final long m = hasNext ? n - 1 : n;

                JdbcUtil.skip(resultSet, m);

                hasNext = false;
            }
        };

        return newStream(iter);
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @param recordGetter
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> rows(final ResultSet resultSet,
            final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNull(recordGetter, "recordGetter");

        final ExceptionalIterator<T, SQLException> iter = new ExceptionalIterator<T, SQLException>() {
            private List<String> columnLabels = null;
            private boolean hasNext;

            @Override
            public boolean hasNext() throws SQLException {
                if (hasNext == false) {
                    hasNext = resultSet.next();
                }

                return hasNext;
            }

            @Override
            public T next() throws SQLException {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                if (columnLabels == null) {
                    columnLabels = JdbcUtil.getColumnLabelList(resultSet);
                }

                return recordGetter.apply(resultSet, columnLabels);
            }

            @Override
            public void skip(long n) throws SQLException {
                N.checkArgNotNegative(n, "n");

                final long m = hasNext ? n - 1 : n;

                JdbcUtil.skip(resultSet, m);

                hasNext = false;
            }

            @Override
            public long count() throws SQLException {
                long cnt = 0;

                while (resultSet.next()) {
                    cnt++;
                }

                return cnt;
            }
        };

        return newStream(iter);
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @param columnIndex starts from 0, not 1.
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> rows(final ResultSet resultSet, final int columnIndex) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNegative(columnIndex, "columnIndex");

        final ExceptionalIterator<T, SQLException> iter = new ExceptionalIterator<T, SQLException>() {
            private final int newColumnIndex = columnIndex + 1;
            private boolean hasNext = false;

            @Override
            public boolean hasNext() throws SQLException {
                if (hasNext == false) {
                    hasNext = resultSet.next();
                }

                return hasNext;
            }

            @Override
            public T next() throws SQLException {
                if (!hasNext()) {
                    throw new NoSuchElementException("No more rows");
                }

                final T next = (T) JdbcUtil.getColumnValue(resultSet, newColumnIndex);
                hasNext = false;
                return next;
            }

            @Override
            public void skip(long n) throws SQLException {
                N.checkArgNotNegative(n, "n");

                final long m = hasNext ? n - 1 : n;

                JdbcUtil.skip(resultSet, m);

                hasNext = false;
            }
        };

        return newStream(iter);
    }

    /**
     * 
     * @param resultSet
     * @param columnIndex starts from 0, not 1.
     * @param closeResultSet
     * @return
     * @deprecated
     */
    @Deprecated
    static <T> ExceptionalStream<T, SQLException> rows(final ResultSet resultSet, final int columnIndex, final boolean closeResultSet) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNegative(columnIndex, "columnIndex");

        if (closeResultSet) {
            return (ExceptionalStream<T, SQLException>) rows(resultSet, columnIndex).onClose(new Try.Runnable<SQLException>() {
                @Override
                public void run() throws SQLException {
                    JdbcUtil.closeQuietly(resultSet);
                }
            });
        } else {
            return rows(resultSet, columnIndex);
        }
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @param columnName
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> rows(final ResultSet resultSet, final String columnName) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNull(columnName, "columnName");

        return rows(resultSet, getColumnIndex(resultSet, columnName));
    }

    /**
     * 
     * @param resultSet
     * @param columnName
     * @param closeResultSet
     * @return
     * @deprecated
     */
    @Deprecated
    static <T> ExceptionalStream<T, SQLException> rows(final ResultSet resultSet, final String columnName, final boolean closeResultSet) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNull(columnName, "columnName");

        return rows(resultSet, getColumnIndex(resultSet, columnName), closeResultSet);
    }

    private static int getColumnIndex(final ResultSet resultSet, final String columnName) {
        int columnIndex = -1;

        try {
            final ResultSetMetaData rsmd = resultSet.getMetaData();
            final int columnCount = rsmd.getColumnCount();

            for (int i = 1; i <= columnCount; i++) {
                if (JdbcUtil.getColumnLabel(rsmd, i).equals(columnName)) {
                    columnIndex = i - 1;
                    break;
                }
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }

        N.checkArgument(columnIndex >= 0, "No column found by name %s", columnName);

        return columnIndex;
    }

    @SafeVarargs
    public static <T, E extends Exception> ExceptionalStream<T, E> concat(final ExceptionalStream<? extends T, E>... a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        return concat(N.asList(a));
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> concat(final Collection<? extends ExceptionalStream<? extends T, E>> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final Deque<Try.Runnable<? extends E>> closeHandlers = new ArrayDeque<>();

        for (ExceptionalStream<? extends T, E> e : c) {
            if (N.notNullOrEmpty(e.closeHandlers)) {
                closeHandlers.addAll(e.closeHandlers);
            }
        }

        return newStream(new ExceptionalIterator<T, E>() {
            private final Iterator<? extends ExceptionalStream<? extends T, E>> iterators = c.iterator();
            private ExceptionalStream<? extends T, E> cur;
            private ExceptionalIterator<? extends T, E> iter;

            @Override
            public boolean hasNext() throws E {
                while ((iter == null || iter.hasNext() == false) && iterators.hasNext()) {
                    if (cur != null) {
                        cur.close();
                    }

                    cur = iterators.next();
                    iter = cur.elements;
                }

                return iter != null && iter.hasNext();
            }

            @Override
            public T next() throws E {
                if ((iter == null || iter.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return iter.next();
            }
        }, closeHandlers);
    }

    public ExceptionalStream<T, E> filter(final Try.Predicate<? super T, ? extends E> predicate) {
        checkArgNotNull(predicate, "predicate");

        return newStream(new ExceptionalIterator<T, E>() {
            private final T NONE = (T) N.NULL_MASK;
            private T next = NONE;

            @Override
            public boolean hasNext() throws E {
                while (next == NONE && elements.hasNext()) {
                    next = elements.next();

                    if (predicate.test(next)) {
                        break;
                    }
                }

                return next != NONE;
            }

            @Override
            public T next() throws E {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final T result = next;
                next = NONE;
                return result;
            }
        }, sorted, comparator, closeHandlers);
    }

    public ExceptionalStream<T, E> takeWhile(final Try.Predicate<? super T, ? extends E> predicate) {
        checkArgNotNull(predicate, "predicate");

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() throws E {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.next();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public T next() throws E {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, sorted, comparator, closeHandlers);
    }

    public ExceptionalStream<T, E> dropWhile(final Try.Predicate<? super T, ? extends E> predicate) {
        checkArgNotNull(predicate, "predicate");

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean hasNext = false;
            private T next = null;
            private boolean dropped = false;

            @Override
            public boolean hasNext() throws E {
                if (hasNext == false) {
                    if (dropped == false) {
                        dropped = true;

                        while (elements.hasNext()) {
                            next = elements.next();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }
                    } else if (elements.hasNext()) {
                        next = elements.next();
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public T next() throws E {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted, comparator, closeHandlers);
    }

    /**
     * Distinct and filter by occurrences.
     * 
     * @param occurrencesFilter
     * @return
     */
    public ExceptionalStream<T, E> distinct() {
        final Set<Object> set = new HashSet<>();

        return filter(new Try.Predicate<T, E>() {
            @Override
            public boolean test(T value) {
                return set.add(hashKey(value));
            }
        });
    }

    /**
     * Distinct by the value mapped from <code>keyExtractor</code> 
     * 
     * @param keyExtractor don't change value of the input parameter.
     * @return
     */
    public ExceptionalStream<T, E> distinctBy(final Try.Function<? super T, ?, ? extends E> keyExtractor) {
        checkArgNotNull(keyExtractor, "keyExtractor");

        final Set<Object> set = new HashSet<>();

        return filter(new Try.Predicate<T, E>() {
            @Override
            public boolean test(T value) throws E {
                return set.add(hashKey(keyExtractor.apply(value)));
            }
        });
    }

    public <U> ExceptionalStream<U, E> map(final Try.Function<? super T, ? extends U, ? extends E> mapper) {
        checkArgNotNull(mapper, "mapper");

        return newStream(new ExceptionalIterator<U, E>() {
            @Override
            public boolean hasNext() throws E {
                return elements.hasNext();
            }

            @Override
            public U next() throws E {
                return mapper.apply(elements.next());
            }
        }, closeHandlers);
    }

    public <R> ExceptionalStream<R, E> flatMap(final Try.Function<? super T, ? extends ExceptionalStream<? extends R, ? extends E>, ? extends E> mapper) {
        checkArgNotNull(mapper, "mapper");

        final ExceptionalIterator<R, E> iter = new ExceptionalIterator<R, E>() {
            private ExceptionalIterator<? extends R, ? extends E> cur = null;
            private ExceptionalStream<? extends R, ? extends E> s = null;
            private Try.Runnable<E> closeHandle = null;

            @Override
            public boolean hasNext() throws E {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Try.Runnable<E> tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.next());

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        closeHandle = new Try.Runnable<E>() {
                            @Override
                            public void run() throws E {
                                s.close();
                            }
                        };
                    }

                    cur = s.elements;
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public R next() throws E {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }

            @Override
            public void close() throws E {
                if (closeHandle != null) {
                    final Try.Runnable<E> tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Deque<Try.Runnable<? extends E>> newCloseHandlers = new ArrayDeque<>(N.size(closeHandlers) + 1);

        newCloseHandlers.add(new Try.Runnable<E>() {
            @Override
            public void run() throws E {
                iter.close();
            }
        });

        if (N.notNullOrEmpty(closeHandlers)) {
            newCloseHandlers.addAll(closeHandlers);
        }

        return newStream(iter, newCloseHandlers);
    }

    public <R> ExceptionalStream<R, E> flattMap(final Try.Function<? super T, ? extends Collection<? extends R>, ? extends E> mapper) {
        checkArgNotNull(mapper, "mapper");

        return flatMap(new Try.Function<T, ExceptionalStream<? extends R, ? extends E>, E>() {
            @Override
            public ExceptionalStream<? extends R, ? extends E> apply(T t) throws E {
                return ExceptionalStream.of(mapper.apply(t));
            }
        });
    }

    /**
     * 
     * @param classifier
     * @return
     */
    public <K> ExceptionalStream<Map.Entry<K, List<T>>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier) {
        return groupBy(classifier, Suppliers.<K, List<T>> ofMap());
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     */
    public <K> ExceptionalStream<Map.Entry<K, List<T>>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Supplier<? extends Map<K, List<T>>> mapFactory) {
        return groupBy(classifier, Fn.FN.<T, E> identity(), mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public <K, V> ExceptionalStream<Map.Entry<K, List<V>>, E> groupBy(Try.Function<? super T, ? extends K, ? extends E> classifier,
            Try.Function<? super T, ? extends V, E> valueMapper) {
        return groupBy(classifier, valueMapper, Suppliers.<K, List<V>> ofMap());
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <K, V> ExceptionalStream<Map.Entry<K, List<V>>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Supplier<? extends Map<K, List<V>>> mapFactory) {
        checkArgNotNull(classifier, "classifier");
        checkArgNotNull(valueMapper, "valueMapper");
        checkArgNotNull(mapFactory, "mapFactory");

        return newStream(new ExceptionalIterator<Map.Entry<K, List<V>>, E>() {
            private Iterator<Map.Entry<K, List<V>>> iter = null;

            @Override
            public boolean hasNext() throws E {
                init();
                return iter.hasNext();
            }

            @Override
            public Map.Entry<K, List<V>> next() throws E {
                init();
                return iter.next();
            }

            private void init() throws E {
                if (iter == null) {
                    iter = ExceptionalStream.this.groupTo(classifier, valueMapper, mapFactory).entrySet().iterator();
                }
            }
        }, false, null, closeHandlers);

    }

    public <K, V> ExceptionalStream<Map.Entry<K, V>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, Try.BinaryOperator<V, ? extends E> mergeFunction) {
        return groupBy(classifier, valueMapper, mergeFunction, Suppliers.<K, V> ofMap());
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see {@link Fn.FN#throwingMerger()}
     * @see {@link Fn.FN#replacingMerger()}
     * @see {@link Fn.FN#ignoringMerger()}
     */
    public <K, V> ExceptionalStream<Map.Entry<K, V>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Try.BinaryOperator<V, ? extends E> mergeFunction,
            final Supplier<? extends Map<K, V>> mapFactory) {
        checkArgNotNull(classifier, "classifier");
        checkArgNotNull(valueMapper, "valueMapper");
        checkArgNotNull(mergeFunction, "mergeFunction");
        checkArgNotNull(mapFactory, "mapFactory");

        return newStream(new ExceptionalIterator<Map.Entry<K, V>, E>() {
            private Iterator<Map.Entry<K, V>> iter = null;

            @Override
            public boolean hasNext() throws E {
                init();
                return iter.hasNext();
            }

            @Override
            public Map.Entry<K, V> next() throws E {
                init();
                return iter.next();
            }

            private void init() throws E {
                if (iter == null) {
                    iter = ExceptionalStream.this.toMap(classifier, valueMapper, mergeFunction, mapFactory).entrySet().iterator();
                }
            }
        }, false, null, closeHandlers);
    }

    public <K, A, D> ExceptionalStream<Map.Entry<K, D>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Collector<? super T, A, D> downstream) throws E {
        return groupBy(keyExtractor, downstream, Suppliers.<K, D> ofMap());
    }

    public <K, A, D, M extends Map<K, D>> ExceptionalStream<Map.Entry<K, D>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) throws E {
        return groupBy(keyExtractor, Fn.FN.<T, E> identity(), downstream, mapFactory);
    }

    public <K, V, A, D, M extends Map<K, D>> ExceptionalStream<Map.Entry<K, D>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Collector<? super V, A, D> downstream) throws E {
        return groupBy(keyExtractor, valueMapper, downstream, Suppliers.<K, D> ofMap());
    }

    public <K, V, A, D, M extends Map<K, D>> ExceptionalStream<Map.Entry<K, D>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Collector<? super V, A, D> downstream, final Supplier<M> mapFactory)
            throws E {
        checkArgNotNull(keyExtractor, "keyExtractor");
        checkArgNotNull(valueMapper, "valueMapper");
        checkArgNotNull(downstream, "downstream");
        checkArgNotNull(mapFactory, "mapFactory");

        return newStream(new ExceptionalIterator<Map.Entry<K, D>, E>() {
            private Iterator<Map.Entry<K, D>> iter = null;

            @Override
            public boolean hasNext() throws E {
                init();
                return iter.hasNext();
            }

            @Override
            public Map.Entry<K, D> next() throws E {
                init();
                return iter.next();
            }

            private void init() throws E {
                if (iter == null) {
                    iter = ExceptionalStream.this.toMap(keyExtractor, valueMapper, downstream, mapFactory).entrySet().iterator();
                }
            }
        }, false, null, closeHandlers);
    }

    public ExceptionalStream<Stream<T>, E> collapse(final Try.BiPredicate<? super T, ? super T, ? extends E> collapsible) {
        checkArgNotNull(collapsible, "collapsible");

        final ExceptionalIterator<T, E> iter = elements;

        return newStream(new ExceptionalIterator<Stream<T>, E>() {
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() throws E {
                return hasNext || iter.hasNext();
            }

            @Override
            public Stream<T> next() throws E {
                if (hasNext == false) {
                    next = iter.next();
                }

                final List<T> c = new ArrayList<>();
                c.add(next);

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.next()))) {
                        c.add(next);
                    } else {
                        break;
                    }
                }

                return Stream.of(c);
            }
        }, false, null, closeHandlers);
    }

    public <C extends Collection<T>> ExceptionalStream<C, E> collapse(final Try.BiPredicate<? super T, ? super T, ? extends E> collapsible,
            final Supplier<C> supplier) {
        checkArgNotNull(collapsible, "collapsible");
        checkArgNotNull(supplier, "supplier");

        final ExceptionalIterator<T, E> iter = elements;

        return newStream(new ExceptionalIterator<C, E>() {
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() throws E {
                return hasNext || iter.hasNext();
            }

            @Override
            public C next() throws E {
                if (hasNext == false) {
                    next = iter.next();
                }

                final C c = supplier.get();
                c.add(next);

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.next()))) {
                        c.add(next);
                    } else {
                        break;
                    }
                }

                return c;
            }
        }, false, null, closeHandlers);
    }

    /**
     * Merge series of adjacent elements which satisfy the given predicate using
     * the merger function and return a new stream.
     * 
     * <p>Example:
     * <pre>
     * <code>
     * Stream.of(new Integer[0]).collapse((a, b) -> a < b, (a, b) -> a + b) => []
     * Stream.of(1).collapse((a, b) -> a < b, (a, b) -> a + b) => [1]
     * Stream.of(1, 2).collapse((a, b) -> a < b, (a, b) -> a + b) => [3]
     * Stream.of(1, 2, 3).collapse((a, b) -> a < b, (a, b) -> a + b) => [6]
     * Stream.of(1, 2, 3, 3, 2, 1).collapse((a, b) -> a < b, (a, b) -> a + b) => [6, 3, 2, 1]
     * </code>
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    public ExceptionalStream<T, E> collapse(final Try.BiPredicate<? super T, ? super T, ? extends E> collapsible,
            final Try.BiFunction<? super T, ? super T, T, ? extends E> mergeFunction) {
        checkArgNotNull(collapsible, "collapsible");
        checkArgNotNull(mergeFunction, "mergeFunction");

        final ExceptionalIterator<T, E> iter = elements;

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() throws E {
                return hasNext || iter.hasNext();
            }

            @Override
            public T next() throws E {
                T res = hasNext ? next : (next = iter.next());

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.next()))) {
                        res = mergeFunction.apply(res, next);
                    } else {
                        break;
                    }
                }

                return res;
            }
        }, false, null, closeHandlers);
    }

    public <R> ExceptionalStream<R, E> collapse(final Try.BiPredicate<? super T, ? super T, ? extends E> collapsible, final Try.Supplier<R, E> supplier,
            final Try.BiConsumer<R, ? super T, ? extends E> accumulator) {
        checkArgNotNull(collapsible, "collapsible");
        checkArgNotNull(supplier, "supplier");
        checkArgNotNull(accumulator, "accumulator");

        final ExceptionalIterator<T, E> iter = elements;

        return newStream(new ExceptionalIterator<R, E>() {
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() throws E {
                return hasNext || iter.hasNext();
            }

            @Override
            public R next() throws E {
                final R container = supplier.get();
                accumulator.accept(container, hasNext ? next : (next = iter.next()));

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.next()))) {
                        accumulator.accept(container, next);
                    } else {
                        break;
                    }
                }

                return container;
            }
        }, false, null, closeHandlers);
    }

    public <R, A> ExceptionalStream<R, E> collapse(final Try.BiPredicate<? super T, ? super T, ? extends E> collapsible,
            final Collector<? super T, A, R> collector) {
        checkArgNotNull(collapsible, "collapsible");
        checkArgNotNull(collector, "collector");

        final Supplier<A> supplier = collector.supplier();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final Function<A, R> finisher = collector.finisher();

        final ExceptionalIterator<T, E> iter = elements;

        return newStream(new ExceptionalIterator<R, E>() {
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() throws E {
                return hasNext || iter.hasNext();
            }

            @Override
            public R next() throws E {
                final A container = supplier.get();
                accumulator.accept(container, hasNext ? next : (next = iter.next()));

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.next()))) {
                        accumulator.accept(container, next);
                    } else {
                        break;
                    }
                }

                return finisher.apply(container);
            }
        }, false, null, closeHandlers);
    }

    public ExceptionalStream<T, E> append(ExceptionalStream<T, E> s) {
        return concat(this, s);
    }

    public ExceptionalStream<T, E> prepend(ExceptionalStream<T, E> s) {
        return concat(s, this);
    }

    public ExceptionalStream<T, E> peek(final Try.Consumer<? super T, ? extends E> action) {
        checkArgNotNull(action, "action");

        return newStream(new ExceptionalIterator<T, E>() {
            @Override
            public boolean hasNext() throws E {
                return elements.hasNext();
            }

            @Override
            public T next() throws E {
                final T next = elements.next();
                action.accept(next);
                return next;
            }
        }, sorted, comparator, closeHandlers);
    }

    public ExceptionalStream<Stream<T>, E> split(final int size) {
        return splitToList(size).map(new Try.Function<List<T>, Stream<T>, E>() {
            @Override
            public Stream<T> apply(List<T> t) {
                return Stream.of(t);
            }
        });
    }

    public ExceptionalStream<List<T>, E> splitToList(final int size) {
        return split(size, Factory.<T> ofList());
    }

    public ExceptionalStream<Set<T>, E> splitToSet(final int size) {
        return split(size, Factory.<T> ofSet());
    }

    public <C extends Collection<T>> ExceptionalStream<C, E> split(final int size, final IntFunction<C> collectionSupplier) {
        checkArgPositive(size, "size");
        checkArgNotNull(collectionSupplier, "collectionSupplier");

        return newStream(new ExceptionalIterator<C, E>() {
            @Override
            public boolean hasNext() throws E {
                return elements.hasNext();
            }

            @Override
            public C next() throws E {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final C result = collectionSupplier.apply(size);
                int cnt = 0;

                while (cnt++ < size && elements.hasNext()) {
                    result.add(elements.next());
                }

                return result;
            }

            @Override
            public long count() throws E {
                final long len = elements.count();
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) throws E {
                checkArgNotNegative(n, "n");

                elements.skip(n > Long.MAX_VALUE / size ? Long.MAX_VALUE : n * size);
            }
        }, false, null, closeHandlers);
    }

    public ExceptionalStream<Stream<T>, E> sliding(final int windowSize, final int increment) {
        return slidingToList(windowSize, increment).map(new Try.Function<List<T>, Stream<T>, E>() {
            @Override
            public Stream<T> apply(List<T> t) {
                return Stream.of(t);
            }
        });
    }

    public ExceptionalStream<List<T>, E> slidingToList(final int windowSize, final int increment) {
        return sliding(windowSize, increment, Factory.<T> ofList());
    }

    public ExceptionalStream<Set<T>, E> slidingToSet(final int windowSize, final int increment) {
        return sliding(windowSize, increment, Factory.<T> ofSet());
    }

    public <C extends Collection<T>> ExceptionalStream<C, E> sliding(final int windowSize, final int increment, final IntFunction<C> collectionSupplier) {
        checkArgument(windowSize > 0 && increment > 0, "windowSize=%s and increment=%s must be bigger than 0", windowSize, increment);
        checkArgNotNull(collectionSupplier, "collectionSupplier");

        return newStream(new ExceptionalIterator<C, E>() {
            private Deque<T> queue = null;
            private boolean toSkip = false;

            @Override
            public boolean hasNext() throws E {
                if (toSkip) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.next();
                    }

                    toSkip = false;
                }

                return elements.hasNext();
            }

            @Override
            public C next() throws E {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (queue == null) {
                    queue = new ArrayDeque<>(N.max(0, windowSize - increment));
                }

                final C result = collectionSupplier.apply(windowSize);
                int cnt = 0;

                if (queue.size() > 0 && increment < windowSize) {
                    cnt = queue.size();

                    for (T e : queue) {
                        result.add(e);
                    }

                    if (queue.size() <= increment) {
                        queue.clear();
                    } else {
                        for (int i = 0; i < increment; i++) {
                            queue.removeFirst();
                        }
                    }
                }

                T next = null;

                while (cnt++ < windowSize && elements.hasNext()) {
                    next = elements.next();
                    result.add(next);

                    if (cnt > increment) {
                        queue.add(next);
                    }
                }

                toSkip = increment > windowSize;

                return result;
            }

            @Override
            public long count() throws E {
                final int prevSize = increment >= windowSize ? 0 : (queue == null ? 0 : queue.size());
                final long len = prevSize + elements.count();

                if (len == prevSize) {
                    return 0;
                } else if (len <= windowSize) {
                    return 1;
                } else {
                    final long rlen = len - windowSize;
                    return 1 + (rlen % increment == 0 ? rlen / increment : rlen / increment + 1);
                }
            }

            @Override
            public void skip(long n) throws E {
                checkArgNotNegative(n, "n");

                if (n == 0) {
                    return;
                }

                if (increment >= windowSize) {
                    elements.skip(n > Long.MAX_VALUE / increment ? Long.MAX_VALUE : n * increment);
                } else {
                    if (N.isNullOrEmpty(queue)) {
                        final long m = ((n - 1) > Long.MAX_VALUE / increment ? Long.MAX_VALUE : (n - 1) * increment);
                        elements.skip(m);
                    } else {
                        final long m = (n > Long.MAX_VALUE / increment ? Long.MAX_VALUE : n * increment);
                        final int prevSize = increment >= windowSize ? 0 : (queue == null ? 0 : queue.size());

                        if (m < prevSize) {
                            for (int i = 0; i < m; i++) {
                                queue.removeFirst();
                            }
                        } else {
                            if (queue != null) {
                                queue.clear();
                            }

                            elements.skip(m - prevSize);
                        }
                    }

                    if (queue == null) {
                        queue = new ArrayDeque<>(windowSize);
                    }

                    int cnt = queue.size();

                    while (cnt++ < windowSize && elements.hasNext()) {
                        queue.add(elements.next());
                    }
                }
            }
        }, false, null, closeHandlers);
    }

    public ExceptionalStream<T, E> skip(final long n) {
        checkArgNotNegative(n, "n");

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() throws E {
                if (skipped == false) {
                    skipped = true;
                    skip(n);
                }

                return elements.hasNext();
            }

            @Override
            public T next() throws E {
                if (skipped == false) {
                    skipped = true;
                    skip(n);
                }

                return elements.next();
            }
        }, sorted, comparator, closeHandlers);
    }

    public ExceptionalStream<T, E> limit(final long maxSize) {
        checkArgNotNegative(maxSize, "maxSize");

        return newStream(new ExceptionalIterator<T, E>() {
            private long cnt = 0;

            @Override
            public boolean hasNext() throws E {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public T next() throws E {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.next();
            }

        }, sorted, comparator, closeHandlers);
    }

    public ExceptionalStream<T, E> sorted() {
        return sorted(Comparators.NATURAL_ORDER);
    }

    public ExceptionalStream<T, E> reverseSorted() {
        return sorted(Comparators.REVERSED_ORDER);
    }

    public ExceptionalStream<T, E> sorted(final Comparator<? super T> comparator) {
        final Comparator<? super T> cmp = comparator == null ? Comparators.NATURAL_ORDER : comparator;

        if (sorted && cmp == this.comparator) {
            return this;
        }

        return lazyLoad(new Function<Object[], Object[]>() {
            @Override
            public Object[] apply(final Object[] a) {
                N.sort((T[]) a, cmp);

                return a;
            }
        }, true, cmp);
    }

    @SuppressWarnings("rawtypes")
    public ExceptionalStream<T, E> sortedBy(final Function<? super T, ? extends Comparable> keyExtractor) {
        final Comparator<? super T> comparator = new Comparator<T>() {
            @Override
            public int compare(T o1, T o2) {
                return N.compare(keyExtractor.apply(o1), keyExtractor.apply(o2));
            }
        };

        return sorted(comparator);
    }

    private ExceptionalStream<T, E> lazyLoad(final Function<Object[], Object[]> op, final boolean sorted, final Comparator<? super T> cmp) {
        return newStream(new ExceptionalIterator<T, E>() {
            private boolean initialized = false;
            private T[] aar;
            private int cursor = 0;
            private int len;

            @Override
            public boolean hasNext() throws E {
                if (initialized == false) {
                    init();
                }

                return cursor < len;
            }

            @Override
            public T next() throws E {
                if (initialized == false) {
                    init();
                }

                if (cursor >= len) {
                    throw new NoSuchElementException();
                }

                return aar[cursor++];
            }

            @Override
            public long count() throws E {
                if (initialized == false) {
                    init();
                }

                return len - cursor;
            }

            @Override
            public void skip(long n) throws E {
                checkArgNotNegative(n, "n");

                if (initialized == false) {
                    init();
                }

                cursor = n > len - cursor ? len : cursor + (int) n;
            }

            private void init() throws E {
                if (initialized == false) {
                    initialized = true;
                    aar = (T[]) op.apply(ExceptionalStream.this.toArray());
                    len = aar.length;
                }
            }
        }, sorted, cmp, closeHandlers);
    }

    public void forEach(Try.Consumer<? super T, ? extends E> action) throws E {
        checkArgNotNull(action, "action");
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                action.accept(elements.next());
            }
        } finally {
            close();
        }
    }

    public Optional<T> min(Comparator<? super T> comparator) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return Optional.empty();
            } else if (sorted && isSameComparator(comparator, comparator)) {
                return Optional.of(elements.next());
            }

            comparator = comparator == null ? Comparators.NATURAL_ORDER : comparator;
            T candidate = elements.next();
            T next = null;

            while (elements.hasNext()) {
                next = elements.next();
                if (comparator.compare(next, candidate) < 0) {
                    candidate = next;
                }
            }

            return Optional.of(candidate);
        } finally {
            close();
        }
    }

    @SuppressWarnings("rawtypes")

    public Optional<T> minBy(final Function<? super T, ? extends Comparable> keyExtractor) throws E {
        checkArgNotNull(keyExtractor, "keyExtractor");
        assertNotClosed();

        try {
            final Comparator<? super T> comparator = Fn.comparingBy(keyExtractor);

            return min(comparator);
        } finally {
            close();
        }
    }

    public Optional<T> max(Comparator<? super T> comparator) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return Optional.empty();
            } else if (sorted && isSameComparator(comparator, comparator)) {
                T next = null;

                while (elements.hasNext()) {
                    next = elements.next();
                }

                return Optional.of(next);
            }

            comparator = comparator == null ? Comparators.NATURAL_ORDER : comparator;
            T candidate = elements.next();
            T next = null;

            while (elements.hasNext()) {
                next = elements.next();

                if (comparator.compare(next, candidate) > 0) {
                    candidate = next;
                }
            }

            return Optional.of(candidate);
        } finally {
            close();
        }
    }

    @SuppressWarnings("rawtypes")

    public Optional<T> maxBy(final Function<? super T, ? extends Comparable> keyExtractor) throws E {
        checkArgNotNull(keyExtractor, "keyExtractor");
        assertNotClosed();

        try {
            final Comparator<? super T> comparator = Fn.comparingBy(keyExtractor);

            return max(comparator);
        } finally {
            close();
        }
    }

    public boolean anyMatch(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        checkArgNotNull(predicate, "predicate");
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                if (predicate.test(elements.next())) {
                    return true;
                }
            }

            return false;
        } finally {
            close();
        }
    }

    public boolean allMatch(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        checkArgNotNull(predicate, "predicate");
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                if (predicate.test(elements.next()) == false) {
                    return false;
                }
            }

            return true;
        } finally {
            close();
        }
    }

    public boolean noneMatch(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        checkArgNotNull(predicate, "predicate");
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                if (predicate.test(elements.next())) {
                    return false;
                }
            }

            return true;
        } finally {
            close();
        }
    }

    public Optional<T> findFirst(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        checkArgNotNull(predicate, "predicate");
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                T e = elements.next();

                if (predicate.test(e)) {
                    return Optional.of(e);
                }
            }

            return (Optional<T>) Optional.empty();
        } finally {
            close();
        }
    }

    public Optional<T> findLast(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        checkArgNotNull(predicate, "predicate");
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return (Optional<T>) Optional.empty();
            }

            boolean hasResult = false;
            T e = null;
            T result = null;

            while (elements.hasNext()) {
                e = elements.next();

                if (predicate.test(e)) {
                    result = e;
                    hasResult = true;
                }
            }

            return hasResult ? Optional.of(result) : (Optional<T>) Optional.empty();
        } finally {
            close();
        }
    }

    public Optional<T> first() throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return Optional.empty();
            }

            return Optional.of(elements.next());
        } finally {
            close();
        }
    }

    public Optional<T> last() throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return Optional.empty();
            }

            T next = elements.next();

            while (elements.hasNext()) {
                next = elements.next();
            }

            return Optional.of(next);
        } finally {
            close();
        }
    }

    public Object[] toArray() throws E {
        assertNotClosed();

        try {
            return toList().toArray();
        } finally {
            close();
        }
    }

    public <A> A[] toArray(IntFunction<A[]> generator) throws E {
        checkArgNotNull(generator, "generator");
        assertNotClosed();

        try {
            final List<T> list = toList();

            return list.toArray(generator.apply(list.size()));
        } finally {
            close();
        }
    }

    public List<T> toList() throws E {
        assertNotClosed();

        try {
            final List<T> result = new ArrayList<>();

            while (elements.hasNext()) {
                result.add(elements.next());
            }

            return result;
        } finally {
            close();
        }
    }

    public Set<T> toSet() throws E {
        assertNotClosed();

        try {
            final Set<T> result = new HashSet<>();

            while (elements.hasNext()) {
                result.add(elements.next());
            }

            return result;
        } finally {
            close();
        }
    }

    public <C extends Collection<T>> C toCollection(final Supplier<C> supplier) throws E {
        checkArgNotNull(supplier, "supplier");
        assertNotClosed();

        try {
            final C result = supplier.get();

            while (elements.hasNext()) {
                result.add(elements.next());
            }

            return result;
        } finally {
            close();
        }
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @throws E
     * @throws IllegalStateException if there are duplicated keys.
     * @see {@link Fn.FN#throwingMerger()}
     * @see {@link Fn.FN#replacingMerger()}
     * @see {@link Fn.FN#ignoringMerger()}
     */
    public <K, V> Map<K, V> toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper) throws E, IllegalStateException {
        return toMap(keyExtractor, valueMapper, Suppliers.<K, V> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @throws E
     * @throws IllegalStateException if there are duplicated keys.
     * @see {@link Fn.FN#throwingMerger()}
     * @see {@link Fn.FN#replacingMerger()}
     * @see {@link Fn.FN#ignoringMerger()}
     */
    public <K, V, M extends Map<K, V>> M toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Supplier<M> mapFactory) throws E, IllegalStateException {
        return toMap(keyExtractor, valueMapper, Fn.FN.<V, E> throwingMerger(), mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @throws E
     * @see {@link Fn.FN#throwingMerger()}
     * @see {@link Fn.FN#replacingMerger()}
     * @see {@link Fn.FN#ignoringMerger()}
     */
    public <K, V> Map<K, V> toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Try.BinaryOperator<V, ? extends E> mergeFunction) throws E {
        return toMap(keyExtractor, valueMapper, mergeFunction, Suppliers.<K, V> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @throws E
     * @see {@link Fn.FN#throwingMerger()}
     * @see {@link Fn.FN#replacingMerger()}
     * @see {@link Fn.FN#ignoringMerger()}
     */
    public <K, V, M extends Map<K, V>> M toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Try.BinaryOperator<V, ? extends E> mergeFunction,
            final Supplier<M> mapFactory) throws E {
        checkArgNotNull(keyExtractor, "keyExtractor");
        checkArgNotNull(valueMapper, "valueMapper");
        checkArgNotNull(mergeFunction, "mergeFunction");
        checkArgNotNull(mapFactory, "mapFactory");
        assertNotClosed();

        try {
            final M result = mapFactory.get();
            T next = null;

            while (elements.hasNext()) {
                next = elements.next();
                Fn.merge(result, keyExtractor.apply(next), valueMapper.apply(next), mergeFunction);
            }

            return result;
        } finally {
            close();
        }
    }

    /**
     * 
     * @param keyExtractor
     * @param downstream
     * @return
     * @throws E 
     */
    public <K, A, D> Map<K, D> toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor, final Collector<? super T, A, D> downstream)
            throws E {
        return toMap(keyExtractor, downstream, Suppliers.<K, D> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param downstream
     * @param mapFactory
     * @return
     * @throws E 
     */
    public <K, A, D, M extends Map<K, D>> M toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) throws E {
        return toMap(keyExtractor, Fn.FN.<T, E> identity(), downstream, mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param downstream
     * @return
     * @throws E 
     */
    public <K, V, A, D> Map<K, D> toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Collector<? super V, A, D> downstream) throws E {
        return toMap(keyExtractor, valueMapper, downstream, Suppliers.<K, D> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param downstream
     * @param mapFactory
     * @return
     * @throws E 
     */
    public <K, V, A, D, M extends Map<K, D>> M toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Collector<? super V, A, D> downstream, final Supplier<M> mapFactory)
            throws E {
        checkArgNotNull(keyExtractor, "keyExtractor");
        checkArgNotNull(valueMapper, "valueMapper");
        checkArgNotNull(downstream, "downstream");
        checkArgNotNull(mapFactory, "mapFactory");
        assertNotClosed();

        try {
            final Supplier<A> downstreamSupplier = downstream.supplier();
            final BiConsumer<A, ? super V> downstreamAccumulator = downstream.accumulator();
            final Function<A, D> downstreamFinisher = downstream.finisher();

            final M result = mapFactory.get();
            final Map<K, A> tmp = (Map<K, A>) result;
            T next = null;
            K key = null;
            A container = null;

            while (elements.hasNext()) {
                next = elements.next();
                key = keyExtractor.apply(next);
                container = tmp.get(key);

                if (container == null) {
                    container = downstreamSupplier.get();
                    tmp.put(key, container);
                }

                downstreamAccumulator.accept(container, valueMapper.apply(next));
            }

            for (Map.Entry<K, D> entry : result.entrySet()) {
                entry.setValue(downstreamFinisher.apply((A) entry.getValue()));
            }

            return result;
        } finally {
            close();
        }
    }

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public <K> Map<K, List<T>> groupTo(Try.Function<? super T, ? extends K, ? extends E> classifier) throws E {
        return groupTo(classifier, Suppliers.<K, List<T>> ofMap());
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public <K, M extends Map<K, List<T>>> M groupTo(final Try.Function<? super T, ? extends K, ? extends E> classifier, final Supplier<M> mapFactory) throws E {
        final Try.Function<T, T, E> valueMapper = Fn.FN.identity();

        return groupTo(classifier, valueMapper, mapFactory);
    }

    public <K, V> Map<K, List<V>> groupTo(Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            Try.Function<? super T, ? extends V, ? extends E> valueMapper) throws E {
        return groupTo(keyExtractor, valueMapper, Suppliers.<K, List<V>> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <K, V, M extends Map<K, List<V>>> M groupTo(Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            Try.Function<? super T, ? extends V, ? extends E> valueMapper, Supplier<M> mapFactory) throws E {
        checkArgNotNull(keyExtractor, "keyExtractor");
        checkArgNotNull(valueMapper, "valueMapper");
        checkArgNotNull(mapFactory, "mapFactory");
        assertNotClosed();

        try {
            final M result = mapFactory.get();
            T next = null;
            K key = null;

            while (elements.hasNext()) {
                next = elements.next();
                key = keyExtractor.apply(next);

                if (result.containsKey(key) == false) {
                    result.put(key, new ArrayList<V>());
                }

                result.get(key).add(valueMapper.apply(next));
            }

            return result;
        } finally {
            close();
        }
    }

    public long count() throws E {
        assertNotClosed();

        try {
            return elements.count();
        } finally {
            close();
        }
    }

    /**
     * 
     * @return
     * @throws DuplicatedResultException if there are more than one elements.
     * @throws E
     */
    public Optional<T> onlyOne() throws DuplicatedResultException, E {
        assertNotClosed();

        try {
            Optional<T> result = Optional.empty();

            if (elements.hasNext()) {
                result = Optional.of(elements.next());

                if (elements.hasNext()) {
                    throw new DuplicatedResultException("There are at least two elements: " + Strings.concat(result.get(), ", ", elements.next()));
                }
            }

            return result;
        } finally {
            close();
        }
    }

    public OptionalInt sumInt(Try.ToIntFunction<T, E> func) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalInt.empty();
            }

            long sum = 0;

            while (elements.hasNext()) {
                sum += func.applyAsInt(elements.next());
            }

            return OptionalInt.of(N.toIntExact(sum));
        } finally {
            close();
        }
    }

    public OptionalLong sumLong(Try.ToLongFunction<T, E> func) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalLong.empty();
            }

            long sum = 0;

            while (elements.hasNext()) {
                sum += func.applyAsLong(elements.next());
            }

            return OptionalLong.of(sum);
        } finally {
            close();
        }
    }

    public OptionalDouble sumDouble(Try.ToDoubleFunction<T, E> func) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalDouble.empty();
            }

            final List<Double> list = new ArrayList<>();

            while (elements.hasNext()) {
                list.add(func.applyAsDouble(elements.next()));
            }

            return OptionalDouble.of(N.sumDouble(list));
        } finally {
            close();
        }
    }

    public OptionalDouble averageInt(Try.ToIntFunction<T, E> func) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalDouble.empty();
            }

            long sum = 0;
            long count = 0;

            while (elements.hasNext()) {
                sum += func.applyAsInt(elements.next());
                count++;
            }

            return OptionalDouble.of(((double) sum) / count);
        } finally {
            close();
        }
    }

    public OptionalDouble averageLong(Try.ToLongFunction<T, E> func) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalDouble.empty();
            }

            long sum = 0;
            long count = 0;

            while (elements.hasNext()) {
                sum += func.applyAsLong(elements.next());
                count++;
            }

            return OptionalDouble.of(((double) sum) / count);
        } finally {
            close();
        }
    }

    public OptionalDouble averageDouble(Try.ToDoubleFunction<T, E> func) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalDouble.empty();
            }

            final List<Double> list = new ArrayList<>();

            while (elements.hasNext()) {
                list.add(func.applyAsDouble(elements.next()));
            }

            return N.averageLong(list);
        } finally {
            close();
        }
    }

    public T reduce(T identity, Try.BinaryOperator<T, ? extends E> accumulator) throws E {
        checkArgNotNull(accumulator, "accumulator");
        assertNotClosed();

        try {
            T result = identity;

            while (elements.hasNext()) {
                result = accumulator.apply(result, elements.next());
            }

            return result;
        } finally {
            close();
        }
    }

    public Optional<T> reduce(Try.BinaryOperator<T, ? extends E> accumulator) throws E {
        checkArgNotNull(accumulator, "accumulator");
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return Optional.empty();
            }

            T result = elements.next();

            while (elements.hasNext()) {
                result = accumulator.apply(result, elements.next());
            }

            return Optional.of(result);
        } finally {
            close();
        }
    }

    public <R> R collect(Try.Supplier<R, E> supplier, final Try.BiConsumer<R, ? super T, ? extends E> accumulator) throws E {
        checkArgNotNull(supplier, "supplier");
        checkArgNotNull(accumulator, "accumulator");
        assertNotClosed();

        try {
            final R result = supplier.get();

            while (elements.hasNext()) {
                accumulator.accept(result, elements.next());
            }

            return result;
        } finally {
            close();
        }
    }

    public <R, RR> RR collect(Try.Supplier<R, E> supplier, final Try.BiConsumer<R, ? super T, ? extends E> accumulator,
            final Try.Function<? super R, ? extends RR, E> finisher) throws E {
        checkArgNotNull(supplier, "supplier");
        checkArgNotNull(accumulator, "accumulator");
        checkArgNotNull(finisher, "finisher");
        assertNotClosed();

        try {
            final R result = supplier.get();

            while (elements.hasNext()) {
                accumulator.accept(result, elements.next());
            }

            return finisher.apply(result);
        } finally {
            close();
        }
    }

    public <R, A> R collect(final Collector<? super T, A, R> collector) throws E {
        checkArgNotNull(collector, "collector");
        assertNotClosed();

        try {
            final A container = collector.supplier().get();
            final BiConsumer<A, ? super T> accumulator = collector.accumulator();

            while (elements.hasNext()) {
                accumulator.accept(container, elements.next());
            }

            return collector.finisher().apply(container);
        } finally {
            close();
        }
    }

    public <R, RR, A> RR collectAndThen(final Collector<? super T, A, R> collector, final Try.Function<? super R, ? extends RR, E> func) throws E {
        checkArgNotNull(collector, "collector");
        checkArgNotNull(func, "func");
        assertNotClosed();

        try {
            final A container = collector.supplier().get();
            final BiConsumer<A, ? super T> accumulator = collector.accumulator();

            while (elements.hasNext()) {
                accumulator.accept(container, elements.next());
            }

            return func.apply(collector.finisher().apply(container));
        } finally {
            close();
        }
    }

    public <R, A> R collect(java.util.stream.Collector<? super T, A, R> collector) throws E {
        try {
            final A container = collector.supplier().get();
            final java.util.function.BiConsumer<A, ? super T> accumulator = collector.accumulator();

            while (elements.hasNext()) {
                accumulator.accept(container, elements.next());
            }

            return collector.finisher().apply(container);
        } finally {
            close();
        }
    }

    public <R, RR, A> RR collectAndThen(final java.util.stream.Collector<? super T, A, R> collector, final Try.Function<? super R, ? extends RR, E> func)
            throws E {
        N.checkArgNotNull(collector, "collector");
        N.checkArgNotNull(func, "func");

        try {
            final A container = collector.supplier().get();
            final java.util.function.BiConsumer<A, ? super T> accumulator = collector.accumulator();

            while (elements.hasNext()) {
                accumulator.accept(container, elements.next());
            }

            return func.apply(collector.finisher().apply(container));
        } finally {
            close();
        }
    }

    Stream<T> unchecked() {
        if (N.isNullOrEmpty(this.closeHandlers)) {
            return Stream.of(new ObjIteratorEx<T>() {
                @Override
                public boolean hasNext() {
                    try {
                        return elements.hasNext();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }

                @Override
                public T next() {
                    try {
                        return elements.next();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }

                @Override
                public void skip(long n) {
                    N.checkArgNotNegative(n, "n");

                    try {
                        elements.skip(n);
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }

                @Override
                public long count() {
                    try {
                        return elements.count();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }
            });
        } else {
            return Stream.of(new ObjIteratorEx<T>() {
                @Override
                public boolean hasNext() {
                    try {
                        return elements.hasNext();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }

                @Override
                public T next() {
                    try {
                        return elements.next();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }

                @Override
                public void skip(long n) {
                    N.checkArgNotNegative(n, "n");

                    try {
                        elements.skip(n);
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }

                @Override
                public long count() {
                    try {
                        return elements.count();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }
            }).onClose(new Runnable() {
                @Override
                public void run() {
                    try {
                        ExceptionalStream.this.close();
                    } catch (Exception e) {
                        throw N.toRuntimeException(e);
                    }
                }
            });
        }
    }

    public Stream<T> __() {
        return unchecked();
    }

    //    public <E2 extends Exception> ExceptionalStream<T, E2> __(final Class<E2> targetExceptionType) {
    //        checkArgNotNull(targetExceptionType, "targetExceptionType");
    //
    //        final Constructor<E2> msgCauseConstructor = ClassUtil.getDeclaredConstructor(targetExceptionType, String.class, Throwable.class);
    //        final Constructor<E2> causeOnlyConstructor = ClassUtil.getDeclaredConstructor(targetExceptionType, Throwable.class);
    //
    //        checkArgument(msgCauseConstructor != null || causeOnlyConstructor != null,
    //                "No constructor found with parameters: (String.class, Throwable.class), or (Throwable.class)");
    //
    //        final Function<Exception, E2> convertE = msgCauseConstructor != null ? new Function<Exception, E2>() {
    //            @Override
    //            public E2 apply(Exception e) {
    //                return ClassUtil.invokeConstructor(msgCauseConstructor, e.getMessage(), e);
    //            }
    //        } : new Function<Exception, E2>() {
    //            @Override
    //            public E2 apply(Exception e) {
    //                return ClassUtil.invokeConstructor(causeOnlyConstructor, e);
    //            }
    //        };
    //
    //        Deque<Try.Runnable<? extends E2>> newCloseHandlers = null;
    //
    //        if (closeHandlers != null) {
    //            newCloseHandlers = new ArrayDeque<>(1);
    //            newCloseHandlers.add(new Try.Runnable<E2>() {
    //                @Override
    //                public void run() throws E2 {
    //                    try {
    //                        close();
    //                    } catch (Exception e) {
    //                        throw convertE.apply(e);
    //                    }
    //                }
    //            });
    //        }
    //
    //        return newStream(new ExceptionalIterator<T, E2>() {
    //            private ExceptionalIterator<T, E> iter = null;
    //            private boolean initialized = false;
    //
    //            @Override
    //            public boolean hasNext() throws E2 {
    //                if (initialized == false) {
    //                    init();
    //                }
    //
    //                try {
    //                    return iter.hasNext();
    //                } catch (Exception e) {
    //                    throw convertE.apply(e);
    //                }
    //            }
    //
    //            @Override
    //            public T next() throws E2 {
    //                if (initialized == false) {
    //                    init();
    //                }
    //
    //                try {
    //                    return iter.next();
    //                } catch (Exception e) {
    //                    throw convertE.apply(e);
    //                }
    //            }
    //
    //                @Override
    //                public void skip(long n) throws E2 { 
    //                    checkArgNotNegative(n, "n"); 
    //                    
    //                    if (initialized == false) {
    //                        init();
    //                    }
    //    
    //                    try {
    //                        iter.skip(n);
    //                    } catch (Exception e) {
    //                        throw convertE.apply(e);
    //                    }
    //                }
    //
    //            @Override
    //            public long count() throws E2 {
    //                if (initialized == false) {
    //                    init();
    //                }
    //
    //                try {
    //                    return iter.count();
    //                } catch (Exception e) {
    //                    throw convertE.apply(e);
    //                }
    //            }
    //
    //            private void init() {
    //                if (initialized == false) {
    //                    initialized = true;
    //
    //                    iter = ExceptionalStream.this.elements;
    //
    //                }
    //            }
    //        }, sorted, comparator, newCloseHandlers);
    //    }

    public <R> R __(Try.Function<? super ExceptionalStream<T, E>, R, ? extends E> transfer) throws E {
        checkArgNotNull(transfer, "transfer");

        return transfer.apply(this);
    }

    /** 
     * 
     * @param action a terminal operation should be called.
     * @return
     */
    public ContinuableFuture<Void> asyncRun(final Try.Consumer<? super ExceptionalStream<T, E>, E> action) {
        checkArgNotNull(action, "action");

        return ContinuableFuture.run(new Try.Runnable<E>() {
            @Override
            public void run() throws E {
                action.accept(ExceptionalStream.this);
            }
        });
    }

    /**
     * 
     * @param action a terminal operation should be called.
     * @param executor
     * @return
     */
    public ContinuableFuture<Void> asyncRun(final Try.Consumer<? super ExceptionalStream<T, E>, E> action, final Executor executor) {
        checkArgNotNull(action, "action");
        checkArgNotNull(executor, "executor");

        return ContinuableFuture.run(new Try.Runnable<E>() {
            @Override
            public void run() throws E {
                action.accept(ExceptionalStream.this);
            }
        }, executor);
    }

    /**
     * 
     * @param action a terminal operation should be called.
     * @return
     */
    public <R> ContinuableFuture<R> asyncCall(final Try.Function<? super ExceptionalStream<T, E>, R, E> action) {
        checkArgNotNull(action, "action");

        return ContinuableFuture.call(new Try.Callable<R, E>() {
            @Override
            public R call() throws E {
                return action.apply(ExceptionalStream.this);
            }
        });
    }

    /**
     * 
     * @param action a terminal operation should be called.
     * @param executor
     * @return
     */
    public <R> ContinuableFuture<R> asyncCall(final Try.Function<? super ExceptionalStream<T, E>, R, E> action, final Executor executor) {
        checkArgNotNull(action, "action");
        checkArgNotNull(executor, "executor");

        return ContinuableFuture.call(new Try.Callable<R, E>() {
            @Override
            public R call() throws E {
                return action.apply(ExceptionalStream.this);
            }
        }, executor);
    }

    public ExceptionalStream<T, E> onClose(final Try.Runnable<? extends E> closeHandler) {
        checkArgNotNull(closeHandler, "closeHandler");

        final Deque<Try.Runnable<? extends E>> newCloseHandlers = new ArrayDeque<>(N.size(closeHandlers) + 1);

        newCloseHandlers.add(new Try.Runnable<E>() {
            private volatile boolean isClosed = false;

            @Override
            public void run() throws E {
                if (isClosed) {
                    return;
                }

                isClosed = true;
                closeHandler.run();
            }
        });

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        return newStream(elements, newCloseHandlers);
    }

    @Override
    public synchronized void close() throws E {
        if (isClosed) {
            return;
        }

        if (N.isNullOrEmpty(closeHandlers)) {
            isClosed = true;
            return;
        }

        //    // Only mark the stream closed if closeHandlers are not empty.
        //    if (isClosed || N.isNullOrEmpty(closeHandlers)) {
        //        return;
        //    }

        isClosed = true;

        Throwable ex = null;

        for (Try.Runnable<? extends E> closeHandler : closeHandlers) {
            try {
                closeHandler.run();
            } catch (Exception e) {
                if (ex == null) {
                    ex = e;
                } else {
                    if (ex instanceof RuntimeException && !(ex instanceof RuntimeException)) {
                        e.addSuppressed(ex);
                        ex = e;
                    } else {
                        ex.addSuppressed(e);
                    }
                }
            }
        }

        if (ex != null) {
            if (ex instanceof RuntimeException) {
                throw (RuntimeException) ex;
            } else {
                throw (E) ex;
            }
        }
    }

    void assertNotClosed() {
        if (isClosed) {
            throw new IllegalStateException("This stream has been closed");
        }
    }

    private int checkArgPositive(final int arg, final String argNameOrErrorMsg) {
        if (arg <= 0) {
            try {
                N.checkArgPositive(arg, argNameOrErrorMsg);
            } finally {
                try {
                    close();
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        }

        return arg;
    }

    private long checkArgNotNegative(final long arg, final String argNameOrErrorMsg) {
        if (arg < 0) {
            try {
                N.checkArgNotNegative(arg, argNameOrErrorMsg);
            } finally {
                try {
                    close();
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        }

        return arg;
    }

    private <ARG> ARG checkArgNotNull(final ARG obj, final String errorMessage) {
        if (obj == null) {
            try {
                N.checkArgNotNull(obj, errorMessage);
            } finally {
                try {
                    close();
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        }

        return obj;
    }

    private void checkArgument(boolean b, String errorMessageTemplate, int p1, int p2) {
        if (!b) {
            try {
                N.checkArgument(b, errorMessageTemplate, p1, p2);
            } finally {
                try {
                    close();
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        }
    }

    static <T, E extends Exception> ExceptionalStream<T, E> newStream(final ExceptionalIterator<T, E> iter) {
        return new ExceptionalStream<>(iter, null);
    }

    static <T, E extends Exception> ExceptionalStream<T, E> newStream(final ExceptionalIterator<T, E> iter,
            final Deque<Try.Runnable<? extends E>> closeHandlers) {
        return new ExceptionalStream<>(iter, closeHandlers);
    }

    static <T, E extends Exception> ExceptionalStream<T, E> newStream(final ExceptionalIterator<T, E> iter, final boolean sorted,
            final Comparator<? super T> comparator, final Deque<Try.Runnable<? extends E>> closeHandlers) {
        return new ExceptionalStream<>(iter, sorted, comparator, closeHandlers);
    }

    static Object hashKey(Object obj) {
        return obj == null || obj.getClass().isArray() == false ? obj : Wrapper.of(obj);
    }

    static boolean isSameComparator(Comparator<?> a, Comparator<?> b) {
        return a == b || (a == null && b == Comparators.NATURAL_ORDER) || (b == null && a == Comparators.NATURAL_ORDER);
    }

    public static final class StreamE<T, E extends Exception> extends ExceptionalStream<T, E> {
        StreamE(ExceptionalIterator<T, E> iter, boolean sorted, Comparator<? super T> comparator, Deque<Try.Runnable<? extends E>> closeHandlers) {
            super(iter, sorted, comparator, closeHandlers);
        }
    }

    static abstract class ExceptionalIterator<T, E extends Exception> {

        @SuppressWarnings("rawtypes")
        private static final ExceptionalIterator EMPTY = new ExceptionalIterator() {
            @Override
            public boolean hasNext() throws Exception {
                return false;
            }

            @Override
            public Object next() throws Exception {
                throw new NoSuchElementException();
            }
        };

        /**
         * Lazy evaluation.
         * 
         * @param iteratorSupplier
         * @return
         */
        public static <T, E extends Exception> ExceptionalIterator<T, E> of(final Try.Supplier<ExceptionalIterator<T, E>, E> iteratorSupplier) {
            N.checkArgNotNull(iteratorSupplier, "iteratorSupplier");

            return new ExceptionalIterator<T, E>() {
                private ExceptionalIterator<T, E> iter = null;
                private boolean isInitialized = false;

                @Override
                public boolean hasNext() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    return iter.hasNext();
                }

                @Override
                public T next() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    return iter.next();
                }

                @Override
                public void skip(long n) throws E {
                    N.checkArgNotNegative(n, "n");

                    if (isInitialized == false) {
                        init();
                    }

                    iter.skip(n);
                }

                @Override
                public long count() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    return iter.count();
                }

                @Override
                public void close() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    iter.close();
                }

                private void init() throws E {
                    if (isInitialized == false) {
                        isInitialized = true;
                        iter = iteratorSupplier.get();
                    }
                }
            };
        }

        /**
         * Lazy evaluation.
         * 
         * @param arraySupplier
         * @return
         */
        public static <T, E extends Exception> ExceptionalIterator<T, E> oF(final Try.Supplier<T[], E> arraySupplier) {
            N.checkArgNotNull(arraySupplier, "arraySupplier");

            return new ExceptionalIterator<T, E>() {
                private T[] a;
                private int len;
                private int position = 0;
                private boolean isInitialized = false;

                @Override
                public boolean hasNext() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    return position < len;
                }

                @Override
                public T next() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    if (position >= len) {
                        throw new NoSuchElementException();
                    }

                    return a[position++];
                }

                @Override
                public long count() throws E {
                    if (isInitialized == false) {
                        init();
                    }

                    return len - position;
                }

                @Override
                public void skip(long n) throws E {
                    N.checkArgNotNegative(n, "n");

                    if (isInitialized == false) {
                        init();
                    }

                    if (n <= 0) {
                        return;
                    } else if (n > len - position) {
                        position = len;
                    }

                    position += n;
                }

                private void init() throws E {
                    if (isInitialized == false) {
                        isInitialized = true;
                        a = arraySupplier.get();
                        len = N.len(a);
                    }
                }
            };
        }

        public abstract boolean hasNext() throws E;

        public abstract T next() throws E;

        public void skip(long n) throws E {
            N.checkArgNotNegative(n, "n");

            while (n-- > 0 && hasNext()) {
                next();
            }
        }

        public long count() throws E {
            long result = 0;

            while (hasNext()) {
                next();
                result++;
            }

            return result;
        }

        public void close() throws E {
            // Nothing to do by default.
        }
    }
}
