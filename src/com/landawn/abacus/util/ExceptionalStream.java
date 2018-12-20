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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.exception.NonUniqueResultException;
import com.landawn.abacus.exception.UncheckedSQLException;
import com.landawn.abacus.util.Fn.Suppliers;
import com.landawn.abacus.util.JdbcUtil.BiRecordGetter;
import com.landawn.abacus.util.StringUtil.Strings;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.ObjIteratorEx;
import com.landawn.abacus.util.stream.Stream;

public class ExceptionalStream<T, E extends Exception> implements AutoCloseable {
    @SuppressWarnings("rawtypes")
    private static final ExceptionalStream EMPTY = new ExceptionalStream(new ExceptionalIterator() {
        @Override
        public boolean hasNext() throws Exception {
            return false;
        }

        @Override
        public Object next() throws Exception {
            throw new NoSuchElementException();
        }
    }, null);

    private final ExceptionalIterator<T, E> elements;
    private final boolean sorted;
    private final Comparator<? super T> comparator;
    private final Set<Try.Runnable<? extends E>> closeHandlers;
    private boolean isClosed = false;

    ExceptionalStream(final ExceptionalIterator<T, E> iter, final Set<Try.Runnable<? extends E>> closeHandlers) {
        this(iter, false, null, closeHandlers);
    }

    ExceptionalStream(final ExceptionalIterator<T, E> iter, final boolean sorted, final Comparator<? super T> comparator,
            final Set<Try.Runnable<? extends E>> closeHandlers) {
        this.elements = iter;
        this.sorted = sorted;
        this.comparator = comparator;
        this.closeHandlers = closeHandlers;
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> empty() {
        return EMPTY;
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
                if (n <= 0) {
                    return;
                } else if (n > len - position) {
                    position = len;
                }

                position += n;
            }
        });
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Collection<T> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return newStream(new ExceptionalIterator<T, E>() {
            private Iterator<T> iter = c.iterator();

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

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Iterator<T> iter) {
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

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Iterable<T> iterable) {
        if (iterable == null) {
            return empty();
        }

        return of(iterable.iterator());
    }

    public static <T, E extends Exception> ExceptionalStream<T, E> of(final Stream<? extends T> stream) {
        if (stream == null) {
            return empty();
        }

        final ExceptionalIterator<T, E> iter = new ExceptionalIterator<T, E>() {
            private Stream<? extends T> s = stream;
            private Iterator<? extends T> iter = null;

            @Override
            public boolean hasNext() throws E {
                init();

                return iter.hasNext();
            }

            @Override
            public T next() throws E {
                init();

                return iter.next();
            }

            @Override
            public void skip(long n) throws E {
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
                if (iter == null) {
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

    public static Try<ExceptionalStream<String, IOException>> of(final File file) {
        return of(file, Charsets.DEFAULT);
    }

    public static Try<ExceptionalStream<String, IOException>> of(final File file, final Charset charset) {
        N.checkArgNotNull(file, "file");

        final Reader reader = IOUtil.newBufferedReader(file, charset == null ? Charsets.DEFAULT : charset);

        return of(reader).onClose(new Try.Runnable<IOException>() {
            @Override
            public void run() throws IOException {
                reader.close();
            }
        }).tried();
    }

    public static Try<ExceptionalStream<String, IOException>> of(final Path path) {
        return of(path, Charsets.DEFAULT);
    }

    public static Try<ExceptionalStream<String, IOException>> of(final Path path, final Charset charset) {
        N.checkArgNotNull(path, "path");

        final Reader reader = IOUtil.newBufferedReader(path, charset == null ? Charsets.DEFAULT : charset);

        return of(reader).onClose(new Try.Runnable<IOException>() {
            @Override
            public void run() throws IOException {
                reader.close();
            }
        }).tried();
    }

    public static ExceptionalStream<String, IOException> of(final Reader reader) {
        N.checkArgNotNull(reader, "reader");

        return newStream(new ExceptionalIterator<String, IOException>() {
            private final BufferedReader bufferedReader = reader instanceof BufferedReader ? ((BufferedReader) reader) : new BufferedReader(reader);
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
        });
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @return
     */
    public static ExceptionalStream<Object[], SQLException> of(final ResultSet resultSet) {
        return of(Object[].class, resultSet);
    }

    public static Try<ExceptionalStream<Object[], SQLException>> of(final ResultSet resultSet, final boolean closeResultSet) {
        return of(Object[].class, resultSet, closeResultSet);
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param targetClass Array/List/Map or Entity with getter/setter methods.
     * @param resultSet
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> of(final Class<T> targetClass, final ResultSet resultSet) {
        N.checkArgNotNull(targetClass, "targetClass");
        N.checkArgNotNull(resultSet, "resultSet");

        final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter = new Try.BiFunction<ResultSet, List<String>, T, SQLException>() {
            private final BiRecordGetter<T, RuntimeException> biRecordGetter = BiRecordGetter.to(targetClass);

            @Override
            public T apply(ResultSet resultSet, List<String> columnLabels) throws SQLException {
                return biRecordGetter.apply(resultSet, columnLabels);
            }
        };

        return of(resultSet, recordGetter);
    }

    /**
     * 
     * @param targetClass Array/List/Map or Entity with getter/setter methods.
     * @param resultSet
     * @param closeResultSet
     * @return
     */
    public static <T> Try<ExceptionalStream<T, SQLException>> of(final Class<T> targetClass, final ResultSet resultSet, final boolean closeResultSet) {
        N.checkArgNotNull(targetClass, "targetClass");
        N.checkArgNotNull(resultSet, "resultSet");

        if (closeResultSet) {
            return of(targetClass, resultSet).onClose(new Try.Runnable<SQLException>() {
                @Override
                public void run() throws SQLException {
                    JdbcUtil.closeQuietly(resultSet);
                }
            }).tried();
        } else {
            return of(targetClass, resultSet).tried();
        }
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @param recordGetter
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> of(final ResultSet resultSet, final Try.Function<ResultSet, T, SQLException> recordGetter) {
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
            public void skip(final long n) throws SQLException {
                if (n <= 0) {
                    return;
                }

                final long m = hasNext ? n - 1 : n;

                try {
                    JdbcUtil.skip(resultSet, m);
                } catch (SQLException e) {
                    throw new UncheckedSQLException(e);
                }

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
    public static <T> ExceptionalStream<T, SQLException> of(final ResultSet resultSet,
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
            public void skip(final long n) throws SQLException {
                if (n <= 0) {
                    return;
                }

                final long m = hasNext ? n - 1 : n;

                try {
                    JdbcUtil.skip(resultSet, m);
                } catch (SQLException e) {
                    throw new UncheckedSQLException(e);
                }

                hasNext = false;
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
    public static <T> ExceptionalStream<T, SQLException> of(final ResultSet resultSet, final int columnIndex) {
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
            public void skip(final long n) throws SQLException {
                if (n <= 0) {
                    return;
                }

                final long m = hasNext ? n - 1 : n;

                try {
                    JdbcUtil.skip(resultSet, m);
                } catch (SQLException e) {
                    throw new UncheckedSQLException(e);
                }

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
     */
    public static <T> Try<ExceptionalStream<T, SQLException>> of(final ResultSet resultSet, final int columnIndex, final boolean closeResultSet) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNegative(columnIndex, "columnIndex");

        if (closeResultSet) {
            return ((ExceptionalStream<T, SQLException>) of(resultSet, columnIndex)).onClose(new Try.Runnable<SQLException>() {
                @Override
                public void run() throws SQLException {
                    JdbcUtil.closeQuietly(resultSet);
                }
            }).tried();
        } else {
            return ((ExceptionalStream<T, SQLException>) of(resultSet, columnIndex)).tried();
        }
    }

    /**
     * It's user's responsibility to close the input <code>resultSet</code> after the stream is finished.
     * 
     * @param resultSet
     * @param columnName
     * @return
     */
    public static <T> ExceptionalStream<T, SQLException> of(final ResultSet resultSet, final String columnName) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNull(columnName, "columnName");

        return of(resultSet, getColumnIndex(resultSet, columnName));
    }

    /**
     * 
     * @param resultSet
     * @param columnName
     * @param closeResultSet
     * @return
     */
    public static <T> Try<ExceptionalStream<T, SQLException>> of(final ResultSet resultSet, final String columnName, final boolean closeResultSet) {
        N.checkArgNotNull(resultSet, "resultSet");
        N.checkArgNotNull(columnName, "columnName");

        return of(resultSet, getColumnIndex(resultSet, columnName), closeResultSet);
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

    public static <T, E extends Exception> ExceptionalStream<T, E> iterate(final Try.BooleanSupplier<? extends E> hasNext, final Supplier<? extends T> next) {
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

    public ExceptionalStream<T, E> filter(final Try.Predicate<? super T, ? extends E> predicate) {
        N.checkArgNotNull(predicate, "predicate");

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
        }, closeHandlers);
    }

    public ExceptionalStream<T, E> takeWhile(final Try.Predicate<? super T, ? extends E> predicate) {
        N.checkArgNotNull(predicate, "predicate");

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
        }, closeHandlers);
    }

    public ExceptionalStream<T, E> dropWhile(final Try.Predicate<? super T, ? extends E> predicate) {
        N.checkArgNotNull(predicate, "predicate");

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean hasNext = false;
            private T next = null;
            private boolean dropped = false;

            @Override
            public boolean hasNext() throws E {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.next();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
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

        }, closeHandlers);
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
        N.checkArgNotNull(keyExtractor, "keyExtractor");

        final Set<Object> set = new HashSet<>();

        return filter(new Try.Predicate<T, E>() {
            @Override
            public boolean test(T value) throws E {
                return set.add(hashKey(keyExtractor.apply(value)));
            }
        });
    }

    public <U> ExceptionalStream<U, E> map(final Try.Function<? super T, ? extends U, ? extends E> mapper) {
        N.checkArgNotNull(mapper, "mapper");

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
        N.checkArgNotNull(mapper, "mapper");

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

        final Set<Try.Runnable<? extends E>> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new HashSet<Try.Runnable<? extends E>>(1)
                : new HashSet<Try.Runnable<? extends E>>(closeHandlers);

        newCloseHandlers.add(new Try.Runnable<E>() {
            @Override
            public void run() throws E {
                iter.close();
            }
        });

        return newStream(iter, newCloseHandlers);
    }

    public <R> ExceptionalStream<R, E> flattMap(final Try.Function<? super T, ? extends Collection<? extends R>, ? extends E> mapper) {
        N.checkArgNotNull(mapper, "mapper");

        return flatMap(new Try.Function<T, ExceptionalStream<? extends R, ? extends E>, E>() {
            @Override
            public ExceptionalStream<? extends R, ? extends E> apply(T t) throws E {
                return ExceptionalStream.of(mapper.apply(t));
            }
        });
    }

    public <K> ExceptionalStream<Map.Entry<K, List<T>>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier) {
        final Try.Function<T, T, E> valueMapper = Fn.EE.identity();

        return groupBy(classifier, valueMapper);
    }

    public <K> ExceptionalStream<Map.Entry<K, List<T>>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Supplier<? extends Map<K, List<T>>> mapFactory) {
        final Try.Function<T, T, E> valueMapper = Fn.EE.identity();

        return groupBy(classifier, valueMapper, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public <K, U> ExceptionalStream<Map.Entry<K, List<U>>, E> groupBy(Try.Function<? super T, ? extends K, ? extends E> classifier,
            Try.Function<? super T, ? extends U, E> valueMapper) {
        final Supplier<? extends Map<K, List<U>>> mapFactory = Suppliers.ofMap();

        return groupBy(classifier, valueMapper, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <K, U> ExceptionalStream<Map.Entry<K, List<U>>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Try.Function<? super T, ? extends U, ? extends E> valueMapper, final Supplier<? extends Map<K, List<U>>> mapFactory) {
        N.checkArgNotNull(classifier, "classifier");
        N.checkArgNotNull(valueMapper, "valueMapper");
        N.checkArgNotNull(mapFactory, "mapFactory");

        return newStream(new ExceptionalIterator<Map.Entry<K, List<U>>, E>() {
            private Iterator<Map.Entry<K, List<U>>> iter = null;

            @Override
            public boolean hasNext() throws E {
                init();
                return iter.hasNext();
            }

            @Override
            public Map.Entry<K, List<U>> next() throws E {
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
        final Supplier<? extends Map<K, V>> mapFactory = Suppliers.ofMap();

        return groupBy(classifier, valueMapper, mergeFunction, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see {@link Fn.EE#throwingMerger()}
     * @see {@link Fn.EE#replacingMerger()}
     * @see {@link Fn.EE#ignoringMerger()}
     */
    public <K, V> ExceptionalStream<Map.Entry<K, V>, E> groupBy(final Try.Function<? super T, ? extends K, ? extends E> classifier,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Try.BinaryOperator<V, ? extends E> mergeFunction,
            final Supplier<? extends Map<K, V>> mapFactory) {
        N.checkArgNotNull(classifier, "classifier");
        N.checkArgNotNull(valueMapper, "valueMapper");
        N.checkArgNotNull(mergeFunction, "mergeFunction");
        N.checkArgNotNull(mapFactory, "mapFactory");

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
        N.checkArgNotNull(collapsible, "collapsible");
        N.checkArgNotNull(mergeFunction, "mergeFunction");

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

    public <R> ExceptionalStream<R, E> collapse(final Try.BiPredicate<? super T, ? super T, ? extends E> collapsible, final Supplier<R> supplier,
            final Try.BiConsumer<R, ? super T, ? extends E> accumulator) {
        N.checkArgNotNull(collapsible, "collapsible");
        N.checkArgNotNull(supplier, "supplier");
        N.checkArgNotNull(accumulator, "accumulator");

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
                final R res = supplier.get();
                accumulator.accept(res, hasNext ? next : (next = iter.next()));

                while ((hasNext = iter.hasNext())) {
                    if (collapsible.test(next, (next = iter.next()))) {
                        accumulator.accept(res, next);
                    } else {
                        break;
                    }
                }

                return res;
            }
        }, false, null, closeHandlers);
    }

    public ExceptionalStream<T, E> peek(final Try.Consumer<? super T, ? extends E> action) {
        N.checkArgNotNull(action, "action");

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
        }, closeHandlers);
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
        N.checkArgPositive(size, "size");

        return newStream(new ExceptionalIterator<List<T>, E>() {
            @Override
            public boolean hasNext() throws E {
                return elements.hasNext();
            }

            @Override
            public List<T> next() throws E {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>(size);
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
                elements.skip(n >= Long.MAX_VALUE / size ? Long.MAX_VALUE : n * size);
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
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        final ExceptionalIterator<T, E> elements = this.elements;

        return newStream(new ExceptionalIterator<List<T>, E>() {
            private List<T> prev = null;

            @Override
            public boolean hasNext() throws E {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.next();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public List<T> next() throws E {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>(windowSize);
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    result.addAll(prev.subList(windowSize - cnt, prev.size()));
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.next());
                }

                return prev = result;
            }
        }, false, null, closeHandlers);
    }

    public ExceptionalStream<T, E> skip(final long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        return newStream(new ExceptionalIterator<T, E>() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() throws E {
                if (skipped == false) {
                    skip(n);
                    skipped = true;
                }

                return elements.hasNext();
            }

            @Override
            public T next() throws E {
                if (skipped == false) {
                    skip(n);
                    skipped = true;
                }

                return elements.next();
            }
        }, closeHandlers);
    }

    public ExceptionalStream<T, E> limit(final long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

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

        }, closeHandlers);
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
        N.checkArgNotNull(action, "action");

        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    public Optional<T> min(Comparator<? super T> comparator) throws E {
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
    }

    @SuppressWarnings("rawtypes")

    public Optional<T> minBy(final Function<? super T, ? extends Comparable> keyExtractor) throws E {
        N.checkArgNotNull(keyExtractor, "keyExtractor");

        final Comparator<? super T> comparator = Fn.comparingBy(keyExtractor);

        return min(comparator);
    }

    public Optional<T> max(Comparator<? super T> comparator) throws E {
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
    }

    @SuppressWarnings("rawtypes")

    public Optional<T> maxBy(final Function<? super T, ? extends Comparable> keyExtractor) throws E {
        N.checkArgNotNull(keyExtractor, "keyExtractor");

        final Comparator<? super T> comparator = Fn.comparingBy(keyExtractor);

        return max(comparator);
    }

    public boolean anyMatch(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        N.checkArgNotNull(predicate, "predicate");

        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    public boolean allMatch(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        N.checkArgNotNull(predicate, "predicate");

        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    public boolean noneMatch(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        N.checkArgNotNull(predicate, "predicate");

        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    public Optional<T> findFirst(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        N.checkArgNotNull(predicate, "predicate");

        while (elements.hasNext()) {
            T e = elements.next();

            if (predicate.test(e)) {
                return Optional.of(e);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    public Optional<T> findLast(final Try.Predicate<? super T, ? extends E> predicate) throws E {
        N.checkArgNotNull(predicate, "predicate");

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
    }

    public Optional<T> first() throws E {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        return Optional.of(elements.next());
    }

    public Optional<T> last() throws E {
        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        T next = elements.next();

        while (elements.hasNext()) {
            next = elements.next();
        }

        return Optional.of(next);
    }

    public Object[] toArray() throws E {
        return toList().toArray();
    }

    public <A> A[] toArray(IntFunction<A[]> generator) throws E {
        N.checkArgNotNull(generator, "generator");

        final List<T> list = toList();

        return list.toArray(generator.apply(list.size()));
    }

    public List<T> toList() throws E {
        final List<T> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    public Set<T> toSet() throws E {
        final Set<T> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    public <C extends Collection<T>> C toCollection(final Supplier<C> supplier) throws E {
        N.checkArgNotNull(supplier, "supplier");

        final C result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @throws E
     * @see {@link Fn.EE#throwingMerger()}
     * @see {@link Fn.EE#replacingMerger()}
     * @see {@link Fn.EE#ignoringMerger()}
     */
    public <K, V> Map<K, V> toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper) throws E {
        return toMap(keyExtractor, valueMapper, Suppliers.<K, V> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @throws E
     * @see {@link Fn.EE#throwingMerger()}
     * @see {@link Fn.EE#replacingMerger()}
     * @see {@link Fn.EE#ignoringMerger()}
     */
    public <K, V, M extends Map<K, V>> M toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Supplier<M> mapFactory) throws E {
        return toMap(keyExtractor, valueMapper, Fn.EE.<V, E> throwingMerger(), mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @throws E
     * @see {@link Fn.EE#throwingMerger()}
     * @see {@link Fn.EE#replacingMerger()}
     * @see {@link Fn.EE#ignoringMerger()}
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
     * @see {@link Fn.EE#throwingMerger()}
     * @see {@link Fn.EE#replacingMerger()}
     * @see {@link Fn.EE#ignoringMerger()}
     */
    public <K, V, M extends Map<K, V>> M toMap(final Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            final Try.Function<? super T, ? extends V, ? extends E> valueMapper, final Try.BinaryOperator<V, ? extends E> mergeFunction,
            final Supplier<M> mapFactory) throws E {
        N.checkArgNotNull(keyExtractor, "keyExtractor");
        N.checkArgNotNull(valueMapper, "valueMapper");
        N.checkArgNotNull(mergeFunction, "mergeFunction");
        N.checkArgNotNull(mapFactory, "mapFactory");

        final M result = mapFactory.get();
        T next = null;
        K key = null;

        while (elements.hasNext()) {
            next = elements.next();
            key = keyExtractor.apply(next);

            if (result.containsKey(key)) {
                result.put(key, mergeFunction.apply(result.get(key), valueMapper.apply(next)));
            } else {
                result.put(key, valueMapper.apply(next));
            }
        }

        return result;
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
        final Try.Function<T, T, E> valueMapper = Fn.EE.identity();

        return groupTo(classifier, valueMapper, mapFactory);
    }

    public <K, U> Map<K, List<U>> groupTo(Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            Try.Function<? super T, ? extends U, ? extends E> valueMapper) throws E {
        return groupTo(keyExtractor, valueMapper, Suppliers.<K, List<U>> ofMap());
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <K, U, M extends Map<K, List<U>>> M groupTo(Try.Function<? super T, ? extends K, ? extends E> keyExtractor,
            Try.Function<? super T, ? extends U, ? extends E> valueMapper, Supplier<M> mapFactory) throws E {
        N.checkArgNotNull(keyExtractor, "keyExtractor");
        N.checkArgNotNull(valueMapper, "valueMapper");
        N.checkArgNotNull(mapFactory, "mapFactory");

        final M result = mapFactory.get();
        T next = null;
        K key = null;

        while (elements.hasNext()) {
            next = elements.next();
            key = keyExtractor.apply(next);

            if (result.containsKey(key) == false) {
                result.put(key, new ArrayList<U>());
            }

            result.get(key).add(valueMapper.apply(next));
        }

        return result;
    }

    public long count() throws E {
        return elements.count();
    }

    /**
     * 
     * @return
     * @throws NonUniqueResultException if there are more than one elements.
     * @throws E
     */
    public Optional<T> onlyOne() throws NonUniqueResultException, E {
        Optional<T> result = Optional.empty();

        if (elements.hasNext()) {
            result = Optional.of(elements.next());

            if (elements.hasNext()) {
                throw new NonUniqueResultException("There are at least two elements: " + Strings.concat(result.get(), ", ", elements.next()));
            }
        }

        return result;
    }

    public OptionalInt sumInt(Try.ToIntFunction<T, E> func) throws E {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        long sum = 0;

        while (elements.hasNext()) {
            sum += func.applyAsInt(elements.next());
        }

        return OptionalInt.of(N.toIntExact(sum));
    }

    public OptionalLong sumLong(Try.ToLongFunction<T, E> func) throws E {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        long sum = 0;

        while (elements.hasNext()) {
            sum += func.applyAsLong(elements.next());
        }

        return OptionalLong.of(sum);
    }

    public OptionalDouble sumDouble(Try.ToDoubleFunction<T, E> func) throws E {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        final List<Double> list = new ArrayList<>();

        while (elements.hasNext()) {
            list.add(func.applyAsDouble(elements.next()));
        }

        return OptionalDouble.of(N.sumDouble(list));
    }

    public OptionalDouble averageInt(Try.ToIntFunction<T, E> func) throws E {
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
    }

    public OptionalDouble averageLong(Try.ToLongFunction<T, E> func) throws E {
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
    }

    public OptionalDouble averageDouble(Try.ToDoubleFunction<T, E> func) throws E {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        final List<Double> list = new ArrayList<>();

        while (elements.hasNext()) {
            list.add(func.applyAsDouble(elements.next()));
        }

        return N.averageLong(list);
    }

    public T reduce(T identity, Try.BinaryOperator<T, ? extends E> accumulator) throws E {
        N.checkArgNotNull(accumulator, "accumulator");

        T result = identity;

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return result;
    }

    public Optional<T> reduce(Try.BinaryOperator<T, ? extends E> accumulator) throws E {
        N.checkArgNotNull(accumulator, "accumulator");

        if (elements.hasNext() == false) {
            return Optional.empty();
        }

        T result = elements.next();

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return Optional.of(result);
    }

    public <R> R collect(Supplier<R> supplier, final Try.BiConsumer<R, ? super T, ? extends E> accumulator) throws E {
        N.checkArgNotNull(supplier, "supplier");
        N.checkArgNotNull(accumulator, "accumulator");

        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    public <R, RR> RR collect(Supplier<R> supplier, final Try.BiConsumer<R, ? super T, ? extends E> accumulator,
            final Try.Function<? super R, ? extends RR, E> finisher) throws E {
        N.checkArgNotNull(supplier, "supplier");
        N.checkArgNotNull(accumulator, "accumulator");
        N.checkArgNotNull(finisher, "finisher");

        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return finisher.apply(result);
    }

    public <R, A> R collect(final Collector<? super T, A, R> collector) throws E {
        N.checkArgNotNull(collector, "collector");

        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (elements.hasNext()) {
            accumulator.accept(container, elements.next());
        }

        return collector.finisher().apply(container);
    }

    public <R, RR, A> RR collectAndThen(final Collector<? super T, A, R> collector, final Try.Function<? super R, ? extends RR, E> func) throws E {
        N.checkArgNotNull(collector, "collector");
        N.checkArgNotNull(func, "func");

        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (elements.hasNext()) {
            accumulator.accept(container, elements.next());
        }

        return func.apply(collector.finisher().apply(container));
    }

    public <R, A> R collect(java.util.stream.Collector<? super T, A, R> collector) throws E {
        final A container = collector.supplier().get();
        final java.util.function.BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (elements.hasNext()) {
            accumulator.accept(container, elements.next());
        }

        return collector.finisher().apply(container);
    }

    public <R, RR, A> RR collectAndThen(final java.util.stream.Collector<? super T, A, R> collector, final Try.Function<? super R, ? extends RR, E> func)
            throws E {
        N.checkArgNotNull(collector, "collector");
        N.checkArgNotNull(func, "func");

        final A container = collector.supplier().get();
        final java.util.function.BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (elements.hasNext()) {
            accumulator.accept(container, elements.next());
        }

        return func.apply(collector.finisher().apply(container));
    }

    public Try<ExceptionalStream<T, E>> tried() {
        return Try.of(this);
    }

    public Stream<T> __() {
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
                    try {
                        elements.skip(n);
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
                    try {
                        elements.skip(n);
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

    public <R> R __(Try.Function<? super ExceptionalStream<T, E>, R, ? extends E> transfer) throws E {
        N.checkArgNotNull(transfer, "transfer");

        return transfer.apply(this);
    }

    public ExceptionalStream<T, E> onClose(final Try.Runnable<? extends E> closeHandler) {
        N.checkArgNotNull(closeHandler, "closeHandler");

        final Set<Try.Runnable<? extends E>> newCloseHandlers = new HashSet<>();

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return newStream(elements, newCloseHandlers);
    }

    @Override
    public synchronized void close() throws E {
        if (isClosed || N.isNullOrEmpty(closeHandlers)) {
            return;
        }

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

    static <T, E extends Exception> ExceptionalStream<T, E> newStream(final ExceptionalIterator<T, E> iter) {
        return new ExceptionalStream<>(iter, null);
    }

    static <T, E extends Exception> ExceptionalStream<T, E> newStream(final ExceptionalIterator<T, E> iter,
            final Set<Try.Runnable<? extends E>> closeHandlers) {
        return new ExceptionalStream<>(iter, closeHandlers);
    }

    static <T, E extends Exception> ExceptionalStream<T, E> newStream(final ExceptionalIterator<T, E> iter, final boolean sorted,
            final Comparator<? super T> comparator, final Set<Try.Runnable<? extends E>> closeHandlers) {
        return new ExceptionalStream<>(iter, sorted, comparator, closeHandlers);
    }

    static Object hashKey(Object obj) {
        return obj == null || obj.getClass().isArray() == false ? obj : Wrapper.of(obj);
    }

    static boolean isSameComparator(Comparator<?> a, Comparator<?> b) {
        return a == b || (a == null && b == Comparators.NATURAL_ORDER) || (b == null && a == Comparators.NATURAL_ORDER);
    }

    static abstract class ExceptionalIterator<T, E extends Exception> {
        public abstract boolean hasNext() throws E;

        public abstract T next() throws E;

        public void skip(long n) throws E {
            if (n <= 0) {
                return;
            }

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

    public static final class StreamE<T, E extends Exception> extends ExceptionalStream<T, E> {
        StreamE(com.landawn.abacus.util.ExceptionalStream.ExceptionalIterator<T, E> iter, boolean sorted, Comparator<? super T> comparator,
                Set<com.landawn.abacus.util.Try.Runnable<? extends E>> closeHandlers) {
            super(iter, sorted, comparator, closeHandlers);
        }
    }
}
