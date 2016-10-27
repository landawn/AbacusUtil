package com.landawn.abacus.util.stream;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.exception.AbacusIOException;
import com.landawn.abacus.exception.AbacusSQLException;
import com.landawn.abacus.util.BufferedWriter;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.JdbcUtil;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectFactory;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 */
abstract class AbstractStream<T> extends Stream<T> implements BaseStream<T, Stream<T>> {

    final Set<Runnable> closeHandlers;

    AbstractStream(Collection<Runnable> closeHandlers) {
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null
                : (closeHandlers instanceof LocalLinkedHashSet ? (LocalLinkedHashSet<Runnable>) closeHandlers : new LocalLinkedHashSet<>(closeHandlers));
    }

    @Override
    public Optional<Map<Percentage, T>> distribution() {
        final Object[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of((Map<Percentage, T>) N.distribution(a));
    }

    @Override
    public Optional<Map<Percentage, T>> distribution(Comparator<? super T> comparator) {
        final Object[] a = sorted(comparator).toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of((Map<Percentage, T>) N.distribution(a));
    }

    @Override
    public Pair<CharSummaryStatistics, Optional<Map<Percentage, Character>>> summarizeChar2(ToCharFunction<? super T> mapper) {
        final char[] a = mapToChar(mapper).sorted().toArray();

        final CharSummaryStatistics summaryStatistics = new CharSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Character>> distribution = a.length == 0 ? Optional.<Map<Percentage, Character>> empty()
                : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarizeByte2(ToByteFunction<? super T> mapper) {
        final byte[] a = mapToByte(mapper).sorted().toArray();

        final ByteSummaryStatistics summaryStatistics = new ByteSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Byte>> distribution = a.length == 0 ? Optional.<Map<Percentage, Byte>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<ShortSummaryStatistics, Optional<Map<Percentage, Short>>> summarizeShort2(ToShortFunction<? super T> mapper) {
        final short[] a = mapToShort(mapper).sorted().toArray();

        final ShortSummaryStatistics summaryStatistics = new ShortSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Short>> distribution = a.length == 0 ? Optional.<Map<Percentage, Short>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<IntSummaryStatistics, Optional<Map<Percentage, Integer>>> summarizeInt2(ToIntFunction<? super T> mapper) {
        final int[] a = mapToInt(mapper).sorted().toArray();

        final IntSummaryStatistics summaryStatistics = new IntSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Integer>> distribution = a.length == 0 ? Optional.<Map<Percentage, Integer>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<LongSummaryStatistics, Optional<Map<Percentage, Long>>> summarizeLong2(ToLongFunction<? super T> mapper) {
        final long[] a = mapToLong(mapper).sorted().toArray();

        final LongSummaryStatistics summaryStatistics = new LongSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Long>> distribution = a.length == 0 ? Optional.<Map<Percentage, Long>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<FloatSummaryStatistics, Optional<Map<Percentage, Float>>> summarizeFloat2(ToFloatFunction<? super T> mapper) {
        final float[] a = mapToFloat(mapper).sorted().toArray();

        final FloatSummaryStatistics summaryStatistics = new FloatSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Float>> distribution = a.length == 0 ? Optional.<Map<Percentage, Float>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarizeDouble2(ToDoubleFunction<? super T> mapper) {
        final double[] a = mapToDouble(mapper).sorted().toArray();

        final DoubleSummaryStatistics summaryStatistics = new DoubleSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Double>> distribution = a.length == 0 ? Optional.<Map<Percentage, Double>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public DataSet toDataSet(List<String> columnNames) {
        return N.newDataSet(columnNames, toList());
    }

    @Override
    public String join(CharSequence delimiter) {
        final Function<T, String> mapper = new Function<T, String>() {
            @Override
            public String apply(T t) {
                return N.toString(t);
            }
        };

        return this.map(mapper).collect(Collectors.joining(delimiter));
    }

    @Override
    public String join(CharSequence delimiter, CharSequence prefix, CharSequence suffix) {
        final Function<T, String> mapper = new Function<T, String>() {
            @Override
            public String apply(T t) {
                return N.toString(t);
            }
        };

        return this.map(mapper).collect(Collectors.joining(delimiter, prefix, suffix));
    }

    @Override
    public long persist(File file, Function<? super T, String> toLine) {
        Writer writer = null;

        try {
            writer = new FileWriter(file);
            return persist(writer, toLine);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(writer);
        }
    }

    @Override
    public long persist(OutputStream os, Function<? super T, String> toLine) {
        final BufferedWriter bw = ObjectFactory.createBufferedWriter(os);

        try {
            return persist(bw, toLine);
        } finally {
            ObjectFactory.recycle(bw);
        }
    }

    @Override
    public long persist(Writer writer, Function<? super T, String> toLine) {
        final Iterator<T> iter = iterator();
        final BufferedWriter bw = writer instanceof BufferedWriter ? (BufferedWriter) writer : ObjectFactory.createBufferedWriter(writer);
        long cnt = 0;

        try {
            while (iter.hasNext()) {
                bw.write(toLine.apply(iter.next()));
                bw.write(N.LINE_SEPARATOR);
                cnt++;
            }
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (bw != writer) {
                ObjectFactory.recycle(bw);
            }
        }
        return cnt;
    }

    @Override
    public long persist(final Connection conn, final String insertSQL, final int batchSize, final int batchInterval,
            final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

            return persist(stmt, batchSize, batchInterval, stmtSetter);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    @Override
    public long persist(final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        final Iterator<T> iter = iterator();

        long cnt = 0;
        try {
            while (iter.hasNext()) {
                stmtSetter.accept(stmt, iter.next());

                stmt.addBatch();

                if ((++cnt % batchSize) == 0) {
                    stmt.executeBatch();
                    stmt.clearBatch();

                    if (batchInterval > 0) {
                        N.sleep(batchInterval);
                    }
                }
            }

            if ((cnt % batchSize) > 0) {
                stmt.executeBatch();
                stmt.clearBatch();
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        }

        return cnt;
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public Stream<T> parallel() {
        return parallel(Stream.DEFAULT_SPILTTER);
    }

    @Override
    public Stream<T> parallel(int maxThreadNum) {
        return parallel(maxThreadNum, Stream.DEFAULT_SPILTTER);
    }

    @Override
    public Stream<T> parallel(Splitter splitter) {
        return parallel(Stream.DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public Stream<T> maxThreadNum(int maxThreadNum) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");  

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public Splitter splitter() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return Stream.DEFAULT_SPILTTER;
    }

    @Override
    public Stream<T> splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        Stream.close(closeHandlers);
    }

    static final class LocalLinkedHashSet<T> extends LinkedHashSet<T> {
        private static final long serialVersionUID = -97425473105100734L;

        public LocalLinkedHashSet() {
            super();
        }

        public LocalLinkedHashSet(int initialCapacity) {
            super(initialCapacity);
        }

        public LocalLinkedHashSet(int initialCapacity, float loadFactor) {
            super(initialCapacity, loadFactor);
        }

        public LocalLinkedHashSet(Collection<? extends T> c) {
            super(c);
        }
    }
}
