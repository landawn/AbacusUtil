package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.AbstractStream.LocalLinkedHashSet;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractShortStream extends ShortStream {
    final Set<Runnable> closeHandlers;

    AbstractShortStream(Collection<Runnable> closeHandlers) {
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null
                : (closeHandlers instanceof LocalLinkedHashSet ? (LocalLinkedHashSet<Runnable>) closeHandlers : new LocalLinkedHashSet<>(closeHandlers));
    }

    @Override
    public Optional<Map<Percentage, Short>> distribution() {
        final short[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<ShortSummaryStatistics, Optional<Map<Percentage, Short>>> summarize2() {
        final short[] a = sorted().toArray();

        final ShortSummaryStatistics summaryStatistics = new ShortSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Short>> distribution = a.length == 0 ? Optional.<Map<Percentage, Short>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public ShortStream parallel() {
        return parallel(Stream.DEFAULT_SPILTTER);
    }

    @Override
    public ShortStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, Stream.DEFAULT_SPILTTER);
    }

    @Override
    public ShortStream parallel(BaseStream.Splitter splitter) {
        return parallel(Stream.DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public ShortStream maxThreadNum(int maxThreadNum) {
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
    public ShortStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        Stream.close(closeHandlers);
    }
}
