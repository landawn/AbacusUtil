package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Map;

import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractLongStream extends LongStream {

    AbstractLongStream(Collection<Runnable> closeHandlers) {
        super(closeHandlers);
    }

    @Override
    public Optional<Map<Percentage, Long>> distribution() {
        final long[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<LongSummaryStatistics, Optional<Map<Percentage, Long>>> summarize2() {
        final long[] a = sorted().toArray();

        final LongSummaryStatistics summaryStatistics = new LongSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Long>> distribution = a.length == 0 ? Optional.<Map<Percentage, Long>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public LongStream append(LongStream stream) {
        return LongStream.concat(this, stream);
    }

    @Override
    public LongStream merge(LongStream b, LongBiFunction<Nth> nextSelector) {
        return LongStream.merge(this, b, nextSelector);
    }

    @Override
    public LongStream zipWith(LongStream b, LongBiFunction<Long> zipFunction) {
        return LongStream.zip(this, b, zipFunction);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, LongTriFunction<Long> zipFunction) {
        return LongStream.zip(this, b, c, zipFunction);
    }

    @Override
    public LongStream zipWith(LongStream b, long valueForNoneA, long valueForNoneB, LongBiFunction<Long> zipFunction) {
        return LongStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, long valueForNoneA, long valueForNoneB, long valueForNoneC, LongTriFunction<Long> zipFunction) {
        return LongStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public LongStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public LongStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public LongStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public LongStream maxThreadNum(int maxThreadNum) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");  

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public Splitter splitter() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return DEFAULT_SPILTTER;
    }

    @Override
    public LongStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        close(closeHandlers);
    }
}
