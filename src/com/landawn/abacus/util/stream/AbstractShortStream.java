package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Map;

import com.landawn.abacus.util.IndexedShort;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortTriFunction;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractShortStream extends ShortStream {

    AbstractShortStream(Collection<Runnable> closeHandlers) {
        super(closeHandlers);
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
    public Stream<IndexedShort> indexed() {
        final MutableLong idx = new MutableLong();

        return mapToObj(new ShortFunction<IndexedShort>() {
            @Override
            public IndexedShort apply(short t) {
                return IndexedShort.of(idx.getAndIncrement(), t);
            }
        });
    }

    @Override
    public ShortStream append(ShortStream stream) {
        return ShortStream.concat(this, stream);
    }

    @Override
    public ShortStream merge(ShortStream b, ShortBiFunction<Nth> nextSelector) {
        return ShortStream.merge(this, b, nextSelector);
    }

    @Override
    public ShortStream zipWith(ShortStream b, ShortBiFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, zipFunction);
    }

    @Override
    public ShortStream zipWith(ShortStream b, ShortStream c, ShortTriFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, c, zipFunction);
    }

    @Override
    public ShortStream zipWith(ShortStream b, short valueForNoneA, short valueForNoneB, ShortBiFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public ShortStream zipWith(ShortStream b, ShortStream c, short valueForNoneA, short valueForNoneB, short valueForNoneC,
            ShortTriFunction<Short> zipFunction) {
        return ShortStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public ShortStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public ShortStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public ShortStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
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
        return DEFAULT_SPILTTER;
    }

    @Override
    public ShortStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        close(closeHandlers);
    }
}
