package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Map;

import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractDoubleStream extends DoubleStream {

    AbstractDoubleStream(Collection<Runnable> closeHandlers) {
        super(closeHandlers);
    }

    @Override
    public Optional<Map<Percentage, Double>> distribution() {
        final double[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<DoubleSummaryStatistics, Optional<Map<Percentage, Double>>> summarize2() {
        final double[] a = sorted().toArray();

        final DoubleSummaryStatistics summaryStatistics = new DoubleSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Double>> distribution = a.length == 0 ? Optional.<Map<Percentage, Double>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public DoubleStream append(DoubleStream stream) {
        return DoubleStream.concat(this, stream);
    }

    @Override
    public DoubleStream merge(DoubleStream b, DoubleBiFunction<Nth> nextSelector) {
        return DoubleStream.merge(this, b, nextSelector);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleBiFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, zipFunction);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, DoubleTriFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, c, zipFunction);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, double valueForNoneA, double valueForNoneB, DoubleBiFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, double valueForNoneA, double valueForNoneB, double valueForNoneC,
            DoubleTriFunction<Double> zipFunction) {
        return DoubleStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public DoubleStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public DoubleStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public DoubleStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public DoubleStream maxThreadNum(int maxThreadNum) {
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
    public DoubleStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        close(closeHandlers);
    }
}
