package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Map;

import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.IntBiFunction;
import com.landawn.abacus.util.function.IntTriFunction;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractIntStream extends IntStream {

    AbstractIntStream(Collection<Runnable> closeHandlers) {
        super(closeHandlers);
    }

    @Override
    public Optional<Map<Percentage, Integer>> distribution() {
        final int[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<IntSummaryStatistics, Optional<Map<Percentage, Integer>>> summarize2() {
        final int[] a = sorted().toArray();

        final IntSummaryStatistics summaryStatistics = new IntSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Integer>> distribution = a.length == 0 ? Optional.<Map<Percentage, Integer>> empty() : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public IntStream append(IntStream stream) {
        return IntStream.concat(this, stream);
    }

    @Override
    public IntStream merge(IntStream b, IntBiFunction<Nth> nextSelector) {
        return IntStream.merge(this, b, nextSelector);
    }

    @Override
    public IntStream zipWith(IntStream b, IntBiFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, zipFunction);
    }

    @Override
    public IntStream zipWith(IntStream b, IntStream c, IntTriFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, c, zipFunction);
    }

    @Override
    public IntStream zipWith(IntStream b, int valueForNoneA, int valueForNoneB, IntBiFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public IntStream zipWith(IntStream b, IntStream c, int valueForNoneA, int valueForNoneB, int valueForNoneC, IntTriFunction<Integer> zipFunction) {
        return IntStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public IntStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public IntStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public IntStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public IntStream maxThreadNum(int maxThreadNum) {
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
    public IntStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        close(closeHandlers);
    }
}
