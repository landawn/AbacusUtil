package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Map;

import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharTriFunction;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractCharStream extends CharStream {

    AbstractCharStream(Collection<Runnable> closeHandlers) {
        super(closeHandlers);
    }

    @Override
    public Optional<Map<Percentage, Character>> distribution() {
        final char[] a = sorted().toArray();

        if (a.length == 0) {
            return Optional.empty();
        }

        return Optional.of(N.distribution(a));
    }

    @Override
    public Pair<CharSummaryStatistics, Optional<Map<Percentage, Character>>> summarize2() {
        final char[] a = sorted().toArray();

        final CharSummaryStatistics summaryStatistics = new CharSummaryStatistics(a.length, N.sum(a), a[0], a[a.length - 1]);
        final Optional<Map<Percentage, Character>> distribution = a.length == 0 ? Optional.<Map<Percentage, Character>> empty()
                : Optional.of(N.distribution(a));

        return Pair.of(summaryStatistics, distribution);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public CharStream append(CharStream stream) {
        return CharStream.concat(this, stream);
    }

    @Override
    public CharStream merge(CharStream b, CharBiFunction<Nth> nextSelector) {
        return CharStream.merge(this, b, nextSelector);
    }

    @Override
    public CharStream zipWith(CharStream b, CharBiFunction<Character> zipFunction) {
        return CharStream.zip(this, b, zipFunction);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, CharTriFunction<Character> zipFunction) {
        return CharStream.zip(this, b, c, zipFunction);
    }

    @Override
    public CharStream zipWith(CharStream b, char valueForNoneA, char valueForNoneB, CharBiFunction<Character> zipFunction) {
        return CharStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, char valueForNoneA, char valueForNoneB, char valueForNoneC, CharTriFunction<Character> zipFunction) {
        return CharStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    @Override
    public CharStream parallel() {
        return parallel(DEFAULT_SPILTTER);
    }

    @Override
    public CharStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, DEFAULT_SPILTTER);
    }

    @Override
    public CharStream parallel(BaseStream.Splitter splitter) {
        return parallel(DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public CharStream maxThreadNum(int maxThreadNum) {
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
    public CharStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        close(closeHandlers);
    }
}
