package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Set;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.function.ObjFloatConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.AbstractStream.LocalLinkedHashSet;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
abstract class AbstractFloatStream extends FloatStream {
    final Set<Runnable> closeHandlers;

    AbstractFloatStream(Collection<Runnable> closeHandlers) {
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null
                : (closeHandlers instanceof LocalLinkedHashSet ? (LocalLinkedHashSet<Runnable>) closeHandlers : new LocalLinkedHashSet<>(closeHandlers));
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjFloatConsumer<R> accumulator) {
        throw new UnsupportedOperationException("It's not supported parallel stream.");
    }

    @Override
    public FloatStream parallel() {
        return parallel(Stream.DEFAULT_SPILTTER);
    }

    @Override
    public FloatStream parallel(int maxThreadNum) {
        return parallel(maxThreadNum, Stream.DEFAULT_SPILTTER);
    }

    @Override
    public FloatStream parallel(BaseStream.Splitter splitter) {
        return parallel(Stream.DEFAULT_MAX_THREAD_NUM, splitter);
    }

    @Override
    public int maxThreadNum() {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return 1;
    }

    @Override
    public FloatStream maxThreadNum(int maxThreadNum) {
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
    public FloatStream splitter(Splitter splitter) {
        // throw new UnsupportedOperationException("It's not supported sequential stream.");

        // ignore, do nothing if it's sequential stream.
        return this;
    }

    @Override
    public void close() {
        Stream.close(closeHandlers);
    }
}
