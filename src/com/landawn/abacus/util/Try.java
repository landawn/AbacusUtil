/*
 * Copyright (C) 2016 HaiYang Li
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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import com.landawn.abacus.util.stream.Stream;

/**
 * Catch checked exception and convert it to <code>RuntimeException</code>.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Try<T extends AutoCloseable> {
    private final T t;

    Try(final T t) {
        this.t = t;
    }

    public static <T extends AutoCloseable> Try<T> of(final T t) {
        return new Try<>(t);
    }

    public static <T extends AutoCloseable> Try<T> of(final Supplier<T, ? extends Exception> supplier) {
        try {
            return new Try<>(supplier.get());
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public static Try<Reader> reader(final File file) {
        try {
            return of((Reader) new FileReader(file));
        } catch (FileNotFoundException e) {
            throw N.toRuntimeException(e);
        }
    }

    public static Try<Writer> writer(final File file) {
        try {
            return of((Writer) new FileWriter(file));
        } catch (IOException e) {
            throw N.toRuntimeException(e);
        }
    }

    public static Try<Stream<String>> stream(final File file) {
        final Reader reader = IOUtil.newBufferedReader(file);

        return new Try<>(Stream.of(reader).onClose(new java.lang.Runnable() {
            @Override
            public void run() {
                IOUtil.close(reader);
            }
        }));
    }

    //    public static java.lang.Runnable of(final Try.Runnable run) {
    //        return new java.lang.Runnable() {
    //            @Override
    //            public void run() {
    //                try {
    //                    run.run();
    //                } catch (Exception e) {
    //                    throw N.toRuntimeException(e);
    //                }
    //            }
    //        };
    //    }
    //
    //    public static <R> Try.Callable<R> of(final java.util.concurrent.Callable<R> call) {
    //        return new Try.Callable<R>() {
    //            @Override
    //            public R call() {
    //                try {
    //                    return call.call();
    //                } catch (Exception e) {
    //                    throw N.toRuntimeException(e);
    //                }
    //            }
    //        };
    //    }

    /**
     * 
     * @param cmd
     * @throws RuntimeException if some error happens
     */
    public static void run(final Try.Runnable<? extends Exception> cmd) {
        try {
            cmd.run();
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public static void run(final Try.Runnable<? extends Exception> cmd, final com.landawn.abacus.util.function.Consumer<? super Exception> actionOnError) {
        N.requireNonNull(actionOnError);

        try {
            cmd.run();
        } catch (Exception e) {
            actionOnError.accept(e);
        }
    }

    /**
     * 
     * @param cmd
     * @return
     * @throws RuntimeException if some error happens
     */
    public static <R> R call(final java.util.concurrent.Callable<R> cmd) {
        try {
            return cmd.call();
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public static <R> R call(final java.util.concurrent.Callable<R> cmd, final com.landawn.abacus.util.function.Function<? super Exception, R> actionOnError) {
        N.requireNonNull(actionOnError);

        try {
            return cmd.call();
        } catch (Exception e) {
            return actionOnError.apply(e);
        }
    }

    public static <R> R call(final java.util.concurrent.Callable<R> cmd, final com.landawn.abacus.util.function.Supplier<R> supplier) {
        N.requireNonNull(supplier);

        try {
            return cmd.call();
        } catch (Exception e) {
            return supplier.get();
        }
    }

    public static <R> R call(final java.util.concurrent.Callable<R> cmd, final R defaultValue) {
        try {
            return cmd.call();
        } catch (Exception e) {
            return defaultValue;
        }
    }

    /**
     * 
     * @param cmd
     * @param predicate
     * @param supplier
     * @return the value returned <code>Supplier.get()</code> if some error happens and <code>predicate</code> return true.
     * @throws RuntimeException if some error happens and <code>predicate</code> return false.
     */
    public static <R> R call(final java.util.concurrent.Callable<R> cmd, final com.landawn.abacus.util.function.Predicate<? super Exception> predicate,
            final com.landawn.abacus.util.function.Supplier<R> supplier) {
        N.requireNonNull(predicate);
        N.requireNonNull(supplier);

        try {
            return cmd.call();
        } catch (Exception e) {
            if (predicate.test(e)) {
                return supplier.get();
            } else {
                throw N.toRuntimeException(e);
            }
        }
    }

    /**
     * 
     * @param cmd
     * @param predicate
     * @param defaultValue
     * @return the <code>defaultValue()</code> if some error happens and <code>predicate</code> return true.
     * @throws RuntimeException if some error happens and <code>predicate</code> return false.
     */
    public static <R> R call(final java.util.concurrent.Callable<R> cmd, final com.landawn.abacus.util.function.Predicate<? super Exception> predicate,
            final R defaultValue) {
        N.requireNonNull(predicate);

        try {
            return cmd.call();
        } catch (Exception e) {
            if (predicate.test(e)) {
                return defaultValue;
            } else {
                throw N.toRuntimeException(e);
            }
        }
    }

    public T val() {
        return t;
    }

    public void run(final Try.Consumer<? super T, ? extends Exception> cmd) {
        try {
            cmd.accept(t);
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        } finally {
            IOUtil.close(t);
        }
    }

    public void run(final Try.Consumer<? super T, ? extends Exception> cmd, final com.landawn.abacus.util.function.Consumer<? super Exception> actionOnError) {
        N.requireNonNull(actionOnError);

        try {
            cmd.accept(t);
        } catch (Exception e) {
            actionOnError.accept(e);
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R, ? extends Exception> cmd) {
        try {
            return cmd.apply(t);
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R, ? extends Exception> cmd,
            final com.landawn.abacus.util.function.Function<? super Exception, R> actionOnError) {
        N.requireNonNull(actionOnError);

        try {
            return cmd.apply(t);
        } catch (Exception e) {
            return actionOnError.apply(e);
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R, ? extends Exception> cmd, final com.landawn.abacus.util.function.Supplier<R> supplier) {
        N.requireNonNull(supplier);

        try {
            return cmd.apply(t);
        } catch (Exception e) {
            return supplier.get();
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R, ? extends Exception> cmd, final R defaultValue) {
        try {
            return cmd.apply(t);
        } catch (Exception e) {
            return defaultValue;
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R, ? extends Exception> cmd, final com.landawn.abacus.util.function.Predicate<? super Exception> predicate,
            final com.landawn.abacus.util.function.Supplier<R> supplier) {
        N.requireNonNull(predicate);
        N.requireNonNull(supplier);

        try {
            return cmd.apply(t);
        } catch (Exception e) {
            if (predicate.test(e)) {
                return supplier.get();
            } else {
                throw N.toRuntimeException(e);
            }
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R, ? extends Exception> cmd, final com.landawn.abacus.util.function.Predicate<? super Exception> predicate,
            final R defaultValue) {
        N.requireNonNull(predicate);

        try {
            return cmd.apply(t);
        } catch (Exception e) {
            if (predicate.test(e)) {
                return defaultValue;
            } else {
                throw N.toRuntimeException(e);
            }
        } finally {
            IOUtil.close(t);
        }
    }

    public static interface Runnable<E extends Exception> {
        void run() throws E;
    }

    public static interface Callable<R, E extends Exception> extends java.util.concurrent.Callable<R> {

        @Override
        R call() throws E;

        public static <E extends Exception> Callable<Void, E> of(Runnable<E> cmd) {
            N.requireNonNull(cmd);

            return new Callable<Void, E>() {
                @Override
                public Void call() throws E {
                    cmd.run();
                    return null;
                }
            };
        }
    }

    public static interface Supplier<T, E extends Exception> {
        T get() throws E;
    }

    public static interface BooleanSupplier<E extends Exception> {
        boolean getAsBoolean() throws E;
    }

    public static interface CharSupplier<E extends Exception> {
        char getAsChar() throws E;
    }

    public static interface ByteSupplier<E extends Exception> {
        byte getAsByte() throws E;
    }

    public static interface ShortSupplier<E extends Exception> {
        short getAsShort() throws E;
    }

    public static interface IntSupplier<E extends Exception> {
        int getAsInt() throws E;
    }

    public static interface LongSupplier<E extends Exception> {
        long getAsLong() throws E;
    }

    public static interface FloatSupplier<E extends Exception> {
        float getAsFloat() throws E;
    }

    public static interface DoubleSupplier<E extends Exception> {
        double getAsDouble() throws E;
    }

    public static interface Predicate<T, E extends Exception> {
        boolean test(T t) throws E;
    }

    public static interface BiPredicate<T, U, E extends Exception> {
        boolean test(T t, U u) throws E;
    }

    public static interface TriPredicate<A, B, C, E extends Exception> {
        boolean test(A a, B b, C c) throws E;
    }

    @SuppressWarnings("hiding")
    public static interface QuadPredicate<A, B, C, D, E extends Exception> {
        boolean test(A a, B b, C c, D d) throws E;
    }

    public static interface Function<T, R, E extends Exception> {
        R apply(T t) throws E;
    }

    public static interface BiFunction<T, U, R, E extends Exception> {
        R apply(T t, U u) throws E;
    }

    public static interface TriFunction<A, B, C, R, E extends Exception> {
        R apply(A a, B b, C c) throws E;
    }

    @SuppressWarnings("hiding")
    public interface QuadFunction<A, B, C, D, R, E extends Exception> {
        R apply(A a, B b, C c, D d) throws E;
    }

    public static interface Consumer<T, E extends Exception> {
        void accept(T t) throws E;
    }

    public static interface BiConsumer<T, U, E extends Exception> {
        void accept(T t, U u) throws E;
    }

    public static interface TriConsumer<A, B, C, E extends Exception> {
        void accept(A a, B b, C c) throws E;
    }

    @SuppressWarnings("hiding")
    public interface QuadConsumer<A, B, C, D, E extends Exception> {
        void accept(A a, B b, C c, D d) throws E;
    }

    public interface IndexedConsumer<T, E extends Exception> {
        void accept(int idx, T e) throws E;
    }

    public interface IndexedBiConsumer<U, T, E extends Exception> {
        void accept(U u, int idx, T e) throws E;
    }

    public interface IndexedFunction<T, R, E extends Exception> {
        R apply(int idx, T e) throws E;
    }

    public interface IndexedBiFunction<U, T, R, E extends Exception> {
        R apply(U u, int idx, T e) throws E;
    }

    public interface IndexedPredicate<T, E extends Exception> {
        boolean test(int idx, T e) throws E;
    }

    public interface IndexedBiPredicate<U, T, E extends Exception> {
        boolean test(U u, int idx, T e) throws E;
    }

    public interface BooleanPredicate<E extends Exception> {
        boolean test(boolean value) throws E;
    }

    public interface BooleanFunction<R, E extends Exception> {
        R apply(boolean value) throws E;
    }

    public interface BooleanConsumer<E extends Exception> {
        void accept(boolean t) throws E;
    }

    public interface CharPredicate<E extends Exception> {
        boolean test(char value) throws E;
    }

    public interface CharFunction<R, E extends Exception> {
        R apply(char value) throws E;
    }

    public interface CharConsumer<E extends Exception> {
        void accept(char t) throws E;
    }

    public interface BytePredicate<E extends Exception> {
        boolean test(byte value) throws E;
    }

    public interface ByteFunction<R, E extends Exception> {
        R apply(byte value) throws E;
    }

    public interface ByteConsumer<E extends Exception> {
        void accept(byte t) throws E;
    }

    public interface ShortPredicate<E extends Exception> {
        boolean test(short value) throws E;
    }

    public interface ShortFunction<R, E extends Exception> {
        R apply(short value) throws E;
    }

    public interface ShortConsumer<E extends Exception> {
        void accept(short t) throws E;
    }

    public interface IntPredicate<E extends Exception> {
        boolean test(int value) throws E;
    }

    public interface IntFunction<R, E extends Exception> {
        R apply(int value) throws E;
    }

    public interface IntConsumer<E extends Exception> {
        void accept(int t) throws E;
    }

    public interface LongPredicate<E extends Exception> {
        boolean test(long value) throws E;
    }

    public interface LongFunction<R, E extends Exception> {
        R apply(long value) throws E;
    }

    public interface LongConsumer<E extends Exception> {
        void accept(long t) throws E;
    }

    public interface FloatPredicate<E extends Exception> {
        boolean test(float value) throws E;
    }

    public interface FloatFunction<R, E extends Exception> {
        R apply(float value) throws E;
    }

    public interface FloatConsumer<E extends Exception> {
        void accept(float t) throws E;
    }

    public interface DoublePredicate<E extends Exception> {
        boolean test(double value) throws E;
    }

    public interface DoubleFunction<R, E extends Exception> {
        R apply(double value) throws E;
    }

    public interface DoubleConsumer<E extends Exception> {
        void accept(double t) throws E;
    }

    public interface ToBooleanFunction<T, E extends Exception> {
        boolean applyAsBoolean(T t) throws E;
    }

    public interface ToCharFunction<T, E extends Exception> {
        char applyAsChar(T t) throws E;
    }

    public interface ToByteFunction<T, E extends Exception> {
        byte applyAsByte(T t) throws E;
    }

    public interface ToShortFunction<T, E extends Exception> {
        short applyAsShort(T t) throws E;
    }

    public interface ToIntFunction<T, E extends Exception> {
        int applyAsInt(T t) throws E;
    }

    public interface ToLongFunction<T, E extends Exception> {
        long applyAsLong(T t) throws E;
    }

    public interface ToFloatFunction<T, E extends Exception> {
        float applyAsFloat(T t) throws E;
    }

    public interface ToDoubleFunction<T, E extends Exception> {
        double applyAsDouble(T t) throws E;
    }

    public interface UnaryOperator<T, E extends Exception> extends Function<T, T, E> {
    }

    public interface BinaryOperator<T, E extends Exception> extends BiFunction<T, T, T, E> {
    }

    public interface BooleanBinaryOperator<E extends Exception> {
        boolean applyAsBoolean(boolean left, boolean right) throws E;
    }

    public interface CharBinaryOperator<E extends Exception> {
        char applyAsChar(char left, char right) throws E;
    }

    public interface ByteBinaryOperator<E extends Exception> {
        byte applyAsByte(byte left, byte right) throws E;
    }

    public interface ShortBinaryOperator<E extends Exception> {
        short applyAsShort(short left, short right) throws E;
    }

    public interface IntBinaryOperator<E extends Exception> {
        int applyAsInt(int left, int right) throws E;
    }

    public interface LongBinaryOperator<E extends Exception> {
        long applyAsLong(long left, long right) throws E;
    }

    public interface FloatBinaryOperator<E extends Exception> {
        float applyAsFloat(float left, float right) throws E;
    }

    public interface DoubleBinaryOperator<E extends Exception> {
        double applyAsDouble(double left, double right) throws E;
    }

    public interface BooleanUnaryOperator<E extends Exception> {
        boolean applyAsBoolean(boolean operand) throws E;
    }

    public interface CharUnaryOperator<E extends Exception> {
        char applyAsChar(char operand) throws E;
    }

    public interface ByteUnaryOperator<E extends Exception> {
        byte applyAsByte(byte operand) throws E;
    }

    public interface ShortUnaryOperator<E extends Exception> {
        short applyAsShort(short operand) throws E;
    }

    public interface IntUnaryOperator<E extends Exception> {
        int applyAsInt(int operand) throws E;
    }

    public interface LongUnaryOperator<E extends Exception> {
        long applyAsLong(long operand) throws E;
    }

    public interface FloatUnaryOperator<E extends Exception> {
        float applyAsFloat(float operand) throws E;
    }

    public interface DoubleUnaryOperator<E extends Exception> {
        double applyAsDouble(double operand) throws E;
    }

    public interface BooleanBiPredicate<E extends Exception> {
        boolean test(boolean t, boolean u) throws E;
    }

    public interface CharBiPredicate<E extends Exception> {
        boolean test(char t, char u) throws E;
    }

    public interface ByteBiPredicate<E extends Exception> {
        boolean test(byte t, byte u) throws E;
    }

    public interface ShortBiPredicate<E extends Exception> {
        boolean test(short t, short u) throws E;
    }

    public interface IntBiPredicate<E extends Exception> {
        boolean test(int t, int u) throws E;
    }

    public interface LongBiPredicate<E extends Exception> {
        boolean test(long t, long u) throws E;
    }

    public interface FloatBiPredicate<E extends Exception> {
        boolean test(float t, float u) throws E;
    }

    public interface DoubleBiPredicate<E extends Exception> {
        boolean test(double t, double u) throws E;
    }

    public interface BooleanBiFunction<R, E extends Exception> {
        R apply(boolean t, boolean u) throws E;
    }

    public interface CharBiFunction<R, E extends Exception> {
        R apply(char t, char u) throws E;
    }

    public interface ByteBiFunction<R, E extends Exception> {
        R apply(byte t, byte u) throws E;
    }

    public interface ShortBiFunction<R, E extends Exception> {
        R apply(short t, short u) throws E;
    }

    public interface IntBiFunction<R, E extends Exception> {
        R apply(int t, int u) throws E;
    }

    public interface LongBiFunction<R, E extends Exception> {
        R apply(long t, long u) throws E;
    }

    public interface FloatBiFunction<R, E extends Exception> {
        R apply(float t, float u) throws E;
    }

    public interface DoubleBiFunction<R, E extends Exception> {
        R apply(double t, double u) throws E;
    }

    public interface BooleanBiConsumer<E extends Exception> {
        void accept(boolean t, boolean u) throws E;
    }

    public interface CharBiConsumer<E extends Exception> {
        void accept(char t, char u) throws E;
    }

    public interface ByteBiConsumer<E extends Exception> {
        void accept(byte t, byte u) throws E;
    }

    public interface ShortBiConsumer<E extends Exception> {
        void accept(short t, short u) throws E;
    }

    public interface IntBiConsumer<E extends Exception> {
        void accept(int t, int u) throws E;
    }

    public interface LongBiConsumer<E extends Exception> {
        void accept(long t, long u) throws E;
    }

    public interface FloatBiConsumer<E extends Exception> {
        void accept(float t, float u) throws E;
    }

    public interface DoubleBiConsumer<E extends Exception> {
        void accept(double t, double u) throws E;
    }
}
