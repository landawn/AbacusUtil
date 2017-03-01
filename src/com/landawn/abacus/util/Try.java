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

    public static <T extends AutoCloseable> Try<T> of(final Supplier<T> supplier) {
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

    public static void run(final Try.Runnable cmd) {
        try {
            cmd.run();
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public static <R> R call(final java.util.concurrent.Callable<R> cmd) {
        try {
            return cmd.call();
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public void run(final Try.Consumer<? super T> cmd) {
        try {
            cmd.accept(t);
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        } finally {
            IOUtil.close(t);
        }
    }

    public <R> R call(final Try.Function<? super T, R> cmd) {
        try {
            return cmd.apply(t);
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        } finally {
            IOUtil.close(t);
        }
    }

    public static final class Try0<T extends AutoCloseable> {
        private final T t;

        Try0(final T t) {
            this.t = t;
        }

        public static <T extends AutoCloseable> Try0<T> of(final T t) {
            return new Try0<>(t);
        }

        public static <T extends AutoCloseable> Try0<T> of(final Supplier<T> supplier) {
            try {
                return new Try0<>(supplier.get());
            } catch (Exception e) {
                throw N.toRuntimeException(e);
            }
        }

        //    public static Try<Reader> reader(final File file) {
        //        return of((Reader) IOUtil.createBufferedReader(file));
        //    }
        //
        //    public static Try<Writer> writer(final File file) {
        //        return of((Writer) IOUtil.createBufferedWriter(file));
        //    }
        //
        //    public static Try<Stream<String>> stream(final File file) {
        //        final Reader reader = IOUtil.createBufferedReader(file);
        //
        //        return new Try<Stream<String>>(Stream.of(reader).onClose(new java.lang.Runnable() {
        //            @Override
        //            public void run() {
        //                IOUtil.close(reader);
        //            }
        //        }));
        //    }

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

        public void run(final Try.Runnable cmd) {
            try {
                cmd.run();
            } catch (Exception e) {
                throw N.toRuntimeException(e);
            } finally {
                IOUtil.close(t);
            }
        }

        public <R> R call(final java.util.concurrent.Callable<R> cmd) {
            try {
                return cmd.call();
            } catch (Exception e) {
                throw N.toRuntimeException(e);
            } finally {
                IOUtil.close(t);
            }
        }
    }

    public static interface Runnable {
        void run() throws Exception;
    }

    public static interface Callable<R> extends java.util.concurrent.Callable<R> {
        @Override
        R call();
    }

    public static interface Supplier<T> {
        T get() throws Exception;
    }

    public static interface Function<T, R> {
        R apply(T t) throws Exception;
    }

    public static interface Consumer<T> {
        void accept(T t) throws Exception;
    }
}
