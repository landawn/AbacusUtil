package com.landawn.abacus.util;

public final class If {
    private static final If TRUE = new If(true);
    private static final If FALSE = new If(false);

    private final boolean b;

    If(boolean b) {
        this.b = b;
    }

    public static If of(boolean b) {
        return b ? TRUE : FALSE;
    }

    public static If not(boolean b) {
        return b ? FALSE : TRUE;
    }

    //    public <E extends Exception> void thenRun(final Try.Runnable<E> cmd) throws E {
    //        if (b) {
    //            cmd.run();
    //        }
    //    }
    //
    //    public <T, E extends Exception> void thenRun(final T seed, final Try.Consumer<? super T, E> action) throws E {
    //        if (b) {
    //            action.accept(seed);
    //        }
    //    }
    //
    //    public <T, E extends Exception> Nullable<T> thenCall(final Try.Callable<? extends T, E> callable) throws E {
    //        return b ? Nullable.of(callable.call()) : Nullable.<T> empty();
    //    }
    //
    //    public <T, R, E extends Exception> Nullable<R> thenCall(final T seed, final Try.Function<? super T, R, E> func) throws E {
    //        return b ? Nullable.of(func.apply(seed)) : Nullable.<R> empty();
    //    }

    public Or then() {
        return Or.of(b);
    }

    public <E extends Exception> Or then(final Try.Runnable<E> cmd) throws E {
        N.requireNonNull(cmd);

        if (b) {
            cmd.run();
        }

        return Or.of(b);
    }

    public <U, E extends Exception> Or then(final U seed, final Try.Consumer<? super U, E> action) throws E {
        N.requireNonNull(action);

        if (b) {
            action.accept(seed);
        }

        return Or.of(b);
    }

    //    public <T, E extends Exception> Nullable<T> then(final Try.Callable<T, E> callable) throws E {
    //        return b ? Nullable.of(callable.call()) : Nullable.<T> empty();
    //    }
    //
    //    public <T, R, E extends Exception> Nullable<R> then(final T seed, final Try.Function<? super T, R, E> func) throws E {
    //        return b ? Nullable.of(func.apply(seed)) : Nullable.<R> empty();
    //    }

    public static final class Or {
        private static final Or TRUE = new Or(true);
        private static final Or FALSE = new Or(false);

        private final boolean b;

        Or(final boolean b) {
            this.b = b;
        }

        static Or of(boolean b) {
            return b ? TRUE : FALSE;
        }

        public <E extends Exception> void orElse(final Try.Runnable<E> cmd) throws E {
            N.requireNonNull(cmd);

            if (!b) {
                cmd.run();
            }
        }

        public <U, E extends Exception> void orElse(final U seed, final Try.Consumer<? super U, E> action) throws E {
            N.requireNonNull(action);

            if (!b) {
                action.accept(seed);
            }
        }
    }

    public static final class iif {
        private static final iif TRUE = new iif(true);
        private static final iif FALSE = new iif(false);

        @SuppressWarnings("rawtypes")
        private static final Or EMPTY_TRUE_OR = new TrueOr(Nullable.empty());
        @SuppressWarnings("rawtypes")
        private static final Or FALSE_OR = new FalseOr();

        private final boolean b;

        iif(boolean b) {
            this.b = b;
        }

        public static iif of(boolean b) {
            return b ? TRUE : FALSE;
        }

        public static iif not(boolean b) {
            return b ? FALSE : TRUE;
        }

        /**
         * 
         * @return
         */
        public <T> Or<T> then() {
            return b ? EMPTY_TRUE_OR : FALSE_OR;
        }

        public <T, E extends Exception> Or<T> then(final Try.Callable<T, E> callable) throws E {
            N.requireNonNull(callable);

            return b ? new TrueOr<T>(Nullable.of(callable.call())) : FALSE_OR;
        }

        public <T, U, E extends Exception> Or<T> then(final U seed, final Try.Function<? super U, T, E> func) throws E {
            N.requireNonNull(func);

            return b ? new TrueOr<T>(Nullable.of(func.apply(seed))) : FALSE_OR;
        }

        public static abstract class Or<T> {
            Or() {
            }

            public abstract <E extends Exception> Nullable<T> orElse(final Try.Callable<T, E> callable) throws E;

            public abstract <U, E extends Exception> Nullable<T> orElse(final U seed, final Try.Function<? super U, T, E> func) throws E;
        }

        static final class TrueOr<T> extends Or<T> {
            private final Nullable<T> result;

            TrueOr(final Nullable<T> result) {
                this.result = result;
            }

            @Override
            public <E extends Exception> Nullable<T> orElse(final Try.Callable<T, E> callable) throws E {
                N.requireNonNull(callable);

                return result;
            }

            @Override
            public <U, E extends Exception> Nullable<T> orElse(final U seed, final Try.Function<? super U, T, E> func) throws E {
                N.requireNonNull(func);

                return result;
            }
        }

        static final class FalseOr<T> extends Or<T> {
            FalseOr() {
            }

            @Override
            public <E extends Exception> Nullable<T> orElse(final Try.Callable<T, E> callable) throws E {
                return Nullable.of(callable.call());
            }

            @Override
            public <U, E extends Exception> Nullable<T> orElse(final U seed, final Try.Function<? super U, T, E> func) throws E {
                return Nullable.of(func.apply(seed));
            }
        }
    }
}
