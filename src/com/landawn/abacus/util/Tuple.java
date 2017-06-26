/*
 * Copyright (c) 2017, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 *
 */
public abstract class Tuple {
    public static final Tuple EMPTY = new Tuple() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object[] toArray() {
            return N.EMPTY_OBJECT_ARRAY;
        }

        @Override
        public <A> A[] toArray(A[] a) {
            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            // do nothing.
        }

        @Override
        public Stream<? extends Tuple> stream() {
            return Stream.empty();
        }

        @Override
        public String toString() {
            return "[]";
        }
    };

    Tuple() {
    }

    public abstract int arity();

    public abstract Object[] toArray();

    public abstract <A> A[] toArray(A[] a);

    public abstract void forEach(Consumer<?> comsumer);

    protected abstract Stream<? extends Tuple> stream();

    public static <T1> Tuple1<T1> of(T1 _1) {
        return new Tuple1<>(_1);
    }

    public static <T1, T2> Tuple2<T1, T2> of(T1 _1, T2 _2) {
        return new Tuple2<>(_1, _2);
    }

    public static <T1, T2, T3> Tuple3<T1, T2, T3> of(T1 _1, T2 _2, T3 _3) {
        return new Tuple3<>(_1, _2, _3);
    }

    public static <T1, T2, T3, T4> Tuple4<T1, T2, T3, T4> of(T1 _1, T2 _2, T3 _3, T4 _4) {
        return new Tuple4<>(_1, _2, _3, _4);
    }

    public static <T1, T2, T3, T4, T5> Tuple5<T1, T2, T3, T4, T5> of(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5) {
        return new Tuple5<>(_1, _2, _3, _4, _5);
    }

    public static <T1, T2, T3, T4, T5, T6> Tuple6<T1, T2, T3, T4, T5, T6> of(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6) {
        return new Tuple6<>(_1, _2, _3, _4, _5, _6);
    }

    public static <T1, T2, T3, T4, T5, T6, T7> Tuple7<T1, T2, T3, T4, T5, T6, T7> of(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7) {
        return new Tuple7<>(_1, _2, _3, _4, _5, _6, _7);
    }

    public static <T1, T2, T3, T4, T5, T6, T7, T8> Tuple8<T1, T2, T3, T4, T5, T6, T7, T8> of(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7, T8 _8) {
        return new Tuple8<>(_1, _2, _3, _4, _5, _6, _7, _8);
    }

    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9> Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9> of(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7, T8 _8,
            T9 _9) {
        return new Tuple9<>(_1, _2, _3, _4, _5, _6, _7, _8, _9);
    }

    public static <T extends Tuple> T from(final Object[] a) {
        final int len = a == null ? 0 : a.length;

        Tuple result = null;

        switch (len) {
            case 0:
                result = EMPTY;
                break;

            case 1:
                result = new Tuple1<>(a[0]);
                break;

            case 2:
                result = new Tuple2<>(a[0], a[1]);
                break;

            case 3:
                result = new Tuple3<>(a[0], a[1], a[2]);
                break;

            case 4:
                result = new Tuple4<>(a[0], a[1], a[2], a[3]);
                break;

            case 5:
                result = new Tuple5<>(a[0], a[1], a[2], a[3], a[4]);
                break;

            case 6:
                result = new Tuple6<>(a[0], a[1], a[2], a[3], a[4], a[5]);
                break;

            case 7:
                result = new Tuple7<>(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
                break;

            case 8:
                result = new Tuple8<>(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
                break;

            default:
                result = new Tuple9<>(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
                break;
        }

        return (T) result;
    }

    public static <T extends Tuple> T from(final Collection<?> c) {
        final int len = c == null ? 0 : c.size();
        final Iterator<?> iter = c == null ? null : c.iterator();

        Tuple result = null;

        switch (len) {
            case 0:
                result = EMPTY;
                break;

            case 1:
                result = new Tuple1<>(iter.next());
                break;

            case 2:
                result = new Tuple2<>(iter.next(), iter.next());
                break;

            case 3:
                result = new Tuple3<>(iter.next(), iter.next(), iter.next());
                break;

            case 4:
                result = new Tuple4<>(iter.next(), iter.next(), iter.next(), iter.next());
                break;

            case 5:
                result = new Tuple5<>(iter.next(), iter.next(), iter.next(), iter.next(), iter.next());
                break;

            case 6:
                result = new Tuple6<>(iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next());
                break;

            case 7:
                result = new Tuple7<>(iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next());
                break;

            case 8:
                result = new Tuple8<>(iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next());
                break;

            default:
                result = new Tuple9<>(iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next(), iter.next());
                break;
        }

        return (T) result;
    }

    public final static class Tuple1<T1> extends Tuple {
        public final T1 _1;

        // For Kryo
        Tuple1() {
            this(null);
        }

        Tuple1(T1 _1) {
            this._1 = _1;
        }

        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 1) {
                a = N.copyOf(a, 1);
            }

            a[0] = (A) _1;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
        }

        public void accept(final Consumer<? super Tuple1<T1>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple1<T1>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple1<T1>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple1.class)) {
                final Tuple1<?> other = (Tuple1<?>) obj;

                return N.equals(this._1, other._1);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + "]";
        }
    }

    public final static class Tuple2<T1, T2> extends Tuple implements Map.Entry<T1, T2> {
        public final T1 _1;
        public final T2 _2;

        // For Kryo
        Tuple2() {
            this(null, null);
        }

        Tuple2(T1 _1, T2 _2) {
            this._1 = _1;
            this._2 = _2;
        }

        @Override
        public int arity() {
            return 2;
        }

        public Tuple2<T2, T1> reversed() {
            return of(_2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 2) {
                a = N.copyOf(a, 2);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
        }

        public void accept(final Consumer<? super Tuple2<T1, T2>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple2<T1, T2>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple2<T1, T2>> stream() {
            return Stream.of(this);
        }

        @Override
        public T1 getKey() {
            return _1;
        }

        @Override
        public T2 getValue() {
            return _2;
        }

        /**
         * @param R
         * @deprecated UnsupportedOperationException
         */
        @Override
        @Deprecated
        public T2 setValue(T2 value) {
            throw new UnsupportedOperationException();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple2.class)) {
                final Tuple2<?, ?> other = (Tuple2<?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + "]";
        }
    }

    public final static class Tuple3<T1, T2, T3> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;

        // For Kryo
        Tuple3() {
            this(null, null, null);
        }

        Tuple3(T1 _1, T2 _2, T3 _3) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
        }

        @Override
        public int arity() {
            return 3;
        }

        public Tuple3<T3, T2, T1> reversed() {
            return new Tuple3<>(_3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 3) {
                a = N.copyOf(a, 3);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
        }

        public void accept(final Consumer<? super Tuple3<T1, T2, T3>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple3<T1, T2, T3>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple3<T1, T2, T3>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple3.class)) {
                final Tuple3<?, ?, ?> other = (Tuple3<?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + "]";
        }
    }

    public final static class Tuple4<T1, T2, T3, T4> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;

        // For Kryo
        Tuple4() {
            this(null, null, null, null);
        }

        Tuple4(T1 _1, T2 _2, T3 _3, T4 _4) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
        }

        @Override
        public int arity() {
            return 4;
        }

        public Tuple4<T4, T3, T2, T1> reversed() {
            return new Tuple4<>(_4, _3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3, _4 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 4) {
                a = N.copyOf(a, 4);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;
            a[3] = (A) _4;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
            objComsumer.accept(_4);
        }

        public void accept(final Consumer<? super Tuple4<T1, T2, T3, T4>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple4<T1, T2, T3, T4>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple4<T1, T2, T3, T4>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            result = prime * result + N.hashCode(_4);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple4.class)) {
                final Tuple4<?, ?, ?, ?> other = (Tuple4<?, ?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3) && N.equals(this._4, other._4);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + ", " + N.toString(_4) + "]";
        }
    }

    public final static class Tuple5<T1, T2, T3, T4, T5> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;

        // For Kryo
        Tuple5() {
            this(null, null, null, null, null);
        }

        Tuple5(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
        }

        @Override
        public int arity() {
            return 5;
        }

        public Tuple5<T5, T4, T3, T2, T1> reversed() {
            return new Tuple5<>(_5, _4, _3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3, _4, _5 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 5) {
                a = N.copyOf(a, 5);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;
            a[3] = (A) _4;
            a[4] = (A) _5;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
            objComsumer.accept(_4);
            objComsumer.accept(_5);
        }

        public void accept(final Consumer<? super Tuple5<T1, T2, T3, T4, T5>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple5<T1, T2, T3, T4, T5>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple5<T1, T2, T3, T4, T5>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            result = prime * result + N.hashCode(_4);
            result = prime * result + N.hashCode(_5);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple5.class)) {
                final Tuple5<?, ?, ?, ?, ?> other = (Tuple5<?, ?, ?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3) && N.equals(this._4, other._4)
                        && N.equals(this._5, other._5);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + ", " + N.toString(_4) + ", " + N.toString(_5) + "]";
        }
    }

    public static final class Tuple6<T1, T2, T3, T4, T5, T6> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;
        public final T6 _6;

        // For Kryo
        Tuple6() {
            this(null, null, null, null, null, null);
        }

        Tuple6(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
            this._6 = _6;
        }

        @Override
        public int arity() {
            return 6;
        }

        public Tuple6<T6, T5, T4, T3, T2, T1> reversed() {
            return new Tuple6<>(_6, _5, _4, _3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3, _4, _5, _6 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 6) {
                a = N.copyOf(a, 6);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;
            a[3] = (A) _4;
            a[4] = (A) _5;
            a[5] = (A) _6;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
            objComsumer.accept(_4);
            objComsumer.accept(_5);
            objComsumer.accept(_6);
        }

        public void accept(final Consumer<? super Tuple6<T1, T2, T3, T4, T5, T6>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple6<T1, T2, T3, T4, T5, T6>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple6<T1, T2, T3, T4, T5, T6>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            result = prime * result + N.hashCode(_4);
            result = prime * result + N.hashCode(_5);
            result = prime * result + N.hashCode(_6);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple6.class)) {
                final Tuple6<?, ?, ?, ?, ?, ?> other = (Tuple6<?, ?, ?, ?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3) && N.equals(this._4, other._4)
                        && N.equals(this._5, other._5) && N.equals(this._6, other._6);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + ", " + N.toString(_4) + ", " + N.toString(_5) + ", " + N.toString(_6)
                    + "]";
        }
    }

    public static final class Tuple7<T1, T2, T3, T4, T5, T6, T7> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;
        public final T6 _6;
        public final T7 _7;

        // For Kryo
        Tuple7() {
            this(null, null, null, null, null, null, null);
        }

        Tuple7(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
            this._6 = _6;
            this._7 = _7;
        }

        @Override
        public int arity() {
            return 7;
        }

        public Tuple7<T7, T6, T5, T4, T3, T2, T1> reversed() {
            return new Tuple7<>(_7, _6, _5, _4, _3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3, _4, _5, _6, _7 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 7) {
                a = N.copyOf(a, 7);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;
            a[3] = (A) _4;
            a[4] = (A) _5;
            a[5] = (A) _6;
            a[6] = (A) _7;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
            objComsumer.accept(_4);
            objComsumer.accept(_5);
            objComsumer.accept(_6);
            objComsumer.accept(_7);
        }

        public void accept(final Consumer<? super Tuple7<T1, T2, T3, T4, T5, T6, T7>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple7<T1, T2, T3, T4, T5, T6, T7>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple7<T1, T2, T3, T4, T5, T6, T7>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            result = prime * result + N.hashCode(_4);
            result = prime * result + N.hashCode(_5);
            result = prime * result + N.hashCode(_6);
            result = prime * result + N.hashCode(_7);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple7.class)) {
                final Tuple7<?, ?, ?, ?, ?, ?, ?> other = (Tuple7<?, ?, ?, ?, ?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3) && N.equals(this._4, other._4)
                        && N.equals(this._5, other._5) && N.equals(this._6, other._6) && N.equals(this._7, other._7);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + ", " + N.toString(_4) + ", " + N.toString(_5) + ", " + N.toString(_6)
                    + ", " + N.toString(_7) + "]";
        }
    }

    public static final class Tuple8<T1, T2, T3, T4, T5, T6, T7, T8> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;
        public final T6 _6;
        public final T7 _7;
        public final T8 _8;

        // For Kryo
        Tuple8() {
            this(null, null, null, null, null, null, null, null);
        }

        Tuple8(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7, T8 _8) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
            this._6 = _6;
            this._7 = _7;
            this._8 = _8;
        }

        @Override
        public int arity() {
            return 8;
        }

        public Tuple8<T8, T7, T6, T5, T4, T3, T2, T1> reversed() {
            return new Tuple8<>(_8, _7, _6, _5, _4, _3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3, _4, _5, _6, _7, _8 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 8) {
                a = N.copyOf(a, 8);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;
            a[3] = (A) _4;
            a[4] = (A) _5;
            a[5] = (A) _6;
            a[6] = (A) _7;
            a[7] = (A) _8;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
            objComsumer.accept(_4);
            objComsumer.accept(_5);
            objComsumer.accept(_6);
            objComsumer.accept(_7);
            objComsumer.accept(_8);
        }

        public void accept(final Consumer<? super Tuple8<T1, T2, T3, T4, T5, T6, T7, T8>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple8<T1, T2, T3, T4, T5, T6, T7, T8>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple8<T1, T2, T3, T4, T5, T6, T7, T8>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            result = prime * result + N.hashCode(_4);
            result = prime * result + N.hashCode(_5);
            result = prime * result + N.hashCode(_6);
            result = prime * result + N.hashCode(_7);
            result = prime * result + N.hashCode(_8);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple7.class)) {
                final Tuple8<?, ?, ?, ?, ?, ?, ?, ?> other = (Tuple8<?, ?, ?, ?, ?, ?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3) && N.equals(this._4, other._4)
                        && N.equals(this._5, other._5) && N.equals(this._6, other._6) && N.equals(this._7, other._7) && N.equals(this._8, other._8);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + ", " + N.toString(_4) + ", " + N.toString(_5) + ", " + N.toString(_6)
                    + ", " + N.toString(_7) + ", " + N.toString(_8) + "]";
        }
    }

    public static final class Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;
        public final T6 _6;
        public final T7 _7;
        public final T8 _8;
        public final T9 _9;

        // For Kryo
        Tuple9() {
            this(null, null, null, null, null, null, null, null, null);
        }

        Tuple9(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7, T8 _8, T9 _9) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
            this._6 = _6;
            this._7 = _7;
            this._8 = _8;
            this._9 = _9;
        }

        @Override
        public int arity() {
            return 9;
        }

        public Tuple9<T9, T8, T7, T6, T5, T4, T3, T2, T1> reversed() {
            return new Tuple9<>(_9, _8, _7, _6, _5, _4, _3, _2, _1);
        }

        @Override
        public Object[] toArray() {
            return new Object[] { _1, _2, _3, _4, _5, _6, _7, _8, _9 };
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < 9) {
                a = N.copyOf(a, 9);
            }

            a[0] = (A) _1;
            a[1] = (A) _2;
            a[2] = (A) _3;
            a[3] = (A) _4;
            a[4] = (A) _5;
            a[5] = (A) _6;
            a[6] = (A) _7;
            a[7] = (A) _8;
            a[8] = (A) _9;

            return a;
        }

        @Override
        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(_1);
            objComsumer.accept(_2);
            objComsumer.accept(_3);
            objComsumer.accept(_4);
            objComsumer.accept(_5);
            objComsumer.accept(_6);
            objComsumer.accept(_7);
            objComsumer.accept(_8);
            objComsumer.accept(_9);
        }

        public void accept(final Consumer<? super Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9>> action) {
            action.accept(this);
        }

        public <U> U apply(final Function<? super Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9>, U> action) {
            return action.apply(this);
        }

        @Override
        public Stream<Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9>> stream() {
            return Stream.of(this);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(_1);
            result = prime * result + N.hashCode(_2);
            result = prime * result + N.hashCode(_3);
            result = prime * result + N.hashCode(_4);
            result = prime * result + N.hashCode(_5);
            result = prime * result + N.hashCode(_6);
            result = prime * result + N.hashCode(_7);
            result = prime * result + N.hashCode(_8);
            result = prime * result + N.hashCode(_9);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj != null && obj.getClass().equals(Tuple7.class)) {
                final Tuple9<?, ?, ?, ?, ?, ?, ?, ?, ?> other = (Tuple9<?, ?, ?, ?, ?, ?, ?, ?, ?>) obj;

                return N.equals(this._1, other._1) && N.equals(this._2, other._2) && N.equals(this._3, other._3) && N.equals(this._4, other._4)
                        && N.equals(this._5, other._5) && N.equals(this._6, other._6) && N.equals(this._7, other._7) && N.equals(this._8, other._8)
                        && N.equals(this._9, other._9);
            }

            return false;
        }

        @Override
        public String toString() {
            return "[" + N.toString(_1) + ", " + N.toString(_2) + ", " + N.toString(_3) + ", " + N.toString(_4) + ", " + N.toString(_5) + ", " + N.toString(_6)
                    + ", " + N.toString(_7) + ", " + N.toString(_8) + ", " + N.toString(_9) + "]";
        }
    }
}
