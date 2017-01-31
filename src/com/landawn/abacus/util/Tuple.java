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

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 *
 */
public abstract class Tuple {

    Tuple() {
    }

    public abstract int arity();

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

    public static final class Tuple1<T1> extends Tuple {
        public final T1 _1;

        public Tuple1(T1 _1) {
            this._1 = _1;
        }

        public T1 _1() {
            return _1;
        }

        @Override
        public int arity() {
            return 1;
        }
    }

    public static final class Tuple2<T1, T2> extends Tuple {
        public final T1 _1;
        public final T2 _2;

        public Tuple2(T1 _1, T2 _2) {
            this._1 = _1;
            this._2 = _2;
        }

        public T1 _1() {
            return _1;
        }

        public T2 _2() {
            return _2;
        }

        @Override
        public int arity() {
            return 2;
        }
    }

    public static final class Tuple3<T1, T2, T3> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;

        public Tuple3(T1 _1, T2 _2, T3 _3) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
        }

        public T1 _1() {
            return _1;
        }

        public T2 _2() {
            return _2;
        }

        public T3 _3() {
            return _3;
        }

        @Override
        public int arity() {
            return 3;
        }
    }

    public static final class Tuple4<T1, T2, T3, T4> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;

        public Tuple4(T1 _1, T2 _2, T3 _3, T4 _4) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
        }

        public T1 _1() {
            return _1;
        }

        public T2 _2() {
            return _2;
        }

        public T3 _3() {
            return _3;
        }

        public T4 _4() {
            return _4;
        }

        @Override
        public int arity() {
            return 4;
        }
    }

    public static final class Tuple5<T1, T2, T3, T4, T5> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;

        public Tuple5(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
        }

        public T1 _1() {
            return _1;
        }

        public T2 _2() {
            return _2;
        }

        public T3 _3() {
            return _3;
        }

        public T4 _4() {
            return _4;
        }

        public T5 _5() {
            return _5;
        }

        @Override
        public int arity() {
            return 5;
        }
    }

    public static final class Tuple6<T1, T2, T3, T4, T5, T6> extends Tuple {
        public final T1 _1;
        public final T2 _2;
        public final T3 _3;
        public final T4 _4;
        public final T5 _5;
        public final T6 _6;

        public Tuple6(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
            this._6 = _6;
        }

        public T1 _1() {
            return _1;
        }

        public T2 _2() {
            return _2;
        }

        public T3 _3() {
            return _3;
        }

        public T4 _4() {
            return _4;
        }

        public T5 _5() {
            return _5;
        }

        public T6 _6() {
            return _6;
        }

        @Override
        public int arity() {
            return 6;
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

        public Tuple7(T1 _1, T2 _2, T3 _3, T4 _4, T5 _5, T6 _6, T7 _7) {
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
            this._4 = _4;
            this._5 = _5;
            this._6 = _6;
            this._7 = _7;
        }

        public T1 _1() {
            return _1;
        }

        public T2 _2() {
            return _2;
        }

        public T3 _3() {
            return _3;
        }

        public T4 _4() {
            return _4;
        }

        public T5 _5() {
            return _5;
        }

        public T6 _6() {
            return _6;
        }

        public T7 _7() {
            return _7;
        }

        @Override
        public int arity() {
            return 7;
        }
    }
}
