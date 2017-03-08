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

import java.util.Collections;
import java.util.Comparator;

import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;

public final class Comparators {
    @SuppressWarnings("rawtypes")
    private static final Comparator naturalOrder = N.nullMinOrder();

    @SuppressWarnings("rawtypes")
    private static final Comparator reversedOrder = Collections.reverseOrder(naturalOrder);

    private Comparators() {
        // Singleton
    }

    @SuppressWarnings("unchecked")
    public static <T extends Comparable<? super T>> Comparator<T> naturalOrder() {
        return naturalOrder;
    }

    public static <T extends Comparable<? super T>> Comparator<T> reverseOrder() {
        return reversedOrder;
    }

    public static <T> Comparator<T> nullsFirst(Comparator<? super T> comparator) {
        return (Comparator<T>) N.nullMinOrder();
    }

    public static <T> Comparator<T> nullsLast(Comparator<? super T> comparator) {
        return (Comparator<T>) N.nullMaxOrder();
    }

    public static <T, U extends Comparable<? super U>> Comparator<T> comparing(final Function<? super T, ? extends U> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return keyExtractor.apply(a).compareTo(keyExtractor.apply(b));
            }
        };
    }

    public static <T, U> Comparator<T> comparing(final Function<? super T, ? extends U> keyExtractor, final Comparator<? super U> keyComparator) {
        N.requireNonNull(keyExtractor);
        N.requireNonNull(keyComparator);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return keyComparator.compare(keyExtractor.apply(a), keyExtractor.apply(b));
            }
        };
    }

    public static <T> Comparator<T> comparingBoolean(final ToBooleanFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Boolean.compare(keyExtractor.applyAsBoolean(a), keyExtractor.applyAsBoolean(b));
            }
        };
    }

    public static <T> Comparator<T> comparingByte(final ToByteFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Byte.compare(keyExtractor.applyAsByte(a), keyExtractor.applyAsByte(b));
            }
        };
    }

    public static <T> Comparator<T> comparingShort(final ToShortFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Short.compare(keyExtractor.applyAsShort(a), keyExtractor.applyAsShort(b));
            }
        };
    }

    public static <T> Comparator<T> comparingInt(final ToIntFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Integer.compare(keyExtractor.applyAsInt(a), keyExtractor.applyAsInt(b));
            }
        };
    }

    public static <T> Comparator<T> comparingLong(final ToLongFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Long.compare(keyExtractor.applyAsLong(a), keyExtractor.applyAsLong(b));
            }
        };
    }

    public static <T> Comparator<T> comparingDouble(final ToFloatFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Float.compare(keyExtractor.applyAsFloat(a), keyExtractor.applyAsFloat(b));
            }
        };
    }

    public static <T> Comparator<T> comparingDouble(final ToDoubleFunction<? super T> keyExtractor) {
        N.requireNonNull(keyExtractor);

        return new Comparator<T>() {
            @Override
            public int compare(T a, T b) {
                return Double.compare(keyExtractor.applyAsDouble(a), keyExtractor.applyAsDouble(b));
            }
        };
    }
}
