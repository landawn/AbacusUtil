/*
 * Copyright (c) 2012, 2013, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package com.landawn.abacus.util.stream;

import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Supplier;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 *
 */
public interface Collector<T, A, R> extends java.util.stream.Collector<T, A, R> {
    /**
     * A function that creates and returns a new mutable result container.
     *
     * @return a function which returns a new, mutable result container
     */
    @Override
    Supplier<A> supplier();

    /**
     * A function that folds a value into a mutable result container.
     *
     * @return a function which folds a value into a mutable result container
     */
    @Override
    BiConsumer<A, T> accumulator();

    /**
     * A function that accepts two partial results and merges them.  The
     * combiner function may fold state from one argument into the other and
     * return that, or may return a new result container.
     *
     * @return a function which combines two partial results into a combined
     * result
     */
    @Override
    BinaryOperator<A> combiner();

    /**
     * Perform the final transformation from the intermediate accumulation type
     * {@code A} to the final result type {@code R}.
     *
     * <p>If the characteristic {@code IDENTITY_TRANSFORM} is
     * set, this function may be presumed to be an identity transform with an
     * unchecked cast from {@code A} to {@code R}.
     *
     * @return a function which transforms the intermediate result to the final
     * result
     */
    @Override
    Function<A, R> finisher();

    /**
     * Returns a {@code Set} of {@code Collector.Characteristics} indicating
     * the characteristics of this Collector.  This set should be immutable.
     *
     * @return an immutable set of collector characteristics
     */
    @Override
    Set<Characteristics> characteristics();

    public static <T, A, R> Collector<T, A, R> of(java.util.stream.Collector<T, A, R> collector) {
        N.checkArgNotNull(collector);

        final Supplier<A> supplier = () -> collector.supplier().get();
        final BiConsumer<A, T> accumulator = (t, u) -> collector.accumulator().accept(t, u);
        final BinaryOperator<A> combiner = (t, u) -> collector.combiner().apply(t, u);
        final Function<A, R> finisher = t -> collector.finisher().apply(t);

        return new Collectors.CollectorImpl<>(supplier, accumulator, combiner, finisher, collector.characteristics());
    }

    @SafeVarargs
    public static <T, R> Collector<T, R, R> of(Supplier<R> supplier, BiConsumer<R, T> accumulator, BinaryOperator<R> combiner,
            Characteristics... characteristics) {
        N.checkArgNotNull(supplier);
        N.checkArgNotNull(accumulator);
        N.checkArgNotNull(combiner);
        N.checkArgNotNull(characteristics);

        final Set<Characteristics> cs = (characteristics.length == 0) ? Collectors.CH_ID
                : Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.IDENTITY_FINISH, characteristics));

        return new Collectors.CollectorImpl<>(supplier, accumulator, combiner, cs);
    }

    @SafeVarargs
    public static <T, A, R> Collector<T, A, R> of(Supplier<A> supplier, BiConsumer<A, T> accumulator, BinaryOperator<A> combiner, Function<A, R> finisher,
            Characteristics... characteristics) {
        N.checkArgNotNull(supplier);
        N.checkArgNotNull(accumulator);
        N.checkArgNotNull(combiner);
        N.checkArgNotNull(characteristics);

        Set<Characteristics> cs = Collectors.CH_NOID;

        if (characteristics.length > 0) {
            cs = EnumSet.noneOf(Characteristics.class);
            Collections.addAll(cs, characteristics);
            cs = Collections.unmodifiableSet(cs);
        }

        return new Collectors.CollectorImpl<>(supplier, accumulator, combiner, finisher, cs);
    }
}
