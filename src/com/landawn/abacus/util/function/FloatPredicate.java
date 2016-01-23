/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.Predicate
 */
public interface FloatPredicate {
    /**
     * Returns {@code true} if the specified {@code value} matches the expectation, otherwise, {@code false} is returned.
     *
     * @param value
     * @return
     */
    boolean test(float value);

    default FloatPredicate and(FloatPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default FloatPredicate negate() {
        return (t) -> !test(t);
    }

    default FloatPredicate or(FloatPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
