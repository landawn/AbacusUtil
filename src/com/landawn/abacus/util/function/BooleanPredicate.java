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
public interface BooleanPredicate {
    /**
     * Returns {@code true} if the specified {@code value} matches the expectation, otherwise, {@code false} is returned.
     *
     * @param value
     * @return
     */
    boolean test(boolean value);

    default BooleanPredicate and(BooleanPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default BooleanPredicate negate() {
        return (t) -> !test(t);
    }

    default BooleanPredicate or(BooleanPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
