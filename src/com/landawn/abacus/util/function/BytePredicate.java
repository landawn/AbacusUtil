/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */
package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface BytePredicate {

    boolean test(byte value);

    default BytePredicate and(BytePredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default BytePredicate negate() {
        return (t) -> !test(t);
    }

    default BytePredicate or(BytePredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
