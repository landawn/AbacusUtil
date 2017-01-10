package com.landawn.abacus.test;

import org.junit.Test;

import com.landawn.abacus.util.Profiler;
import com.landawn.abacus.util.Reflection;
import com.landawn.abacus.util.stream.IntStream;

import junit.framework.TestCase;

public class ReflectionTest extends TestCase {

    @Test
    public void test_perf() {
        Profiler.run(3, 100000, 3, "m_01 by refelct", () -> Reflection.on(X.class)._new().invoke("m_01")).printResult();

        Profiler.run(3, 100000, 3, "m_01 direct call", () -> new X().m_01()).printResult();

        Profiler.run(3, 100000, 3, "m_02 by refelct", () -> Reflection.on(X.class)._new().invoke("m_02")).printResult();

        Profiler.run(3, 100000, 3, "m_02 direct call", () -> new X().m_02()).printResult();

        Profiler.run(3, 100000, 3, "m_11 by refelct", () -> Reflection.on(X.class)._new().invoke("m_11")).printResult();

        Profiler.run(3, 100000, 3, "m_11 direct call", () -> X.m_11()).printResult();

        Profiler.run(3, 100000, 3, "m_12 by refelct", () -> Reflection.on(X.class)._new().invoke("m_12")).printResult();

        Profiler.run(3, 100000, 3, "m_12 direct call", () -> X.m_12()).printResult();
    }

    public static class X {
        public long m_01() {
            return m_11();
        }

        public long m_02() {
            return m_12();
        }

        public static long m_11() {
            long sum = IntStream.range(0, 10).sum();
            assertEquals(45, sum);
            return sum;
        }

        public static long m_12() {
            long sum = IntStream.range(0, 10000).sum();
            assertEquals(49995000, sum);
            return sum;
        }
    }
}
