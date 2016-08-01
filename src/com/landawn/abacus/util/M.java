/*
 * Copyright (c) 2015, Haiyang Li.
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.RandomAccess;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.math3.exception.MathIllegalArgumentException;

import com.landawn.abacus.exception.AbacusException;

/**
 * (Note: This class includes a lot of methods copied from Apache Commons Math)
 * 
 * "M" = Math or More...To be continued...
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 *
 */
final class M {
    static final AsyncExecutor parallelSortExecutor = new AsyncExecutor(64, 300L, TimeUnit.SECONDS);

    static volatile int CPU_CORES = N.CPU_CORES;

    static final int MIN_ARRAY_SORT_GRAN = 8192;
    static final int BINARYSEARCH_THRESHOLD = 64;

    /**
     * Primality test: tells if the argument is a (provable) prime or not.
     * <p>
     * It uses the Miller-Rabin probabilistic test in such a way that a result is guaranteed:
     * it uses the firsts prime numbers as successive base (see Handbook of applied cryptography
     * by Menezes, table 4.1).
     *
     * @param n number to test.
     * @return true if n is prime. (All numbers &lt; 2 return false).
     */
    public static boolean isPrime(final int n) {
        if (n < 2) {
            return false;
        }

        for (int p : SmallPrimes.PRIMES) {
            if (0 == (n % p)) {
                return n == p;
            }
        }
        return SmallPrimes.millerRabinPrimeTest(n);
    }

    /**
     * Return the smallest prime greater than or equal to n.
     *
     * @param n a positive number.
     * @return the smallest prime greater than or equal to n.
     * @throws MathIllegalArgumentException if n &lt; 0.
     */
    public static int nextPrime(int n) {
        if (n < 0) {
            throw new IllegalArgumentException("The specified number can't be negative");
        }
        if (n == 2) {
            return 2;
        }
        n |= 1;//make sure n is odd
        if (n == 1) {
            return 2;
        }

        if (isPrime(n)) {
            return n;
        }

        // prepare entry in the +2, +4 loop:
        // n should not be a multiple of 3
        final int rem = n % 3;
        if (0 == rem) { // if n % 3 == 0
            n += 2; // n % 3 == 2
        } else if (1 == rem) { // if n % 3 == 1
            // if (isPrime(n)) return n;
            n += 4; // n % 3 == 2
        }
        while (true) { // this loop skips all multiple of 3
            if (isPrime(n)) {
                return n;
            }
            n += 2; // n % 3 == 1
            if (isPrime(n)) {
                return n;
            }
            n += 4; // n % 3 == 2
        }
    }

    /**
     * Prime factors decomposition
     *
     * @param n number to factorize: must be &ge; 2
     * @return list of prime factors of n
     * @throws MathIllegalArgumentException if n &lt; 2.
     */
    public static List<Integer> primeFactors(final int n) {
        if (n < 2) {
            throw new IllegalArgumentException("The specified number can't be less than 2");
        }
        // slower than trial div unless we do an awful lot of computation
        // (then it finally gets JIT-compiled efficiently
        // List<Integer> out = PollardRho.primeFactors(n);
        return SmallPrimes.trialDivision(n);
    }

    static void sort(final boolean[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        int numOfFalse = 0;
        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == false) {
                numOfFalse++;
            }
        }

        N.fill(a, 0, numOfFalse, false);
        N.fill(a, numOfFalse, a.length, true);
    }

    static void sort(final char[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final char[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final byte[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final short[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final short[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final int[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final long[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final float[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.sort(a);
    }

    static void sort(final double[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex);
    }

    static void sort(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        sort(a, 0, a.length);
    }

    static void sort(final Object[] a, final int fromIndex, final int toIndex) {
        sort(a, fromIndex, toIndex, null);
    }

    static <T> void sort(final T[] a, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        sort(a, 0, a.length, cmp);
    }

    static <T> void sort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        Arrays.sort(a, fromIndex, toIndex, cmp);
    }

    static <T extends Comparable<? super T>> void sort(final List<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        sort(c, 0, c.size());
    }

    static <T extends Comparable<? super T>> void sort(final List<? extends T> c, final int fromIndex, final int toIndex) {
        sort(c, fromIndex, toIndex, null);
    }

    static <T> void sort(final List<? extends T> list, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(list)) {
            return;
        }

        sort(list, 0, list.size(), cmp);
    }

    @SuppressWarnings("rawtypes")
    static <T> void sort(final List<? extends T> c, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (N.isListElementDataFieldGettable && N.listElementDataField != null && c instanceof ArrayList) {
            T[] array = null;

            try {
                array = (T[]) N.listElementDataField.get(c);
            } catch (Exception e) {
                // ignore;
                N.isListElementDataFieldGettable = false;
            }

            if (array != null) {
                sort(array, fromIndex, toIndex, cmp);

                return;
            }
        }

        final T[] array = (T[]) c.toArray();
        Arrays.sort(array, fromIndex, toIndex, cmp);
        final ListIterator i = c.listIterator();

        for (int j = 0; j < array.length; j++) {
            i.next();
            i.set(array[j]);
        }
    }

    // ============================= Java 8 and above
    static void parallelSort(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a);
    }

    static void parallelSort(final int[] a, final int fromIndex, final int toIndex) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, fromIndex, toIndex);
    }

    static void parallelSort(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a);
    }

    static void parallelSort(final long[] a, final int fromIndex, final int toIndex) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, fromIndex, toIndex);
    }

    static void parallelSort(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a);
    }

    static void parallelSort(final float[] a, final int fromIndex, final int toIndex) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, fromIndex, toIndex);
    }

    static void parallelSort(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a);
    }

    static void parallelSort(final double[] a, final int fromIndex, final int toIndex) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, fromIndex, toIndex);
    }

    static <T extends Comparable<? super T>> void parallelSort(T[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a);
    }

    static <T extends Comparable<? super T>> void parallelSort(T[] a, int fromIndex, int toIndex) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, fromIndex, toIndex);
    }

    static <T> void parallelSort(final T[] a, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, cmp);
    }

    static <T> void parallelSort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        Arrays.parallelSort(a, fromIndex, toIndex, cmp);
    }

    //    static void parallelSort(final int[] a) {
    //        if (N.isNullOrEmpty(a)) {
    //            return;
    //        }
    //
    //        parallelSort(a, 0, a.length);
    //    }
    //
    //    static void parallelSort(final int[] a, final int fromIndex, final int toIndex) {
    //        N.checkFromToIndex(fromIndex, toIndex);
    //
    //        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
    //            return;
    //        }
    //
    //        final int len = toIndex - fromIndex;
    //
    //        if (len < MIN_ARRAY_SORT_GRAN) {
    //            sort(a, fromIndex, toIndex);
    //            return;
    //        }
    //
    //        final int size = len % CPU_CORES == 0 ? len / CPU_CORES : (len / CPU_CORES) + 1;
    //        final int[][] subArrays = new int[CPU_CORES][];
    //
    //        for (int i = 0; i < CPU_CORES; i++) {
    //            subArrays[i] = N.copyOfRange(a, fromIndex + i * size, fromIndex + i * size < toIndex - size ? fromIndex + i * size + size : toIndex);
    //        }
    //
    //        final AtomicInteger activeThreadNum = new AtomicInteger();
    //        final Holder<Throwable> errorHolder = new Holder<Throwable>();
    //
    //        for (final int[] tmp : subArrays) {
    //            activeThreadNum.incrementAndGet();
    //
    //            parallelSortExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    try {
    //                        if (errorHolder.getValue() != null) {
    //                            return;
    //                        }
    //
    //                        Arrays.sort(tmp);
    //                    } catch (Throwable e) {
    //                        errorHolder.setValue(e);
    //                    } finally {
    //                        activeThreadNum.decrementAndGet();
    //                    }
    //                }
    //            });
    //        }
    //
    //        while (activeThreadNum.get() > 0) {
    //            N.sleep(10);
    //        }
    //
    //        if (errorHolder.getValue() != null) {
    //            throw new AbacusException("Failed to sort", errorHolder.getValue());
    //        }
    //
    //        parallelMergeSort(a, fromIndex, subArrays);
    //    }
    //
    //    static void parallelSort(final long[] array) {
    //        if (N.isNullOrEmpty(array)) {
    //            return;
    //        }
    //
    //        parallelSort(array, 0, array.length);
    //    }
    //
    //    static void parallelSort(final long[] a, final int fromIndex, final int toIndex) {
    //        N.checkFromToIndex(fromIndex, toIndex);
    //
    //        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
    //            return;
    //        }
    //
    //        final int len = toIndex - fromIndex;
    //
    //        if (len < MIN_ARRAY_SORT_GRAN) {
    //            sort(a, fromIndex, toIndex);
    //            return;
    //        }
    //
    //        final int size = len % CPU_CORES == 0 ? len / CPU_CORES : (len / CPU_CORES) + 1;
    //        final long[][] subArrays = new long[CPU_CORES][];
    //
    //        for (int i = 0; i < CPU_CORES; i++) {
    //            subArrays[i] = N.copyOfRange(a, fromIndex + i * size, fromIndex + i * size < toIndex - size ? fromIndex + i * size + size : toIndex);
    //        }
    //
    //        final AtomicInteger activeThreadNum = new AtomicInteger();
    //        final Holder<Throwable> errorHolder = new Holder<Throwable>();
    //
    //        for (final long[] tmp : subArrays) {
    //            activeThreadNum.incrementAndGet();
    //
    //            parallelSortExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    try {
    //                        if (errorHolder.getValue() != null) {
    //                            return;
    //                        }
    //
    //                        Arrays.sort(tmp);
    //                    } catch (Throwable e) {
    //                        errorHolder.setValue(e);
    //                    } finally {
    //                        activeThreadNum.decrementAndGet();
    //                    }
    //                }
    //            });
    //        }
    //
    //        while (activeThreadNum.get() > 0) {
    //            N.sleep(10);
    //        }
    //
    //        if (errorHolder.getValue() != null) {
    //            throw new AbacusException("Failed to sort", errorHolder.getValue());
    //        }
    //
    //        parallelMergeSort(a, fromIndex, subArrays);
    //    }
    //
    //    static void parallelSort(final float[] a) {
    //        if (N.isNullOrEmpty(a)) {
    //            return;
    //        }
    //
    //        parallelSort(a, 0, a.length);
    //    }
    //
    //    static void parallelSort(final float[] a, final int fromIndex, final int toIndex) {
    //        N.checkFromToIndex(fromIndex, toIndex);
    //
    //        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
    //            return;
    //        }
    //
    //        final int len = toIndex - fromIndex;
    //
    //        if (len < MIN_ARRAY_SORT_GRAN) {
    //            sort(a, fromIndex, toIndex);
    //            return;
    //        }
    //
    //        final int size = len % CPU_CORES == 0 ? len / CPU_CORES : (len / CPU_CORES) + 1;
    //        final float[][] subArrays = new float[CPU_CORES][];
    //
    //        for (int i = 0; i < CPU_CORES; i++) {
    //            subArrays[i] = N.copyOfRange(a, fromIndex + i * size, fromIndex + i * size < toIndex - size ? fromIndex + i * size + size : toIndex);
    //        }
    //
    //        final AtomicInteger activeThreadNum = new AtomicInteger();
    //        final Holder<Throwable> errorHolder = new Holder<Throwable>();
    //
    //        for (final float[] tmp : subArrays) {
    //            activeThreadNum.incrementAndGet();
    //
    //            parallelSortExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    try {
    //                        if (errorHolder.getValue() != null) {
    //                            return;
    //                        }
    //
    //                        Arrays.sort(tmp);
    //                    } catch (Throwable e) {
    //                        errorHolder.setValue(e);
    //                    } finally {
    //                        activeThreadNum.decrementAndGet();
    //                    }
    //                }
    //            });
    //        }
    //
    //        while (activeThreadNum.get() > 0) {
    //            N.sleep(10);
    //        }
    //
    //        if (errorHolder.getValue() != null) {
    //            throw new AbacusException("Failed to sort", errorHolder.getValue());
    //        }
    //
    //        parallelMergeSort(a, fromIndex, subArrays);
    //    }
    //
    //    static void parallelSort(final double[] array) {
    //        if (N.isNullOrEmpty(array)) {
    //            return;
    //        }
    //
    //        parallelSort(array, 0, array.length);
    //    }
    //
    //    static void parallelSort(final double[] a, final int fromIndex, final int toIndex) {
    //        N.checkFromToIndex(fromIndex, toIndex);
    //
    //        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
    //            return;
    //        }
    //
    //        final int len = toIndex - fromIndex;
    //
    //        if (len < MIN_ARRAY_SORT_GRAN) {
    //            sort(a, fromIndex, toIndex);
    //            return;
    //        }
    //
    //        final int size = len % CPU_CORES == 0 ? len / CPU_CORES : (len / CPU_CORES) + 1;
    //        final double[][] subArrays = new double[CPU_CORES][];
    //
    //        for (int i = 0; i < CPU_CORES; i++) {
    //            subArrays[i] = N.copyOfRange(a, fromIndex + i * size, fromIndex + i * size < toIndex - size ? fromIndex + i * size + size : toIndex);
    //        }
    //
    //        final AtomicInteger activeThreadNum = new AtomicInteger();
    //        final Holder<Throwable> errorHolder = new Holder<Throwable>();
    //
    //        for (final double[] tmp : subArrays) {
    //            activeThreadNum.incrementAndGet();
    //
    //            parallelSortExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    try {
    //                        if (errorHolder.getValue() != null) {
    //                            return;
    //                        }
    //
    //                        Arrays.sort(tmp);
    //                    } catch (Throwable e) {
    //                        errorHolder.setValue(e);
    //                    } finally {
    //                        activeThreadNum.decrementAndGet();
    //                    }
    //                }
    //            });
    //        }
    //
    //        while (activeThreadNum.get() > 0) {
    //            N.sleep(10);
    //        }
    //
    //        if (errorHolder.getValue() != null) {
    //            throw new AbacusException("Failed to sort", errorHolder.getValue());
    //        }
    //
    //        parallelMergeSort(a, fromIndex, subArrays);
    //    }
    //
    //    static <T extends Comparable<? super T>> void parallelSort(final T[] a) {
    //        if (N.isNullOrEmpty(a)) {
    //            return;
    //        }
    //
    //        parallelSort(a, 0, a.length);
    //    }
    //
    //    static <T extends Comparable<? super T>> void parallelSort(final T[] a, final int fromIndex, final int toIndex) {
    //        parallelSort(a, fromIndex, toIndex, null);
    //    }
    //
    //    static <T> void parallelSort(final T[] a, final Comparator<? super T> cmp) {
    //        if (N.isNullOrEmpty(a)) {
    //            return;
    //        }
    //
    //        parallelSort(a, 0, a.length, cmp);
    //    }
    //
    //    static <T> void parallelSort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
    //        N.checkFromToIndex(fromIndex, toIndex);
    //
    //        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
    //            return;
    //        }
    //
    //        final int len = toIndex - fromIndex;
    //
    //        if (len < MIN_ARRAY_SORT_GRAN) {
    //            sort(a, fromIndex, toIndex, cmp);
    //            return;
    //        }
    //
    //        final int size = len % CPU_CORES == 0 ? len / CPU_CORES : (len / CPU_CORES) + 1;
    //        final T[][] subArrays = N.newArray(a.getClass(), CPU_CORES);
    //
    //        for (int i = 0; i < CPU_CORES; i++) {
    //            subArrays[i] = N.copyOfRange(a, fromIndex + i * size, fromIndex + i * size < toIndex - size ? fromIndex + i * size + size : toIndex);
    //        }
    //
    //        final AtomicInteger activeThreadNum = new AtomicInteger();
    //        final Holder<Throwable> errorHolder = new Holder<Throwable>();
    //
    //        for (final T[] tmp : subArrays) {
    //            activeThreadNum.incrementAndGet();
    //
    //            parallelSortExecutor.execute(new Runnable() {
    //                @Override
    //                public void run() {
    //                    try {
    //                        if (errorHolder.getValue() != null) {
    //                            return;
    //                        }
    //
    //                        Arrays.sort(tmp, cmp);
    //                    } catch (Throwable e) {
    //                        errorHolder.setValue(e);
    //                    } finally {
    //                        activeThreadNum.decrementAndGet();
    //                    }
    //                }
    //            });
    //        }
    //
    //        while (activeThreadNum.get() > 0) {
    //            N.sleep(10);
    //        }
    //
    //        if (errorHolder.getValue() != null) {
    //            throw new AbacusException("Failed to sort", errorHolder.getValue());
    //        }
    //
    //        parallelMergeSort(a, fromIndex, subArrays, cmp);
    //    }

    static <T extends Comparable<? super T>> void parallelSort(final List<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        parallelSort(c, 0, c.size());
    }

    static <T extends Comparable<? super T>> void parallelSort(final List<? extends T> c, final int fromIndex, final int toIndex) {
        parallelSort(c, fromIndex, toIndex, null);
    }

    static <T> void parallelSort(final List<? extends T> list, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(list)) {
            return;
        }

        parallelSort(list, 0, list.size(), cmp);
    }

    @SuppressWarnings("rawtypes")
    static <T> void parallelSort(final List<? extends T> c, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (N.isListElementDataFieldGettable && N.listElementDataField != null && c instanceof ArrayList) {
            T[] array = null;

            try {
                array = (T[]) N.listElementDataField.get(c);
            } catch (Exception e) {
                // ignore;
                N.isListElementDataFieldGettable = false;
            }

            if (array != null) {
                parallelSort(array, fromIndex, toIndex, cmp);

                return;
            }
        }

        final T[] array = (T[]) c.toArray();

        parallelSort(array, fromIndex, toIndex, cmp);

        final ListIterator<Object> it = (ListIterator) c.listIterator();

        for (int i = 0, len = array.length; i < len; i++) {
            it.next();

            it.set(array[i]);
        }
    }

    static int[] mergeSort(final int[][] sortedArrays) {
        int totalLength = 0;

        for (int[] a : sortedArrays) {
            totalLength += a.length;
        }

        final int[] a = new int[totalLength];

        mergeSort(a, 0, sortedArrays);

        return a;
    }

    static void mergeSort(final int[] output, final int fromIndex, final int[][] sortedArrays) {
        if (sortedArrays.length == 1) {
            N.copy(sortedArrays[0], 0, output, fromIndex, sortedArrays[0].length);
        } else if (sortedArrays.length == 2) {
            final int[] a = sortedArrays[0];
            final int[] b = sortedArrays[1];

            int aLength = a.length;
            int bLength = b.length;
            int aIndex = 0;
            int bIndex = 0;
            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength) {
                if (a[aIndex] <= b[bIndex]) {
                    output[k++] = a[aIndex++];
                } else {
                    output[k++] = b[bIndex++];
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            }
        } else if (sortedArrays.length == 3) {
            final int[] a = sortedArrays[0];
            final int[] b = sortedArrays[1];
            final int[] c = sortedArrays[2];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            }

        } else if (sortedArrays.length == 4) {
            final int[] a = sortedArrays[0];
            final int[] b = sortedArrays[1];
            final int[] c = sortedArrays[2];
            final int[] d = sortedArrays[3];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int dLength = d.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int dIndex = 0;
            int k = fromIndex;
            while (aIndex < aLength && bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= c[cIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    } else {
                        if (b[bIndex] <= c[cIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                while (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                while (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                while (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (aIndex < aLength && dIndex < dLength) {
                while (aIndex < aLength && dIndex < dLength) {
                    if (a[aIndex] <= d[dIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && dIndex < dLength) {
                while (bIndex < bLength && dIndex < dLength) {
                    if (b[bIndex] <= d[dIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (cIndex < cLength && dIndex < dLength) {
                while (cIndex < cLength && dIndex < dLength) {
                    if (c[cIndex] <= d[dIndex]) {
                        output[k++] = c[cIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            } else if (dIndex < dLength) {
                N.copy(d, dIndex, output, k, dLength - dIndex);
            }
        } else {
            final int[][] tmpSortedArrays = N.copyOfRange(sortedArrays, 0, 4);
            final int[] sortedTmpArray = mergeSort(tmpSortedArrays);

            final int[][] newSortedArrays = new int[sortedArrays.length - 4 + 1][];

            newSortedArrays[0] = sortedTmpArray;

            N.copy(sortedArrays, 4, newSortedArrays, 1, newSortedArrays.length - 1);

            mergeSort(output, fromIndex, newSortedArrays);
        }
    }

    static long[] mergeSort(final long[][] sortedArrays) {
        int totalLength = 0;

        for (long[] a : sortedArrays) {
            totalLength += a.length;
        }

        final long[] a = new long[totalLength];

        mergeSort(a, 0, sortedArrays);

        return a;
    }

    static void mergeSort(final long[] output, final int fromIndex, final long[][] sortedArrays) {
        if (sortedArrays.length == 1) {
            N.copy(sortedArrays[0], 0, output, fromIndex, sortedArrays[0].length);
        } else if (sortedArrays.length == 2) {
            final long[] a = sortedArrays[0];
            final long[] b = sortedArrays[1];

            int aLength = a.length;
            int bLength = b.length;
            int aIndex = 0;
            int bIndex = 0;
            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength) {
                if (a[aIndex] <= b[bIndex]) {
                    output[k++] = a[aIndex++];
                } else {
                    output[k++] = b[bIndex++];
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            }
        } else if (sortedArrays.length == 3) {
            final long[] a = sortedArrays[0];
            final long[] b = sortedArrays[1];
            final long[] c = sortedArrays[2];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            }

        } else if (sortedArrays.length == 4) {
            final long[] a = sortedArrays[0];
            final long[] b = sortedArrays[1];
            final long[] c = sortedArrays[2];
            final long[] d = sortedArrays[3];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int dLength = d.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int dIndex = 0;
            int k = fromIndex;
            while (aIndex < aLength && bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= c[cIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    } else {
                        if (b[bIndex] <= c[cIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                while (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                while (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                while (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (aIndex < aLength && dIndex < dLength) {
                while (aIndex < aLength && dIndex < dLength) {
                    if (a[aIndex] <= d[dIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && dIndex < dLength) {
                while (bIndex < bLength && dIndex < dLength) {
                    if (b[bIndex] <= d[dIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (cIndex < cLength && dIndex < dLength) {
                while (cIndex < cLength && dIndex < dLength) {
                    if (c[cIndex] <= d[dIndex]) {
                        output[k++] = c[cIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            } else if (dIndex < dLength) {
                N.copy(d, dIndex, output, k, dLength - dIndex);
            }
        } else {
            final long[][] tmpSortedArrays = N.copyOfRange(sortedArrays, 0, 4);
            final long[] sortedTmpArray = mergeSort(tmpSortedArrays);

            final long[][] newSortedArrays = new long[sortedArrays.length - 4 + 1][];

            newSortedArrays[0] = sortedTmpArray;

            N.copy(sortedArrays, 4, newSortedArrays, 1, newSortedArrays.length - 1);

            mergeSort(output, fromIndex, newSortedArrays);
        }
    }

    static float[] mergeSort(final float[][] sortedArrays) {
        int totalLength = 0;

        for (float[] a : sortedArrays) {
            totalLength += a.length;
        }

        final float[] a = new float[totalLength];

        mergeSort(a, 0, sortedArrays);

        return a;
    }

    static void mergeSort(final float[] output, final int fromIndex, final float[][] sortedArrays) {
        if (sortedArrays.length == 1) {
            N.copy(sortedArrays[0], 0, output, fromIndex, sortedArrays[0].length);
        } else if (sortedArrays.length == 2) {
            final float[] a = sortedArrays[0];
            final float[] b = sortedArrays[1];

            int aLength = a.length;
            int bLength = b.length;
            int aIndex = 0;
            int bIndex = 0;

            int numOfNaN = 0;
            while (aLength > 0 && Float.isNaN(a[aLength - 1])) {
                aLength--;
                numOfNaN++;
            }
            while (bLength > 0 && Float.isNaN(b[bLength - 1])) {
                bLength--;
                numOfNaN++;
            }

            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength) {
                if (a[aIndex] <= b[bIndex]) {
                    output[k++] = a[aIndex++];
                } else {
                    output[k++] = b[bIndex++];
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            }

            if (numOfNaN > 0) {
                N.fill(output, fromIndex + aLength + bLength, fromIndex + aLength + bLength + numOfNaN, Float.NaN);
            }

        } else if (sortedArrays.length == 3) {
            final float[] a = sortedArrays[0];
            final float[] b = sortedArrays[1];
            final float[] c = sortedArrays[2];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;

            int numOfNaN = 0;
            while (aLength > 0 && Float.isNaN(a[aLength - 1])) {
                aLength--;
                numOfNaN++;
            }
            while (bLength > 0 && Float.isNaN(b[bLength - 1])) {
                bLength--;
                numOfNaN++;
            }
            while (cLength > 0 && Float.isNaN(c[cLength - 1])) {
                cLength--;
                numOfNaN++;
            }

            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            }

            if (numOfNaN > 0) {
                N.fill(output, fromIndex + aLength + bLength + cLength, fromIndex + aLength + bLength + cLength + numOfNaN, Float.NaN);
            }

        } else if (sortedArrays.length == 4) {
            final float[] a = sortedArrays[0];
            final float[] b = sortedArrays[1];
            final float[] c = sortedArrays[2];
            final float[] d = sortedArrays[3];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int dLength = d.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int dIndex = 0;

            int numOfNaN = 0;
            while (aLength > 0 && Float.isNaN(a[aLength - 1])) {
                aLength--;
                numOfNaN++;
            }
            while (bLength > 0 && Float.isNaN(b[bLength - 1])) {
                bLength--;
                numOfNaN++;
            }
            while (cLength > 0 && Float.isNaN(c[cLength - 1])) {
                cLength--;
                numOfNaN++;
            }
            while (dLength > 0 && Float.isNaN(d[dLength - 1])) {
                dLength--;
                numOfNaN++;
            }

            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= c[cIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    } else {
                        if (b[bIndex] <= c[cIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                while (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                while (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                while (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (aIndex < aLength && dIndex < dLength) {
                while (aIndex < aLength && dIndex < dLength) {
                    if (a[aIndex] <= d[dIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && dIndex < dLength) {
                while (bIndex < bLength && dIndex < dLength) {
                    if (b[bIndex] <= d[dIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (cIndex < cLength && dIndex < dLength) {
                while (cIndex < cLength && dIndex < dLength) {
                    if (c[cIndex] <= d[dIndex]) {
                        output[k++] = c[cIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            } else if (dIndex < dLength) {
                N.copy(d, dIndex, output, k, dLength - dIndex);
            }

            if (numOfNaN > 0) {
                N.fill(output, fromIndex + aLength + bLength + cLength + dLength, fromIndex + aLength + bLength + cLength + dLength + numOfNaN, Float.NaN);
            }
        } else {
            final float[][] tmpSortedArrays = N.copyOfRange(sortedArrays, 0, 4);
            final float[] sortedTmpArray = mergeSort(tmpSortedArrays);

            final float[][] newSortedArrays = new float[sortedArrays.length - 4 + 1][];

            newSortedArrays[0] = sortedTmpArray;

            N.copy(sortedArrays, 4, newSortedArrays, 1, newSortedArrays.length - 1);

            mergeSort(output, fromIndex, newSortedArrays);
        }
    }

    static double[] mergeSort(final double[][] sortedArrays) {
        int totalLength = 0;

        for (double[] a : sortedArrays) {
            totalLength += a.length;
        }

        final double[] a = new double[totalLength];

        mergeSort(a, 0, sortedArrays);

        return a;
    }

    static void mergeSort(final double[] output, final int fromIndex, final double[][] sortedArrays) {
        if (sortedArrays.length == 1) {
            N.copy(sortedArrays[0], 0, output, fromIndex, sortedArrays[0].length);
        } else if (sortedArrays.length == 2) {
            final double[] a = sortedArrays[0];
            final double[] b = sortedArrays[1];

            int aLength = a.length;
            int bLength = b.length;
            int aIndex = 0;
            int bIndex = 0;

            int numOfNaN = 0;
            while (aLength > 0 && Double.isNaN(a[aLength - 1])) {
                aLength--;
                numOfNaN++;
            }
            while (bLength > 0 && Double.isNaN(b[bLength - 1])) {
                bLength--;
                numOfNaN++;
            }

            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength) {
                if (a[aIndex] <= b[bIndex]) {
                    output[k++] = a[aIndex++];
                } else {
                    output[k++] = b[bIndex++];
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            }

            if (numOfNaN > 0) {
                N.fill(output, fromIndex + aLength + bLength, fromIndex + aLength + bLength + numOfNaN, Double.NaN);
            }

        } else if (sortedArrays.length == 3) {
            final double[] a = sortedArrays[0];
            final double[] b = sortedArrays[1];
            final double[] c = sortedArrays[2];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;

            int numOfNaN = 0;
            while (aLength > 0 && Double.isNaN(a[aLength - 1])) {
                aLength--;
                numOfNaN++;
            }
            while (bLength > 0 && Double.isNaN(b[bLength - 1])) {
                bLength--;
                numOfNaN++;
            }
            while (cLength > 0 && Double.isNaN(c[cLength - 1])) {
                cLength--;
                numOfNaN++;
            }

            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            }

            if (numOfNaN > 0) {
                N.fill(output, fromIndex + aLength + bLength + cLength, fromIndex + aLength + bLength + cLength + numOfNaN, Double.NaN);
            }

        } else if (sortedArrays.length == 4) {
            final double[] a = sortedArrays[0];
            final double[] b = sortedArrays[1];
            final double[] c = sortedArrays[2];
            final double[] d = sortedArrays[3];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int dLength = d.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int dIndex = 0;

            int numOfNaN = 0;
            while (aLength > 0 && Double.isNaN(a[aLength - 1])) {
                aLength--;
                numOfNaN++;
            }
            while (bLength > 0 && Double.isNaN(b[bLength - 1])) {
                bLength--;
                numOfNaN++;
            }
            while (cLength > 0 && Double.isNaN(c[cLength - 1])) {
                cLength--;
                numOfNaN++;
            }
            while (dLength > 0 && Double.isNaN(d[dLength - 1])) {
                dLength--;
                numOfNaN++;
            }

            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                if (a[aIndex] <= b[bIndex]) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }

                } else {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= c[cIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    } else {
                        if (b[bIndex] <= c[cIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                while (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                while (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        if (a[aIndex] <= d[dIndex]) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                while (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        if (b[bIndex] <= d[dIndex]) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (c[cIndex] <= d[dIndex]) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (a[aIndex] <= b[bIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (a[aIndex] <= c[cIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (aIndex < aLength && dIndex < dLength) {
                while (aIndex < aLength && dIndex < dLength) {
                    if (a[aIndex] <= d[dIndex]) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (b[bIndex] <= c[cIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && dIndex < dLength) {
                while (bIndex < bLength && dIndex < dLength) {
                    if (b[bIndex] <= d[dIndex]) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (cIndex < cLength && dIndex < dLength) {
                while (cIndex < cLength && dIndex < dLength) {
                    if (c[cIndex] <= d[dIndex]) {
                        output[k++] = c[cIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            } else if (dIndex < dLength) {
                N.copy(d, dIndex, output, k, dLength - dIndex);
            }

            if (numOfNaN > 0) {
                N.fill(output, fromIndex + aLength + bLength + cLength + dLength, fromIndex + aLength + bLength + cLength + dLength + numOfNaN, Double.NaN);
            }
        } else {
            final double[][] tmpSortedArrays = N.copyOfRange(sortedArrays, 0, 4);
            final double[] sortedTmpArray = mergeSort(tmpSortedArrays);

            final double[][] newSortedArrays = new double[sortedArrays.length - 4 + 1][];

            newSortedArrays[0] = sortedTmpArray;

            N.copy(sortedArrays, 4, newSortedArrays, 1, newSortedArrays.length - 1);

            mergeSort(output, fromIndex, newSortedArrays);
        }
    }

    static <T extends Comparable<? super T>> T[] mergeSort(final T[][] sortedArrays) {
        return mergeSort(sortedArrays, null);
    }

    static <T> T[] mergeSort(final T[][] sortedArrays, final Comparator<? super T> cmp) {
        int totalLength = 0;

        for (T[] a : sortedArrays) {
            totalLength += a.length;
        }

        final T[] a = N.newArray(sortedArrays.getClass().getComponentType().getComponentType(), totalLength);

        mergeSort(a, 0, sortedArrays, cmp);

        return a;
    }

    static <T> void mergeSort(final T[] output, final int fromIndex, final T[][] sortedArrays, final Comparator<? super T> cmp) {
        final Comparator<? super T> comparator = cmp == null ? N.comparableCmp : cmp;

        if (sortedArrays.length == 1) {
            N.copy(sortedArrays[0], 0, output, fromIndex, sortedArrays[0].length);

        } else if (sortedArrays.length == 2) {
            final T[] a = sortedArrays[0];
            final T[] b = sortedArrays[1];

            int aLength = a.length;
            int bLength = b.length;
            int aIndex = 0;
            int bIndex = 0;
            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength) {
                if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                    output[k++] = a[aIndex++];
                } else {
                    output[k++] = b[bIndex++];
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            }
        } else if (sortedArrays.length == 3) {
            final T[] a = sortedArrays[0];
            final T[] b = sortedArrays[1];
            final T[] c = sortedArrays[2];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int k = fromIndex;

            while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                    if (comparator.compare(a[aIndex], c[cIndex]) <= 0) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }

                } else {
                    if (comparator.compare(b[bIndex], c[cIndex]) <= 0) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (comparator.compare(a[aIndex], c[cIndex]) <= 0) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (comparator.compare(b[bIndex], c[cIndex]) <= 0) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            }

        } else if (sortedArrays.length == 4) {
            final T[] a = sortedArrays[0];
            final T[] b = sortedArrays[1];
            final T[] c = sortedArrays[2];
            final T[] d = sortedArrays[3];

            int aLength = a.length;
            int bLength = b.length;
            int cLength = c.length;
            int dLength = d.length;
            int aIndex = 0;
            int bIndex = 0;
            int cIndex = 0;
            int dIndex = 0;
            int k = fromIndex;
            while (aIndex < aLength && bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                    if (comparator.compare(a[aIndex], c[cIndex]) <= 0) {
                        if (comparator.compare(a[aIndex], d[dIndex]) <= 0) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }

                    } else {
                        if (comparator.compare(c[cIndex], d[dIndex]) <= 0) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }

                } else {
                    if (comparator.compare(b[bIndex], c[cIndex]) <= 0) {
                        if (comparator.compare(b[bIndex], d[dIndex]) <= 0) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (comparator.compare(c[cIndex], d[dIndex]) <= 0) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                while (aIndex < aLength && bIndex < bLength && cIndex < cLength) {
                    if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                        if (comparator.compare(a[aIndex], c[cIndex]) <= 0) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    } else {
                        if (comparator.compare(b[bIndex], c[cIndex]) <= 0) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = c[cIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                while (aIndex < aLength && bIndex < bLength && dIndex < dLength) {
                    if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                        if (comparator.compare(a[aIndex], d[dIndex]) <= 0) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (comparator.compare(b[bIndex], d[dIndex]) <= 0) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                while (aIndex < aLength && cIndex < cLength && dIndex < dLength) {
                    if (comparator.compare(a[aIndex], c[cIndex]) <= 0) {
                        if (comparator.compare(a[aIndex], d[dIndex]) <= 0) {
                            output[k++] = a[aIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (comparator.compare(c[cIndex], d[dIndex]) <= 0) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                while (bIndex < bLength && cIndex < cLength && dIndex < dLength) {
                    if (comparator.compare(b[bIndex], c[cIndex]) <= 0) {
                        if (comparator.compare(b[bIndex], d[dIndex]) <= 0) {
                            output[k++] = b[bIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    } else {
                        if (comparator.compare(c[cIndex], d[dIndex]) <= 0) {
                            output[k++] = c[cIndex++];
                        } else {
                            output[k++] = d[dIndex++];
                        }
                    }
                }
            }

            if (aIndex < aLength && bIndex < bLength) {
                while (aIndex < aLength && bIndex < bLength) {
                    if (comparator.compare(a[aIndex], b[bIndex]) <= 0) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = b[bIndex++];
                    }
                }

            } else if (aIndex < aLength && cIndex < cLength) {
                while (aIndex < aLength && cIndex < cLength) {
                    if (comparator.compare(a[aIndex], c[cIndex]) <= 0) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (aIndex < aLength && dIndex < dLength) {
                while (aIndex < aLength && dIndex < dLength) {
                    if (comparator.compare(a[aIndex], d[dIndex]) <= 0) {
                        output[k++] = a[aIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (bIndex < bLength && cIndex < cLength) {
                while (bIndex < bLength && cIndex < cLength) {
                    if (comparator.compare(b[bIndex], c[cIndex]) <= 0) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = c[cIndex++];
                    }
                }

            } else if (bIndex < bLength && dIndex < dLength) {
                while (bIndex < bLength && dIndex < dLength) {
                    if (comparator.compare(b[bIndex], d[dIndex]) <= 0) {
                        output[k++] = b[bIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }

            } else if (cIndex < cLength && dIndex < dLength) {
                while (cIndex < cLength && dIndex < dLength) {
                    if (comparator.compare(c[cIndex], d[dIndex]) <= 0) {
                        output[k++] = c[cIndex++];
                    } else {
                        output[k++] = d[dIndex++];
                    }
                }
            }

            if (aIndex < aLength) {
                N.copy(a, aIndex, output, k, aLength - aIndex);
            } else if (bIndex < bLength) {
                N.copy(b, bIndex, output, k, bLength - bIndex);
            } else if (cIndex < cLength) {
                N.copy(c, cIndex, output, k, cLength - cIndex);
            } else if (dIndex < dLength) {
                N.copy(d, dIndex, output, k, dLength - dIndex);
            }
        } else {
            final T[][] tmpSortedArrays = N.copyOfRange(sortedArrays, 0, 4);
            final T[] sortedTmpArray = mergeSort(tmpSortedArrays, comparator);

            final T[][] newSortedArrays = N.newArray(sortedArrays.getClass().getComponentType(), sortedArrays.length - 4 + 1);

            newSortedArrays[0] = sortedTmpArray;

            N.copy(sortedArrays, 4, newSortedArrays, 1, newSortedArrays.length - 1);

            mergeSort(output, fromIndex, newSortedArrays, comparator);
        }
    }

    static int[] parallelMergeSort(final int[][] sortedArrays) {
        int totalLength = 0;

        for (int[] a : sortedArrays) {
            totalLength += a.length;
        }

        final int[] a = new int[totalLength];

        parallelMergeSort(a, 0, sortedArrays);

        return a;
    }

    static void parallelMergeSort(final int[] output, final int fromIndex, int[][] sortedArrays) {
        if (sortedArrays.length <= 3) {
            mergeSort(output, fromIndex, sortedArrays);
            return;
        }

        final int len = sortedArrays.length;
        final Queue<int[]> queue = N.newConcurrentLinkedQueue();

        for (int[] e : sortedArrays) {
            queue.add(e);
        }

        sortedArrays = null; // GC

        final AtomicInteger activeThreadNum = new AtomicInteger();
        final AtomicInteger sortCount = new AtomicInteger();
        final Holder<Throwable> errorHolder = new Holder<Throwable>();

        for (int i = 0; i < N.min(CPU_CORES, len / 2); i++) {
            activeThreadNum.incrementAndGet();

            parallelSortExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (true) {
                            if (errorHolder.getValue() != null || sortCount.get() >= len - 1) {
                                return;
                            }

                            if (queue.size() < 2) {
                                N.sleep(10);
                            }

                            int[][] ab = null;

                            synchronized (queue) {
                                if (queue.size() >= 2) {
                                    ab = N.asArray(queue.poll(), queue.poll());
                                }
                            }

                            if (ab != null) {
                                final int[] c = mergeSort(ab);
                                queue.offer(c);
                                sortCount.incrementAndGet();
                            }
                        }
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    } finally {
                        activeThreadNum.decrementAndGet();
                    }
                }
            });
        }

        while (activeThreadNum.get() > 0) {
            N.sleep(10);
        }

        if (errorHolder.getValue() != null) {
            throw new AbacusException("Failed to sort", errorHolder.getValue());
        }

        final int[] last = queue.poll();

        N.copy(last, 0, output, fromIndex, last.length);
    }

    static long[] parallelMergeSort(final long[][] sortedArrays) {
        int totalLength = 0;

        for (long[] a : sortedArrays) {
            totalLength += a.length;
        }

        final long[] a = new long[totalLength];

        parallelMergeSort(a, 0, sortedArrays);

        return a;
    }

    static void parallelMergeSort(final long[] output, final int fromIndex, long[][] sortedArrays) {
        if (sortedArrays.length <= 3) {
            mergeSort(output, fromIndex, sortedArrays);
            return;
        }

        final int len = sortedArrays.length;
        final Queue<long[]> queue = N.newConcurrentLinkedQueue();

        for (long[] e : sortedArrays) {
            queue.add(e);
        }

        sortedArrays = null; // GC

        final AtomicInteger activeThreadNum = new AtomicInteger();
        final AtomicInteger sortCount = new AtomicInteger();
        final Holder<Throwable> errorHolder = new Holder<Throwable>();

        for (int i = 0; i < N.min(CPU_CORES, len / 2); i++) {
            activeThreadNum.incrementAndGet();

            parallelSortExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (true) {
                            if (errorHolder.getValue() != null || sortCount.get() >= len - 1) {
                                return;
                            }

                            if (queue.size() < 2) {
                                N.sleep(10);
                            }

                            long[][] ab = null;

                            synchronized (queue) {
                                if (queue.size() >= 2) {
                                    ab = N.asArray(queue.poll(), queue.poll());
                                }
                            }

                            if (ab != null) {
                                final long[] c = mergeSort(ab);
                                queue.offer(c);
                                sortCount.incrementAndGet();
                            }
                        }
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    } finally {
                        activeThreadNum.decrementAndGet();
                    }
                }
            });
        }

        while (activeThreadNum.get() > 0) {
            N.sleep(10);
        }

        if (errorHolder.getValue() != null) {
            throw new AbacusException("Failed to sort", errorHolder.getValue());
        }

        final long[] last = queue.poll();

        N.copy(last, 0, output, fromIndex, last.length);
    }

    static float[] parallelMergeSort(final float[][] sortedArrays) {
        int totalLength = 0;

        for (float[] a : sortedArrays) {
            totalLength += a.length;
        }

        final float[] a = new float[totalLength];

        parallelMergeSort(a, 0, sortedArrays);

        return a;
    }

    static void parallelMergeSort(final float[] output, final int fromIndex, float[][] sortedArrays) {
        if (sortedArrays.length <= 3) {
            mergeSort(output, fromIndex, sortedArrays);
            return;
        }

        final int len = sortedArrays.length;
        final Queue<float[]> queue = N.newConcurrentLinkedQueue();

        for (float[] e : sortedArrays) {
            queue.add(e);
        }

        sortedArrays = null; // GC

        final AtomicInteger activeThreadNum = new AtomicInteger();
        final AtomicInteger sortCount = new AtomicInteger();
        final Holder<Throwable> errorHolder = new Holder<Throwable>();

        for (int i = 0; i < N.min(CPU_CORES, len / 2); i++) {
            activeThreadNum.incrementAndGet();

            parallelSortExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (true) {
                            if (errorHolder.getValue() != null || sortCount.get() >= len - 1) {
                                return;
                            }

                            if (queue.size() < 2) {
                                N.sleep(10);
                            }

                            float[][] ab = null;

                            synchronized (queue) {
                                if (queue.size() >= 2) {
                                    ab = N.asArray(queue.poll(), queue.poll());
                                }
                            }

                            if (ab != null) {
                                final float[] c = mergeSort(ab);
                                queue.offer(c);
                                sortCount.incrementAndGet();
                            }
                        }
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    } finally {
                        activeThreadNum.decrementAndGet();
                    }
                }
            });
        }

        while (activeThreadNum.get() > 0) {
            N.sleep(10);
        }

        if (errorHolder.getValue() != null) {
            throw new AbacusException("Failed to sort", errorHolder.getValue());
        }

        final float[] last = queue.poll();

        N.copy(last, 0, output, fromIndex, last.length);
    }

    static double[] parallelMergeSort(final double[][] sortedArrays) {
        int totalLength = 0;

        for (double[] a : sortedArrays) {
            totalLength += a.length;
        }

        final double[] a = new double[totalLength];

        parallelMergeSort(a, 0, sortedArrays);

        return a;
    }

    static void parallelMergeSort(final double[] output, final int fromIndex, double[][] sortedArrays) {
        if (sortedArrays.length <= 3) {
            mergeSort(output, fromIndex, sortedArrays);
            return;
        }

        final int len = sortedArrays.length;
        final Queue<double[]> queue = N.newConcurrentLinkedQueue();

        for (double[] e : sortedArrays) {
            queue.add(e);
        }

        sortedArrays = null; // GC

        final AtomicInteger activeThreadNum = new AtomicInteger();
        final AtomicInteger sortCount = new AtomicInteger();
        final Holder<Throwable> errorHolder = new Holder<Throwable>();

        for (int i = 0; i < N.min(CPU_CORES, len / 2); i++) {
            activeThreadNum.incrementAndGet();

            parallelSortExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (true) {
                            if (errorHolder.getValue() != null || sortCount.get() >= len - 1) {
                                return;
                            }

                            if (queue.size() < 2) {
                                N.sleep(10);
                            }

                            double[][] ab = null;

                            synchronized (queue) {
                                if (queue.size() >= 2) {
                                    ab = N.asArray(queue.poll(), queue.poll());
                                }
                            }

                            if (ab != null) {
                                final double[] c = mergeSort(ab);
                                queue.offer(c);
                                sortCount.incrementAndGet();
                            }
                        }
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    } finally {
                        activeThreadNum.decrementAndGet();
                    }
                }
            });
        }

        while (activeThreadNum.get() > 0) {
            N.sleep(10);
        }

        if (errorHolder.getValue() != null) {
            throw new AbacusException("Failed to sort", errorHolder.getValue());
        }

        final double[] last = queue.poll();

        N.copy(last, 0, output, fromIndex, last.length);
    }

    static <T extends Comparable<? super T>> T[] parallelMergeSort(final T[][] sortedArrays) {
        return parallelMergeSort(sortedArrays, null);
    }

    static <T> T[] parallelMergeSort(final T[][] sortedArrays, final Comparator<? super T> cmp) {
        int totalLength = 0;

        for (T[] a : sortedArrays) {
            totalLength += a.length;
        }

        final T[] a = N.newArray(sortedArrays.getClass().getComponentType().getComponentType(), totalLength);

        parallelMergeSort(a, 0, sortedArrays, cmp);

        return a;
    }

    static <T> void parallelMergeSort(final T[] output, final int fromIndex, T[][] sortedArrays, final Comparator<? super T> cmp) {
        if (sortedArrays.length <= 3) {
            mergeSort(output, fromIndex, sortedArrays, cmp);
            return;
        }

        final int len = sortedArrays.length;
        final Queue<T[]> queue = N.newConcurrentLinkedQueue();

        for (T[] e : sortedArrays) {
            queue.add(e);
        }

        sortedArrays = null; // GC

        final AtomicInteger activeThreadNum = new AtomicInteger();
        final AtomicInteger sortCount = new AtomicInteger();
        final Holder<Throwable> errorHolder = new Holder<Throwable>();

        for (int i = 0; i < N.min(CPU_CORES, len / 2); i++) {
            activeThreadNum.incrementAndGet();

            parallelSortExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (true) {
                            if (errorHolder.getValue() != null || sortCount.get() >= len - 1) {
                                return;
                            }

                            if (queue.size() < 2) {
                                N.sleep(10);
                            }

                            T[][] ab = null;

                            synchronized (queue) {
                                if (queue.size() >= 2) {
                                    ab = N.asArray(queue.poll(), queue.poll());
                                }
                            }

                            if (ab != null) {
                                final T[] c = mergeSort(ab, cmp);
                                queue.offer(c);
                                sortCount.incrementAndGet();
                            }
                        }
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    } finally {
                        activeThreadNum.decrementAndGet();
                    }
                }
            });
        }

        while (activeThreadNum.get() > 0) {
            N.sleep(10);
        }

        if (errorHolder.getValue() != null) {
            throw new AbacusException("Failed to sort", errorHolder.getValue());
        }

        final T[] last = queue.poll();

        N.copy(last, 0, output, fromIndex, last.length);
    }

    static void bucketSort(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        bucketSort(a, 0, a.length);
    }

    static void bucketSort(final int[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (toIndex - fromIndex < 32) {
            sort(a, fromIndex, toIndex);
            return;
        }

        final Multiset<Integer> multiset = N.newMultiset();

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(a[i]);
        }

        final Map<Integer, Integer> m = multiset.toMapSortedByElement();
        int idx = fromIndex;

        for (Map.Entry<Integer, Integer> entry : m.entrySet()) {
            N.fill(a, idx, idx + entry.getValue(), entry.getKey());
            idx += entry.getValue();
        }
    }

    static void bucketSort(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        bucketSort(a, 0, a.length);
    }

    static void bucketSort(final long[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (toIndex - fromIndex < 32) {
            sort(a, fromIndex, toIndex);
            return;
        }

        final Multiset<Long> multiset = N.newMultiset();

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(a[i]);
        }

        final Map<Long, Integer> m = multiset.toMapSortedByElement();
        int idx = fromIndex;

        for (Map.Entry<Long, Integer> entry : m.entrySet()) {
            N.fill(a, idx, idx + entry.getValue(), entry.getKey());
            idx += entry.getValue();
        }
    }

    static void bucketSort(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        bucketSort(a, 0, a.length);
    }

    static void bucketSort(final float[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (toIndex - fromIndex < 32) {
            sort(a, fromIndex, toIndex);
            return;
        }

        final Multiset<Float> multiset = N.newMultiset();

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(a[i]);
        }

        final Map<Float, Integer> m = multiset.toMapSortedByElement();
        int idx = fromIndex;

        for (Map.Entry<Float, Integer> entry : m.entrySet()) {
            N.fill(a, idx, idx + entry.getValue(), entry.getKey());
            idx += entry.getValue();
        }
    }

    static void bucketSort(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        bucketSort(a, 0, a.length);
    }

    static void bucketSort(final double[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (toIndex - fromIndex < 32) {
            sort(a, fromIndex, toIndex);
            return;
        }

        final Multiset<Double> multiset = N.newMultiset();

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(a[i]);
        }

        final Map<Double, Integer> m = multiset.toMapSortedByElement();
        int idx = fromIndex;

        for (Map.Entry<Double, Integer> entry : m.entrySet()) {
            N.fill(a, idx, idx + entry.getValue(), entry.getKey());
            idx += entry.getValue();
        }
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     *   
     * @param a
     */
    static void bucketSort(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        bucketSort(a, 0, a.length);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param a the elements in the array must implements the <code>Comparable</code> interface.
     * @param fromIndex
     * @param toIndex
     */
    static void bucketSort(final Object[] a, final int fromIndex, final int toIndex) {
        bucketSort(a, fromIndex, toIndex, null);
    }

    static <T> void bucketSort(final T[] a, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        bucketSort(a, 0, a.length, cmp);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param cmp
     */
    static <T> void bucketSort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(a) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (toIndex - fromIndex < 32) {
            sort(a, fromIndex, toIndex, cmp);
            return;
        }

        final Multiset<T> multiset = N.newMultiset();

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(a[i]);
        }

        final Map<T, Integer> m = multiset.toMapSortedByElement(cmp);
        int idx = fromIndex;

        for (Map.Entry<T, Integer> entry : m.entrySet()) {
            N.fill(a, idx, idx + entry.getValue(), entry.getKey());
            idx += entry.getValue();
        }
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     */
    static <T extends Comparable<T>> void bucketSort(final List<T> c) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        bucketSort(c, 0, c.size());
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     */
    static <T extends Comparable<T>> void bucketSort(final List<T> c, final int fromIndex, final int toIndex) {
        bucketSort(c, fromIndex, toIndex, null);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     * @param cmp
     */
    static <T> void bucketSort(final List<? extends T> c, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        bucketSort(c, 0, c.size(), cmp);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param cmp
     */
    static <T> void bucketSort(final List<? extends T> c, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        N.checkFromToIndex(fromIndex, toIndex);

        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || fromIndex == toIndex) {
            return;
        }

        if (toIndex - fromIndex < 32) {
            sort(c, fromIndex, toIndex, cmp);
            return;
        }

        final Multiset<T> multiset = N.newMultiset();
        ListIterator<T> itr = (ListIterator<T>) c.listIterator(fromIndex);
        int i = fromIndex;

        while (itr.hasNext()) {
            if (i++ >= toIndex) {
                break;
            }

            multiset.add(itr.next());
        }

        final Map<T, Integer> m = multiset.toMapSortedByElement(cmp);
        itr = (ListIterator<T>) c.listIterator(fromIndex);

        for (Map.Entry<T, Integer> entry : m.entrySet()) {
            final T key = entry.getKey();
            for (int j = 0; j < entry.getValue(); j++) {
                itr.next();
                itr.set(key);
            }
        }
    }

    /**
     * {@link Arrays#binarySearch(boolean[], boolean)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final boolean[] a, final boolean key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        if (a[0] == key) {
            return 0;
        } else if (a[a.length - 1] != key) {
            return N.INDEX_NOT_FOUND;
        }

        int left = 0, right = a.length - 1;
        while (left < right) {
            int mid = left + (right - left) / 2;

            if (a[mid] == key) {
                right = mid;
            } else {
                left = mid + 1;
            }
        }
        return left;
    }

    /**
     * {@link Arrays#binarySearch(char[], char)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final char[] a, final char key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(char[], int, int, char)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final char[] a, final int fromIndex, final int toIndex, final char key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(byte[], byte)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final byte[] a, final byte key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(byte[], int, int, byte)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final byte[] a, final int fromIndex, final int toIndex, final byte key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(short[], short)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final short[] a, final short key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(short[], int, int, short)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final short[] a, final int fromIndex, final int toIndex, final short key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(int[], int)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final int[] a, final int key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(int[], int, int, int)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final int[] a, final int fromIndex, final int toIndex, final int key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(long[], long)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final long[] a, final long key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(long[], int, int, long)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final long[] a, final int fromIndex, final int toIndex, final long key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(float[], float)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final float[] a, final float key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(float[], int, int, float)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final float[] a, final int fromIndex, final int toIndex, final float key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(double[], double)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final double[] a, final double key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(double[], int, int, double)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static int binarySearch(final double[] a, final int fromIndex, final int toIndex, final double key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(Object[], Object)}
     *
     * @param a
     * @param key
     * @return
     */
    static <T extends Comparable<T>> int binarySearch(final T[] a, final T key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(Object[], int, int, Object)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    static <T extends Comparable<T>> int binarySearch(final T[] a, final int fromIndex, final int toIndex, final T key) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(Object[], Object, Comparator)}
     *
     * @param a
     * @param key
     * @param cmp
     * @return
     */
    static <T> int binarySearch(final T[] a, final T key, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key, cmp == null ? N.comparableCmp : cmp);
    }

    /**
     * {@link Arrays#binarySearch(Object[], int, int, Object, Comparator)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @param c
     * @return
     */
    static <T> int binarySearch(final T[] a, final int fromIndex, final int toIndex, final T key, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        return Arrays.binarySearch(a, key, cmp == null ? N.comparableCmp : cmp);
    }

    /**
     * {@link Collections#binarySearch(List, Object)}
     *
     * @param list
     * @param key
     * @return
     */
    static <T extends Comparable<? super T>> int binarySearch(final List<? extends T> list, final T key) {
        if (N.isNullOrEmpty(list)) {
            return N.INDEX_NOT_FOUND;
        }

        return binarySearch(list, 0, list.size(), key);
    }

    static <T extends Comparable<? super T>> int binarySearch(final List<? extends T> list, final int fromIndex, final int toIndex, final T key) {
        if (N.isNullOrEmpty(list)) {
            return N.INDEX_NOT_FOUND;
        }

        return binarySearch(list, fromIndex, toIndex, key, N.comparableCmp);
    }

    static <T> int binarySearch(final List<? extends T> list, final T key, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(list)) {
            return N.INDEX_NOT_FOUND;
        }

        return binarySearch(list, 0, list.size(), key, cmp);
    }

    /**
     *
     * @param list
     * @param key
     * @param fromIndex
     * @param toIndex
     * @param cmp
     * @return
     * @see Collections#binarySearch(List, Object, Comparator)
     */
    static <T> int binarySearch(final List<? extends T> list, final int fromIndex, final int toIndex, final T key, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(list)) {
            return N.INDEX_NOT_FOUND;
        }

        if (N.isListElementDataFieldGettable && N.listElementDataField != null && list instanceof ArrayList) {
            T[] array = null;

            try {
                array = (T[]) N.listElementDataField.get(list);
            } catch (Exception e) {
                // ignore;
                N.isListElementDataFieldGettable = false;
            }

            if (array != null) {
                return binarySearch(array, fromIndex, toIndex, key, cmp == null ? N.comparableCmp : cmp);
            }
        }

        if (list instanceof RandomAccess || list.size() < BINARYSEARCH_THRESHOLD) {
            return indexedBinarySearch(list, fromIndex, toIndex, key, cmp == null ? N.comparableCmp : cmp);
        } else {
            return iteratorBinarySearch(list, fromIndex, toIndex, key, cmp == null ? N.comparableCmp : cmp);
        }
    }

    private static <T> int indexedBinarySearch(final List<? extends T> l, final int fromIndex, final int toIndex, final T key,
            final Comparator<? super T> cmp) {
        int low = fromIndex;
        int high = toIndex - 1;

        while (low <= high) {
            int mid = (low + high) >>> 1;
            T midVal = l.get(mid);

            int res = cmp.compare(midVal, key);

            if (res < 0) {
                low = mid + 1;
            } else if (res > 0) {
                high = mid - 1;
            } else {
                return mid; // key found
            }
        }

        return N.INDEX_NOT_FOUND; // key not found
    }

    private static <T> int iteratorBinarySearch(final List<? extends T> l, final int fromIndex, final int toIndex, final T key,
            final Comparator<? super T> cmp) {
        int low = fromIndex;
        int high = toIndex - 1;

        ListIterator<? extends T> iterator = l.listIterator();

        while (low <= high) {
            int mid = (low + high) >>> 1;
            T midVal = get(iterator, mid);

            int res = cmp.compare(midVal, key);

            if (res < 0) {
                low = mid + 1;
            } else if (res > 0) {
                high = mid - 1;
            } else {
                return mid; // key found
            }
        }

        return N.INDEX_NOT_FOUND; // key not found
    }

    /**
     * Gets the ith element from the given list by repositioning the specified
     * list listIterator.
     */
    private static <T> T get(final ListIterator<? extends T> iterator, final int index) {
        T obj = null;
        int pos = iterator.nextIndex();

        if (pos <= index) {
            do {
                obj = iterator.next();
            } while (pos++ < index);
        } else {
            do {
                obj = iterator.previous();
            } while (--pos > index);
        }

        return obj;
    }

    static char kthLargest(final char[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Character> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Character>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] > queue.peek().charValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Character>(k, new Comparator<Character>() {
                @Override
                public int compare(final Character o1, final Character o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] < queue.peek().charValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static byte kthLargest(final byte[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Byte> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Byte>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] > queue.peek().byteValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Byte>(k, new Comparator<Byte>() {
                @Override
                public int compare(final Byte o1, final Byte o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] < queue.peek().byteValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static short kthLargest(final short[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Short> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Short>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] > queue.peek().shortValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Short>(k, new Comparator<Short>() {
                @Override
                public int compare(final Short o1, final Short o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] < queue.peek().shortValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static int kthLargest(final int[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Integer> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Integer>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] > queue.peek().intValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Integer>(k, new Comparator<Integer>() {
                @Override
                public int compare(final Integer o1, final Integer o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] < queue.peek().intValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static long kthLargest(final long[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Long> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Long>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] > queue.peek().longValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Long>(k, new Comparator<Long>() {
                @Override
                public int compare(final Long o1, final Long o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (a[i] < queue.peek().longValue()) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static float kthLargest(final float[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Float> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Float>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (Float.compare(a[i], queue.peek().floatValue()) > 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Float>(k, new Comparator<Float>() {
                @Override
                public int compare(final Float o1, final Float o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (Float.compare(a[i], queue.peek().floatValue()) < 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static double kthLargest(final double[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<Double> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<Double>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (Double.compare(a[i], queue.peek().doubleValue()) > 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<Double>(k, new Comparator<Double>() {
                @Override
                public int compare(final Double o1, final Double o2) {
                    return o2.compareTo(o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (Double.compare(a[i], queue.peek().doubleValue()) < 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static <T extends Comparable<T>> T kthLargest(final T[] a, int k) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a);
        } else if (k == a.length) {
            return N.min(a);
        }

        Queue<T> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<T>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (N.compare(a[i], queue.peek()) > 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<T>(k, new Comparator<T>() {
                @Override
                public int compare(final T o1, final T o2) {
                    return N.compare(o2, o1);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (N.compare(a[i], queue.peek()) < 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();

    }

    static <T> T kthLargest(final T[] a, int k, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a) || k <= 0 || a.length < k) {
            throw new IllegalArgumentException("Array is empty or null, or the input k is less than 1 or bigger than the length of input array. k=" + k);
        }

        final int len = a.length;

        if (k == 1) {
            return N.max(a, cmp);
        } else if (k == a.length) {
            return N.min(a, cmp);
        }

        Queue<T> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<T>(k);

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (N.compare(a[i], queue.peek(), cmp) > 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<T>(k, new Comparator<T>() {
                @Override
                public int compare(final T o1, final T o2) {
                    return N.compare(o2, o1, cmp);
                }
            });

            for (int i = 0; i < len; i++) {
                if (queue.size() < k) {
                    queue.add(a[i]);
                } else {
                    if (N.compare(a[i], queue.peek(), cmp) < 0) {
                        queue.remove();
                        queue.add(a[i]);
                    }
                }
            }
        }

        return queue.remove();
    }

    static <T extends Comparable<T>> T kthLargest(final Collection<T> c, int k) {
        final int len = c.size();

        if (k == 1) {
            return N.max(c);
        } else if (k == len) {
            return N.min(c);
        }

        Queue<T> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<T>(k);

            for (T e : c) {
                if (queue.size() < k) {
                    queue.add(e);
                } else {
                    if (N.compare(e, queue.peek()) > 0) {
                        queue.remove();
                        queue.add(e);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<T>(k, new Comparator<T>() {
                @Override
                public int compare(final T o1, final T o2) {
                    return N.compare(o2, o1);
                }
            });

            for (T e : c) {
                if (queue.size() < k) {
                    queue.add(e);
                } else {
                    if (N.compare(e, queue.peek()) < 0) {
                        queue.remove();
                        queue.add(e);
                    }
                }
            }
        }

        return queue.remove();
    }

    static <T> T kthLargest(final Collection<T> c, int k, final Comparator<? super T> cmp) {
        final int len = c.size();

        if (k == 1) {
            return N.max(c, cmp);
        } else if (k == len) {
            return N.min(c, cmp);
        }

        Queue<T> queue = null;

        if (k <= len / 2) {
            queue = new PriorityQueue<T>(k);

            for (T e : c) {
                if (queue.size() < k) {
                    queue.add(e);
                } else {
                    if (N.compare(e, queue.peek(), cmp) > 0) {
                        queue.remove();
                        queue.add(e);
                    }
                }
            }
        } else {
            k = len - k + 1;

            queue = new PriorityQueue<T>(k, new Comparator<T>() {
                @Override
                public int compare(final T o1, final T o2) {
                    return N.compare(o2, o1, cmp);
                }
            });

            for (T e : c) {
                if (queue.size() < k) {
                    queue.add(e);
                } else {
                    if (N.compare(e, queue.peek(), cmp) < 0) {
                        queue.remove();
                        queue.add(e);
                    }
                }
            }
        }

        return queue.remove();
    }

    //    static double medianOfTwoSortedArrays(final int[] a, final int[] b) {
    //        final int n = a.length;
    //        final int m = b.length;
    //
    //        if (n > m) {
    //            return medianOfTwoSortedArrays(b, a);
    //        }
    //
    //        int k = (n + m - 1) / 2;
    //        int l = 0, r = Math.min(k, n);
    //        while (l < r) {
    //            int mid1 = (l + r) / 2;
    //            int mid2 = k - mid1;
    //
    //            if (a[mid1] < b[mid2]) {
    //                l = mid1 + 1;
    //            } else {
    //                r = mid1;
    //            }
    //        }
    //
    //        int num1 = Math.max(l - 1 >= 0 ? a[l - 1] : Integer.MIN_VALUE, k - l >= 0 ? b[k - l] : Integer.MIN_VALUE);
    //
    //        if ((n + m) % 2 != 0) {
    //            return num1;
    //        }
    //
    //        int num2 = Math.min(l < n ? a[l] : Integer.MAX_VALUE, k - l + 1 < m ? b[k - l + 1] : Integer.MAX_VALUE);
    //
    //        return (num1 + num2) / (double) 2;
    //    }
    //
    //    static int theKthNumberOfTwoSortedArrays(final int[] a, final int[] b, final int k) {
    //        final int n = a.length;
    //        final int m = b.length;
    //
    //        if (n > m) {
    //            return theKthNumberOfTwoSortedArrays(b, a, k);
    //        }
    //
    //        int l = 0, r = Math.min(k, n);
    //        while (l < r) {
    //            int mid1 = (l + r) / 2;
    //            int mid2 = k - mid1;
    //
    //            if (a[mid1] < b[mid2]) {
    //                l = mid1 + 1;
    //            } else {
    //                r = mid1;
    //            }
    //        }
    //
    //        return Math.max(l - 1 >= 0 ? a[l - 1] : Integer.MIN_VALUE, k - l >= 0 ? b[k - l] : Integer.MIN_VALUE);
    //    }
    //
    //    static long theKthNumberOfTwoSortedArrays(final long[] a, final long[] b, final int k) {
    //        final int n = a.length;
    //        final int m = b.length;
    //
    //        if (n > m) {
    //            return theKthNumberOfTwoSortedArrays(b, a, k);
    //        }
    //
    //        int l = 0, r = Math.min(k, n);
    //        while (l < r) {
    //            int mid1 = (l + r) / 2;
    //            int mid2 = k - mid1;
    //
    //            if (a[mid1] < b[mid2]) {
    //                l = mid1 + 1;
    //            } else {
    //                r = mid1;
    //            }
    //        }
    //
    //        return Math.max(l - 1 >= 0 ? a[l - 1] : Integer.MIN_VALUE, k - l >= 0 ? b[k - l] : Integer.MIN_VALUE);
    //    }
    //
    //    static float theKthNumberOfTwoSortedArrays(final float[] a, final float[] b, final int k) {
    //        final int n = a.length;
    //        final int m = b.length;
    //
    //        if (n > m) {
    //            return theKthNumberOfTwoSortedArrays(b, a, k);
    //        }
    //
    //        int l = 0, r = Math.min(k, n);
    //        while (l < r) {
    //            int mid1 = (l + r) / 2;
    //            int mid2 = k - mid1;
    //
    //            if (a[mid1] < b[mid2]) {
    //                l = mid1 + 1;
    //            } else {
    //                r = mid1;
    //            }
    //        }
    //
    //        return Math.max(l - 1 >= 0 ? a[l - 1] : Integer.MIN_VALUE, k - l >= 0 ? b[k - l] : Integer.MIN_VALUE);
    //    }
    //
    //    static double theKthNumberOfTwoSortedArrays(final double[] a, final double[] b, final int k) {
    //        final int n = a.length;
    //        final int m = b.length;
    //
    //        if (n > m) {
    //            return theKthNumberOfTwoSortedArrays(b, a, k);
    //        }
    //
    //        int l = 0, r = Math.min(k, n);
    //        while (l < r) {
    //            int mid1 = (l + r) / 2;
    //            int mid2 = k - mid1;
    //
    //            if (a[mid1] < b[mid2]) {
    //                l = mid1 + 1;
    //            } else {
    //                r = mid1;
    //            }
    //        }
    //
    //        return Math.max(l - 1 >= 0 ? a[l - 1] : Integer.MIN_VALUE, k - l >= 0 ? b[k - l] : Integer.MIN_VALUE);
    //    }
    //
    //    static <E> Collection<List<E>> permutationsOf(final Collection<E> elements) {
    //        return Collections2.permutations(elements);
    //    }
    //
    //    static <E extends Comparable<? super E>> Collection<List<E>> orderedPermutationsOf(final Collection<E> elements) {
    //        return Collections2.orderedPermutations(elements);
    //    }
    //
    //    static <E> Collection<List<E>> orderedPermutationsOf(final Collection<E> elements, final Comparator<? super E> comparator) {
    //        return Collections2.orderedPermutations(elements, comparator);
    //    }
    //
    //    private static final String[] tens = { "", "Ten", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety" };
    //    private static final String[] lessThan20 = { "", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve",
    //            "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen" };
    //    private static final String[] thousands = { "", "Thousand", "Million", "Billion" };
    //
    //    static String numberToWords(int num) {
    //        // https://leetcode.com/discuss/55462/my-clean-java-solution-very-easy-to-understand
    //        if (num == 0) {
    //            return "Zero";
    //        }
    //        int i = 0;
    //        String words = "";
    //
    //        while (num > 0) {
    //            if (num % 1000 != 0) {
    //                words = numberToWordsHelper(num % 1000) + thousands[i] + " " + words;
    //            }
    //            num /= 1000;
    //            i++;
    //        }
    //
    //        return words.trim();
    //    }
    //
    //    private static String numberToWordsHelper(int num) {
    //        if (num == 0)
    //            return "";
    //        else if (num < 20)
    //            return lessThan20[num] + " ";
    //        else if (num < 100)
    //            return tens[num / 10] + " " + numberToWordsHelper(num % 10);
    //        else
    //            return lessThan20[num / 100] + " Hundred " + numberToWordsHelper(num % 100);
    //    }
    //
    //    private static final String[] t = { "", "", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz" };
    //
    //    static List<String> letterCombinationsOfPhoneNum(final String digits) {
    //        List<String> res = new ArrayList<String>();
    //
    //        if (digits == null || digits.length() == 0) {
    //            return res;
    //        }
    //
    //        res.add("");
    //        for (int i = 0, len = digits.length(); i < len; i++) {
    //            String str = t[digits.charAt(i) - '0'];
    //            if (str.length() == 0) {
    //                continue;
    //            }
    //            int size = res.size();
    //            for (int j = 0; j < size; j++) {
    //                for (int k = 0; k < str.length(); k++) {
    //                    res.add(res.get(j) + str.charAt(k));
    //                }
    //            }
    //
    //            res = res.subList(size, res.size());
    //        }
    //        return res;
    //    }
    //
    //    static boolean isPowerOfTwo(final int n) {
    //        return (n > 0 && (n & (n - 1)) == 0);
    //    }
    //
    //    static int reverse(int x) {
    //        long res = 0;
    //        while (x != 0) {
    //            res = res * 10 + x % 10;
    //            x = x / 10;
    //        }
    //
    //        return (res > Integer.MAX_VALUE || res < Integer.MIN_VALUE) ? 0 : (int) res;
    //    }
    //
    //    static int reverse(long x) {
    //        long res = 0;
    //        while (x != 0) {
    //            res = res * 10 + x % 10;
    //            x = x / 10;
    //        }
    //
    //        return (res > Long.MAX_VALUE || res < Long.MIN_VALUE) ? 0 : (int) res;
    //    }
    //
    //    static boolean isPalindromeNumber(final int x) {
    //        if (x < 0) {
    //            return false;
    //        }
    //        if (x < 10) {
    //            return true;
    //        }
    //        int y = x;
    //        long z = 0;
    //        while (y != 0) {
    //            z = z * 10 + y % 10;
    //            y = y / 10;
    //        }
    //        return z == x;
    //    }
    //
    //    /**
    //     * Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
    //     * The brackets must close in the correct order, "()" and "()[]{}" are all valid but "(]" and "([)]" are not.
    //     * @param str
    //     * @return
    //     */
    //    static boolean isValidParentheses(final String str) {
    //        if (str == null || str.length() == 0) {
    //            return true;
    //        }
    //
    //        final Map<Character, Character> m = new HashMap<Character, Character>();
    //        m.put('(', ')');
    //        m.put('{', '}');
    //        m.put('[', ']');
    //
    //        final Stack<Character> stack = new Stack<>();
    //        for (int i = 0, len = str.length(); i < len; i++) {
    //            char ch = str.charAt(i);
    //            Character p = m.get(ch);
    //
    //            if (p == null) {
    //                if (stack.size() == 0 || m.get(stack.pop()) != ch) {
    //                    return false;
    //                }
    //            } else {
    //                stack.push(ch);
    //            }
    //        }
    //
    //        return stack.size() == 0;
    //    }
    //
    //    static List<String> generateParenthesis(final int n) {
    //        final List<String> res = new ArrayList<>();
    //        generate(n, 0, 0, res, "");
    //        return res;
    //    }
    //
    //    private static void generate(int n, int open, int close, List<String> result, String current) {
    //        if (close == n && open == n) {
    //            result.add(current);
    //        } else {
    //            if (open < n) {
    //                generate(n, open + 1, close, result, current + "(");
    //            }
    //
    //            if (close < open) {
    //                generate(n, open, close + 1, result, current + ")");
    //            }
    //        }
    //    }
    //
    //    static void rotate90Degree(int[][] matrix) {
    //        int n = matrix.length;
    //
    //        for (int i = 0; i < n / 2; ++i) {
    //            for (int j = i; j < n - 1 - i; ++j) {
    //                int tmp = matrix[i][j];
    //                matrix[i][j] = matrix[n - j - 1][i];
    //                matrix[n - j - 1][i] = matrix[n - i - 1][n - j - 1];
    //                matrix[n - i - 1][n - j - 1] = matrix[j][n - i - 1];
    //                matrix[j][n - i - 1] = tmp;
    //            }
    //        }
    //    }
}
