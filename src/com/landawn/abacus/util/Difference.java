package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.landawn.abacus.util.Pair.Pair0;

@SuppressWarnings("hiding")
public class Difference<L, R> {
    final L common;
    final L leftOnly;
    final R rightOnly;

    Difference(L common, L leftOnly, R rightOnly) {
        this.common = common;
        this.leftOnly = leftOnly;
        this.rightOnly = rightOnly;
    }

    public static Difference<List<Boolean>, List<Boolean>> of(boolean[] a, boolean[] b) {
        return of(BooleanList.of(a), BooleanList.of(b));
    }

    public static Difference<List<Character>, List<Character>> of(char[] a, char[] b) {
        return of(CharList.of(a), CharList.of(b));
    }

    public static Difference<List<Byte>, List<Byte>> of(byte[] a, byte[] b) {
        return of(ByteList.of(a), ByteList.of(b));
    }

    public static Difference<List<Short>, List<Short>> of(short[] a, short[] b) {
        return of(ShortList.of(a), ShortList.of(b));
    }

    public static Difference<List<Integer>, List<Integer>> of(int[] a, int[] b) {
        return of(IntList.of(a), IntList.of(b));
    }

    public static Difference<List<Long>, List<Long>> of(long[] a, long[] b) {
        return of(LongList.of(a), LongList.of(b));
    }

    public static Difference<List<Float>, List<Float>> of(float[] a, float[] b) {
        return of(FloatList.of(a), FloatList.of(b));
    }

    public static Difference<List<Double>, List<Double>> of(double[] a, double[] b) {
        return of(DoubleList.of(a), DoubleList.of(b));
    }

    public static <T1, T2, L extends List<? super T1>, R extends List<? super T2>> Difference<L, R> of(T1[] a, T2[] b) {
        return of(Arrays.asList(a), Arrays.asList(b));
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     */
    public static <T1, T2, L extends List<? super T1>, R extends List<? super T2>> Difference<L, R> of(Collection<T1> a, Collection<T2> b) {
        List<T1> common = new ArrayList<>();
        List<T1> leftOnly = new ArrayList<>();
        List<T2> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly.addAll(b);
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly.addAll(a);
        } else {
            final Multiset<T2> bOccurrences = Multiset.of(b);

            for (T1 e : a) {
                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (T2 e : b) {
                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>((L) common, (L) leftOnly, (R) rightOnly);
    }

    public static Difference<List<Boolean>, List<Boolean>> of(BooleanList a, BooleanList b) {
        List<Boolean> common = new ArrayList<>();
        List<Boolean> leftOnly = new ArrayList<>();
        List<Boolean> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Boolean> bOccurrences = b.toMultiset();

            boolean e = false;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Character>, List<Character>> of(CharList a, CharList b) {
        List<Character> common = new ArrayList<>();
        List<Character> leftOnly = new ArrayList<>();
        List<Character> rightOnly = new ArrayList<>();
    
        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Character> bOccurrences = b.toMultiset();
    
            char e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);
    
                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }
    
            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);
    
                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }
    
                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }
    
        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Byte>, List<Byte>> of(ByteList a, ByteList b) {
        List<Byte> common = new ArrayList<>();
        List<Byte> leftOnly = new ArrayList<>();
        List<Byte> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Byte> bOccurrences = b.toMultiset();

            byte e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Short>, List<Short>> of(ShortList a, ShortList b) {
        List<Short> common = new ArrayList<>();
        List<Short> leftOnly = new ArrayList<>();
        List<Short> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Short> bOccurrences = b.toMultiset();

            short e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Integer>, List<Integer>> of(IntList a, IntList b) {
        List<Integer> common = new ArrayList<>();
        List<Integer> leftOnly = new ArrayList<>();
        List<Integer> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Integer> bOccurrences = b.toMultiset();

            int e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Long>, List<Long>> of(LongList a, LongList b) {
        List<Long> common = new ArrayList<>();
        List<Long> leftOnly = new ArrayList<>();
        List<Long> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Long> bOccurrences = b.toMultiset();

            long e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Float>, List<Float>> of(FloatList a, FloatList b) {
        List<Float> common = new ArrayList<>();
        List<Float> leftOnly = new ArrayList<>();
        List<Float> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Float> bOccurrences = b.toMultiset();

            float e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public static Difference<List<Double>, List<Double>> of(DoubleList a, DoubleList b) {
        List<Double> common = new ArrayList<>();
        List<Double> leftOnly = new ArrayList<>();
        List<Double> rightOnly = new ArrayList<>();

        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                // Do nothing. All empty.
            } else {
                rightOnly = b.toList();
            }
        } else if (N.isNullOrEmpty(b)) {
            leftOnly = a.toList();
        } else {
            final Multiset<Double> bOccurrences = b.toMultiset();

            double e = 0;
            for (int i = 0, len = a.size(); i < len; i++) {
                e = a.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    common.add(e);
                } else {
                    leftOnly.add(e);
                }
            }

            for (int i = 0, len = b.size(); i < len; i++) {
                e = b.get(i);

                if (bOccurrences.getAndRemove(e) > 0) {
                    rightOnly.add(e);
                }

                if (bOccurrences.isEmpty()) {
                    break;
                }
            }
        }

        return new Difference<>(common, leftOnly, rightOnly);
    }

    public L inCommon() {
        return common;
    }

    public L onLeftOnly() {
        return leftOnly;
    }

    public R onRightOnly() {
        return rightOnly;
    }

    @SuppressWarnings("rawtypes")
    public boolean areEqual() {
        return (leftOnly instanceof Map && (((Map) leftOnly).isEmpty() && ((Map) rightOnly).isEmpty()))
                || (leftOnly instanceof Collection && (((Collection) leftOnly).isEmpty() && ((Collection) rightOnly).isEmpty()));
    }

    @Override
    public String toString() {
        return "{inCommon=" + common + ", onLeftOnly=" + leftOnly + ", onRightOnly=" + rightOnly + "}";
    }

    public static final class MapDifference<L, R, D> extends Difference<L, R> {
        private final D diffValues;

        MapDifference(L common, L leftOnly, R rightOnly, D diff) {
            super(common, leftOnly, rightOnly);
            this.diffValues = diff;
        }

        /**
         * 
         * @param map1
         * @param map2
         * @return
         */
        public static <K1, V1, K2, V2, L extends Map<? super K1, ? super V1>, R extends Map<? super K2, ? super V2>, D extends Map<?, Pair0<V1, V2>>> MapDifference<L, R, D> of(
                Map<K1, V1> map1, Map<K2, V2> map2) {
            final L common = (L) new LinkedHashMap<>();
            final L leftOnly = (L) new LinkedHashMap<>();
            final R rightOnly = (R) new LinkedHashMap<>();
            final Map<Object, Pair0<V1, V2>> diff = new LinkedHashMap<>();

            if (N.isNullOrEmpty(map1)) {
                if (N.isNullOrEmpty(map2)) {
                    // Do nothing. All empty.
                } else {
                    rightOnly.putAll(map2);
                }
            } else if (N.isNullOrEmpty(map2)) {
                leftOnly.putAll(map1);
            } else {
                V2 val2 = null;
                for (Entry<K1, V1> entry1 : map1.entrySet()) {
                    val2 = map2.get(entry1.getKey());

                    if (val2 == null) {
                        if (map2.containsKey(entry1.getKey())) {
                            if (entry1.getValue() == null) {
                                common.put(entry1.getKey(), entry1.getValue());
                            } else {
                                diff.put(entry1.getKey(), Pair0.of(entry1.getValue(), val2));
                            }
                        } else {
                            leftOnly.put(entry1.getKey(), entry1.getValue());
                        }
                    } else if (N.equals(entry1.getValue(), val2)) {
                        common.put(entry1.getKey(), entry1.getValue());
                    } else {
                        diff.put(entry1.getKey(), Pair0.of(entry1.getValue(), val2));
                    }
                }

                for (Entry<K2, V2> entry2 : map2.entrySet()) {
                    if (common.containsKey(entry2.getKey()) || diff.containsKey(entry2.getKey())) {
                        continue;
                    }

                    rightOnly.put(entry2.getKey(), entry2.getValue());
                }
            }

            return new MapDifference<L, R, D>(common, leftOnly, rightOnly, (D) diff);
        }

        /**
         * 
         * @param entity1
         * @param entity2
         * @return
         */
        public static MapDifference<Map<String, Object>, Map<String, Object>, Map<String, Pair0<Object, Object>>> of(Object entity1, Object entity2) {
            if (N.isEntity(entity1.getClass()) == false || N.isEntity(entity2.getClass()) == false) {
                throw new IllegalArgumentException(
                        entity1.getClass().getCanonicalName() + " or " + entity2.getClass().getCanonicalName() + " is not an entity class");
            }

            return of(N.entity2Map(entity1), N.entity2Map(entity2));
        }

        /**
         * Returns the entries that appear in both maps, but with different values.
         * @return
         */
        public D withDifferentValues() {
            return diffValues;
        }

        @Override
        @SuppressWarnings("rawtypes")
        public boolean areEqual() {
            return super.areEqual() && ((Map) diffValues).isEmpty();
        }

        @Override
        public String toString() {
            return "{inCommon=" + common + ", onLeftOnly=" + leftOnly + ", onRightOnly=" + rightOnly + ", withDifferentValues=" + diffValues + "}";
        }
    }
}
