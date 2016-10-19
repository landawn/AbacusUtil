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

    public static <T1, E2, L extends List<? super T1>, R extends List<? super E2>> Difference<L, R> of(T1[] a, E2[] b) {
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
            common = N.intersect(a, b);
            leftOnly = N.except(a, b);
            rightOnly = N.except(b, a);
        }

        return new Difference<>((L) common, (L) leftOnly, (R) rightOnly);
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
