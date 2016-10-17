package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.landawn.abacus.util.Pair.Pair0;

@SuppressWarnings("hiding")
public class Difference<L, R, D> {
    private final L common;
    private final L leftOnly;
    private final R rightOnly;
    private final D diff;

    Difference(L common, L leftOnly, R rightOnly, D diff) {
        this.common = common;
        this.leftOnly = leftOnly;
        this.rightOnly = rightOnly;
        this.diff = diff;
    }

    /**
     * 
     * @param c1
     * @param c2
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static <E1, E2, L extends List<? super E1>, R extends List<? super E2>, D extends List> Difference<L, R, D> of(Collection<E1> c1,
            Collection<E2> c2) {
        List<E1> common = new ArrayList<>();
        List<E1> leftOnly = new ArrayList<>();
        List<E2> rightOnly = new ArrayList<>();
        List diff = new ArrayList<>();

        if (N.isNullOrEmpty(c1)) {
            if (N.isNullOrEmpty(c2)) {
                // Do nothing. All empty.
            } else {
                rightOnly.addAll(c2);
                diff.addAll(c2);
            }
        } else if (N.isNullOrEmpty(c2)) {
            leftOnly.addAll(c1);
            diff.addAll(c1);
        } else {
            common = N.intersect(c1, c2);
            leftOnly = N.except(c1, c2);
            rightOnly = N.except(c2, c1);
            diff.addAll(leftOnly);
            diff.addAll(rightOnly);
        }

        return new Difference(common, leftOnly, rightOnly, diff);
    }

    /**
     * 
     * @param map1
     * @param map2
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static <K1, V1, K2, V2, L extends Map<? super K1, ? super V1>, R extends Map<? super K2, ? super V2>, D extends Map<?, Pair0<V1, V2>>> Difference<L, R, D> of(
            Map<K1, V1> map1, Map<K2, V2> map2) {
        final Map<K1, V1> common = new LinkedHashMap<>();
        final Map<K1, V1> leftOnly = new LinkedHashMap<>();
        final Map<K2, V2> rightOnly = new LinkedHashMap<>();
        final Map diff = new LinkedHashMap<>();

        if (N.isNullOrEmpty(map1)) {
            if (N.isNullOrEmpty(map2)) {
                // Do nothing. All empty.
            } else {
                rightOnly.putAll(map2);
                diff.putAll(map2);
            }
        } else if (N.isNullOrEmpty(map2)) {
            leftOnly.putAll(map1);
            diff.putAll(map1);
        } else {
            Object val2 = null;
            for (Entry<K1, V1> entry1 : map1.entrySet()) {
                val2 = map2.get(entry1.getKey());

                if (val2 == null) {
                    if (map2.containsKey(entry1.getKey()) && entry1.getValue() == null) {
                        common.put(entry1.getKey(), entry1.getValue());
                    } else {
                        leftOnly.put(entry1.getKey(), entry1.getValue());
                        diff.put(entry1.getValue(), Pair0.of(entry1.getValue(), val2));
                    }
                } else if (N.equals(entry1.getValue(), val2)) {
                    common.put(entry1.getKey(), entry1.getValue());
                } else {
                    leftOnly.put(entry1.getKey(), entry1.getValue());
                    diff.put(entry1.getValue(), Pair0.of(entry1.getValue(), val2));
                }
            }

            for (Entry<K2, V2> entry2 : map2.entrySet()) {
                if (common.containsKey(entry2.getKey())) {
                    continue;
                } else {
                    rightOnly.put(entry2.getKey(), entry2.getValue());
                    diff.put(entry2.getKey(), Pair0.of(null, entry2.getValue()));
                }
            }
        }

        return new Difference(common, leftOnly, rightOnly, diff);
    }

    /**
     * 
     * @param entity1
     * @param entity2
     * @return
     */
    public static Difference<Map<String, Object>, Map<String, Object>, Map<String, Pair0<Object, Object>>> of(Object entity1, Object entity2) {
        return of(N.entity2Map(entity1), N.entity2Map(entity2));
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

    public D get() {
        return diff;
    }

    @SuppressWarnings("rawtypes")
    public boolean areEqual() {
        return (diff instanceof Map && ((Map) diff).isEmpty()) || (diff instanceof Collection && ((Collection) diff).isEmpty());
    }
}
