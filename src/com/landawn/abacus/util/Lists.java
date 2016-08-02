package com.landawn.abacus.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.EntityId;
import com.landawn.abacus.condition.Condition;
import com.landawn.abacus.type.Type;

public final class Lists {
    private Lists() {
        // singleton
    }

    public static BooleanList of(final boolean... a) {
        return BooleanList.of(a);
    }

    public static CharList of(final char... a) {
        return CharList.of(a);
    }

    public static ByteList of(final byte... a) {
        return ByteList.of(a);
    }

    public static ShortList of(final short... a) {
        return ShortList.of(a);
    }

    public static IntList of(final int... a) {
        return IntList.of(a);
    }

    public static LongList of(final long... a) {
        return LongList.of(a);
    }

    public static FloatList of(final float... a) {
        return FloatList.of(a);
    }

    public static DoubleList of(final double... a) {
        return DoubleList.of(a);
    }

    public static ObjectList<BigInteger> of(final BigInteger... a) {
        return BigIntegerList.of(a);
    }

    public static ObjectList<BigDecimal> of(final BigDecimal... a) {
        return BigDecimalList.of(a);
    }

    public static ObjectList<String> of(final String... a) {
        return StringList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Enum<T>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends java.util.Date> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Calendar> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Runnable> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Callable<?>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static ObjectList<Class> of(final Class... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends EntityId> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends DirtyMarker> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Condition> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Type<?>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends List<?>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Set<?>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Queue<?>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }

    /**
     * Returns the list of specified elements
     *
     * @param a
     * @return
     */
    public static <T extends Map<?, ?>> ObjectList<T> of(final T... a) {
        return ObjectList.of(a);
    }
}
