/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.landawn.abacus.util;

/**
 * <p>
 * Note: it's copied from Apache Commons Lang developed at The Apache Software Foundation (http://www.apache.org/), or
 * under the Apache License 2.0. The methods copied from other products/frameworks may be modified in this class.
 * </p>
 * 
 * A mutable <code>short</code> wrapper.
 * <p>
 * Note that as MutableShort does not extend Short, it is not treated by String.format as a Short parameter. 
 * 
 * @see Short
 * @since 2.1
 * @version $Id: MutableShort.java 1669791 2015-03-28 15:22:59Z britter $
 */
public final class MutableShort extends Number implements Comparable<MutableShort>, Mutable<Number> {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = -2135791679L;

    /** The mutable value. */
    private short value;

    /**
     * Constructs a new MutableShort with the default value of zero.
     */
    public MutableShort() {
        super();
    }

    /**
     * Constructs a new MutableShort with the specified value.
     * 
     * @param value  the initial value to store
     */
    public MutableShort(final short value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableShort with the specified value.
     * 
     * @param value  the initial value to store, not null
     * @throws NullPointerException if the object is null
     */
    public MutableShort(final Number value) {
        super();
        this.value = value.shortValue();
    }

    /**
     * Constructs a new MutableShort parsing the given string.
     * 
     * @param value  the string to parse, not null
     * @throws NumberFormatException if the string cannot be parsed into a short
     * @since 2.5
     */
    public MutableShort(final String value) throws NumberFormatException {
        super();
        this.value = Short.parseShort(value);
    }

    public static MutableShort of(final short value) {
        return new MutableShort(value);
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the value as a Short instance.
     * 
     * @return the value as a Short, never null
     */
    @Override
    public Short getValue() {
        return Short.valueOf(this.value);
    }

    /**
     * Sets the value from any Number instance.
     * 
     * @param value  the value to set, not null
     * @throws NullPointerException if the object is null
     */
    @Override
    public void setValue(final Number value) {
        this.value = value.shortValue();
    }

    /**
     * Sets the value.
     * 
     * @param value  the value to set
     */
    public void setValue(final short value) {
        this.value = value;
    }

    //-----------------------------------------------------------------------
    /**
     * Increments the value.
     *
     * @since Commons Lang 2.2
     */
    public void increment() {
        value++;
    }

    /**
     * Decrements the value.
     *
     * @since Commons Lang 2.2
     */
    public void decrement() {
        value--;
    }

    //-----------------------------------------------------------------------
    /**
     * Adds a value to the value of this instance.
     * 
     * @param operand  the value to add, not null
     * @since Commons Lang 2.2
     */
    public void add(final short operand) {
        this.value += operand;
    }

    /**
     * Adds a value to the value of this instance.
     * 
     * @param operand  the value to add, not null
     * @throws NullPointerException if the object is null
     * @since Commons Lang 2.2
     */
    public void add(final Number operand) {
        this.value += operand.shortValue();
    }

    /**
     * Subtracts a value from the value of this instance.
     * 
     * @param operand  the value to subtract, not null
     * @since Commons Lang 2.2
     */
    public void subtract(final short operand) {
        this.value -= operand;
    }

    /**
     * Subtracts a value from the value of this instance.
     * 
     * @param operand  the value to subtract, not null
     * @throws NullPointerException if the object is null
     * @since Commons Lang 2.2
     */
    public void subtract(final Number operand) {
        this.value -= operand.shortValue();
    }

    /**
     * Increments by one the current value.
     *
     * @return the previous value
     */
    public final short getAndIncrement() {
        return value++;
    }

    /**
     * Decrements by one the current value.
     *
     * @return the previous value
     */
    public final short getAndDecrement() {
        return value--;
    }

    /**
     * Adds the given value to the current value.
     *
     * @param delta the value to add
     * @return the previous value
     */
    public final short getAndAdd(final short delta) {
        final short prev = value;
        value += delta;
        return prev;
    }

    /**
     * Increments by one the current value.
     *
     * @return the updated value
     */
    public final short incrementAndGet() {
        return ++value;
    }

    /**
     * Decrements by one the current value.
     *
     * @return the updated value
     */
    public final short decrementAndGet() {
        return --value;
    }

    /**
     * Adds the given value to the current value.
     *
     * @param delta the value to add
     * @return the updated value
     */
    public final short addAndGet(final short delta) {
        return value += delta;
    }

    //-----------------------------------------------------------------------
    // byteValue relies on Number implementation
    /**
     * Returns the value of this MutableShort as a short.
     *
     * @return the numeric value represented by this object after conversion to type short.
     */
    @Override
    public short shortValue() {
        return value;
    }

    /**
     * Returns the value of this MutableShort as an int.
     *
     * @return the numeric value represented by this object after conversion to type int.
     */
    @Override
    public int intValue() {
        return value;
    }

    /**
     * Returns the value of this MutableShort as a long.
     *
     * @return the numeric value represented by this object after conversion to type long.
     */
    @Override
    public long longValue() {
        return value;
    }

    /**
     * Returns the value of this MutableShort as a float.
     *
     * @return the numeric value represented by this object after conversion to type float.
     */
    @Override
    public float floatValue() {
        return value;
    }

    /**
     * Returns the value of this MutableShort as a double.
     *
     * @return the numeric value represented by this object after conversion to type double.
     */
    @Override
    public double doubleValue() {
        return value;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets this mutable as an instance of Short.
     *
     * @return a Short instance containing the value from this mutable, never null
     */
    public Short toShort() {
        return Short.valueOf(shortValue());
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this mutable to another in ascending order.
     * 
     * @param other  the other mutable to compare to, not null
     * @return negative if this is less, zero if equal, positive if greater
     */
    @Override
    public int compareTo(final MutableShort other) {
        return (this.value > other.value) ? 1 : ((this.value == other.value) ? 0 : -1);
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this object to the specified object. The result is <code>true</code> if and only if the argument
     * is not <code>null</code> and is a <code>MutableShort</code> object that contains the same <code>short</code>
     * value as this object.
     * 
     * @param obj  the object to compare with, null returns false
     * @return <code>true</code> if the objects are the same; <code>false</code> otherwise.
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj instanceof MutableShort) {
            return value == ((MutableShort) obj).shortValue();
        }
        return false;
    }

    /**
     * Returns a suitable hash code for this mutable.
     * 
     * @return a suitable hash code
     */
    @Override
    public int hashCode() {
        return value;
    }

    //-----------------------------------------------------------------------
    /**
     * Returns the String value of this mutable.
     * 
     * @return the mutable value as a string
     */
    @Override
    public String toString() {
        return String.valueOf(value);
    }

}
