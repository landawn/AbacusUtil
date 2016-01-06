/*
 * Copyright (c) 2012, 2013, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

/*
 * This file is available under and governed by the GNU General Public
 * License version 2 only, as published by the Free Software Foundation.
 * However, the following notice accompanied the original version of this
 * file:
 *
 * Copyright (c) 2007-2012, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.landawn.abacus.util;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;

/**
 * <p>
 * Note: it's referred to the implementation in jdk 8 with milliseconds supported
 *
 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/time/Duration.html">https://docs.oracle.com/javase/8/docs/api/java/time/Duration.html</a>
 */
public final class Duration implements Comparable<Duration>, Serializable {
    private static final long serialVersionUID = 1289592302677535830L;

    /**
     * Constant for a duration of zero.
     */
    public static final Duration ZERO = new Duration(0);

    /**
     * Milliseconds per second.
     */
    static final long MILLIS_PER_SECOND = 1000L;

    /**
     * Milliseconds per hour.
     */
    static final long MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60;

    /**
     * Milliseconds per hour.
     */
    static final long MILLIS_PER_HOUR = MILLIS_PER_MINUTE * 60;

    /**
     * Milliseconds per day.
     */
    static final long MILLIS_PER_DAY = MILLIS_PER_HOUR * 24;

    /**
     * The number of milliseconds in the duration.
     */
    private final long milliseconds;

    //-----------------------------------------------------------------------
    /**
     * Obtains a {@code Duration} representing a number of standard 24 hour days.
     * <p>
     * The milliseconds are calculated based on the standard definition of a day, where each day is 86400 * 1000L milliseconds which
     * implies a 24 hour day.
     *
     * @param days
     *            the number of days, positive or negative
     * @return a {@code Duration}, not null
     * @throws ArithmeticException
     *             if the input days exceeds the capacity of {@code Duration}
     */
    public static Duration ofDays(long days) {
        return create(multiplyExact(days, MILLIS_PER_DAY));
    }

    /**
     * Obtains a {@code Duration} representing a number of standard hours.
     * <p>
     * The milliseconds are calculated based on the standard definition of an hour, where each hour is 3600 * 1000L milliseconds.
     *
     * @param hours
     *            the number of hours, positive or negative
     * @return a {@code Duration}, not null
     * @throws ArithmeticException
     *             if the input hours exceeds the capacity of {@code Duration}
     */
    public static Duration ofHours(long hours) {
        return create(multiplyExact(hours, MILLIS_PER_HOUR));
    }

    /**
     * Obtains a {@code Duration} representing a number of standard minutes.
     * <p>
     * The milliseconds are calculated based on the standard definition of a minute, where each minute is 60 * 1000L milliseconds.
     *
     * @param minutes
     *            the number of minutes, positive or negative
     * @return a {@code Duration}, not null
     * @throws ArithmeticException
     *             if the input minutes exceeds the capacity of {@code Duration}
     */
    public static Duration ofMinutes(long minutes) {
        return create(multiplyExact(minutes, MILLIS_PER_MINUTE));
    }

    //-----------------------------------------------------------------------
    /**
     * Obtains a {@code Duration} representing a number of standard seconds.
     * <p>
     * The milliseconds are calculated based on the standard definition of a minute, where each second is 1000L milliseconds.
     *
     * @param seconds
     *            the number of seconds, positive or negative
     * @return a {@code Duration}, not null
     */
    public static Duration ofSeconds(long seconds) {
        return create(multiplyExact(seconds, MILLIS_PER_SECOND));
    }

    //-----------------------------------------------------------------------
    /**
     * Obtains a {@code Duration} representing a number of milliseconds.
     *
     * @param millis
     *            the number of milliseconds, positive or negative
     * @return a {@code Duration}, not null
     */
    public static Duration ofMillis(long millis) {
        return create(millis);
    }

    private static Duration create(long milliseconds) {
        if (milliseconds == 0) {
            return ZERO;
        }

        return new Duration(milliseconds);
    }

    private Duration(long milliseconds) {
        this.milliseconds = milliseconds;
    }

    //-----------------------------------------------------------------------
    /**
     * Checks if this duration is zero length.
     * <p>
     * A {@code Duration} represents a directed distance between two points on the time-line and can therefore be
     * positive, zero or negative. This method checks whether the length is zero.
     *
     * @return true if this duration has a total length equal to zero
     */
    public boolean isZero() {
        return milliseconds == 0;
    }

    /**
     * Checks if this duration is negative, excluding zero.
     * <p>
     * A {@code Duration} represents a directed distance between two points on the time-line and can therefore be
     * positive, zero or negative. This method checks whether the length is less than zero.
     *
     * @return true if this duration has a total length less than zero
     */
    public boolean isNegative() {
        return milliseconds < 0;
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a copy of this duration with the specified duration added.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param duration
     *            the duration to add, positive or negative, not null
     * @return a {@code Duration} based on this duration with the specified duration added, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration plus(Duration duration) {
        return plus(duration.milliseconds);
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a copy of this duration with the specified duration in standard 24 hour days added.
     * <p>
     * The number of days is multiplied by 86400 * 1000L to obtain the number of milliseconds to add. This is based on the standard
     * definition of a day as 24 hours.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param daysToAdd
     *            the days to add, positive or negative
     * @return a {@code Duration} based on this duration with the specified days added, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration plusDays(long daysToAdd) {
        return plus(multiplyExact(daysToAdd, MILLIS_PER_DAY));
    }

    /**
     * Returns a copy of this duration with the specified duration in hours added.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param hoursToAdd
     *            the hours to add, positive or negative
     * @return a {@code Duration} based on this duration with the specified hours added, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration plusHours(long hoursToAdd) {
        return plus(multiplyExact(hoursToAdd, MILLIS_PER_HOUR));
    }

    /**
     * Returns a copy of this duration with the specified duration in minutes added.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param minutesToAdd
     *            the minutes to add, positive or negative
     * @return a {@code Duration} based on this duration with the specified minutes added, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration plusMinutes(long minutesToAdd) {
        return plus(multiplyExact(minutesToAdd, MILLIS_PER_MINUTE));
    }

    /**
     * Returns a copy of this duration with the specified duration in seconds added.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param secondsToAdd
     *            the seconds to add, positive or negative
     * @return a {@code Duration} based on this duration with the specified seconds added, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration plusSeconds(long secondsToAdd) {
        return plus(multiplyExact(secondsToAdd, MILLIS_PER_SECOND));
    }

    /**
     * Returns a copy of this duration with the specified duration in milliseconds added.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param millisToAdd
     *            the milliseconds to add, positive or negative
     * @return a {@code Duration} based on this duration with the specified milliseconds added, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration plusMillis(long millisToAdd) {
        return plus(millisToAdd);
    }

    private Duration plus(long millisecondsToAdd) {
        if (millisecondsToAdd == 0) {
            return this;
        }

        long epochMillis = addExact(milliseconds, millisecondsToAdd);

        return ofMillis(epochMillis);
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a copy of this duration with the specified duration subtracted.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param duration
     *            the duration to subtract, positive or negative, not null
     * @return a {@code Duration} based on this duration with the specified duration subtracted, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration minus(Duration duration) {
        return (duration.milliseconds == Long.MIN_VALUE ? plus(Long.MAX_VALUE).plus(1) : plus(-duration.milliseconds));
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a copy of this duration with the specified duration in standard 24 hour days subtracted.
     * <p>
     * The number of days is multiplied by 86400 * 1000L to obtain the number of milliseconds to subtract. This is based on the
     * standard definition of a day as 24 hours.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param daysToSubtract
     *            the days to subtract, positive or negative
     * @return a {@code Duration} based on this duration with the specified days subtracted, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration minusDays(long daysToSubtract) {
        return (daysToSubtract == Long.MIN_VALUE ? plusDays(Long.MAX_VALUE).plusDays(1) : plusDays(-daysToSubtract));
    }

    /**
     * Returns a copy of this duration with the specified duration in hours subtracted.
     *
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param hoursToSubtract
     *            the hours to subtract, positive or negative
     * @return a {@code Duration} based on this duration with the specified hours subtracted, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration minusHours(long hoursToSubtract) {
        return (hoursToSubtract == Long.MIN_VALUE ? plusHours(Long.MAX_VALUE).plusHours(1) : plusHours(-hoursToSubtract));
    }

    /**
     * Returns a copy of this duration with the specified duration in minutes subtracted.
     *
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param minutesToSubtract
     *            the minutes to subtract, positive or negative
     * @return a {@code Duration} based on this duration with the specified minutes subtracted, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration minusMinutes(long minutesToSubtract) {
        return (minutesToSubtract == Long.MIN_VALUE ? plusMinutes(Long.MAX_VALUE).plusMinutes(1) : plusMinutes(-minutesToSubtract));
    }

    /**
     * Returns a copy of this duration with the specified duration in seconds subtracted.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param secondsToSubtract
     *            the seconds to subtract, positive or negative
     * @return a {@code Duration} based on this duration with the specified seconds subtracted, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration minusSeconds(long secondsToSubtract) {
        return (secondsToSubtract == Long.MIN_VALUE ? plusSeconds(Long.MAX_VALUE).plusSeconds(1) : plusSeconds(-secondsToSubtract));
    }

    /**
     * Returns a copy of this duration with the specified duration in milliseconds subtracted.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param millisToSubtract
     *            the milliseconds to subtract, positive or negative
     * @return a {@code Duration} based on this duration with the specified milliseconds subtracted, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration minusMillis(long millisToSubtract) {
        return (millisToSubtract == Long.MIN_VALUE ? plusMillis(Long.MAX_VALUE).plusMillis(1) : plusMillis(-millisToSubtract));
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a copy of this duration multiplied by the scalar.
     *
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param multiplicand
     *            the value to multiply the duration by, positive or negative
     * @return a {@code Duration} based on this duration multiplied by the specified scalar, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration multipliedBy(long multiplicand) {
        if (multiplicand == 0) {
            return ZERO;
        }

        if (multiplicand == 1) {
            return this;
        }

        return create(multiplyExact(milliseconds, multiplicand));
    }

    /**
     * Returns a copy of this duration divided by the specified value.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param divisor
     *            the value to divide the duration by, positive or negative, not zero
     * @return a {@code Duration} based on this duration divided by the specified divisor, not null
     * @throws ArithmeticException
     *             if the divisor is zero or if numeric overflow occurs
     */
    public Duration dividedBy(long divisor) {
        if (divisor == 0) {
            throw new ArithmeticException("Cannot divide by zero");
        }

        if (divisor == 1) {
            return this;
        }

        return create(milliseconds / divisor);
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a copy of this duration with the length negated.
     * <p>
     * This method swaps the sign of the total length of this duration. For example, {@code PT1.3S} will be returned as
     * {@code PT-1.3S}.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return a {@code Duration} based on this duration with the amount negated, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration negated() {
        return multipliedBy(-1);
    }

    /**
     * Returns a copy of this duration with a positive length.
     * <p>
     * This method returns a positive duration by effectively removing the sign from any negative total length. For
     * example, {@code PT-1.3S} will be returned as {@code PT1.3S}.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return a {@code Duration} based on this duration with an absolute length, not null
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public Duration abs() {
        return isNegative() ? negated() : this;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the number of days in this duration.
     * <p>
     * This returns the total number of days in the duration by dividing the number of milliseconds by 86400 * 1000L. This is based
     * on the standard definition of a day as 24 hours.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return the number of days in the duration, may be negative
     */
    public long toDays() {
        return milliseconds / MILLIS_PER_DAY;
    }

    /**
     * Gets the number of hours in this duration.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return the number of hours in the duration, may be negative
     */
    public long toHours() {
        return milliseconds / MILLIS_PER_HOUR;
    }

    /**
     * Gets the number of minutes in this duration.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return the number of minutes in the duration, may be negative
     */
    public long toMinutes() {
        return milliseconds / MILLIS_PER_MINUTE;
    }

    /**
     * Gets the number of seconds in this duration.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return the number of seconds in the duration, may be negative
     */
    public long toSeconds() {
        return milliseconds / MILLIS_PER_SECOND;
    }

    /**
     * Gets the number of milliseconds in this duration.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @return the total length of the duration in milliseconds
     * @throws ArithmeticException
     *             if numeric overflow occurs
     */
    public long toMillis() {
        return milliseconds;
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this duration to the specified {@code Duration}.
     * <p>
     * The comparison is based on the total length of the durations. It is "consistent with equals", as defined by
     * {@link Comparable}.
     *
     * @param otherDuration
     *            the other duration to compare to, not null
     * @return the comparator value, negative if less, positive if greater
     */
    @Override
    public int compareTo(Duration otherDuration) {
        // return Long.compare(milliseconds, otherDuration.milliseconds);

        return (milliseconds > otherDuration.milliseconds) ? 1 : ((milliseconds == otherDuration.milliseconds) ? 0 : -1);
    }

    //-----------------------------------------------------------------------
    /**
     * Checks if this duration is equal to the specified {@code Duration}.
     * <p>
     * The comparison is based on the total length of the durations.
     *
     * @param obj
     *            the other duration, null returns false
     * @return true if the other duration is equal to this one
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        return obj instanceof Duration && ((Duration) obj).milliseconds == this.milliseconds;
    }

    /**
     * A hash code for this duration.
     *
     * @return a suitable hash code
     */
    @Override
    public int hashCode() {
        return ((int) (milliseconds ^ (milliseconds >>> 32)));
    }

    //-----------------------------------------------------------------------
    /**
     * A string representation of this duration using ISO-8601 seconds based representation, such as
     * {@code PT8H6M12.345S}.
     * <p>
     * The format of the returned string will be {@code PTnHnMnS}, where n is the relevant hours, minutes or seconds
     * part of the duration. Any fractional seconds are placed after a decimal point i the seconds section. If a section
     * has a zero value, it is omitted. The hours, minutes and seconds will all have the same sign.
     * <p>
     * Examples:
     *
     * <pre>
     *    "20.345 seconds"                 -- "PT20.345S
     *    "15 minutes" (15 * 60 seconds)   -- "PT15M"
     *    "10 hours" (10 * 3600 seconds)   -- "PT10H"
     *    "2 days" (2 * 86400 seconds)     -- "PT48H"
     * </pre>
     *
     * Note that multiples of 24 hours are not output as days to avoid confusion with {@code Period}.
     *
     * @return an ISO-8601 representation of this duration, not null
     */
    @Override
    public String toString() {
        if (this == ZERO) {
            return "PT0S";
        }

        long hours = milliseconds / MILLIS_PER_HOUR;
        int minutes = (int) ((milliseconds % MILLIS_PER_HOUR) / MILLIS_PER_MINUTE);
        int seconds = (int) ((milliseconds % MILLIS_PER_MINUTE) / MILLIS_PER_SECOND);
        int millis = (int) ((milliseconds % MILLIS_PER_SECOND));

        StringBuilder buf = new StringBuilder(24);
        buf.append("PT");
        if (hours != 0) {
            buf.append(hours).append('H');
        }

        if (minutes != 0) {
            buf.append(minutes).append('M');
        }

        if (seconds == 0 && millis == 0 && buf.length() > 2) {
            return buf.toString();
        }

        if (seconds == 0 && millis < 0) {
            buf.append("-0");
        } else {
            buf.append(seconds);
        }

        millis = Math.abs(millis);

        if (millis > 0) {
            buf.append('.');
            buf.append(millis);
        }

        buf.append('S');

        return buf.toString();
    }

    /**
     * Defend against malicious streams.
     *
     * @param s
     *            the stream to read
     * @throws InvalidObjectException
     *             always
     */
    private void readObject(ObjectInputStream s) throws InvalidObjectException {
        throw new InvalidObjectException("Deserialization via serialization delegate");
    }

    void writeExternal(DataOutput out) throws IOException {
        out.writeLong(milliseconds);
    }

    static Duration readExternal(DataInput in) throws IOException {
        return Duration.ofMillis(in.readLong());
    }

    /**
     * Returns the product of the arguments, throwing an exception if the result overflows a {@code long}.
     *
     * @param x
     *            the first value
     * @param y
     *            the second value
     * @return the result
     * @throws ArithmeticException
     *             if the result overflows a long
     * @since 1.8
     */
    static long multiplyExact(long x, long y) {
        long r = x * y;
        long ax = Math.abs(x);
        long ay = Math.abs(y);
        if (((ax | ay) >>> 31 != 0)) {
            // Some bits greater than 2^31 that might cause overflow
            // Check the result using the divide operator
            // and check for the special case of Long.MIN_VALUE * -1
            if (((y != 0) && (r / y != x)) || (x == Long.MIN_VALUE && y == -1)) {
                throw new ArithmeticException("long overflow");
            }
        }
        return r;
    }

    /**
     * Returns the sum of its arguments, throwing an exception if the result overflows a {@code long}.
     *
     * @param x
     *            the first value
     * @param y
     *            the second value
     * @return the result
     * @throws ArithmeticException
     *             if the result overflows a long
     * @since 1.8
     */
    static long addExact(long x, long y) {
        long r = x + y;
        // HD 2-12 Overflow iff both arguments have the opposite sign of the result
        if (((x ^ r) & (y ^ r)) < 0) {
            throw new ArithmeticException("long overflow");
        }
        return r;
    }
}
