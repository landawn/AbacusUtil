/*
 * Copyright (C) 2018 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.util;

import java.io.IOException;
import java.io.Writer;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.Queue;
import java.util.TimeZone;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.landawn.abacus.exception.UncheckedIOException;

/**
 * 
 * @since 1.2.6
 * 
 * @author Haiyang Li
 */
public final class DateUtil {

    // ...
    public static final TimeZone UTC_TIME_ZONE = TimeZone.getTimeZone("UTC");
    /**
     * The system default time zone
     */
    public static final TimeZone LOCAL_TIME_ZONE = Calendar.getInstance().getTimeZone();

    /**
     * Date format.
     */
    public static final String LOCAL_YEAR_FORMAT = "yyyy";
    public static final String LOCAL_MONTH_DAY_FORMAT = "MM-dd";
    static final String LOCAL_MONTH_DAY_FORMAT_SLASH = "MM/dd";
    public static final String LOCAL_DATE_FORMAT = "yyyy-MM-dd";
    static final String LOCAL_DATE_FORMAT_SLASH = "yyyy/MM/dd";
    public static final String LOCAL_TIME_FORMAT = "HH:mm:ss";
    public static final String LOCAL_DATETIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
    static final String LOCAL_DATETIME_FORMAT_SLASH = "yyyy/MM/dd HH:mm:ss";
    public static final String LOCAL_TIMESTAMP_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS";
    static final String LOCAL_TIMESTAMP_FORMAT_SLASH = "yyyy/MM/dd HH:mm:ss.SSS";

    /**
     * It's default date/time format.
     */
    public static final String ISO_8601_DATETIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    static final String ISO_8601_DATETIME_FORMAT_SLASH = "yyyy/MM/dd'T'HH:mm:ss'Z'";
    /**
     * It's default timestamp format.
     */
    public static final String ISO_8601_TIMESTAMP_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
    static final String ISO_8601_TIMESTAMP_FORMAT_SLASH = "yyyy/MM/dd'T'HH:mm:ss.SSS'Z'";

    /**
     * This constant defines the date format specified by
     * RFC 1123 / RFC 822. Used for parsing via `SimpleDateFormat` as well as
     * error messages.
     */
    public final static String RFC1123_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";

    /**
     * @see System#currentTimeMillis()
     * @return
     */
    public static long currentMillis() {
        return System.currentTimeMillis();
    }

    /**
     * A new instance of <code>java.sql.Time</code> returned is based on the
     * current time in the default time zone with the default locale.
     *
     * @return
     */
    public static Time currentTime() {
        return new Time(System.currentTimeMillis());
    }

    /**
     * A new instance of <code>java.sql.Date</code> returned is based on the
     * current time in the default time zone with the default locale.
     *
     * @return
     */
    public static Date currentDate() {
        return new Date(System.currentTimeMillis());
    }

    /**
     * A new instance of <code>java.sql.Timestamp</code> returned is based on
     * the current time in the default time zone with the default locale.
     *
     * @return
     */
    public static Timestamp currentTimestamp() {
        return new Timestamp(System.currentTimeMillis());
    }

    /**
     * A new instance of <code>java.util.Date</code> returned is based on the
     * current time in the default time zone with the default locale.
     *
     * @return
     */
    public static java.util.Date currentJUDate() {
        return new java.util.Date();
    }

    /**
     * A new instance of <code>java.util.Calendar</code> returned is based on
     * the current time in the default time zone with the default locale.
     *
     * @return a Calendar.
     */
    public static Calendar currentCalendar() {
        return Calendar.getInstance();
    }

    public static GregorianCalendar currentGregorianCalendar() {
        return new GregorianCalendar();
    }

    public static XMLGregorianCalendar currentXMLGregorianCalendar() {
        return dataTypeFactory.newXMLGregorianCalendar(currentGregorianCalendar());
    }

    /**
     * Adds or subtracts the specified amount of time to the given time unit,
     * based on the calendar's rules. For example, to subtract 5 days from the
     * current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(date, -5, TimeUnit.DAYS)</code>.
     *
     * @param date
     * @param amount
     * @param unit
     * @return a new instance of Date with the specified amount rolled.
     */
    public static <T extends java.util.Date> T roll(final T date, final long amount, final TimeUnit unit) {
        return createDate(date.getClass(), date.getTime() + unit.toMillis(amount));
    }

    /**
     * Adds or subtracts the specified amount of time to the given calendar
     * unit, based on the calendar's rules. For example, to subtract 5 days from
     * the current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(date, -5, CalendarUnit.DAY)</code>.
     *
     * @param date
     * @param amount
     * @param unit
     * @return a new instance of Date with the specified amount rolled.
     */
    public static <T extends java.util.Date> T roll(final T date, final long amount, final CalendarUnit unit) {
        if (amount > Integer.MAX_VALUE || amount < Integer.MIN_VALUE) {
            throw new IllegalArgumentException("The amount :" + amount + " is too big for unit: " + unit);
        }

        switch (unit) {
            case MONTH:
            case YEAR:
                final Calendar c = parseCalendar(date);
                c.add(unit.intValue(), (int) amount);

                return createDate(date.getClass(), c.getTimeInMillis());

            default:
                return createDate(date.getClass(), date.getTime() + unit.toMillis(amount));
        }
    }

    /**
     * Adds or subtracts the specified amount of time to the given time unit,
     * based on the calendar's rules. For example, to subtract 5 days from the
     * current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(c, -5, TimeUnit.DAYS)</code>.
     *
     * @param c
     * @param amount
     * @param unit
     * @return a new instance of Calendar with the specified amount rolled.
     */
    public static <T extends Calendar> T roll(final T c, final long amount, final TimeUnit unit) {
        return createCalendar(c, c.getTimeInMillis() + unit.toMillis(amount));
    }

    /**
     * Adds or subtracts the specified amount of time to the given calendar
     * unit, based on the calendar's rules. For example, to subtract 5 days from
     * the current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(c, -5, CalendarUnit.DAY)</code>.
     *
     * @param c
     * @param amount
     * @param unit
     * @return a new instance of Calendar with the specified amount rolled.
     */
    public static <T extends Calendar> T roll(final T c, final long amount, final CalendarUnit unit) {
        if (amount > Integer.MAX_VALUE || amount < Integer.MIN_VALUE) {
            throw new IllegalArgumentException("The amount :" + amount + " is too big for unit: " + unit);
        }

        final T result = createCalendar(c, c.getTimeInMillis());

        result.add(unit.intValue(), (int) amount);

        return result;
    }

    private static final Map<String, Queue<DateFormat>> dfPool = new ObjectPool<>(64);
    private static final Map<TimeZone, Queue<Calendar>> calendarPool = new ObjectPool<>(64);
    private static final Queue<DateFormat> utcTimestampDFPool = new ArrayBlockingQueue<>(N.POOL_SIZE);
    private static final Queue<DateFormat> utcDateTimeDFPool = new ArrayBlockingQueue<>(N.POOL_SIZE);
    private static final Queue<Calendar> utcCalendarPool = new ArrayBlockingQueue<>(N.POOL_SIZE);
    // ...
    private static final Queue<char[]> utcTimestampFormatCharsPool = new ArrayBlockingQueue<>(N.POOL_SIZE);
    private static final DatatypeFactory dataTypeFactory;

    static {
        DatatypeFactory temp = null;

        try {
            temp = DatatypeFactory.newInstance();
        } catch (Exception e) {
            // ignore.
            // logger.error("Failed to initialize XMLGregorianCalendarType: " +
            // e.getMessage(), e);
        }

        dataTypeFactory = temp;
    }

    // ...
    private static final char[][][] cbufOfSTDInt = new char[5][][];

    static {
        for (int i = 0, j = 1; i < 5; i++, j = j * 10) {
            cbufOfSTDInt[i] = new char[j][];

            for (int k = 0; k < j; k++) {
                if (i == 1) {
                    cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                } else if (i == 2) {
                    if (k < 10) {
                        cbufOfSTDInt[i][k] = ("0" + String.valueOf(k)).toCharArray();
                    } else {
                        cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                    }
                } else if (i == 3) {
                    if (k < 10) {
                        cbufOfSTDInt[i][k] = ("00" + String.valueOf(k)).toCharArray();
                    } else if (k < 100) {
                        cbufOfSTDInt[i][k] = ("0" + String.valueOf(k)).toCharArray();
                    } else {
                        cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                    }
                } else if (i == 4) {
                    if (k < 10) {
                        cbufOfSTDInt[i][k] = ("000" + String.valueOf(k)).toCharArray();
                    } else if (k < 100) {
                        cbufOfSTDInt[i][k] = ("00" + String.valueOf(k)).toCharArray();
                    } else if (k < 1000) {
                        cbufOfSTDInt[i][k] = ("0" + String.valueOf(k)).toCharArray();
                    } else {
                        cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                    }
                }
            }
        }
    }

    private DateUtil() {
        // singleton
    }

    public static java.util.Date parseJUDate(final Calendar c) {
        return (c == null) ? null : parseJUDate(c.getTimeInMillis());
    }

    public static java.util.Date parseJUDate(final java.util.Date date) {
        return (date == null) ? null : parseJUDate(date.getTime());
    }

    public static java.util.Date parseJUDate(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new java.util.Date(timeInMillis);
    }

    public static java.util.Date parseJUDate(final String date) {
        return parseJUDate(date, null);
    }

    public static java.util.Date parseJUDate(final String date, final String format) {
        return parseJUDate(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of java.util.Date.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     *
     * @param date
     * @param format
     * @throws IllegalArgumentException
     *             if the date given can't be parsed with specified format.
     */
    public static java.util.Date parseJUDate(final String date, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return parseJUDate(parse(date, format, timeZone));
    }

    public static Date parseDate(final Calendar c) {
        return (c == null) ? null : parseDate(c.getTimeInMillis());
    }

    public static Date parseDate(final java.util.Date date) {
        return (date == null) ? null : parseDate(date.getTime());
    }

    public static Date parseDate(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new Date(timeInMillis);
    }

    public static Date parseDate(final String date) {
        return parseDate(date, null);
    }

    public static Date parseDate(final String date, final String format) {
        return parseDate(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of java.sql.Date.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param date
     * @param format
     * @param timeZone
     * @return
     */
    public static Date parseDate(final String date, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return parseDate(parse(date, format, timeZone));
    }

    public static Time parseTime(final Calendar c) {
        return (c == null) ? null : parseTime(c.getTimeInMillis());
    }

    public static Time parseTime(final java.util.Date date) {
        return (date == null) ? null : parseTime(date.getTime());
    }

    public static Time parseTime(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new Time(timeInMillis);
    }

    public static Time parseTime(final String date) {
        return parseTime(date, null);
    }

    public static Time parseTime(final String date, final String format) {
        return parseTime(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of Time.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param date
     * @param format
     * @param timeZone
     * @return
     */
    public static Time parseTime(final String date, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return parseTime(parse(date, format, timeZone));
    }

    public static Timestamp parseTimestamp(final Calendar c) {
        return (c == null) ? null : parseTimestamp(c.getTimeInMillis());
    }

    public static Timestamp parseTimestamp(final java.util.Date date) {
        return (date == null) ? null : parseTimestamp(date.getTime());
    }

    public static Timestamp parseTimestamp(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new Timestamp(timeInMillis);
    }

    public static Timestamp parseTimestamp(final String date) {
        return parseTimestamp(date, null);
    }

    public static Timestamp parseTimestamp(final String date, final String format) {
        return parseTimestamp(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of Timestamp.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param date
     * @param format
     * @param timeZone
     * @return
     */
    public static Timestamp parseTimestamp(final String date, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return parseTimestamp(parse(date, format, timeZone));
    }

    public static Calendar parseCalendar(final Calendar c) {
        return (c == null) ? null : parseCalendar(c.getTimeInMillis());
    }

    public static Calendar parseCalendar(final java.util.Date date) {
        return (date == null) ? null : parseCalendar(date.getTime());
    }

    public static Calendar parseCalendar(final long timeInMillis) {
        if (timeInMillis == 0) {
            return null;
        }

        final Calendar c = Calendar.getInstance();

        c.setTimeInMillis(timeInMillis);

        return c;
    }

    public static Calendar parseCalendar(final String calendar) {
        return parseCalendar(calendar, null);
    }

    public static Calendar parseCalendar(final String calendar, final String format) {
        return parseCalendar(calendar, format, null);
    }

    /**
     * Converts the specified <code>calendar</code> with the specified {@code format} to a new instance of Calendar.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param calendar
     * @param format
     * @param timeZone
     * @return
     */
    public static Calendar parseCalendar(final String calendar, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(calendar) || (calendar.length() == 4 && "null".equalsIgnoreCase(calendar))) {
            return null;
        }

        return parseCalendar(parse(calendar, format, timeZone));
    }

    public static GregorianCalendar parseGregorianCalendar(final Calendar c) {
        return (c == null) ? null : parseGregorianCalendar(c.getTimeInMillis());
    }

    public static GregorianCalendar parseGregorianCalendar(final java.util.Date date) {
        return (date == null) ? null : parseGregorianCalendar(date.getTime());
    }

    public static GregorianCalendar parseGregorianCalendar(final long timeInMillis) {
        if (timeInMillis == 0) {
            return null;
        }

        final GregorianCalendar c = new GregorianCalendar();

        c.setTimeInMillis(timeInMillis);

        return c;
    }

    public static GregorianCalendar parseGregorianCalendar(final String calendar) {
        return parseGregorianCalendar(calendar, null);
    }

    public static GregorianCalendar parseGregorianCalendar(final String calendar, final String format) {
        return parseGregorianCalendar(calendar, format, null);
    }

    /**
     * Converts the specified <code>calendar</code> with the specified {@code format} to a new instance of GregorianCalendar.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param calendar
     * @param format
     * @param timeZone
     * @return
     */
    public static GregorianCalendar parseGregorianCalendar(final String calendar, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(calendar) || (calendar.length() == 4 && "null".equalsIgnoreCase(calendar))) {
            return null;
        }

        return parseGregorianCalendar(parse(calendar, format, timeZone));
    }

    public static XMLGregorianCalendar parseXMLGregorianCalendar(final Calendar c) {
        return (c == null) ? null : parseXMLGregorianCalendar(c.getTimeInMillis());
    }

    public static XMLGregorianCalendar parseXMLGregorianCalendar(final java.util.Date date) {
        return (date == null) ? null : parseXMLGregorianCalendar(date.getTime());
    }

    public static XMLGregorianCalendar parseXMLGregorianCalendar(final long timeInMillis) {
        if (timeInMillis == 0) {
            return null;
        }

        return dataTypeFactory.newXMLGregorianCalendar(parseGregorianCalendar(timeInMillis));
    }

    public static XMLGregorianCalendar parseXMLGregorianCalendar(final String calendar) {
        return parseXMLGregorianCalendar(calendar, null);
    }

    public static XMLGregorianCalendar parseXMLGregorianCalendar(final String calendar, final String format) {
        return parseXMLGregorianCalendar(calendar, format, null);
    }

    /**
     * Converts the specified <code>calendar</code> with the specified {@code format} to a new instance of XMLGregorianCalendar.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param calendar
     * @param format
     * @param timeZone
     * @return
     */
    public static XMLGregorianCalendar parseXMLGregorianCalendar(final String calendar, final String format, final TimeZone timeZone) {
        if (N.isNullOrEmpty(calendar) || (calendar.length() == 4 && "null".equalsIgnoreCase(calendar))) {
            return null;
        }

        return parseXMLGregorianCalendar(parse(calendar, format, timeZone));
    }

    public static String format(final java.util.Date date) {
        return format(date, null, null);
    }

    public static String format(final java.util.Date date, final String format) {
        return format(date, format, null);
    }

    public static String format(final java.util.Date date, final String format, final TimeZone timeZone) {
        return formatDate(null, date, format, timeZone);
    }

    public static void format(final Writer writer, final java.util.Date date) {
        format(writer, date, null, null);
    }

    public static void format(final Writer writer, final java.util.Date date, final String format, final TimeZone timeZone) {
        formatDate(writer, date, format, timeZone);
    }

    public static String format(final Calendar c) {
        return format(c, null, null);
    }

    public static String format(final Calendar c, final String format) {
        return format(c, format, null);
    }

    public static String format(final Calendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            final BufferedWriter cbuff = ObjectFactory.createBufferedWriter();
            fastDateFormat(cbuff, c.getTimeInMillis(), false);

            String str = cbuff.toString();

            ObjectFactory.recycle(cbuff);

            return str;
        }

        return format(parseJUDate(c), format, timeZone);
    }

    public static void format(final Writer writer, final Calendar c) {
        format(writer, c, null, null);
    }

    public static void format(final Writer writer, final Calendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            fastDateFormat(writer, c.getTimeInMillis(), false);
        } else {
            format(writer, parseJUDate(c), format, timeZone);
        }
    }

    public static String format(final XMLGregorianCalendar c) {
        return format(c, null, null);
    }

    public static String format(final XMLGregorianCalendar c, final String format) {
        return format(c, format, null);
    }

    public static String format(final XMLGregorianCalendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            final BufferedWriter cbuff = ObjectFactory.createBufferedWriter();

            fastDateFormat(cbuff, c.toGregorianCalendar().getTimeInMillis(), false);

            String str = cbuff.toString();

            ObjectFactory.recycle(cbuff);

            return str;
        }

        return format(parseJUDate(c.toGregorianCalendar()), format, timeZone);
    }

    public static void format(final Writer writer, final XMLGregorianCalendar c) {
        format(writer, c, null, null);
    }

    public static void format(final Writer writer, final XMLGregorianCalendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            fastDateFormat(writer, c.toGregorianCalendar().getTimeInMillis(), false);
        } else {
            format(writer, parseJUDate(c.toGregorianCalendar()), format, timeZone);
        }
    }

    private static long parse(final String date, String format, TimeZone timeZone) {
        if ((format == null) && date.length() > 4 && (date.charAt(2) >= '0' && date.charAt(2) <= '9' && date.charAt(4) >= '0' && date.charAt(4) <= '9')) {
            try {
                return Long.parseLong(date);
            } catch (NumberFormatException e) {
                // ignore.
            }
        }

        format = checkDateFormat(date, format);

        if (N.isNullOrEmpty(format)) {
            if (timeZone == null) {
                return ISO8601Util.parse(date).getTime();
            } else {
                throw new RuntimeException("Unsupported date format: " + format + " with time zone: " + timeZone);
            }
        }

        timeZone = checkTimeZone(format, timeZone);

        long timeInMillis = fastDateParse(date, format, timeZone);

        if (timeInMillis > 0) {
            return timeInMillis;
        }

        DateFormat sdf = getSDF(format, timeZone);

        try {
            return sdf.parse(date).getTime();
        } catch (ParseException e) {
            throw new IllegalArgumentException(e);
        } finally {
            recycleSDF(format, timeZone, sdf);
        }
    }

    private static DateFormat getSDF(final String format, final TimeZone timeZone) {
        DateFormat sdf = null;

        if (timeZone == UTC_TIME_ZONE) {
            if ((format.length() == 28) && (format == ISO_8601_TIMESTAMP_FORMAT)) {
                sdf = utcTimestampDFPool.poll();

                if (sdf == null) {
                    sdf = new SimpleDateFormat(format);
                    sdf.setTimeZone(timeZone);
                }

                return sdf;
            } else if ((format.length() == 24) && (format == ISO_8601_DATETIME_FORMAT)) {
                sdf = utcDateTimeDFPool.poll();

                if (sdf == null) {
                    sdf = new SimpleDateFormat(format);
                    sdf.setTimeZone(timeZone);
                }

                return sdf;
            }
        }

        Queue<DateFormat> queue = dfPool.get(format);

        if (queue == null) {
            queue = new ArrayBlockingQueue<>(N.POOL_SIZE);
            dfPool.put(format, queue);
        }

        sdf = queue.poll();

        if (sdf == null) {
            sdf = new SimpleDateFormat(format);
        }

        sdf.setTimeZone(timeZone);

        return sdf;
    }

    private static void recycleSDF(final String format, final TimeZone timeZone, final DateFormat sdf) {
        if (timeZone == UTC_TIME_ZONE) {
            if ((format.length() == 28) && (format == ISO_8601_TIMESTAMP_FORMAT)) {
                utcTimestampDFPool.add(sdf);
            } else if ((format.length() == 24) && (format == ISO_8601_DATETIME_FORMAT)) {
                utcDateTimeDFPool.add(sdf);
            } else {
                dfPool.get(format).add(sdf);
            }
        } else {
            dfPool.get(format).add(sdf);
        }
    }

    private static String checkDateFormat(final String str, final String format) {
        if (N.isNullOrEmpty(format)) {
            int len = str.length();

            switch (len) {
                case 4:
                    return LOCAL_YEAR_FORMAT;

                case 5:
                    if (str.charAt(2) == '/') {
                        return LOCAL_MONTH_DAY_FORMAT_SLASH;
                    } else {
                        return LOCAL_MONTH_DAY_FORMAT;
                    }

                case 8:
                    return LOCAL_TIME_FORMAT;

                case 10:
                    if (str.charAt(4) == '/') {
                        return LOCAL_DATE_FORMAT_SLASH;
                    } else {
                        return LOCAL_DATE_FORMAT;
                    }

                case 19:
                    if (str.charAt(4) == '/') {
                        return LOCAL_DATETIME_FORMAT_SLASH;
                    } else {
                        return LOCAL_DATETIME_FORMAT;
                    }

                case 23:
                    if (str.charAt(4) == '/') {
                        return LOCAL_TIMESTAMP_FORMAT_SLASH;
                    } else {
                        return LOCAL_TIMESTAMP_FORMAT;
                    }

                case 24:
                    if (str.charAt(4) == '/') {
                        return ISO_8601_DATETIME_FORMAT_SLASH;
                    } else {
                        return ISO_8601_DATETIME_FORMAT;
                    }

                case 28:
                    if (str.charAt(4) == '/') {
                        return ISO_8601_TIMESTAMP_FORMAT_SLASH;
                    } else {
                        return ISO_8601_TIMESTAMP_FORMAT;
                    }

                case 29:
                    if (str.charAt(3) == ',') {
                        return RFC1123_DATE_FORMAT;
                    }

                default:
                    // throw new AbacusException("No valid date format found for: " + str);
                    return null;
            }
        }

        return format;
    }

    private static TimeZone checkTimeZone(final String format, TimeZone timeZone) {
        if (timeZone == null) {
            timeZone = format.endsWith("'Z'") ? UTC_TIME_ZONE : LOCAL_TIME_ZONE;
        }

        return timeZone;
    }

    private static long fastDateParse(final String str, final String format, final TimeZone timeZone) {
        if (!((str.length() == 24) || (str.length() == 20) || (str.length() == 19) || (str.length() == 23))) {
            return 0;
        }

        if (!(format.equals(ISO_8601_TIMESTAMP_FORMAT) || format.equals(ISO_8601_DATETIME_FORMAT) || format.equals(LOCAL_DATETIME_FORMAT)
                || format.equals(LOCAL_TIMESTAMP_FORMAT))) {
            return 0;
        }

        //
        // if (!((str.charAt(4) == '-') && (str.charAt(7) == '-') &&
        // (str.charAt(10) == 'T') && (str.charAt(13) == ':') && (str.charAt(16)
        // == ':') && (str
        // .charAt(str.length() - 1) == 'Z'))) {
        // return 0;
        // }
        //
        // int year = Integer.valueOf(str.substring(0, 4));
        // int month = Integer.valueOf(str.substring(5, 7)) - 1;
        // int date = Integer.valueOf(str.substring(8, 10));
        // int hourOfDay = Integer.valueOf(str.substring(11, 13));
        // int minute = Integer.valueOf(str.substring(14, 16));
        // int second = Integer.valueOf(str.substring(17, 19));
        // int milliSecond = (str.length() == 24) ?
        // Integer.valueOf(str.substring(20, 23)) : 0;
        //
        //

        int year = parseInt(str, 0, 4);
        int month = parseInt(str, 5, 7) - 1;
        int date = parseInt(str, 8, 10);
        int hourOfDay = parseInt(str, 11, 13);
        int minute = parseInt(str, 14, 16);
        int second = parseInt(str, 17, 19);
        int milliSecond = ((str.length() == 24) || (str.length() == 23)) ? parseInt(str, 20, 23) : 0;

        Calendar c = null;
        Queue<Calendar> timeZoneCalendarQueue = null;

        if (timeZone == UTC_TIME_ZONE) {
            c = utcCalendarPool.poll();
        } else {
            timeZoneCalendarQueue = calendarPool.get(timeZone);

            if (timeZoneCalendarQueue == null) {
                timeZoneCalendarQueue = new ArrayBlockingQueue<>(N.POOL_SIZE);
                calendarPool.put(timeZone, timeZoneCalendarQueue);
            } else {
                c = timeZoneCalendarQueue.poll();
            }
        }

        if (c == null) {
            c = Calendar.getInstance(timeZone);
        }

        c.set(year, month, date, hourOfDay, minute, second);
        c.set(Calendar.MILLISECOND, milliSecond);

        long timeInMillis = c.getTimeInMillis();

        if (timeZone == UTC_TIME_ZONE) {
            utcCalendarPool.add(c);
        } else {
            timeZoneCalendarQueue.add(c);
        }

        return timeInMillis;
    }

    private static int parseInt(final String str, int from, final int to) {
        int result = 0;

        while (from < to) {
            result = (result * 10) + (str.charAt(from++) - 48);
        }

        return result;
    }

    private static <T extends java.util.Date> T createDate(final Class<? extends java.util.Date> cls, final long millis) {
        java.util.Date result = null;

        if (cls.equals(java.util.Date.class)) {
            result = new java.util.Date(millis);
        } else if (cls.equals(java.sql.Date.class)) {
            result = new java.sql.Date(millis);
        } else if (cls.equals(Time.class)) {
            result = new Time(millis);
        } else if (cls.equals(Timestamp.class)) {
            result = new Timestamp(millis);
        } else {
            result = ClassUtil.invokeConstructor(ClassUtil.getDeclaredConstructor(cls, long.class), millis);
        }

        return (T) result;
    }

    private static <T extends Calendar> T createCalendar(final T c, final long millis) {
        final Class<T> cls = (Class<T>) c.getClass();

        Calendar result = null;

        if (cls.equals(Calendar.class)) {
            result = Calendar.getInstance();
        } else if (cls.equals(GregorianCalendar.class)) {
            result = GregorianCalendar.getInstance();
        } else {
            result = ClassUtil.invokeConstructor(ClassUtil.getDeclaredConstructor(cls, long.class), millis);
        }

        result.setTimeInMillis(millis);

        if (!N.equals(c.getTimeZone(), result.getTimeZone())) {
            result.setTimeZone(c.getTimeZone());
        }

        return (T) result;
    }

    private static String formatDate(final Writer writer, final java.util.Date date, String format, TimeZone timeZone) {
        boolean isTimestamp = date instanceof Timestamp;

        if ((format == null) && (timeZone == null)) {
            if (writer == null) {
                final BufferedWriter bw = ObjectFactory.createBufferedWriter();

                fastDateFormat(bw, date.getTime(), isTimestamp);

                String str = bw.toString();

                ObjectFactory.recycle(bw);

                return str;
            } else {
                fastDateFormat(writer, date.getTime(), isTimestamp);

                return null;
            }
        }

        if (format == null) {
            format = isTimestamp ? ISO_8601_TIMESTAMP_FORMAT : ISO_8601_DATETIME_FORMAT;
        }

        timeZone = checkTimeZone(format, timeZone);

        DateFormat sdf = getSDF(format, timeZone);

        String str = sdf.format(date);

        if (writer != null) {
            try {
                writer.write(str);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        recycleSDF(format, timeZone, sdf);

        return str;
    }

    private static void fastDateFormat(final Writer writer, final long timeInMillis, final boolean isTimestamp) {
        Calendar c = utcCalendarPool.poll();

        if (c == null) {
            c = Calendar.getInstance(UTC_TIME_ZONE);
        }

        c.setTimeInMillis(timeInMillis);

        int year = c.get(Calendar.YEAR);
        int month = c.get(Calendar.MONTH) + 1;
        int day = c.get(Calendar.DAY_OF_MONTH);
        int hour = c.get(Calendar.HOUR_OF_DAY);
        int minute = c.get(Calendar.MINUTE);
        int second = c.get(Calendar.SECOND);

        char[] utcTimestamp = utcTimestampFormatCharsPool.poll();

        if (utcTimestamp == null) {
            utcTimestamp = new char[24];
            utcTimestamp[4] = '-';
            utcTimestamp[7] = '-';
            utcTimestamp[10] = 'T';
            utcTimestamp[13] = ':';
            utcTimestamp[16] = ':';
            utcTimestamp[19] = '.';
            utcTimestamp[23] = 'Z';
        }
        //
        // copy(cbufOfSTDInt[4][year], 0, utcTimestamp, 0, 4);
        // copy(cbufOfSTDInt[2][month], 0, utcTimestamp, 5, 2);
        // copy(cbufOfSTDInt[2][day], 0, utcTimestamp, 8, 2);
        // copy(cbufOfSTDInt[2][hour], 0, utcTimestamp, 11, 2);
        // copy(cbufOfSTDInt[2][minute], 0, utcTimestamp, 14, 2);
        // copy(cbufOfSTDInt[2][second], 0, utcTimestamp, 17, 2);
        //
        utcTimestamp[0] = cbufOfSTDInt[4][year][0];
        utcTimestamp[1] = cbufOfSTDInt[4][year][1];
        utcTimestamp[2] = cbufOfSTDInt[4][year][2];
        utcTimestamp[3] = cbufOfSTDInt[4][year][3];

        utcTimestamp[5] = cbufOfSTDInt[2][month][0];
        utcTimestamp[6] = cbufOfSTDInt[2][month][1];

        utcTimestamp[8] = cbufOfSTDInt[2][day][0];
        utcTimestamp[9] = cbufOfSTDInt[2][day][1];

        utcTimestamp[11] = cbufOfSTDInt[2][hour][0];
        utcTimestamp[12] = cbufOfSTDInt[2][hour][1];

        utcTimestamp[14] = cbufOfSTDInt[2][minute][0];
        utcTimestamp[15] = cbufOfSTDInt[2][minute][1];

        utcTimestamp[17] = cbufOfSTDInt[2][second][0];
        utcTimestamp[18] = cbufOfSTDInt[2][second][1];

        if (isTimestamp) {
            utcTimestamp[19] = '.';

            int milliSecond = c.get(Calendar.MILLISECOND);
            // copy(cbufOfSTDInt[3][milliSecond], 0, utcTimestamp,
            // 20, 3);
            utcTimestamp[20] = cbufOfSTDInt[3][milliSecond][0];
            utcTimestamp[21] = cbufOfSTDInt[3][milliSecond][1];
            utcTimestamp[22] = cbufOfSTDInt[3][milliSecond][2];
        } else {
            utcTimestamp[19] = 'Z';
        }

        try {
            if (isTimestamp) {
                writer.write(utcTimestamp);
            } else {
                writer.write(utcTimestamp, 0, 20);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            utcCalendarPool.add(c);
            utcTimestampFormatCharsPool.add(utcTimestamp);
        }
    }
}
