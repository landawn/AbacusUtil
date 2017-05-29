/*
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved.
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
package com.landawn.abacus.util;

import java.util.Collection;
import java.util.Map;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * {@code StringJoiner} is used to construct a sequence of characters separated
 * by a delimiter and optionally starting with a supplied prefix
 * and ending with a supplied suffix.
 * <p>
 * Prior to adding something to the {@code StringJoiner}, its
 * {@code sj.toString()} method will, by default, return {@code prefix + suffix}.
 * However, if the {@code setEmptyValue} method is called, the {@code emptyValue}
 * supplied will be returned instead. This can be used, for example, when
 * creating a string using set notation to indicate an empty set, i.e.
 * <code>"{}"</code>, where the {@code prefix} is <code>"{"</code>, the
 * {@code suffix} is <code>"}"</code> and nothing has been added to the
 * {@code StringJoiner}.
 *
 * @apiNote
 * <p>The String {@code "[George:Sally:Fred]"} may be constructed as follows:
 *
 * <pre> {@code
 * StringJoiner sj = new StringJoiner(":", "[", "]");
 * sj.add("George").add("Sally").add("Fred");
 * String desiredString = sj.toString();
 * }</pre>
 * <p>
 * A {@code StringJoiner} may be employed to create formatted output from a
 * {@link java.util.stream.Stream} using
 * {@link java.util.stream.Collectors#joining(CharSequence)}. For example:
 *
 * <pre> {@code
 * List<Integer> numbers = Arrays.asList(1, 2, 3, 4);
 * String commaSeparatedNumbers = numbers.stream()
 *     .map(i -> i.toString())
 *     .collect(Collectors.joining(", "));
 * }</pre>
 *
 * @see java.util.stream.Collectors#joining(CharSequence)
 * @see java.util.stream.Collectors#joining(CharSequence, CharSequence, CharSequence)
 * @since  1.8
*/
public class Joiner {
    public static final String DEFAULT_DELIMITER = N.ELEMENT_SEPARATOR;
    public static final String DEFAULT_KEY_VALUE_DELIMITER = "=";

    private final String prefix;
    private final String delimiter;
    private final String keyValueDelimiter;
    private final String suffix;
    private final boolean isEmptyDelimiter;
    private final boolean isEmptyKeyValueDelimiter;
    private boolean trim = false;
    private boolean skipNull = false;
    private String nullText = N.NULL_STRING;

    /*
     * StringBuilder value -- at any time, the characters constructed from the
     * prefix, the added element separated by the delimiter, but without the
     * suffix, so that we can more easily add elements without having to jigger
     * the suffix each time.
     */
    private StringBuilder value;

    /*
     * By default, the string consisting of prefix+suffix, returned by
     * toString(), or properties of value, when no elements have yet been added,
     * i.e. when it is empty.  This may be overridden by the user to be some
     * other value including the empty String.
     */
    private String emptyValue;

    /**
     * Constructs a {@code StringJoiner} with no characters in it, with no
     * {@code prefix} or {@code suffix}, and a copy of the supplied
     * {@code delimiter}.
     * If no characters are added to the {@code StringJoiner} and methods
     * accessing the value of it are invoked, it will not return a
     * {@code prefix} or {@code suffix} (or properties thereof) in the result,
     * unless {@code setEmptyValue} has first been called.
     *
     * @param  delimiter the sequence of characters to be used between each
     *         element added to the {@code StringJoiner} value
     * @throws NullPointerException if {@code delimiter} is {@code null}
     */
    Joiner(CharSequence delimiter) {
        this(delimiter, DEFAULT_KEY_VALUE_DELIMITER);
    }

    Joiner(final CharSequence delimiter, CharSequence keyValueDelimiter) {
        this(delimiter, keyValueDelimiter, "", "");
    }

    /**
     * Constructs a {@code StringJoiner} with no characters in it using copies
     * of the supplied {@code prefix}, {@code delimiter} and {@code suffix}.
     * If no characters are added to the {@code StringJoiner} and methods
     * accessing the string value of it are invoked, it will return the
     * {@code prefix + suffix} (or properties thereof) in the result, unless
     * {@code setEmptyValue} has first been called.
     *
     * @param  delimiter the sequence of characters to be used between each
     *         element added to the {@code StringJoiner}
     * @param  prefix the sequence of characters to be used at the beginning
     * @param  suffix the sequence of characters to be used at the end
     * @throws NullPointerException if {@code prefix}, {@code delimiter}, or
     *         {@code suffix} is {@code null}
     */
    Joiner(CharSequence delimiter, CharSequence prefix, CharSequence suffix) {
        this(delimiter, DEFAULT_KEY_VALUE_DELIMITER, prefix, suffix);
    }

    Joiner(CharSequence delimiter, CharSequence keyValueDelimiter, CharSequence prefix, CharSequence suffix) {
        N.requireNonNull(prefix, "The prefix must not be null");
        N.requireNonNull(delimiter, "The delimiter must not be null");
        N.requireNonNull(keyValueDelimiter, "The keyValueDelimiter must not be null");
        N.requireNonNull(suffix, "The suffix must not be null");
        // make defensive copies of arguments
        this.prefix = prefix.toString();
        this.delimiter = delimiter.toString();
        this.keyValueDelimiter = keyValueDelimiter.toString();
        this.suffix = suffix.toString();
        this.emptyValue = this.prefix + this.suffix;
        this.isEmptyDelimiter = N.isNullOrEmpty(delimiter);
        this.isEmptyKeyValueDelimiter = N.isNullOrEmpty(keyValueDelimiter);
    }

    /**
     * Returns the Map Splitter with the default element and key/value delimiter: <code>", "</code> and <code>"="</code>
     * 
     * @return
     */
    public static Joiner defauLt() {
        return with(DEFAULT_DELIMITER, DEFAULT_KEY_VALUE_DELIMITER);
    }

    public static Joiner with(final CharSequence delimiter) {
        return new Joiner(delimiter);
    }

    public static Joiner with(final CharSequence delimiter, CharSequence keyValueDelimiter) {
        return new Joiner(delimiter, keyValueDelimiter);
    }

    public static Joiner with(CharSequence delimiter, CharSequence prefix, CharSequence suffix) {
        return new Joiner(delimiter, prefix, suffix);
    }

    public static Joiner with(CharSequence delimiter, CharSequence keyValueDelimiter, CharSequence prefix, CharSequence suffix) {
        return new Joiner(delimiter, keyValueDelimiter, prefix, suffix);
    }

    /**
     * Sets the sequence of characters to be used when determining the string
     * representation of this {@code StringJoiner} and no elements have been
     * added yet, that is, when it is empty.  A copy of the {@code emptyValue}
     * parameter is made for this purpose. Note that once an add method has been
     * called, the {@code StringJoiner} is no longer considered empty, even if
     * the element(s) added correspond to the empty {@code String}.
     *
     * @param  emptyValue the characters to return as the value of an empty
     *         {@code StringJoiner}
     * @return this {@code StringJoiner} itself so the calls may be chained
     * @throws NullPointerException when the {@code emptyValue} parameter is
     *         {@code null}
     */
    public Joiner setEmptyValue(CharSequence emptyValue) {
        this.emptyValue = N.requireNonNull(emptyValue, "The empty value must not be null").toString();

        return this;
    }

    public Joiner trim(boolean trim) {
        this.trim = trim;

        return this;
    }

    public Joiner skipNull(boolean skipNull) {
        this.skipNull = skipNull;

        return this;
    }

    public Joiner useForNull(String nullText) {
        this.nullText = nullText;

        return this;
    }

    public Joiner add(boolean element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(char element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(byte element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(short element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(int element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(long element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(float element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(double element) {
        prepareBuilder().append(element);
        return this;
    }

    public Joiner add(String element) {
        if (element != null || skipNull == false) {
            prepareBuilder().append(element == null ? nullText : element);
        }

        return this;
    }

    /**
     * Adds a copy of the given {@code CharSequence} value as the next
     * element of the {@code StringJoiner} value. If {@code element} is
     * {@code null}, then {@code "null"} is added.
     *
     * @param  element The element to add
     * @return a reference to this {@code StringJoiner}
     */
    public Joiner add(CharSequence element) {
        if (element != null || skipNull == false) {
            prepareBuilder().append(element == null ? nullText : element);
        }

        return this;
    }

    public Joiner add(Object element) {
        if (element != null || skipNull == false) {
            prepareBuilder().append(toString(element));
        }

        return this;
    }

    public Joiner join(final boolean[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final boolean[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final char[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final char[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final byte[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final short[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final short[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final int[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final long[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final float[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final double[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        for (int i = fromIndex; i < toIndex; i++) {
            if (sb == null) {
                sb = prepareBuilder().append(a[i]);
            } else {
                if (isEmptyDelimiter) {
                    sb.append(a[i]);
                } else {
                    sb.append(delimiter).append(a[i]);
                }
            }
        }

        return this;
    }

    public Joiner join(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return this;
        }

        return join(a, 0, a.length);
    }

    public Joiner join(final Object[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return this;
        }

        StringBuilder sb = null;

        if (trim) {
            for (int i = fromIndex; i < toIndex; i++) {
                if (a[i] != null || skipNull == false) {
                    if (sb == null) {
                        sb = prepareBuilder().append(toString(a[i]).trim());
                    } else {
                        if (isEmptyDelimiter) {
                            sb.append(toString(a[i]).trim());
                        } else {
                            sb.append(delimiter).append(toString(a[i]).trim());
                        }
                    }
                }
            }
        } else {
            for (int i = fromIndex; i < toIndex; i++) {
                if (a[i] != null || skipNull == false) {
                    if (sb == null) {
                        sb = prepareBuilder().append(toString(a[i]));
                    } else {
                        if (isEmptyDelimiter) {
                            sb.append(toString(a[i]));
                        } else {
                            sb.append(delimiter).append(toString(a[i]));
                        }
                    }
                }
            }
        }

        return this;
    }

    public Joiner join(final Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c, 0, c.size());
    }

    public Joiner join(final Collection<?> c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || (fromIndex == toIndex && fromIndex < c.size())) {
            return this;
        }

        StringBuilder sb = null;

        int i = 0;
        for (Object e : c) {
            if (i++ < fromIndex) {
                continue;
            }

            if (trim) {
                if (e != null || skipNull == false) {
                    if (sb == null) {
                        sb = prepareBuilder().append(toString(e).trim());
                    } else {
                        if (isEmptyDelimiter) {
                            sb.append(toString(e).trim());
                        } else {
                            sb.append(delimiter).append(toString(e).trim());
                        }
                    }
                }
            } else {
                if (e != null || skipNull == false) {
                    if (sb == null) {
                        sb = prepareBuilder().append(toString(e));
                    } else {
                        if (isEmptyDelimiter) {
                            sb.append(toString(e));
                        } else {
                            sb.append(delimiter).append(toString(e));
                        }
                    }
                }
            }

            if (i >= toIndex) {
                break;
            }
        }

        return this;
    }

    public Joiner join(final Map<?, ?> m) {
        if (N.isNullOrEmpty(m)) {
            return this;
        }

        return join(m, 0, m.size());
    }

    public Joiner join(final Map<?, ?> m, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, m == null ? 0 : m.size());

        if ((N.isNullOrEmpty(m) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < m.size())) {
            return this;
        }

        StringBuilder sb = null;

        int i = 0;
        for (Map.Entry<?, ?> entry : m.entrySet()) {
            if (i++ < fromIndex) {
                continue;
            }

            if (trim) {
                if (sb == null) {
                    sb = prepareBuilder().append(toString(entry.getKey()).trim()).append(keyValueDelimiter).append(toString(entry.getValue()).trim());
                } else {
                    if (isEmptyDelimiter) {
                        sb.append(toString(entry.getKey()).trim());
                    } else {
                        sb.append(delimiter).append(toString(entry.getKey()).trim());
                    }

                    if (isEmptyKeyValueDelimiter) {
                        sb.append(toString(entry.getValue()).trim());
                    } else {
                        sb.append(keyValueDelimiter).append(toString(entry.getValue()).trim());
                    }
                }
            } else {
                if (sb == null) {
                    sb = prepareBuilder().append(toString(entry.getKey())).append(keyValueDelimiter).append(toString(entry.getValue()));
                } else {
                    if (isEmptyDelimiter) {
                        sb.append(toString(entry.getKey()));
                    } else {
                        sb.append(delimiter).append(toString(entry.getKey()));
                    }

                    if (isEmptyKeyValueDelimiter) {
                        sb.append(toString(entry.getValue()));
                    } else {
                        sb.append(keyValueDelimiter).append(toString(entry.getValue()));
                    }
                }
            }

            if (i >= toIndex) {
                break;
            }
        }

        return this;
    }

    public Joiner join(final BooleanList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final BooleanList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final CharList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final CharList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final ByteList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final ByteList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final ShortList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final ShortList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final IntList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final IntList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final LongList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final LongList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final FloatList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final FloatList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final DoubleList c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final DoubleList c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    public Joiner join(final ExList<?> c) {
        if (N.isNullOrEmpty(c)) {
            return this;
        }

        return join(c.array(), 0, c.size());
    }

    public Joiner join(final ExList<?> c, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || fromIndex == toIndex) {
            return this;
        }

        return join(c.array(), fromIndex, toIndex);
    }

    /**
     * Adds the contents of the given {@code StringJoiner} without prefix and
     * suffix as the next element if it is non-empty. If the given {@code
     * StringJoiner} is empty, the call has no effect.
     *
     * <p>A {@code StringJoiner} is empty if {@link #add(CharSequence) add()}
     * has never been called, and if {@code merge()} has never been called
     * with a non-empty {@code StringJoiner} argument.
     *
     * <p>If the other {@code StringJoiner} is using a different delimiter,
     * then elements from the other {@code StringJoiner} are concatenated with
     * that delimiter and the result is appended to this {@code StringJoiner}
     * as a single element.
     *
     * @param other The {@code StringJoiner} whose contents should be merged
     *              into this one
     * @throws NullPointerException if the other {@code StringJoiner} is null
     * @return This {@code StringJoiner}
     */
    public Joiner merge(Joiner other) {
        N.requireNonNull(other);
        if (other.value != null) {
            final int length = other.value.length();
            // lock the length so that we can seize the data to be appended
            // before initiate copying to avoid interference, especially when
            // merge 'this'
            StringBuilder builder = prepareBuilder();
            builder.append(other.value, other.prefix.length(), length);
        }
        return this;
    }

    private StringBuilder prepareBuilder() {
        if (value != null) {
            if (isEmptyDelimiter == false) {
                value.append(delimiter);
            }
        } else {
            value = new StringBuilder().append(prefix);
        }
        return value;
    }

    private String toString(Object element) {
        return element == null ? nullText : N.deepToString(element);
    }

    /**
     * Returns the length of the {@code String} representation
     * of this {@code StringJoiner}. Note that if
     * no add methods have been called, then the length of the {@code String}
     * representation (either {@code prefix + suffix} or {@code emptyValue})
     * will be returned. The value should be equivalent to
     * {@code toString().length()}.
     *
     * @return the length of the current value of {@code StringJoiner}
     */
    public int length() {
        // Remember that we never actually append the suffix unless we return
        // the full (present) value or some sub-string or length of it, so that
        // we can add on more if we need to.
        return (value != null ? value.length() + suffix.length() : emptyValue.length());
    }

    /**
     * Returns the current value, consisting of the {@code prefix}, the values
     * added so far separated by the {@code delimiter}, and the {@code suffix},
     * unless no elements have been added in which case, the
     * {@code prefix + suffix} or the {@code emptyValue} characters are returned
     *
     * @return the string representation of this {@code StringJoiner}
     */
    @Override
    public String toString() {
        if (value == null) {
            return emptyValue;
        } else {
            if (suffix.equals("")) {
                return value.toString();
            } else {
                int initialLength = value.length();
                String result = value.append(suffix).toString();
                // reset value to pre-append initialLength
                value.setLength(initialLength);
                return result;
            }
        }
    }
}
