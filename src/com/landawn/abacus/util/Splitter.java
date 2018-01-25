/*
 * Copyright (C) 2016 HaiYang Li
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.landawn.abacus.type.Type;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Splitter {
    private static final Splitter DEFAULT = new Splitter(Joiner.DEFAULT_DELIMITER, null);
    private final String delimiter;
    private final String delimiterRegex;
    private int max = Integer.MAX_VALUE;
    private boolean trim = false;

    Splitter(String delimiter, String delimiterRegex) {
        this.delimiter = delimiter;
        this.delimiterRegex = delimiterRegex;
    }

    /**
     * Returns the Splitter with the default delimiter: <code>", "</code>
     * 
     * @return
     */
    public static Splitter defauLt() {
        return DEFAULT;
    }

    public static Splitter with(CharSequence delimiter) {
        if (N.isNullOrEmpty(delimiter)) {
            throw new IllegalArgumentException("'delimiter' can't be null or empty");
        }

        return new Splitter(delimiter.toString(), null);
    }

    public static Splitter pattern(CharSequence delimiterRegex) {
        if (N.isNullOrEmpty(delimiterRegex)) {
            throw new IllegalArgumentException("'delimiterRegex' can't be null or empty");
        }

        return new Splitter(null, delimiterRegex.toString());
    }

    public Splitter limit(int max) {
        if (max < 1) {
            throw new IllegalArgumentException("'max' must be greater than 0");
        }

        this.max = max;

        return this;
    }

    public Splitter trim(boolean trim) {
        this.trim = trim;

        return this;
    }

    public List<String> split(CharSequence source) {
        if (N.isNullOrEmpty(source)) {
            return new ArrayList<>();
        }

        return N.asList(split(source, delimiter, delimiterRegex, max, trim));
    }

    public <T> List<T> split(Class<T> targetType, CharSequence source) {
        final Type<T> type = N.typeOf(targetType);

        return split(type, source);
    }

    public <T> List<T> split(Type<T> type, CharSequence source) {
        if (N.isNullOrEmpty(source)) {
            return new ArrayList<>();
        }

        final String[] strs = split(source, delimiter, delimiterRegex, max, trim);
        final List<T> result = new ArrayList<>(strs.length);

        for (String str : strs) {
            result.add(type.valueOf(str));
        }

        return result;
    }

    public <C extends Collection<String>> C split(final C output, CharSequence source) {
        final C result = output;

        if (N.isNullOrEmpty(source)) {
            return result;
        }

        final String[] strs = split(source, delimiter, delimiterRegex, max, trim);
        result.addAll(Arrays.asList(strs));

        return result;
    }

    public <T, C extends Collection<T>> C split(final C output, Class<T> targetType, CharSequence source) {
        final Type<T> type = N.typeOf(targetType);

        return split(output, type, source);
    }

    public <T, C extends Collection<T>> C split(final C output, Type<T> type, CharSequence source) {
        final C result = output;

        if (N.isNullOrEmpty(source)) {
            return result;
        }

        final String[] strs = split(source, delimiter, delimiterRegex, max, trim);

        for (String str : strs) {
            result.add(type.valueOf(str));
        }

        return result;
    }

    //    public <C extends Collection<String>> C split(final Supplier<C> supplier, CharSequence source) {
    //        return this.split(supplier.get(), source);
    //    }
    //
    //    public <T, C extends Collection<T>> C split(final Supplier<C> supplier, Class<T> targetType, CharSequence source) {
    //        return this.split(supplier.get(), targetType, source);
    //    }
    //
    //    public <T, C extends Collection<T>> C split(final Supplier<C> supplier, Type<T> type, CharSequence source) {
    //        return this.split(supplier.get(), type, source);
    //    }
    //
    //    public <T, C extends Collection<T>> C split(final Supplier<C> supplier, String typeName, CharSequence source) {
    //        return this.split(supplier.get(), typeName, source);
    //    }

    public String[] splitToArray(CharSequence source) {
        if (N.isNullOrEmpty(source)) {
            return N.EMPTY_STRING_ARRAY;
        }

        return Splitter.split(source, delimiter, delimiterRegex, max, trim);
    }

    public <T> T splitToArray(Class<T> arrayType, CharSequence source) {
        final Class<?> eleCls = arrayType.getComponentType();

        if (N.isNullOrEmpty(source)) {
            return N.newArray(eleCls, 0);
        }

        final String[] strs = Splitter.split(source, delimiter, delimiterRegex, max, trim);

        if (eleCls.equals(String.class) || eleCls.equals(Object.class)) {
            return (T) strs;
        } else {
            final Type<?> eleType = N.typeOf(eleCls);
            final Object a = N.newArray(eleCls, strs.length);

            if (N.isPrimitive(eleCls)) {
                for (int i = 0, len = strs.length; i < len; i++) {
                    Array.set(a, i, eleType.valueOf(strs[i]));
                }
            } else {
                final Object[] objArray = (Object[]) a;

                for (int i = 0, len = strs.length; i < len; i++) {
                    objArray[i] = eleType.valueOf(strs[i]);
                }
            }

            return (T) a;
        }
    }

    //    public <C extends Collection<String>> C split(final Supplier<C> supplier, CharSequence source) {
    //        return this.split(supplier.get(), source);
    //    }
    //
    //    public <T, C extends Collection<T>> C split(final Supplier<C> supplier, Class<T> targetType, CharSequence source) {
    //        return this.split(supplier.get(), targetType, source);
    //    }
    //
    //    public <T, C extends Collection<T>> C split(final Supplier<C> supplier, Type<T> type, CharSequence source) {
    //        return this.split(supplier.get(), type, source);
    //    }
    //
    //    public <T, C extends Collection<T>> C split(final Supplier<C> supplier, String typeName, CharSequence source) {
    //        return this.split(supplier.get(), typeName, source);
    //    }
    
    public <T, E extends Exception> T splitAndThen(CharSequence source, Try.Function<? super String[], T, E> converter) throws E {
        return converter.apply(this.splitToArray(source));
    }

    static String[] split(CharSequence source, String delimiter, String delimiterRegex, int max, boolean trim) {
        final String sourceStr = source.toString();
        String[] strs = null;

        if (N.notNullOrEmpty(delimiter)) {
            strs = N.split(sourceStr, delimiter, max, trim);
        } else {
            strs = sourceStr.split(delimiterRegex, max);

            if (trim) {
                for (int i = 0, len = strs.length; i < len; i++) {
                    strs[i] = strs[i].trim();
                }
            }
        }

        return strs;
    }

    public static final class MapSplitter {
        private static final MapSplitter DEFAULT = new MapSplitter(Joiner.DEFAULT_DELIMITER, Joiner.DEFAULT_KEY_VALUE_DELIMITER, null, null);

        private final String entryDelimiter;
        private final String keyValueDelimiter;
        private final String entryDelimiterRegex;
        private final String keyValueDelimiterRegex;
        private int max = Integer.MAX_VALUE;
        private boolean trim = false;

        MapSplitter(final String entryDelimiter, final String keyValueDelimiter, final String entryDelimiterRegex, final String keyValueDelimiterRegex) {
            this.entryDelimiter = entryDelimiter;
            this.keyValueDelimiter = keyValueDelimiter;
            this.entryDelimiterRegex = entryDelimiterRegex;
            this.keyValueDelimiterRegex = keyValueDelimiterRegex;
        }

        /**
         * Returns the Map Splitter with the default entry and key/value delimiter: <code>", "</code> and <code>"="</code>
         * 
         * @return
         */
        public static MapSplitter defauLt() {
            return DEFAULT;
        }

        public static MapSplitter with(CharSequence entryDelimiter, CharSequence keyValueDelimiter) {
            if (N.isNullOrEmpty(entryDelimiter) || N.isNullOrEmpty(keyValueDelimiter)) {
                throw new IllegalArgumentException("'entryDelimiter' and 'keyValueDelimiter' can't be null or empty");
            }

            return new MapSplitter(entryDelimiter.toString(), keyValueDelimiter.toString(), null, null);
        }

        public static MapSplitter pattern(CharSequence entryDelimiterRegex, CharSequence keyValueDelimiterRegex) {
            if (N.isNullOrEmpty(entryDelimiterRegex) || N.isNullOrEmpty(keyValueDelimiterRegex)) {
                throw new IllegalArgumentException("'entryDelimiterRegex' and 'keyValueDelimiterRegex' can't be null or empty");
            }

            return new MapSplitter(null, null, entryDelimiterRegex.toString(), keyValueDelimiterRegex.toString());
        }

        public MapSplitter limit(int max) {
            if (max < 1) {
                throw new IllegalArgumentException("'max' must be greater than 0");
            }

            this.max = max;

            return this;
        }

        public MapSplitter trim(boolean trim) {
            this.trim = trim;

            return this;
        }

        public Map<String, String> split(CharSequence source) {
            if (N.isNullOrEmpty(source)) {
                return new LinkedHashMap<>();
            }

            final String[] strs = Splitter.split(source, entryDelimiter, entryDelimiterRegex, max, trim);
            final Map<String, String> result = new LinkedHashMap<>(N.initHashCapacity(strs.length));
            String[] strEntry = null;

            if (N.notNullOrEmpty(keyValueDelimiter)) {
                for (String str : strs) {
                    strEntry = N.split(str, keyValueDelimiter, 2, trim);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    result.put(strEntry[0], strEntry[1]);
                }
            } else {
                for (String str : strs) {
                    strEntry = str.split(keyValueDelimiterRegex, 2);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    if (trim) {
                        strEntry[0] = strEntry[0].trim();
                        strEntry[1] = strEntry[1].trim();
                    }

                    result.put(strEntry[0], strEntry[1]);
                }
            }

            return result;
        }

        public <K, V> Map<K, V> split(Class<K> keyType, Class<V> valueType, CharSequence source) {
            final Type<K> typeOfKey = N.typeOf(keyType);
            final Type<V> typeOfValue = N.typeOf(valueType);

            return split(typeOfKey, typeOfValue, source);
        }

        public <K, V> Map<K, V> split(Type<K> keyType, Type<V> valueType, CharSequence source) {
            if (N.isNullOrEmpty(source)) {
                return new LinkedHashMap<>();
            }

            final String[] strs = Splitter.split(source, entryDelimiter, entryDelimiterRegex, max, trim);
            final Map<K, V> result = new LinkedHashMap<>(N.initHashCapacity(strs.length));
            String[] strEntry = null;

            if (N.notNullOrEmpty(keyValueDelimiter)) {
                for (String str : strs) {
                    strEntry = N.split(str, keyValueDelimiter, 2, trim);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    result.put(keyType.valueOf(strEntry[0]), valueType.valueOf(strEntry[1]));
                }
            } else {
                for (String str : strs) {
                    strEntry = str.split(keyValueDelimiterRegex, 2);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    if (trim) {
                        strEntry[0] = strEntry[0].trim();
                        strEntry[1] = strEntry[1].trim();
                    }

                    result.put(keyType.valueOf(strEntry[0]), valueType.valueOf(strEntry[1]));
                }
            }

            return result;
        }

        public <M extends Map<String, String>> M split(final M output, CharSequence source) {
            final M result = output;

            if (N.isNullOrEmpty(source)) {
                return result;
            }

            final String[] strs = Splitter.split(source, entryDelimiter, entryDelimiterRegex, max, trim);
            String[] strEntry = null;

            if (N.notNullOrEmpty(keyValueDelimiter)) {
                for (String str : strs) {
                    strEntry = N.split(str, keyValueDelimiter, 2, trim);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    result.put(strEntry[0], strEntry[1]);
                }
            } else {
                for (String str : strs) {
                    strEntry = str.split(keyValueDelimiterRegex, 2);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    if (trim) {
                        strEntry[0] = strEntry[0].trim();
                        strEntry[1] = strEntry[1].trim();
                    }

                    result.put(strEntry[0], strEntry[1]);
                }
            }

            return result;
        }

        public <K, V, M extends Map<K, V>> M split(final M output, Class<K> keyType, Class<V> valueType, CharSequence source) {
            final Type<K> typeOfKey = N.typeOf(keyType);
            final Type<V> typeOfValue = N.typeOf(valueType);

            return split(output, typeOfKey, typeOfValue, source);
        }

        public <K, V, M extends Map<K, V>> M split(final M output, Type<K> keyType, Type<V> valueType, CharSequence source) {
            final M result = output;

            if (N.isNullOrEmpty(source)) {
                return result;
            }

            final String[] strs = Splitter.split(source, entryDelimiter, entryDelimiterRegex, max, trim);
            String[] strEntry = null;

            if (N.notNullOrEmpty(keyValueDelimiter)) {
                for (String str : strs) {
                    strEntry = N.split(str, keyValueDelimiter, 2, trim);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    result.put(keyType.valueOf(strEntry[0]), valueType.valueOf(strEntry[1]));
                }
            } else {
                for (String str : strs) {
                    strEntry = str.split(keyValueDelimiterRegex, 2);

                    if (strEntry.length != 2) {
                        throw new IllegalArgumentException("Invalid map entry String: " + N.toString(strEntry));
                    }

                    if (trim) {
                        strEntry[0] = strEntry[0].trim();
                        strEntry[1] = strEntry[1].trim();
                    }

                    result.put(keyType.valueOf(strEntry[0]), valueType.valueOf(strEntry[1]));
                }
            }

            return result;
        }

        //        public <M extends Map<String, String>> M split(final Supplier<M> supplier, CharSequence source) {
        //            return this.split(supplier.get(), source);
        //        }
        //
        //        public <K, V, M extends Map<K, V>> M split(final Supplier<M> supplier, Class<K> keyType, Class<V> valueType, CharSequence source) {
        //            return this.split(supplier.get(), keyType, valueType, source);
        //        }
        //
        //        public <K, V, M extends Map<K, V>> M split(final Supplier<M> supplier, Type<K> keyType, Type<V> valueType, CharSequence source) {
        //            return this.split(supplier.get(), keyType, valueType, source);
        //        }
        //
        //        public <K, V, M extends Map<K, V>> M split(final Supplier<M> supplier, String keyTypeName, String valueTypeName, CharSequence source) {
        //            return this.split(supplier.get(), keyTypeName, valueTypeName, source);
        //        }

        public <T, E extends Exception> T splitAndThen(CharSequence source, Try.Function<? super Map<String, String>, T, E> converter) throws E {
            return converter.apply(this.split(source));
        }
    }
}
