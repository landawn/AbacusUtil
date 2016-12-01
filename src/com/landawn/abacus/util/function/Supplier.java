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

package com.landawn.abacus.util.function;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface Supplier<T> extends java.util.function.Supplier<T> {

    @SuppressWarnings("rawtypes")
    public static final Supplier NULL = new Supplier() {
        @Override
        public Object get() {
            return null;
        }
    };

    public static final Supplier<String> EMPTY_STRING = new Supplier<String>() {
        @Override
        public String get() {
            return N.EMPTY_STRING;
        }
    };

    public static final Supplier<boolean[]> EMPTY_BOOLEAN_ARRAY = new Supplier<boolean[]>() {
        @Override
        public boolean[] get() {
            return N.EMPTY_BOOLEAN_ARRAY;
        }
    };

    public static final Supplier<char[]> EMPTY_CHAR_ARRAY = new Supplier<char[]>() {
        @Override
        public char[] get() {
            return N.EMPTY_CHAR_ARRAY;
        }
    };

    public static final Supplier<byte[]> EMPTY_BYTE_ARRAY = new Supplier<byte[]>() {
        @Override
        public byte[] get() {
            return N.EMPTY_BYTE_ARRAY;
        }
    };

    public static final Supplier<short[]> EMPTY_SHORT_ARRAY = new Supplier<short[]>() {
        @Override
        public short[] get() {
            return N.EMPTY_SHORT_ARRAY;
        }
    };

    public static final Supplier<int[]> EMPTY_INT_ARRAY = new Supplier<int[]>() {
        @Override
        public int[] get() {
            return N.EMPTY_INT_ARRAY;
        }
    };

    public static final Supplier<long[]> EMPTY_LONG_ARRAY = new Supplier<long[]>() {
        @Override
        public long[] get() {
            return N.EMPTY_LONG_ARRAY;
        }
    };

    public static final Supplier<float[]> EMPTY_FLOAT_ARRAY = new Supplier<float[]>() {
        @Override
        public float[] get() {
            return N.EMPTY_FLOAT_ARRAY;
        }
    };

    public static final Supplier<double[]> EMPTY_DOUBLE_ARRAY = new Supplier<double[]>() {
        @Override
        public double[] get() {
            return N.EMPTY_DOUBLE_ARRAY;
        }
    };

    public static final Supplier<String[]> EMPTY_STRING_ARRAY = new Supplier<String[]>() {
        @Override
        public String[] get() {
            return N.EMPTY_STRING_ARRAY;
        }
    };

    public static final Supplier<Object[]> EMPTY_OBJECT_ARRAY = new Supplier<Object[]>() {
        @Override
        public Object[] get() {
            return N.EMPTY_OBJECT_ARRAY;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Supplier<? super List> LIST = new Supplier<List>() {
        @Override
        public List get() {
            return new ArrayList();
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Supplier<? super LinkedList> LINKED_LIST = new Supplier<LinkedList>() {
        @Override
        public LinkedList get() {
            return new LinkedList();
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Supplier<? super Set> SET = new Supplier<Set>() {
        @Override
        public Set get() {
            return new HashSet();
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Supplier<? super LinkedHashSet> LINKED_HASH_SET = new Supplier<LinkedHashSet>() {
        @Override
        public LinkedHashSet get() {
            return new LinkedHashSet();
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Supplier<? super Map> MAP = new Supplier<Map>() {
        @Override
        public Map get() {
            return new HashMap();
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Supplier<? super LinkedHashMap> LINKED_HASH_MAP = new Supplier<LinkedHashMap>() {
        @Override
        public LinkedHashMap get() {
            return new LinkedHashMap();
        }
    };

    public static final Supplier<String> UUID = new Supplier<String>() {
        @Override
        public String get() {
            return N.uuid();
        }
    };

    public static final Supplier<String> GUID = new Supplier<String>() {
        @Override
        public String get() {
            return N.guid();
        }
    };

    @Override
    T get();

    @SuppressWarnings("rawtypes")
    static <T> Supplier<List<T>> ofList() {
        return (Supplier) LIST;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<LinkedList<T>> ofLinkedList() {
        return (Supplier) LINKED_LIST;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<Set<T>> ofSet() {
        return (Supplier) SET;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<LinkedHashSet<T>> ofLinkedHashSet() {
        return (Supplier) LINKED_HASH_SET;
    }

    @SuppressWarnings("rawtypes")
    static <K, V> Supplier<Map<K, V>> ofMap() {
        return (Supplier) MAP;
    }

    @SuppressWarnings("rawtypes")
    static <K, V> Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
        return (Supplier) LINKED_HASH_MAP;
    }
}
