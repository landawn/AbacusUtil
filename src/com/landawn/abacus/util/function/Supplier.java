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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.landawn.abacus.util.BooleanList;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.ExList;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ShortList;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface Supplier<T> extends java.util.function.Supplier<T> {

    @SuppressWarnings("rawtypes")
    static final Supplier NULL = new Supplier() {
        @Override
        public Object get() {
            return null;
        }
    };

    static final Supplier<String> EMPTY_STRING = new Supplier<String>() {
        @Override
        public String get() {
            return N.EMPTY_STRING;
        }
    };

    static final Supplier<boolean[]> EMPTY_BOOLEAN_ARRAY = new Supplier<boolean[]>() {
        @Override
        public boolean[] get() {
            return N.EMPTY_BOOLEAN_ARRAY;
        }
    };

    static final Supplier<char[]> EMPTY_CHAR_ARRAY = new Supplier<char[]>() {
        @Override
        public char[] get() {
            return N.EMPTY_CHAR_ARRAY;
        }
    };

    static final Supplier<byte[]> EMPTY_BYTE_ARRAY = new Supplier<byte[]>() {
        @Override
        public byte[] get() {
            return N.EMPTY_BYTE_ARRAY;
        }
    };

    static final Supplier<short[]> EMPTY_SHORT_ARRAY = new Supplier<short[]>() {
        @Override
        public short[] get() {
            return N.EMPTY_SHORT_ARRAY;
        }
    };

    static final Supplier<int[]> EMPTY_INT_ARRAY = new Supplier<int[]>() {
        @Override
        public int[] get() {
            return N.EMPTY_INT_ARRAY;
        }
    };

    static final Supplier<long[]> EMPTY_LONG_ARRAY = new Supplier<long[]>() {
        @Override
        public long[] get() {
            return N.EMPTY_LONG_ARRAY;
        }
    };

    static final Supplier<float[]> EMPTY_FLOAT_ARRAY = new Supplier<float[]>() {
        @Override
        public float[] get() {
            return N.EMPTY_FLOAT_ARRAY;
        }
    };

    static final Supplier<double[]> EMPTY_DOUBLE_ARRAY = new Supplier<double[]>() {
        @Override
        public double[] get() {
            return N.EMPTY_DOUBLE_ARRAY;
        }
    };

    static final Supplier<String[]> EMPTY_STRING_ARRAY = new Supplier<String[]>() {
        @Override
        public String[] get() {
            return N.EMPTY_STRING_ARRAY;
        }
    };

    static final Supplier<Object[]> EMPTY_OBJECT_ARRAY = new Supplier<Object[]>() {
        @Override
        public Object[] get() {
            return N.EMPTY_OBJECT_ARRAY;
        }
    };

    static final Supplier<BooleanList> BOOLEAN_LIST_FACTORY = new Supplier<BooleanList>() {
        @Override
        public BooleanList get() {
            return new BooleanList();
        }
    };

    static final Supplier<CharList> CHAR_LIST_FACTORY = new Supplier<CharList>() {
        @Override
        public CharList get() {
            return new CharList();
        }
    };

    static final Supplier<ByteList> BYTE_LIST_FACTORY = new Supplier<ByteList>() {
        @Override
        public ByteList get() {
            return new ByteList();
        }
    };

    static final Supplier<ShortList> SHORT_LIST_FACTORY = new Supplier<ShortList>() {
        @Override
        public ShortList get() {
            return new ShortList();
        }
    };

    static final Supplier<IntList> INT_LIST_FACTORY = new Supplier<IntList>() {
        @Override
        public IntList get() {
            return new IntList();
        }
    };

    static final Supplier<LongList> LONG_LIST_FACTORY = new Supplier<LongList>() {
        @Override
        public LongList get() {
            return new LongList();
        }
    };

    static final Supplier<FloatList> FLOAT_LIST_FACTORY = new Supplier<FloatList>() {
        @Override
        public FloatList get() {
            return new FloatList();
        }
    };

    static final Supplier<DoubleList> DOUBLE_LIST_FACTORY = new Supplier<DoubleList>() {
        @Override
        public DoubleList get() {
            return new DoubleList();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<ExList> EX_LIST = new Supplier<ExList>() {
        @Override
        public ExList get() {
            return new ExList();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super List> LIST = new Supplier<List>() {
        @Override
        public List get() {
            return new ArrayList();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super LinkedList> LINKED_LIST = new Supplier<LinkedList>() {
        @Override
        public LinkedList get() {
            return new LinkedList();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super Set> SET = new Supplier<Set>() {
        @Override
        public Set get() {
            return new HashSet();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super LinkedHashSet> LINKED_HASH_SET = new Supplier<LinkedHashSet>() {
        @Override
        public LinkedHashSet get() {
            return new LinkedHashSet();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super TreeSet> TREE_SET = new Supplier<TreeSet>() {
        @Override
        public TreeSet get() {
            return new TreeSet();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super Map> MAP = new Supplier<Map>() {
        @Override
        public Map get() {
            return new HashMap();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super LinkedHashMap> LINKED_HASH_MAP = new Supplier<LinkedHashMap>() {
        @Override
        public LinkedHashMap get() {
            return new LinkedHashMap();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super TreeMap> TREE_MAP = new Supplier<TreeMap>() {
        @Override
        public TreeMap get() {
            return new TreeMap();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super ConcurrentHashMap> CONCURRENT_HASH_MAP = new Supplier<ConcurrentHashMap>() {
        @Override
        public ConcurrentHashMap get() {
            return new ConcurrentHashMap();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super Queue> QUEUE = new Supplier<Queue>() {
        @Override
        public Queue get() {
            return new LinkedList();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super ArrayDeque> ARRAY_DEQUE = new Supplier<ArrayDeque>() {
        @Override
        public ArrayDeque get() {
            return new ArrayDeque();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super LinkedBlockingQueue> LINKED_BLOCKING_QUEUE = new Supplier<LinkedBlockingQueue>() {
        @Override
        public LinkedBlockingQueue get() {
            return new LinkedBlockingQueue();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super ConcurrentLinkedQueue> CONCURRENT_LINKED_QUEUE = new Supplier<ConcurrentLinkedQueue>() {
        @Override
        public ConcurrentLinkedQueue get() {
            return new ConcurrentLinkedQueue();
        }
    };

    @SuppressWarnings("rawtypes")
    static final Supplier<? super PriorityQueue> PRIORITY_QUEUE = new Supplier<PriorityQueue>() {
        @Override
        public PriorityQueue get() {
            return new PriorityQueue();
        }
    };

    static final Supplier<String> UUID = new Supplier<String>() {
        @Override
        public String get() {
            return N.uuid();
        }
    };

    static final Supplier<String> GUID = new Supplier<String>() {
        @Override
        public String get() {
            return N.guid();
        }
    };

    @Override
    T get();

    @SuppressWarnings("rawtypes")
    static <T> Supplier<ExList<T>> ofExList() {
        return (Supplier) EX_LIST;
    }

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
    static <T> Supplier<TreeSet<T>> ofTreeSet() {
        return (Supplier) TREE_SET;
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

    @SuppressWarnings("rawtypes")
    static <K, V> Supplier<TreeMap<K, V>> ofTreeMap() {
        return (Supplier) TREE_MAP;
    }

    @SuppressWarnings("rawtypes")
    static <K, V> Supplier<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
        return (Supplier) CONCURRENT_HASH_MAP;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<Queue<T>> ofQueue() {
        return (Supplier) QUEUE;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<ArrayDeque<T>> ofArrayDeque() {
        return (Supplier) ARRAY_DEQUE;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
        return (Supplier) LINKED_BLOCKING_QUEUE;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
        return (Supplier) CONCURRENT_LINKED_QUEUE;
    }

    @SuppressWarnings("rawtypes")
    static <T> Supplier<PriorityQueue> ofPriorityQueue() {
        return (Supplier) PRIORITY_QUEUE;
    }
}
