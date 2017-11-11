package com.landawn.abacus.util;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.LinkedBlockingQueue;

public final class Clazz {

    @SuppressWarnings("rawtypes")
    public static final Class<Map<String, Object>> PROPS_MAP = (Class) Map.class;

    private Clazz() {
        // singleton.
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<List<T>> ofList(final Class<T> eleCls) {
        return (Class) List.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<LinkedList<T>> ofLinkedList(final Class<T> eleCls) {
        return (Class) LinkedList.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<Set<T>> ofSet(final Class<T> eleCls) {
        return (Class) Set.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<LinkedHashSet<T>> ofLinkedHashSet(final Class<T> eleCls) {
        return (Class) LinkedHashSet.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<SortedSet<T>> ofSortedSet(final Class<T> eleCls) {
        return (Class) SortedSet.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<NavigableSet<T>> ofNavigableSet(final Class<T> eleCls) {
        return (Class) NavigableSet.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<TreeSet<T>> ofTreeSet(final Class<T> eleCls) {
        return (Class) TreeSet.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<Queue<T>> ofQueue(final Class<T> eleCls) {
        return (Class) Queue.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<Deque<T>> ofDeque(final Class<T> eleCls) {
        return (Class) Deque.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<ArrayDeque<T>> ofArrayDeque(final Class<T> eleCls) {
        return (Class) ArrayDeque.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<LinkedBlockingQueue<T>> ofLinkedBlockingQueue(final Class<T> eleCls) {
        return (Class) LinkedBlockingQueue.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue(final Class<T> eleCls) {
        return (Class) ConcurrentLinkedQueue.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<PriorityQueue<T>> ofPriorityQueue(final Class<T> eleCls) {
        return (Class) PriorityQueue.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<Map<K, V>> ofMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) Map.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<LinkedHashMap<K, V>> ofLinkedHashMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) LinkedHashMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<SortedMap<K, V>> ofSortedMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) SortedMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<NavigableMap<K, V>> ofNavigableMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) NavigableMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<TreeMap<K, V>> ofTreeMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) TreeMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<ConcurrentMap<K, V>> ofConcurrentMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) ConcurrentMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<ConcurrentHashMap<K, V>> ofConcurrentHashMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) ConcurrentHashMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Class<BiMap<K, V>> ofBiMap(final Class<K> keyCls, final Class<V> valueCls) {
        return (Class) BiMap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<Multiset<T>> ofMultiset(final Class<T> eleCls) {
        return (Class) Multiset.class;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Class<LongMultiset<T>> ofLongMultiset(final Class<T> eleCls) {
        return (Class) LongMultiset.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, E> Class<ListMultimap<K, E>> ofListMultimap(final Class<K> keyCls, final Class<E> valueEleCls) {
        return (Class) ListMultimap.class;
    }

    @SuppressWarnings("rawtypes")
    public static <K, E> Class<SetMultimap<K, E>> ofSetMultimap(final Class<K> keyCls, final Class<E> valueEleCls) {
        return (Class) SetMultimap.class;
    }

}
