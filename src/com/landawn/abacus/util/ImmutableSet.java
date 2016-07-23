
package com.landawn.abacus.util;

import java.util.Iterator;
import java.util.Set;

public final class ImmutableSet<E> extends ImmutableCollection<E> implements Set<E> {
    private final Set<E> set;

    ImmutableSet(Set<E> list) {
        this.set = list;
    }

    public static <E> ImmutableSet<E> of(E... a) {
        return new ImmutableSet<E>(N.asUnmodifiableSet(a));
    }

    /**
     * 
     * @param set the elements in this <code>Set</code> are shared by the returned ImmutableSet.
     * @return
     */
    public static <E> ImmutableSet<E> of(Set<E> set) {
        return new ImmutableSet<E>(N.asUnmodifiableSet(set));
    }

    @Override
    public boolean contains(Object o) {
        return set.contains(o);
    }

    @Override
    public Iterator<E> iterator() {
        return set.iterator();
    }

    @Override
    public int size() {
        return set.size();
    }

    @Override
    public Object[] toArray() {
        return set.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return set.toArray(a);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ImmutableSet && ((ImmutableSet<E>) obj).set.equals(set);
    }

    @Override
    public int hashCode() {
        return set.hashCode();
    }

    @Override
    public String toString() {
        return set.toString();
    }
}
