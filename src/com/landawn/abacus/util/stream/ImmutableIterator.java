package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;

public abstract class ImmutableIterator<T> implements java.util.Iterator<T> {

    public static <T> ImmutableIterator<T> of(final Iterator<? extends T> iter) {
        return new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public T next() {
                return iter.next();
            }
        };
    }

    public static <T> ImmutableIterator<T> of(final Collection<? extends T> c) {
        return of(c.iterator());
    }

    public static <T> ImmutableIterator<T> of(final T[] a) {
        return of(a, 0, a.length);
    }

    public static <T> ImmutableIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        Stream.checkIndex(fromIndex, toIndex, a.length);

        return new ImmutableIterator<T>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }
        };
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    public long count() {
        long result = 0;

        while (hasNext()) {
            next();
            result++;
        }

        return result;
    }

    public void skip(long n) {
        while (n > 0 && hasNext()) {
            next();
            n--;
        }
    }

    public <A> A[] toArray(A[] a) {
        final ObjectList<A> list = new ObjectList<>(a);

        while (hasNext()) {
            list.add((A) next());
        }

        return list.array() == a ? a : list.trimToSize().array();
    }

    static abstract class QueuedIterator<T> extends ImmutableIterator<T> {
        private final int max;

        QueuedIterator(int max) {
            this.max = max;
        }

        public int max() {
            return max;
        }
    }
}
