package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;

abstract class QueuedImmutableIterator<T> extends ImmutableIterator<T> {
    private final int max;

    QueuedImmutableIterator(int max) {
        this.max = max;
    }

    public static <T> QueuedImmutableIterator<T> of(final Iterator<? extends T> iter, int max) {
        return new QueuedImmutableIterator<T>(max) {
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

    public static <T> QueuedImmutableIterator<T> of(final Collection<? extends T> c, int max) {
        return of(c.iterator(), max);
    }

    public static <T> QueuedImmutableIterator<T> of(final T[] a, int max) {
        return of(a, 0, a.length, max);
    }

    public static <T> QueuedImmutableIterator<T> of(final T[] a, final int fromIndex, final int toIndex, int max) {
        Stream.checkIndex(fromIndex, toIndex, a.length);

        return new QueuedImmutableIterator<T>(max) {
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

    public int max() {
        return max;
    }
}
