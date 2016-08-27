package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * <code>hasNext()</code> returns false when meet null element.
 *
 * @param <T>
 */
public final class NullSkippedIterator<T> implements Iterator<T> {
    private final Iterator<? extends T> iter;
    private T next;

    NullSkippedIterator(Iterator<? extends T> iter) {
        this.iter = iter;
    }

    public static <T> NullSkippedIterator<T> of(final Iterator<? extends T> iter) {
        return new NullSkippedIterator<T>(iter);
    }

    public static <T> NullSkippedIterator<T> of(final Collection<? extends T> c) {
        return new NullSkippedIterator<T>(c.iterator());
    }

    public static <T> NullSkippedIterator<T> of(final T[] a) {
        return of(a, 0, a.length);
    }

    public static <T> NullSkippedIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        Stream.checkIndex(fromIndex, toIndex, a.length);

        return new NullSkippedIterator<T>(new Iterator<T>() {
            private int cursor = fromIndex;

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
            public void remove() {
                throw new UnsupportedOperationException();
            }
        });
    }

    @Override
    public boolean hasNext() {
        while (next == null && iter.hasNext()) {
            next = iter.next();
        }

        return next != null;
    }

    @Override
    public T next() {
        if (next == null && hasNext() == false) {
            throw new NoSuchElementException();
        }

        final T result = next;
        next = null;
        return result;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
