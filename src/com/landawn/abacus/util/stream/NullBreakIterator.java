package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Ignore any null element and move to next element until next element is not null or end of the iterator.  
 *
 * @param <T>
 */
public final class NullBreakIterator<T> implements Iterator<T> {
    private static final Object NONE = new Object();
    private final Iterator<? extends T> iter;
    private Object next = NONE;

    NullBreakIterator(Iterator<? extends T> iter) {
        this.iter = iter;
    }

    public static <T> NullBreakIterator<T> of(final Iterator<? extends T> iter) {
        return new NullBreakIterator<T>(iter);
    }

    public static <T> NullBreakIterator<T> of(final Collection<? extends T> c) {
        return new NullBreakIterator<T>(c.iterator());
    }

    public static <T> NullBreakIterator<T> of(final T[] a) {
        return of(a, 0, a.length);
    }

    public static <T> NullBreakIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        Stream.checkIndex(fromIndex, toIndex, a.length);

        return new NullBreakIterator<T>(new Iterator<T>() {
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
        if (next == NONE && iter.hasNext()) {
            next = iter.next();
        }

        return next != NONE && next != null;
    }

    @Override
    public T next() {
        if ((next == NONE || next == null) && hasNext() == false) {
            throw new NoSuchElementException();
        }

        final T result = (T) next;
        next = NONE;
        return result;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
