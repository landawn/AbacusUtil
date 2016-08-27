package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Ignore any error happened at calling <code>hasNext()</code> or <code>next()</code> and returns false for <code>hasNext()</code>.
 *
 * @param <T>
 */
public final class ErrorBreakIterator<T> implements Iterator<T> {
    private static final Object NONE = new Object();
    private final Iterator<? extends T> iter;
    private Object next = NONE;

    ErrorBreakIterator(Iterator<? extends T> iter) {
        this.iter = iter;
    }

    public static <T> ErrorBreakIterator<T> of(final Iterator<? extends T> iter) {
        return new ErrorBreakIterator<T>(iter);
    }

    public static <T> ErrorBreakIterator<T> of(final Collection<? extends T> c) {
        return new ErrorBreakIterator<T>(c.iterator());
    }

    public static <T> ErrorBreakIterator<T> of(final T[] a) {
        return of(a, 0, a.length);
    }

    public static <T> ErrorBreakIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        Stream.checkIndex(fromIndex, toIndex, a.length);

        return new ErrorBreakIterator<T>(new Iterator<T>() {
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
        try {
            if (next == NONE && iter.hasNext()) {
                next = iter.next();
            }
        } catch (Throwable e) {
            return false;
        }

        return next != NONE;
    }

    @Override
    public T next() {
        if (next == NONE && hasNext() == false) {
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
