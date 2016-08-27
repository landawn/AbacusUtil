package com.landawn.abacus.util.stream;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Ignore any error happened at calling <code>hasNext()</code> or <code>next()</code> 
 * and call <code>hasNext()</code> or <code>next()</code> continually until error not occurs or <code>hasNext()</code> returns false.
 *
 * @param <T>
 */
public final class ErrorSkippedIterator<T> implements Iterator<T> {
    private static final Object NONE = new Object();
    private final Iterator<? extends T> iter;
    private Object next = NONE;

    ErrorSkippedIterator(Iterator<? extends T> iter) {
        this.iter = iter;
    }

    public static <T> ErrorSkippedIterator<T> of(final Iterator<? extends T> iter) {
        return new ErrorSkippedIterator<T>(iter);
    }

    public static <T> ErrorSkippedIterator<T> of(final T[] a) {
        return of(a, 0, a.length);
    }

    public static <T> ErrorSkippedIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        Stream.checkIndex(fromIndex, toIndex, a.length);

        return new ErrorSkippedIterator<T>(new Iterator<T>() {
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
        while (next == NONE) {
            try {
                if (iter.hasNext()) {
                    next = iter.next();
                }

                break;
            } catch (Throwable e) {
                // continue;
            }
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
