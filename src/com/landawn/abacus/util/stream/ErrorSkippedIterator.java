package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

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

    public static <T> ErrorSkippedIterator<T> of(final Collection<? extends T> c) {
        return new ErrorSkippedIterator<T>(c.iterator());
    }

    public static <T> ErrorSkippedIterator<T> of(final T[] a) {
        return new ErrorSkippedIterator<T>(new Iterator<T>() {
            private final int len = a.length;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < len;
            }

            @Override
            public T next() {
                if (cursor >= len) {
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
