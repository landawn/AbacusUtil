package com.landawn.abacus.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class NullSkippedIterator<T> implements Iterator<T> {
    private final Iterator<T> iter;
    private T next;

    public NullSkippedIterator(Iterator<T> iter) {
        this.iter = iter;
    }

    public static <T> NullSkippedIterator<T> of(Iterator<T> iter) {
        return new NullSkippedIterator<>(iter);
    }

    @Override
    public boolean hasNext() {
        if (next == null && iter.hasNext()) {
            next = iter.next();

            if (next == null) {
                while (iter.hasNext()) {
                    next = iter.next();

                    if (next != null) {
                        break;
                    }
                }
            }
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
        iter.remove();
    }
}
