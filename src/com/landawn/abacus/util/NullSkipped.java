package com.landawn.abacus.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class NullSkipped {

    public static <T> Iterator<T> of(final Iterator<T> iter) {
        return new Iterator<T>() {
            private T next;

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
        };
    }
}
