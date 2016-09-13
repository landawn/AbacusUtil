package com.landawn.abacus.util.stream;

import java.util.Iterator;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;

/**
 * Ignore any error happened at calling <code>hasNext()</code> or <code>next()</code> and returns false for <code>hasNext()</code>.
 *
 * @param <T>
 */
public final class ErrorBreakIterator<T> implements Iterator<T> {
    private static final Object NONE = new Object();
    private final Iterator<? extends T> iter;
    private final int maxRetries;
    private final int retryInterval;
    private Object next = NONE;

    ErrorBreakIterator(final Iterator<? extends T> iter, final int maxRetries, final int retryInterval) {
        this.iter = iter;
        this.maxRetries = maxRetries;
        this.retryInterval = retryInterval;
    }

    /**
     * 
     * @param iter
     * @return
     */
    public static <T> ErrorBreakIterator<T> of(final Iterator<? extends T> iter) {
        return new ErrorBreakIterator<T>(iter, 0, 1000);
    }

    /**
     * 
     * @param iter
     * @param maxRetries default value is 0, which means no retry.
     * @param retryInterval default value 1000. unit is milliseconds.
     * @return
     */
    public static <T> ErrorBreakIterator<T> of(final Iterator<? extends T> iter, final int maxRetries, final int retryInterval) {
        return new ErrorBreakIterator<T>(iter, maxRetries, retryInterval);
    }

    @Override
    public boolean hasNext() {
        if (next == NONE) {
            try {
                if (iter.hasNext()) {
                    next = iter.next();
                }
            } catch (Throwable e) {
                if (maxRetries > 0) {
                    int retriedTimes = 0;

                    while (retriedTimes++ < maxRetries) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            if (iter.hasNext()) {
                                next = iter.next();
                            }

                            break;
                        } catch (Throwable e2) {
                            // ignore.
                        }
                    }
                }
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
