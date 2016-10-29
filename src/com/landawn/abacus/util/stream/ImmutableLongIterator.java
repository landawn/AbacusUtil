package com.landawn.abacus.util.stream;

import static com.landawn.abacus.util.stream.StreamBase.checkIndex;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.N;

public abstract class ImmutableLongIterator implements LongIterator {

    public static ImmutableLongIterator of(final long[] a) {
        return of(a, 0, a.length);
    }

    public static ImmutableLongIterator of(final long[] a, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex, a.length);

        return new ImmutableLongIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
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
            public long[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
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

    public long[] toArray() {
        final LongList list = new LongList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
