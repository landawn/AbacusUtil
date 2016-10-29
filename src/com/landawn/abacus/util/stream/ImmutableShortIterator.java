package com.landawn.abacus.util.stream;

import static com.landawn.abacus.util.stream.StreamBase.checkIndex;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;

public abstract class ImmutableShortIterator implements ShortIterator {

    public static ImmutableShortIterator of(final short[] a) {
        return of(a, 0, a.length);
    }

    public static ImmutableShortIterator of(final short[] a, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex, a.length);

        return new ImmutableShortIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short next() {
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
            public short[] toArray() {
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

    public short[] toArray() {
        final ShortList list = new ShortList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
