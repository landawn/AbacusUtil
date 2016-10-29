package com.landawn.abacus.util.stream;

import static com.landawn.abacus.util.stream.StreamBase.checkIndex;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.N;

public abstract class ImmutableIntIterator implements IntIterator {

    public static ImmutableIntIterator of(final int[] a) {
        return of(a, 0, a.length);
    }

    public static ImmutableIntIterator of(final int[] a, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex, a.length);

        return new ImmutableIntIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
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
            public int[] toArray() {
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

    public int[] toArray() {
        final IntList list = new IntList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
