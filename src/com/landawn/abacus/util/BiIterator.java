/*
 * Copyright (c) 2018, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.landawn.abacus.util;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BooleanSupplier;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 1.2.10
 * 
 * @author Haiyang Li
 */
public abstract class BiIterator<A, B> extends ObjIterator<Pair<A, B>> {

    /**
     * Returns an infinite {@code BiIterator}.
     * 
     * @param output transfer the next values.
     * @return
     */
    public static <A, B> BiIterator<A, B> generate(final Consumer<Pair<A, B>> output) {
        return generate(BooleanSupplier.TRUE, output);
    }

    /**
     * 
     * @param hasNext
     * @param output
     * @return
     */
    public static <A, B> BiIterator<A, B> generate(final BooleanSupplier hasNext, final Consumer<Pair<A, B>> output) {
        N.checkArgNotNull(hasNext);
        N.checkArgNotNull(output);

        return new BiIterator<A, B>() {
            private final Pair<A, B> tmp = new Pair<A, B>();

            @Override
            public boolean hasNext() {
                return hasNext.getAsBoolean();
            }

            @Override
            public Pair<A, B> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                output.accept(tmp);

                return Pair.of(tmp.left, tmp.right);
            }

            @Override
            public <E extends Exception> void forEachRemaining(final Try.BiConsumer<A, B, E> action) throws E {
                N.checkArgNotNull(action);

                while (hasNext.getAsBoolean()) {
                    output.accept(tmp);

                    action.accept(tmp.left, tmp.right);
                }
            }

            @Override
            public <R> ObjIterator<R> map(final BiFunction<A, B, R> mapper) {
                N.checkArgNotNull(mapper);

                return new ObjIterator<R>() {
                    @Override
                    public boolean hasNext() {
                        return hasNext.getAsBoolean();
                    }

                    @Override
                    public R next() {
                        if (hasNext() == false) {
                            throw new NoSuchElementException();
                        }

                        output.accept(tmp);

                        return mapper.apply(tmp.left, tmp.right);
                    }
                };
            }
        };
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param output
     * @return
     */
    public static <A, B> BiIterator<A, B> generate(final int fromIndex, final int toIndex, final IndexedConsumer<Pair<A, B>> output) {
        N.checkFromToIndex(fromIndex, toIndex, Integer.MAX_VALUE);
        N.checkArgNotNull(output);

        return new BiIterator<A, B>() {
            private final MutableInt cursor = MutableInt.of(fromIndex);
            private final Pair<A, B> tmp = new Pair<A, B>();

            @Override
            public boolean hasNext() {
                return cursor.value() < toIndex;
            }

            @Override
            public Pair<A, B> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                output.accept(cursor.getAndIncrement(), tmp);

                return Pair.of(tmp.left, tmp.right);
            }

            @Override
            public <E extends Exception> void forEachRemaining(final Try.BiConsumer<A, B, E> action) throws E {
                N.checkArgNotNull(action);

                while (cursor.value() < toIndex) {
                    output.accept(cursor.getAndIncrement(), tmp);

                    action.accept(tmp.left, tmp.right);
                }
            }

            @Override
            public <R> ObjIterator<R> map(final BiFunction<A, B, R> mapper) {
                N.checkArgNotNull(mapper);

                return new ObjIterator<R>() {
                    @Override
                    public boolean hasNext() {
                        return cursor.value() < toIndex;
                    }

                    @Override
                    public R next() {
                        if (hasNext() == false) {
                            throw new NoSuchElementException();
                        }

                        output.accept(cursor.getAndIncrement(), tmp);

                        return mapper.apply(tmp.left, tmp.right);
                    }
                };
            }
        };
    }

    public abstract <E extends Exception> void forEachRemaining(final Try.BiConsumer<A, B, E> action) throws E;

    public abstract <R> ObjIterator<R> map(final BiFunction<A, B, R> mapper);

    public <R> Stream<R> stream(final BiFunction<A, B, R> mapper) {
        N.checkArgNotNull(mapper);

        return Stream.of(map(mapper));
    }
}
