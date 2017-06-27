/*
 * Copyright (c) 2015, Haiyang Li.
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

import java.util.Collection;
import java.util.Set;

/**
 * It's designed to supported primitive/object array.
 * The elements in the array must not be modified after the array is added into the set.
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public class ArrayHashSet<E> extends XHashSet<E> {

    public ArrayHashSet() {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction);
    }

    public ArrayHashSet(final int initialCapacity) {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction, initialCapacity);
    }

    @SuppressWarnings("rawtypes")
    public ArrayHashSet(final Class<? extends Set> setType) {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction, setType);
    }

    public ArrayHashSet(final Collection<? extends E> coll) {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction);

        addAll(coll);
    }
}
