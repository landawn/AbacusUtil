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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * It's an extension and wrapper for Google Guava.
 * 
 * @since 1.2.7
 * 
 * @author Haiyang Li 
 */
public final class Iterables {

    private Iterables() {
        // singleton.
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     */
    public static <T> Set<T> commonElements(final Collection<? extends T> a, final Collection<?> b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return new HashSet<>();
        }

        return commonElements(N.asList(a, (Collection<? extends T>) b));
    }

    public static <T> Set<T> commonElements(final Collection<? extends Collection<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return new HashSet<>();
        } else if (c.size() == 1) {
            return N.newHashSet(c.iterator().next());
        }

        Collection<? extends T> smallest = null;

        for (Collection<? extends T> e : c) {
            if (N.isNullOrEmpty(e)) {
                return new HashSet<>();
            }

            if (smallest == null || e.size() < smallest.size()) {
                smallest = e;
            }
        }

        final Map<T, MutableInt> map = new HashMap<>();

        for (T e : smallest) {
            map.put(e, new MutableInt(1));
        }

        int cnt = 1;
        MutableInt val = null;

        for (Collection<? extends T> ec : c) {
            if (ec == smallest) {
                continue;
            }

            for (T e : ec) {
                val = map.get(e);

                if (val == null) {
                    // do nothing.
                } else if (val.intValue() < cnt) {
                    // map.remove(e);
                } else if (val.intValue() == cnt) {
                    val.increment();
                }
            }

            cnt++;
        }

        final Set<T> result = new HashSet<>(map.size());

        for (Map.Entry<T, MutableInt> entry : map.entrySet()) {
            if (entry.getValue().intValue() == cnt) {
                result.add(entry.getKey());
            }
        }

        return result;
    }

    public static boolean removeAll(Collection<?> c, Collection<?> objsToRemove) {
        if (N.isNullOrEmpty(c) || N.isNullOrEmpty(objsToRemove)) {
            return false;
        }

        if (c instanceof HashSet && !(objsToRemove instanceof Set)) {
            boolean result = false;

            for (Object e : objsToRemove) {
                result |= c.remove(e);

                if (c.size() == 0) {
                    break;
                }
            }

            return result;
        } else {
            return c.removeAll(objsToRemove);
        }
    }

    public static boolean retainAll(Collection<?> c, Collection<?> objsToKeep) {
        if (N.isNullOrEmpty(c)) {
            return false;
        } else if (N.isNullOrEmpty(objsToKeep)) {
            c.clear();
            return true;
        }

        if (c instanceof HashSet && !(objsToKeep instanceof Set) && (c.size() > 9 || objsToKeep.size() > 9)) {
            return c.retainAll(new HashSet<>(objsToKeep));
        } else {
            return c.retainAll(objsToKeep);
        }
    }
}
