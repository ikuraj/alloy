/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.LinkedList;

/**
 * Mutable; this implements a Key-to-Value map that supports the undo operation; null key and null values are allowed.
 *
 * <p>   To be more precise, every key is internally mapped to a list of values.
 * <br>  The put(X,Y)  method appends Y onto the end of X's list.
 * <br>  The get(X)    method returns the last element in X's list.
 * <br>  The remove(X) method removes the last element in X's list.
 *
 * <p>   This is very useful for representing lexical scoping: when a local
 *       variable is introduced with the same name as an existing variable,
 *       the new variable "hides" the old mapping; and when the new variable falls
 *       out of scope, the previous mapping is once again "revealed".
 *
 * <p><b>Thread Safety:</b>  Safe
 *
 * @param <V> - the type for Value
 */

public final class Env<K,V> {

    // Invariant:  map2.containsKey(x) implies (map1.containsKey(x) && map2.get(x).size()>0)

    /**
     * If a key is bound to one or more values, this stores the first value.
     * <p>
     * For example: if key K is bound to values V1..Vn, then map1.get(K) returns V1
     */
    private final Map<K,V> map1 = new LinkedHashMap<K,V>();

    /**
     * If a key is bound to more than one value, this stores every value except the first value.
     * <p>
     * For example: if key K is bound to values V1..Vn, then map2.get(K) returns the sublist V2..Vn
     */
    private final Map<K,LinkedList<V>> map2 = new LinkedHashMap<K,LinkedList<V>>();

    /** Constructs an initially empty environment. */
    public Env () { }

    /**
     * Returns true if the key is mapped to one or more values.
     *
     * @param key - the key
     * @return true if the key is mapped to one or more values
     */
    public synchronized boolean has (K key) {
        return map1.containsKey(key);
    }

    /**
     * Returns the latest value associated with the key (and returns null if none).
     *
     * <p>
     * Since null is also a possible value, if you get null as the answer,
     * you need to call has(key) to determine whether the key really has a mapping or not.
     *
     * @param key - the key
     * @return the latest value associated with the key (and returns null if none)
     */
    public synchronized V get (K key) {
        LinkedList<V> list = map2.get(key);
        return (list != null) ? list.getLast() : map1.get(key);
    }

    /**
     * Associates the key with the value.
     *
     * @param key - the key
     * @param value - the value (which can be null)
     */
    public synchronized void put (K key, V value) {
        LinkedList<V> list = map2.get(key);
        if (list != null) {
            list.add(value);
        } else if (!map1.containsKey(key)) {
            map1.put(key, value);
        } else {
            list=new LinkedList<V>();
            list.add(value);
            map2.put(key, list);
        }
    }

    /**
     * Removes the latest mapping for the key (and if the key had previous mappings, they become visible).
     *
     * If there are no mappings for the key, then this method does nothing.
     *
     * @param key - the key
     */
    public synchronized void remove(K key) {
        final LinkedList<V> list = map2.get(key);
        if (list == null) {
            map1.remove(key);
        } else if (list.size() <= 1) {
            map2.remove(key);
        } else {
            list.removeLast();
        }
    }

    /** Removes all mappings. */
    public synchronized void clear() {
        map1.clear();
        map2.clear();
    }
}
