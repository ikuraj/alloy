/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;
import java.io.Serializable;

/**
 * This implements an unmodifiable map.
 *
 * @param <K> - the type of key
 * @param <V> - the type of value
 */

public final class ConstMap<K,V> implements Serializable, Map<K,V> {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The underlying Collections.unmodifiableMap map. */
    private final Map<K,V> map;

    /** This caches a readonly empty map. */
    private static final ConstMap<Object,Object> emptymap = new ConstMap<Object,Object>(new HashMap<Object,Object>(1));

    /** Construct an unmodifiable map with the given map as the backing store. */
    private ConstMap(Map<? extends K,? extends V> map) {
        this.map = Collections.unmodifiableMap(map);
    }

    /** Return an unmodifiable empty map. */
    @SuppressWarnings("unchecked")
    public static<K,V> ConstMap<K,V> make() {
        return (ConstMap<K,V>) emptymap;
    }

    /**
     * Return an unmodifiable map with the same entries as the given map.
     * (If map==null, we'll return an unmodifiable empty map)
     */
    public static<K,V> ConstMap<K,V> make(Map<K,V> map) {
        if (map instanceof ConstMap) return (ConstMap<K,V>)map;
        if (map==null || map.isEmpty()) return make(); else return new ConstMap<K,V>(new LinkedHashMap<K,V>(map));
    }

    /** {@inheritDoc} */
    @Override public boolean equals(Object that) { return this==that || map.equals(that); }

    /** {@inheritDoc} */
    @Override public int hashCode() { return map.hashCode(); }

    /** {@inheritDoc} */
    @Override public String toString() { return map.toString(); }

    /** {@inheritDoc} */
    public int size() { return map.size(); }

    /** {@inheritDoc} */
    public boolean isEmpty() { return map.isEmpty(); }

    /** {@inheritDoc} */
    public Set<Map.Entry<K,V>> entrySet() { return map.entrySet(); }

    /** {@inheritDoc} */
    public Set<K> keySet() { return map.keySet(); }

    /** {@inheritDoc} */
    public Collection<V> values() { return map.values(); }

    /** {@inheritDoc} */
    public boolean containsKey(Object key) { return map.containsKey(key); }

    /** {@inheritDoc} */
    public boolean containsValue(Object value) { return map.containsValue(value); }

    /** {@inheritDoc} */
    public V get(Object key) { return map.get(key); }

    /** This map is readonly, so this method always throws UnsupportedOperationException. */
    public V remove(Object key) { throw new UnsupportedOperationException(); }

    /** This map is readonly, so this method always throws UnsupportedOperationException. */
    public V put(K key, V value) { throw new UnsupportedOperationException(); }

    /** This map is readonly, so this method always throws UnsupportedOperationException. */
    public void putAll(Map<? extends K, ? extends V> t) { throw new UnsupportedOperationException(); }

    /** This map is readonly, so this method always throws UnsupportedOperationException. */
    public void clear() { throw new UnsupportedOperationException(); }
}
