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

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;

/**
 * Immutable; this implements an unmodifiable map.
 *
 * <p><b>Thread Safety:</b>  Safe.
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
    private static final ConstMap<Object,Object> emptymap = new ConstMap<Object,Object>();

    /** Construct an unmodifiable empty map. */
    private ConstMap() {
        this.map=Collections.unmodifiableMap(new HashMap<K,V>(1));
    }

    /** Construct an unmodifiable map containing the entries from the given map. */
    private ConstMap(Map<K,V> map) {
        this.map=Collections.unmodifiableMap(new LinkedHashMap<K,V>(map));
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
    @SuppressWarnings("unchecked")
    public static<K,V> ConstMap<K,V> make(Map<K,V> map) {
        if (map instanceof ConstMap) return (ConstMap<K,V>)map;
        else if (map==null || map.isEmpty()) return (ConstMap<K,V>)emptymap;
        else return new ConstMap<K,V>(map);
    }

    /** Returns true if that is a Map with the same entries as this map. */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof Map)) return false;
        return map.equals(that);
    }

    /** Computes a hash code that is consistent with equals(). */
    @Override public int hashCode() { return map.hashCode(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public int size() { return map.size(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public boolean isEmpty() { return map.isEmpty(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public Set<Map.Entry<K,V>> entrySet() { return map.entrySet(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public Set<K> keySet() { return map.keySet(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public Collection<V> values() { return map.values(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public boolean containsKey(Object key) { return map.containsKey(key); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public boolean containsValue(Object value) { return map.containsValue(value); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public V get(Object key) { return map.get(key); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public V put(K key, V value) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public V remove(Object key) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public void putAll(Map<? extends K, ? extends V> t) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Map
     * @inheritDoc
     */
    public void clear() { throw new UnsupportedOperationException(); }
}
