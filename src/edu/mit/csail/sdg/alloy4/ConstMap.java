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
    private static final Map<Object,Object> emptymap = Collections.unmodifiableMap(new HashMap<Object,Object>(1));

    /** Construct an unmodifiable empty map. */
    @SuppressWarnings("unchecked")
    public ConstMap() {
        this.map=(Map<K,V>)emptymap;
    }

    /** Construct an unmodifiable map containing the entries from the given map. */
    @SuppressWarnings("unchecked")
    public ConstMap(Map<K,V> map) {
        if (map.size()==0)
            this.map=(Map<K,V>)emptymap;
        else if (map instanceof ConstMap)
            this.map=((ConstMap<K,V>)map).map;
        else
            this.map=Collections.unmodifiableMap(new LinkedHashMap<K,V>(map));
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
