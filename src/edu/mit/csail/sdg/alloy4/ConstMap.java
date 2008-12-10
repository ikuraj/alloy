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

import java.util.AbstractMap;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;
import java.io.Serializable;

/** This implements an unmodifiable map (where comparison is based on hashCode() and equals()); null keys and values are allowed.
 *
 * @param <K> - the type of key
 * @param <V> - the type of value
 */

public final class ConstMap<K,V> extends AbstractMap<K,V> implements Serializable {

   /** This ensures the class can be serialized reliably. */
   private static final long serialVersionUID = 0;

   /** The underlying Collections.unmodifiableMap map. */
   private final Map<K,V> map;

   /** This caches a read-only empty map. */
   private static final ConstMap<Object,Object> emptymap = new ConstMap<Object,Object>(new HashMap<Object,Object>(0));

   /** Constructs an unmodifiable map with the given map as the backing store. */
   private ConstMap(Map<? extends K,? extends V> map) {
      this.map = Collections.unmodifiableMap(map);
   }

   /** Returns an unmodifiable empty map. */
   @SuppressWarnings("unchecked")
   public static<K,V> ConstMap<K,V> make() {
      return (ConstMap<K,V>) emptymap;
   }

   /** Returns an unmodifiable map with the same entries and traversal order as the given map.
    * (If map==null, we'll return an unmodifiable empty map)
    */
   public static<K,V> ConstMap<K,V> make(Map<K,V> map) {
      if (map instanceof ConstMap) return (ConstMap<K,V>)map;
      if (map == null || map.isEmpty()) return make(); else return new ConstMap<K,V>(new LinkedHashMap<K,V>(map));
   }

   /** Returns a unmodifiable view of the mappings in this map. */
   @Override public Set<Map.Entry<K,V>> entrySet()      { return map.entrySet(); }

   /** Returns a unmodifiable view of the keys in this map. */
   @Override public Set<K> keySet()                     { return map.keySet(); }  // overridden for performance

   /** Returns a unmodifiable view of the values in this map. */
   @Override public Collection<V> values()              { return map.values(); }  // overridden for performance

   /** Returns the number of (key, value) mapping in this map. */
   @Override public int size()                          { return map.size(); }  // overridden for performance

   /** Returns true if the given key is in this map. */
   @Override public boolean containsKey(Object key)     { return map.containsKey(key); }  // overridden for performance

   /** Returns true if at least one key is mapped to the given value. */
   @Override public boolean containsValue(Object value) { return map.containsValue(value); }  // overridden for performance

   /** Returns the value associated with the given key (or null if the key is not bound in this map) */
   @Override public V get(Object key)                   { return map.get(key); }  // overridden for performance
}
