package edu.mit.csail.sdg.alloy4;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.LinkedList;

/**
 * Mutable; this implements a String-to-Value map that supports the undo operation.
 * Null key and null values are allowed.
 *
 * <p/>   To be more precise, every key is internally mapped to a list of values.
 * <br/>  The put(X,Y)  method appends Y onto the end of X's list.
 * <br/>  The get(X)    method returns the last element in X's list.
 * <br/>  The remove(X) method removes the last element in X's list.
 *
 * <p/>   This is very useful for representing lexical scoping: when a local
 *        variable is introduced with the same name as an existing variable,
 *        the new variable "hides" the old mapping; and when the new variable falls
 *        out of scope, the previous mapping is once again "revealed".
 *
 * <p/><b>Invariant:</b>  map2.containsKey(x) => (map1.containsKey(x) && map2.get(x).size()>0)
 *
 * <p/><b>Thread Safety:</b>  Safe.
 *
 * @param <V> - the type for Value
 */

public final class Env<V> {

    /**
     * If a key is bound to one or more values, this stores the first value.
     * <p/>
     * For example: if key K is bound to values V1..Vn, then map1.get(K)==V1
     */
    private final Map<String,V> map1=new LinkedHashMap<String,V>();

    /**
     * If a key is bound to more than one value, this stores every value except the first value.
     * <p/>
     * For example: if key K is bound to values V1..Vn, then map2.get(K) returns the list V2..Vn
     */
    private final Map<String,LinkedList<V>> map2=new LinkedHashMap<String,LinkedList<V>>();

    /** Constructs an empty environement. */
    public Env () { }

    /**
     * Returns true if the key is mapped to one or more values.
     *
     * @param key - the key
     * @return true if the key is mapped to one or more values
     */
    public synchronized boolean has(String key) {
        return map1.containsKey(key);
    }

    /**
     * Returns the latest value associated with the key (and returns null if none).
     *
     * <p/>
     * Since null is also a possible value, if you get null as the answer,
     * you need to call has(key) to determine whether the key really has a mapping or not.
     *
     * @param key - the key
     * @return the latest value associated with the key (and returns null if none).
     */
    public synchronized V get(String key) {
        LinkedList<V> list=map2.get(key);
        if (list!=null) return list.getLast(); else return map1.get(key);
    }

    /**
     * Associates the key with the value.
     *
     * @param key - the key
     * @param value - the value (which can be null)
     */
    public synchronized void put(String key, V value) {
        LinkedList<V> list=map2.get(key);
        if (list!=null) {
            list.add(value);
        } else if (!map1.containsKey(key)) {
            map1.put(key,value);
        } else {
            list=new LinkedList<V>();
            list.add(value);
            map2.put(key,list);
        }
    }

    /**
     * Removes the latest mapping for the key (and if the key had previous mappings, they become visible).
     *
     * If there are no mappings for the key, then this method does nothing.
     *
     * @param key - the key
     */
    public synchronized void remove(String key) {
        LinkedList<V> list=map2.get(key);
        if (list==null) {
            map1.remove(key);
        } else if (list.size()<=1) {
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
