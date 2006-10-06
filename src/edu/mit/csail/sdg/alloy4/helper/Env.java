package edu.mit.csail.sdg.alloy4.helper;

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
 *        the new variable "hides" the old binding; and when the new variable falls
 *        out of scope, the previous binding is once again "revealed".
 *
 * <p/><b>Invariant:</b>  map2.containsKey(x) => (map1.containsKey(x) && map2.get(x).size()>0)
 *
 * <p/><b>Thread Safety:</b>  Unsafe.
 * 
 * @param <V> - the type for Value
 *
 * @author Felix Chang
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

    /** Constructor that builds an empty environement. */
    public Env () { }

    /**
     * Returns true if the key k is mapped to one or more values.
     *
     * @param k - the key
     * @return true if the key is mapped to one or more values
     */
    public boolean has (String k) {
        return map1.containsKey(k);
    }

    /**
     * Returns the latest value associated with the key k (and returns null if none).
     *
     * <p/>
     * Since null is also a possible value, if you get null as the answer,
     * you need to call has(k) to determine whether the key really has a binding or not.
     *
     * @param k - the key
     * @return the latest value associated with key k (and returns null if none).
     */
    public V get (String k) {
        LinkedList<V> list=map2.get(k);
        if (list!=null) return list.getLast(); else return map1.get(k);
    }

    /**
     * Associates the key k with the value v.
     *
     * @param k - the key
     * @param v - the value (which can be null)
     */
    public void put (String k, V v) {
        LinkedList<V> list=map2.get(k);
        if (list!=null) {
            list.add(v);
        } else if (!map1.containsKey(k)) {
            map1.put(k,v);
        } else {
            list=new LinkedList<V>();
            list.add(v);
            map2.put(k,list);
        }
    }

    /**
     * Removes the latest binding for k (and if k had previous bindings, they become visible).
     *
     * If there are no mappings for k, then this method does nothing.
     *
     * @param k - the key
     */
    public void remove (String k) {
        LinkedList<V> list=map2.get(k);
        if (list==null) {
            map1.remove(k);
        } else if (list.size()<=1) {
            map2.remove(k);
        } else {
            list.removeLast();
        }
    }

    /** Removes all bindings. */
    public void clear() {
        map1.clear();
        map2.clear();
    }
}
