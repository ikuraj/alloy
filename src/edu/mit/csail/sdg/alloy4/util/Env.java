package edu.mit.csail.sdg.alloy4.util;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.LinkedList;

/**
 * Mutable; this implements an undoable mapping from nonnull String to nullable Object references.
 *
 * <p/>  To be more precise, every key is internally mapped to a list of values.
 * <br/> The put(X,Y)  method appends Y onto the end of X's list.
 * <br/> The get(X)    method returns the last element in X's list.
 * <br/> The remove(X) method removes the last element in X's list.
 *
 * <p/>
 * This is very useful for representing lexical scoping: when a local variable
 * is introduced with the same name as an existing variable, the new one "hides"
 * the old binding; and when the new variable falls out of scope,
 * the previous binding is once again "revealed".
 *
 * @author Felix Chang
 */

public final class Env {

    /**
     * If a key is bound to one or more values, this stores the first value.
     * <p/>
     * For example: if key K is bound to V1..Vn, then map1.get(K)==V1
     */
    private final Map<String,Object> map1=new LinkedHashMap<String,Object>();

    /**
     * If a key is bound to more than one value, this stores every value except the first value.
     * <p/>
     * For example: if key K is bound to values V1..Vn, then map2.get(K) returns the list V2..Vn
     */
    private final Map<String,LinkedList<Object>> map2=new LinkedHashMap<String,LinkedList<Object>>();

    /** Constructor that builds an empty environement. */
    public Env() { }

    /**
     * Returns true if the key k is mapped to one or more values.
     *
     * @param k - the key (which must not be null)
     * @return true if the key is mapped to one or more values
     */
    public boolean has(String k) {
        return map1.containsKey(k) || map2.containsKey(k);
    }

    /**
     * Returns the latest value associated with the key k (and returns null if none).
     *
     * <p/>
     * Since null is a possible value, if you get null as the answer,
     * you need to call has(k) to determine whether the key really has a binding or not.
     *
     * @param k - the key (which must not be null)
     * @return the latest value associated with key k (and returns null if none).
     */
    public Object get(String k) {
        LinkedList<Object> list=map2.get(k);
        if (list!=null) return list.getLast(); else return map1.get(k);
    }

    /**
     * Associates the key k with the value v.
     *
     * @param k - the key (which must not be null)
     * @param v - the value (which can be null)
     */
    public void put(String k,Object v) {
        LinkedList<Object> list=map2.get(k);
        if (list!=null) {
            list.add(v);
            return;
        }
        if (!map1.containsKey(k)) {
            map1.put(k,v);
            return;
        }
        list=new LinkedList<Object>();
        list.add(v);
        map2.put(k,list);
    }

    /**
     * Removes the latest binding for k (and if k had previous bindings, they become visible).
     *
     * If there are no mappings for k, then this method does nothing.
     *
     * @param k - the key (which must not be null)
     */
    public void remove(String k) {
        LinkedList<Object> list=map2.get(k);
        if (list!=null) {
            list.removeLast();
            if (list.size()==0) map2.remove(k);
            return;
        }
        map1.remove(k);
    }

    /** Removes all bindings. */
    public void clear() {
        map1.clear();
        map2.clear();
    }
}
