package edu.mit.csail.sdg.alloy4.core;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.LinkedList;

public final class Env { // mutable

  private final Map<String,Object> map1=new LinkedHashMap<String,Object>();
  private final Map<String,LinkedList<Object>> map2=new LinkedHashMap<String,LinkedList<Object>>();

  public boolean isin(String k) {return map2.containsKey(k) || map1.containsKey(k);}

  public Object get(String k) {LinkedList<Object> y=map2.get(k); if (y!=null) return y.get(0); return map1.get(k);}

  public void put(String k,Object v) {
    LinkedList<Object> y=map2.get(k); if (y!=null) {y.add(0,v);return;}
    if (!map1.containsKey(k)) {map1.put(k,v);return;}
    y=new LinkedList<Object>();
    y.add(v);
    map2.put(k,y);
  }

  public void remove(String k) {
    LinkedList<Object> y=map2.get(k);
    if (y!=null) {y.removeFirst(); if (y.size()==0) map2.remove(k); return;}
    map1.remove(k);
  }

  public void clear() {
    map1.clear();
    map2.clear();
  }
}
