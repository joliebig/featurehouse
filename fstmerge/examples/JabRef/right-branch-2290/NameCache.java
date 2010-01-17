package net.sf.jabref;

import java.util.HashMap;


public class NameCache extends HashMap {

  int max, toAdd;

  public NameCache(int max_) {
    max = max_;
    toAdd = max_;
  }

  public Object put(Object key, Object o) {
    super.put(key, o);
    if (size() > max) {

      
      
      clear();
      max += toAdd;
      super.put(key, o);
    }

    return o;
  }
}
