

package com.lowagie.text.pdf;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;


public class IntHashtable implements Cloneable {

    
    private transient Entry table[];

    
    private transient int count;

    
    private int threshold;

    
    private float loadFactor;

    
    public IntHashtable() {
        this(150, 0.75f);
    }

    
    public IntHashtable(int initialCapacity) {
        this(initialCapacity, 0.75f);
    }

    
    public IntHashtable(int initialCapacity, float loadFactor) {
        super();
        if (initialCapacity < 0) {
            throw new IllegalArgumentException("Illegal Capacity: " + initialCapacity);
        }
        if (loadFactor <= 0) {
            throw new IllegalArgumentException("Illegal Load: " + loadFactor);
        }
        if (initialCapacity == 0) {
            initialCapacity = 1;
        }
        this.loadFactor = loadFactor;
        table = new Entry[initialCapacity];
        threshold = (int) (initialCapacity * loadFactor);
    }

    
    public int size() {
        return count;
    }

    
    public boolean isEmpty() {
        return count == 0;
    }

    
    public boolean contains(int value) {

        Entry tab[] = table;
        for (int i = tab.length; i-- > 0;) {
            for (Entry e = tab[i]; e != null; e = e.next) {
                if (e.value == value) {
                    return true;
                }
            }
        }
        return false;
     }

    
    public boolean containsValue(int value) {
        return contains(value);
    }

    
    public boolean containsKey(int key) {
        Entry tab[] = table;
        int hash = key;
        int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index]; e != null; e = e.next) {
            if (e.hash == hash && e.key == key) {
                return true;
            }
        }
        return false;
    }

    
    public int get(int key) {
        Entry tab[] = table;
        int hash = key;
        int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index]; e != null; e = e.next) {
            if (e.hash == hash && e.key == key) {
                return e.value;
            }
        }
        return 0;
    }

    
    protected void rehash() {
        int oldCapacity = table.length;
        Entry oldMap[] = table;

        int newCapacity = oldCapacity * 2 + 1;
        Entry newMap[] = new Entry[newCapacity];

        threshold = (int) (newCapacity * loadFactor);
        table = newMap;

        for (int i = oldCapacity; i-- > 0;) {
            for (Entry old = oldMap[i]; old != null;) {
                Entry e = old;
                old = old.next;

                int index = (e.hash & 0x7FFFFFFF) % newCapacity;
                e.next = newMap[index];
                newMap[index] = e;
            }
        }
    }

    
    public int put(int key, int value) {
        
        Entry tab[] = table;
        int hash = key;
        int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index]; e != null; e = e.next) {
            if (e.hash == hash && e.key == key) {
                int old = e.value;
                e.value = value;
                return old;
            }
        }

        if (count >= threshold) {
            
            rehash();

            tab = table;
            index = (hash & 0x7FFFFFFF) % tab.length;
        }
 
         
         Entry e = new Entry(hash, key, value, tab[index]);
         tab[index] = e;
         count++;
         return 0;
    }

    
    public int remove(int key) {
        Entry tab[] = table;
        int hash = key;
        int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index], prev = null; e != null; prev = e, e = e.next) {
            if (e.hash == hash && e.key == key) {
                if (prev != null) {
                    prev.next = e.next;
                } else {
                    tab[index] = e.next;
                }
                count--;
                int oldValue = e.value;
                e.value = 0;
                return oldValue;
            }
        }
        return 0;
    }

    
    public synchronized void clear() {
        Entry tab[] = table;
        for (int index = tab.length; --index >= 0;) {
            tab[index] = null;
        }
        count = 0;
    }
    
    
    static class Entry {
        int hash;
        int key;
        int value;
        Entry next;

        
        protected Entry(int hash, int key, int value, Entry next) {
            this.hash = hash;
            this.key = key;
            this.value = value;
            this.next = next;
        }
        
        
        public int getKey() {
            return key;
        }
        public int getValue() {
            return value;
        }
        protected Object clone() {
            Entry entry = new Entry(hash, key, value, (next != null) ? (Entry)next.clone() : null);
            return entry;
        }
    }
    
    
    static class IntHashtableIterator implements Iterator {
        int index;
        Entry table[];
        Entry entry;
        
        IntHashtableIterator(Entry table[]) {
            this.table = table;
            this.index = table.length;
        }
        public boolean hasNext() {
            if (entry != null) {
                return true;
            }
            while (index-- > 0) {
                if ((entry = table[index]) != null) {
                    return true;
                }
            }
            return false;
        }
        
        public Object next() {
            if (entry == null) {
                while ((index-- > 0) && ((entry = table[index]) == null));
            }
            if (entry != null) {
                Entry e = entry;
                entry = e.next;
                return e;
            }
            throw new NoSuchElementException("IntHashtableIterator");
        }
        public void remove() {
            throw new UnsupportedOperationException("remove() not supported.");
        }
    }
    


    public Iterator getEntryIterator() {
        return new IntHashtableIterator(table);
    }
    
    public int[] toOrderedKeys() {
        int res[] = getKeys();
        Arrays.sort(res);
        return res;
    }
    
    public int[] getKeys() {
        int res[] = new int[count];
        int ptr = 0;
        int index = table.length;
        Entry entry = null;
        while (true) {
            if (entry == null)
                while ((index-- > 0) && ((entry = table[index]) == null));
            if (entry == null)
                break;
            Entry e = entry;
            entry = e.next;
            res[ptr++] = e.key;
        }
        return res;
    }
    
    public int getOneKey() {
        if (count == 0)
            return 0;
        int index = table.length;
        Entry entry = null;
        while ((index-- > 0) && ((entry = table[index]) == null));
        if (entry == null)
            return 0;
        return entry.key;
    }
    
    public Object clone() {
        try {
            IntHashtable t = (IntHashtable)super.clone();
            t.table = new Entry[table.length];
            for (int i = table.length ; i-- > 0 ; ) {
                t.table[i] = (table[i] != null)
                ? (Entry)table[i].clone() : null;
            }
            return t;
        } catch (CloneNotSupportedException e) {
            
            throw new InternalError();
        }
    }
}