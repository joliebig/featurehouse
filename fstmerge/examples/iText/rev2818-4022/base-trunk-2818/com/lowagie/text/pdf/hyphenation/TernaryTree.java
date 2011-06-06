

package com.lowagie.text.pdf.hyphenation;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Stack;



public class TernaryTree implements Cloneable, Serializable {

    

    private static final long serialVersionUID = 5313366505322983510L;

    
    protected char[] lo;

    
    protected char[] hi;

    
    protected char[] eq;

    
    protected char[] sc;

    
    protected CharVector kv;

    protected char root;
    protected char freenode;
    protected int length;    

    protected static final int BLOCK_SIZE = 2048;    

    TernaryTree() {
        init();
    }

    protected void init() {
        root = 0;
        freenode = 1;
        length = 0;
        lo = new char[BLOCK_SIZE];
        hi = new char[BLOCK_SIZE];
        eq = new char[BLOCK_SIZE];
        sc = new char[BLOCK_SIZE];
        kv = new CharVector();
    }

    
    public void insert(String key, char val) {
        
        int len = key.length()
                  + 1;    
        if (freenode + len > eq.length) {
            redimNodeArrays(eq.length + BLOCK_SIZE);
        }
        char strkey[] = new char[len--];
        key.getChars(0, len, strkey, 0);
        strkey[len] = 0;
        root = insert(root, strkey, 0, val);
    }

    public void insert(char[] key, int start, char val) {
        int len = strlen(key) + 1;
        if (freenode + len > eq.length) {
            redimNodeArrays(eq.length + BLOCK_SIZE);
        }
        root = insert(root, key, start, val);
    }

    
    private char insert(char p, char[] key, int start, char val) {
        int len = strlen(key, start);
        if (p == 0) {
            
            
            
            p = freenode++;
            eq[p] = val;           
            length++;
            hi[p] = 0;
            if (len > 0) {
                sc[p] = 0xFFFF;    
                lo[p] = (char)kv.alloc(len
                                       + 1);    
                strcpy(kv.getArray(), lo[p], key, start);
            } else {
                sc[p] = 0;
                lo[p] = 0;
            }
            return p;
        }

        if (sc[p] == 0xFFFF) {
            
            
            
            char pp = freenode++;
            lo[pp] = lo[p];    
            eq[pp] = eq[p];    
            lo[p] = 0;
            if (len > 0) {
                sc[p] = kv.get(lo[pp]);
                eq[p] = pp;
                lo[pp]++;
                if (kv.get(lo[pp]) == 0) {
                    
                    lo[pp] = 0;
                    sc[pp] = 0;
                    hi[pp] = 0;
                } else {
                    
                    sc[pp] = 0xFFFF;
                }
            } else {
                
                
                sc[pp] = 0xFFFF;
                hi[p] = pp;
                sc[p] = 0;
                eq[p] = val;
                length++;
                return p;
            }
        }
        char s = key[start];
        if (s < sc[p]) {
            lo[p] = insert(lo[p], key, start, val);
        } else if (s == sc[p]) {
            if (s != 0) {
                eq[p] = insert(eq[p], key, start + 1, val);
            } else {
                
                eq[p] = val;
            }
        } else {
            hi[p] = insert(hi[p], key, start, val);
        }
        return p;
    }

    
    public static int strcmp(char[] a, int startA, char[] b, int startB) {
        for (; a[startA] == b[startB]; startA++, startB++) {
            if (a[startA] == 0) {
                return 0;
            }
        }
        return a[startA] - b[startB];
    }

    
    public static int strcmp(String str, char[] a, int start) {
        int i, d, len = str.length();
        for (i = 0; i < len; i++) {
            d = (int)str.charAt(i) - a[start + i];
            if (d != 0) {
                return d;
            }
            if (a[start + i] == 0) {
                return d;
        }
        }
        if (a[start + i] != 0) {
            return (int)-a[start + i];
        }
        return 0;

    }

    public static void strcpy(char[] dst, int di, char[] src, int si) {
        while (src[si] != 0) {
            dst[di++] = src[si++];
        }
        dst[di] = 0;
    }

    public static int strlen(char[] a, int start) {
        int len = 0;
        for (int i = start; i < a.length && a[i] != 0; i++) {
            len++;
        }
        return len;
    }

    public static int strlen(char[] a) {
        return strlen(a, 0);
    }

    public int find(String key) {
        int len = key.length();
        char strkey[] = new char[len + 1];
        key.getChars(0, len, strkey, 0);
        strkey[len] = 0;

        return find(strkey, 0);
    }

    public int find(char[] key, int start) {
        int d;
        char p = root;
        int i = start;
        char c;

        while (p != 0) {
            if (sc[p] == 0xFFFF) {
                if (strcmp(key, i, kv.getArray(), lo[p]) == 0) {
                    return eq[p];
                } else {
                    return -1;
            }
            }
            c = key[i];
            d = c - sc[p];
            if (d == 0) {
                if (c == 0) {
                    return eq[p];
                }
                i++;
                p = eq[p];
            } else if (d < 0) {
                p = lo[p];
            } else {
                p = hi[p];
        }
        }
        return -1;
    }

    public boolean knows(String key) {
        return (find(key) >= 0);
    }

    
    private void redimNodeArrays(int newsize) {
        int len = newsize < lo.length ? newsize : lo.length;
        char[] na = new char[newsize];
        System.arraycopy(lo, 0, na, 0, len);
        lo = na;
        na = new char[newsize];
        System.arraycopy(hi, 0, na, 0, len);
        hi = na;
        na = new char[newsize];
        System.arraycopy(eq, 0, na, 0, len);
        eq = na;
        na = new char[newsize];
        System.arraycopy(sc, 0, na, 0, len);
        sc = na;
    }

    public int size() {
        return length;
    }

    public Object clone() {
        TernaryTree t = new TernaryTree();
        t.lo = (char[])this.lo.clone();
        t.hi = (char[])this.hi.clone();
        t.eq = (char[])this.eq.clone();
        t.sc = (char[])this.sc.clone();
        t.kv = (CharVector)this.kv.clone();
        t.root = this.root;
        t.freenode = this.freenode;
        t.length = this.length;

        return t;
    }

    
    protected void insertBalanced(String[] k, char[] v, int offset, int n) {
        int m;
        if (n < 1) {
            return;
        }
        m = n >> 1;

        insert(k[m + offset], v[m + offset]);
        insertBalanced(k, v, offset, m);

        insertBalanced(k, v, offset + m + 1, n - m - 1);
    }


    
    public void balance() {
        

        int i = 0, n = length;
        String[] k = new String[n];
        char[] v = new char[n];
        Iterator iter = new Iterator();
        while (iter.hasMoreElements()) {
            v[i] = iter.getValue();
            k[i++] = (String)iter.nextElement();
        }
        init();
        insertBalanced(k, v, 0, n);

        
        
    }

    
    public void trimToSize() {
        
        balance();

        
        redimNodeArrays(freenode);

        
        CharVector kx = new CharVector();
        kx.alloc(1);
        TernaryTree map = new TernaryTree();
        compact(kx, map, root);
        kv = kx;
        kv.trimToSize();
    }

    private void compact(CharVector kx, TernaryTree map, char p) {
        int k;
        if (p == 0) {
            return;
        }
        if (sc[p] == 0xFFFF) {
            k = map.find(kv.getArray(), lo[p]);
            if (k < 0) {
                k = kx.alloc(strlen(kv.getArray(), lo[p]) + 1);
                strcpy(kx.getArray(), k, kv.getArray(), lo[p]);
                map.insert(kx.getArray(), k, (char)k);
            }
            lo[p] = (char)k;
        } else {
            compact(kx, map, lo[p]);
            if (sc[p] != 0) {
                compact(kx, map, eq[p]);
            }
            compact(kx, map, hi[p]);
        }
    }


    public Enumeration keys() {
        return new Iterator();
    }

    public class Iterator implements Enumeration {

        
        int cur;

        
        String curkey;

        private class Item implements Cloneable {
            char parent;
            char child;

            public Item() {
                parent = 0;
                child = 0;
            }

            public Item(char p, char c) {
                parent = p;
                child = c;
            }

            public Object clone() {
                return new Item(parent, child);
            }

        }

        
        Stack ns;

        
        StringBuffer ks;

        public Iterator() {
            cur = -1;
            ns = new Stack();
            ks = new StringBuffer();
            rewind();
        }

        public void rewind() {
            ns.removeAllElements();
            ks.setLength(0);
            cur = root;
            run();
        }

        public Object nextElement() {
            String res = curkey;
            cur = up();
            run();
            return res;
        }

        public char getValue() {
            if (cur >= 0) {
                return eq[cur];
            }
            return 0;
        }

        public boolean hasMoreElements() {
            return (cur != -1);
        }

        
        private int up() {
            Item i = new Item();
            int res = 0;

            if (ns.empty()) {
                return -1;
            }

            if (cur != 0 && sc[cur] == 0) {
                return lo[cur];
            }

            boolean climb = true;

            while (climb) {
                i = (Item)ns.pop();
                i.child++;
                switch (i.child) {
                case 1:
                    if (sc[i.parent] != 0) {
                        res = eq[i.parent];
                        ns.push(i.clone());
                        ks.append(sc[i.parent]);
                    } else {
                        i.child++;
                        ns.push(i.clone());
                        res = hi[i.parent];
                    }
                    climb = false;
                    break;

                case 2:
                    res = hi[i.parent];
                    ns.push(i.clone());
                    if (ks.length() > 0) {
                        ks.setLength(ks.length() - 1);    
                    }
                    climb = false;
                    break;

                default:
                    if (ns.empty()) {
                        return -1;
                    }
                    climb = true;
                    break;
                }
            }
            return res;
        }

        
        private int run() {
            if (cur == -1) {
                return -1;
            }

            boolean leaf = false;
            while (true) {
                
                while (cur != 0) {
                    if (sc[cur] == 0xFFFF) {
                        leaf = true;
                        break;
                    }
                    ns.push(new Item((char)cur, '\u'));
                    if (sc[cur] == 0) {
                        leaf = true;
                        break;
                    }
                    cur = lo[cur];
                }
                if (leaf) {
                    break;
                }
                    
                cur = up();
                if (cur == -1) {
                    return -1;
                }
            }
            
            
            StringBuffer buf = new StringBuffer(ks.toString());
            if (sc[cur] == 0xFFFF) {
                int p = lo[cur];
                while (kv.get(p) != 0) {
                    buf.append(kv.get(p++));
            }
            }
            curkey = buf.toString();
            return 0;
        }

    }

    public void printStats() {
        System.out.println("Number of keys = " + Integer.toString(length));
        System.out.println("Node count = " + Integer.toString(freenode));
        
        System.out.println("Key Array length = "
                           + Integer.toString(kv.length()));

        

    }



}

