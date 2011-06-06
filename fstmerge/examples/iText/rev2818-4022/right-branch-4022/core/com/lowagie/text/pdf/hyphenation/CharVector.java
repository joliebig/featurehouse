

package com.lowagie.text.pdf.hyphenation;

import java.io.Serializable;


public class CharVector implements Cloneable, Serializable {

    private static final long serialVersionUID = -4875768298308363544L;
    
    private static final int DEFAULT_BLOCK_SIZE = 2048;
    private int blockSize;

    
    private char[] array;

    
    private int n;

    public CharVector() {
        this(DEFAULT_BLOCK_SIZE);
    }

    public CharVector(int capacity) {
        if (capacity > 0) {
            blockSize = capacity;
        } else {
            blockSize = DEFAULT_BLOCK_SIZE;
        }
        array = new char[blockSize];
        n = 0;
    }

    public CharVector(char[] a) {
        blockSize = DEFAULT_BLOCK_SIZE;
        array = a;
        n = a.length;
    }

    public CharVector(char[] a, int capacity) {
        if (capacity > 0) {
            blockSize = capacity;
        } else {
            blockSize = DEFAULT_BLOCK_SIZE;
        }
        array = a;
        n = a.length;
    }

    
    public void clear() {
        n = 0;
    }

    public Object clone() {
        CharVector cv = new CharVector(array.clone(), blockSize);
        cv.n = this.n;
        return cv;
    }

    public char[] getArray() {
        return array;
    }

    
    public int length() {
        return n;
    }

    
    public int capacity() {
        return array.length;
    }

    public void put(int index, char val) {
        array[index] = val;
    }

    public char get(int index) {
        return array[index];
    }

    public int alloc(int size) {
        int index = n;
        int len = array.length;
        if (n + size >= len) {
            char[] aux = new char[len + blockSize];
            System.arraycopy(array, 0, aux, 0, len);
            array = aux;
        }
        n += size;
        return index;
    }

    public void trimToSize() {
        if (n < array.length) {
            char[] aux = new char[n];
            System.arraycopy(array, 0, aux, 0, n);
            array = aux;
        }
    }

}
