

package com.lowagie.text.pdf.hyphenation;

import java.io.Serializable;


public class ByteVector implements Serializable {

    private static final long serialVersionUID = -1096301185375029343L;
    
    private static final int DEFAULT_BLOCK_SIZE = 2048;
    private int blockSize;

    
    private byte[] array;

    
    private int n;

    public ByteVector() {
        this(DEFAULT_BLOCK_SIZE);
    }

    public ByteVector(int capacity) {
        if (capacity > 0) {
            blockSize = capacity;
        } else {
            blockSize = DEFAULT_BLOCK_SIZE;
        }
        array = new byte[blockSize];
        n = 0;
    }

    public ByteVector(byte[] a) {
        blockSize = DEFAULT_BLOCK_SIZE;
        array = a;
        n = 0;
    }

    public ByteVector(byte[] a, int capacity) {
        if (capacity > 0) {
            blockSize = capacity;
        } else {
            blockSize = DEFAULT_BLOCK_SIZE;
        }
        array = a;
        n = 0;
    }

    public byte[] getArray() {
        return array;
    }

    
    public int length() {
        return n;
    }

    
    public int capacity() {
        return array.length;
    }

    public void put(int index, byte val) {
        array[index] = val;
    }

    public byte get(int index) {
        return array[index];
    }

    
    public int alloc(int size) {
        int index = n;
        int len = array.length;
        if (n + size >= len) {
            byte[] aux = new byte[len + blockSize];
            System.arraycopy(array, 0, aux, 0, len);
            array = aux;
        }
        n += size;
        return index;
    }

    public void trimToSize() {
        if (n < array.length) {
            byte[] aux = new byte[n];
            System.arraycopy(array, 0, aux, 0, n);
            array = aux;
        }
    }

}
