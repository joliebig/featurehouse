
package com.lowagie.text.pdf.codec;

import java.io.Serializable;


public class TIFFField extends Object implements Comparable, Serializable {

    private static final long serialVersionUID = 9088332901412823834L;

    
    public static final int TIFF_BYTE      =  1;

    
    public static final int TIFF_ASCII     =  2;

    
    public static final int TIFF_SHORT     =  3;

    
    public static final int TIFF_LONG      =  4;

    
    public static final int TIFF_RATIONAL  =  5;

    
    public static final int TIFF_SBYTE     =  6;

    
    public static final int TIFF_UNDEFINED =  7;

    
    public static final int TIFF_SSHORT    =  8;

    
    public static final int TIFF_SLONG     =  9;

    
    public static final int TIFF_SRATIONAL = 10;

    
    public static final int TIFF_FLOAT     = 11;

    
    public static final int TIFF_DOUBLE    = 12;

    
    int tag;

    
    int type;

    
    int count;

    
    Object data;
    
    
    TIFFField() {}

    
    public TIFFField(int tag, int type, int count, Object data) {
        this.tag = tag;
        this.type = type;
        this.count = count;
        this.data = data;
    }

    
    public int getTag() {
        return tag;
    }

    
    public int getType() {
        return type;
    }

    
    public int getCount() {
        return count;
    }

    
    public byte[] getAsBytes() {
        return (byte[])data;
    }

    
    public char[] getAsChars() {
        return (char[])data;
    }

    
    public short[] getAsShorts() {
        return (short[])data;
    }

    
    public int[] getAsInts() {
        return (int[])data;
    }

    
    public long[] getAsLongs() {
        return (long[])data;
    }

    
    public float[] getAsFloats() {
        return (float[])data;
    }

    
    public double[] getAsDoubles() {
        return (double[])data;
    }

    
    public int[][] getAsSRationals() {
        return (int[][])data;
    }

    
    public long[][] getAsRationals() {
        return (long[][])data;
    }

    
    public int getAsInt(int index) {
        switch (type) {
        case TIFF_BYTE: case TIFF_UNDEFINED:
            return ((byte[])data)[index] & 0xff;
        case TIFF_SBYTE:
            return ((byte[])data)[index];
        case TIFF_SHORT:
            return ((char[])data)[index] & 0xffff;
        case TIFF_SSHORT:
            return ((short[])data)[index];
        case TIFF_SLONG:
            return ((int[])data)[index];
        default:
            throw new ClassCastException();
        }
    }

    
    public long getAsLong(int index) {
        switch (type) {
        case TIFF_BYTE: case TIFF_UNDEFINED:
            return ((byte[])data)[index] & 0xff;
        case TIFF_SBYTE:
            return ((byte[])data)[index];
        case TIFF_SHORT:
            return ((char[])data)[index] & 0xffff;
        case TIFF_SSHORT:
            return ((short[])data)[index];
        case TIFF_SLONG:
            return ((int[])data)[index];
        case TIFF_LONG:
            return ((long[])data)[index];
        default:
            throw new ClassCastException();
        }
    }
    
    
    public float getAsFloat(int index) {
        switch (type) {
        case TIFF_BYTE:
            return ((byte[])data)[index] & 0xff;
        case TIFF_SBYTE:
            return ((byte[])data)[index];
        case TIFF_SHORT:
            return ((char[])data)[index] & 0xffff;
        case TIFF_SSHORT:
            return ((short[])data)[index];
        case TIFF_SLONG:
            return ((int[])data)[index];
        case TIFF_LONG:
            return ((long[])data)[index];
        case TIFF_FLOAT:
            return ((float[])data)[index];
        case TIFF_DOUBLE:
            return (float)((double[])data)[index];
        case TIFF_SRATIONAL:
            int[] ivalue = getAsSRational(index);
            return (float)((double)ivalue[0]/ivalue[1]);
        case TIFF_RATIONAL:
            long[] lvalue = getAsRational(index);
            return (float)((double)lvalue[0]/lvalue[1]);
        default:
            throw new ClassCastException();
        }
    }

    
    public double getAsDouble(int index) {
        switch (type) {
        case TIFF_BYTE:
            return ((byte[])data)[index] & 0xff;
        case TIFF_SBYTE:
            return ((byte[])data)[index];
        case TIFF_SHORT:
            return ((char[])data)[index] & 0xffff;
        case TIFF_SSHORT:
            return ((short[])data)[index];
        case TIFF_SLONG:
            return ((int[])data)[index];
        case TIFF_LONG:
            return ((long[])data)[index];
        case TIFF_FLOAT:
            return ((float[])data)[index];
        case TIFF_DOUBLE:
            return ((double[])data)[index];
        case TIFF_SRATIONAL:
            int[] ivalue = getAsSRational(index);
            return (double)ivalue[0]/ivalue[1];
        case TIFF_RATIONAL:
            long[] lvalue = getAsRational(index);
            return (double)lvalue[0]/lvalue[1];
        default:
            throw new ClassCastException();
        }
    }

    
    public String getAsString(int index) {
        return ((String[])data)[index];
    }

    
    public int[] getAsSRational(int index) {
        return ((int[][])data)[index];
    }

    
    public long[] getAsRational(int index) {
        if (type == TIFF_LONG)
            return getAsLongs();
        return ((long[][])data)[index];
    }

    
    public int compareTo(Object o) {
        if(o == null) {
            throw new IllegalArgumentException();
        }

        int oTag = ((TIFFField)o).getTag();

        if(tag < oTag) {
            return -1;
        } else if(tag > oTag) {
            return 1;
        } else {
            return 0;
        }
    }
}
