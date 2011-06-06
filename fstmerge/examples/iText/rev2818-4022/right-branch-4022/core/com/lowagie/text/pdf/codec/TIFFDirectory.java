
package com.lowagie.text.pdf.codec;
import java.io.EOFException;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import com.lowagie.text.pdf.RandomAccessFileOrArray;


public class TIFFDirectory extends Object implements Serializable {
    
    private static final long serialVersionUID = -168636766193675380L;

    
    boolean isBigEndian;
    
    
    int numEntries;
    
    
    TIFFField[] fields;
    
    
    Hashtable<Integer, Integer> fieldIndex = new Hashtable<Integer, Integer>();
    
    
    long IFDOffset = 8;
    
    
    long nextIFDOffset = 0;
    
    
    TIFFDirectory() {}
    
    private static boolean isValidEndianTag(int endian) {
        return ((endian == 0x4949) || (endian == 0x4d4d));
    }
    
    
    public TIFFDirectory(RandomAccessFileOrArray stream, int directory)
    throws IOException {
        
        long global_save_offset = stream.getFilePointer();
        long ifd_offset;
        
        
        stream.seek(0L);
        int endian = stream.readUnsignedShort();
        if (!isValidEndianTag(endian)) {
            throw new
            IllegalArgumentException("Bad endianness tag (not 0x4949 or 0x4d4d).");
        }
        isBigEndian = (endian == 0x4d4d);
        
        int magic = readUnsignedShort(stream);
        if (magic != 42) {
            throw new
            IllegalArgumentException("Bad magic number, should be 42.");
        }
        
        
        ifd_offset = readUnsignedInt(stream);
        
        for (int i = 0; i < directory; i++) {
            if (ifd_offset == 0L) {
                throw new
                IllegalArgumentException("Directory number too large.");
            }
            
            stream.seek(ifd_offset);
            int entries = readUnsignedShort(stream);
            stream.skip(12*entries);
            
            ifd_offset = readUnsignedInt(stream);
        }
        
        stream.seek(ifd_offset);
        initialize(stream);
        stream.seek(global_save_offset);
    }
    
    
    public TIFFDirectory(RandomAccessFileOrArray stream, long ifd_offset, int directory)
    throws IOException {
        
        long global_save_offset = stream.getFilePointer();
        stream.seek(0L);
        int endian = stream.readUnsignedShort();
        if (!isValidEndianTag(endian)) {
            throw new
            IllegalArgumentException("Bad endianness tag (not 0x4949 or 0x4d4d).");
        }
        isBigEndian = (endian == 0x4d4d);
        
        
        stream.seek(ifd_offset);
        
        
        int dirNum = 0;
        while(dirNum < directory) {
            
            int numEntries = readUnsignedShort(stream);
            
            
            stream.seek(ifd_offset + 12*numEntries);
            
            
            ifd_offset = readUnsignedInt(stream);
            
            
            stream.seek(ifd_offset);
            
            
            dirNum++;
        }
        
        initialize(stream);
        stream.seek(global_save_offset);
    }
    
    private static final int[] sizeOfType = {
        0, 
        1, 
        1, 
        2, 
        4, 
        8, 
        1, 
        1, 
        2, 
        4, 
        8, 
        4, 
        8  
    };
    
    private void initialize(RandomAccessFileOrArray stream) throws IOException {
        long nextTagOffset = 0L;
        long maxOffset = stream.length();
        int i, j;
        
        IFDOffset = stream.getFilePointer();
        
        numEntries = readUnsignedShort(stream);
        fields = new TIFFField[numEntries];
        
        for (i = 0; (i < numEntries) && (nextTagOffset < maxOffset); i++) {
            int tag = readUnsignedShort(stream);
            int type = readUnsignedShort(stream);
            int count = (int)(readUnsignedInt(stream));
            boolean processTag = true;
            
            
            nextTagOffset = stream.getFilePointer() + 4;
            
            try {
                
                
                if (count*sizeOfType[type] > 4) {
                    long valueOffset = readUnsignedInt(stream);
                    
                    
                    if (valueOffset < maxOffset) {
                        stream.seek(valueOffset);
                    }
                    else {
                        
                        processTag = false;
                    }
                }
            } catch (ArrayIndexOutOfBoundsException ae) {
                
                processTag = false;
            }
            
            if (processTag) {
            fieldIndex.put(new Integer(tag), new Integer(i));
            Object obj = null;
            
            switch (type) {
                case TIFFField.TIFF_BYTE:
                case TIFFField.TIFF_SBYTE:
                case TIFFField.TIFF_UNDEFINED:
                case TIFFField.TIFF_ASCII:
                    byte[] bvalues = new byte[count];
                    stream.readFully(bvalues, 0, count);
                    
                    if (type == TIFFField.TIFF_ASCII) {
                        
                        
                        int index = 0, prevIndex = 0;
                        ArrayList<String> v = new ArrayList<String>();
                        
                        while (index < count) {
                            
                            while ((index < count) && (bvalues[index++] != 0));
                            
                            
                            v.add(new String(bvalues, prevIndex,
                            (index - prevIndex)) );
                            prevIndex = index;
                        }
                        
                        count = v.size();
                        String strings[] = new String[count];
                        for (int c = 0 ; c < count; c++) {
                            strings[c] = v.get(c);
                        }
                        
                        obj = strings;
                    } else {
                        obj = bvalues;
                    }
                    
                    break;
                    
                case TIFFField.TIFF_SHORT:
                    char[] cvalues = new char[count];
                    for (j = 0; j < count; j++) {
                        cvalues[j] = (char)(readUnsignedShort(stream));
                    }
                    obj = cvalues;
                    break;
                    
                case TIFFField.TIFF_LONG:
                    long[] lvalues = new long[count];
                    for (j = 0; j < count; j++) {
                        lvalues[j] = readUnsignedInt(stream);
                    }
                    obj = lvalues;
                    break;
                    
                case TIFFField.TIFF_RATIONAL:
                    long[][] llvalues = new long[count][2];
                    for (j = 0; j < count; j++) {
                        llvalues[j][0] = readUnsignedInt(stream);
                        llvalues[j][1] = readUnsignedInt(stream);
                    }
                    obj = llvalues;
                    break;
                    
                case TIFFField.TIFF_SSHORT:
                    short[] svalues = new short[count];
                    for (j = 0; j < count; j++) {
                        svalues[j] = readShort(stream);
                    }
                    obj = svalues;
                    break;
                    
                case TIFFField.TIFF_SLONG:
                    int[] ivalues = new int[count];
                    for (j = 0; j < count; j++) {
                        ivalues[j] = readInt(stream);
                    }
                    obj = ivalues;
                    break;
                    
                case TIFFField.TIFF_SRATIONAL:
                    int[][] iivalues = new int[count][2];
                    for (j = 0; j < count; j++) {
                        iivalues[j][0] = readInt(stream);
                        iivalues[j][1] = readInt(stream);
                    }
                    obj = iivalues;
                    break;
                    
                case TIFFField.TIFF_FLOAT:
                    float[] fvalues = new float[count];
                    for (j = 0; j < count; j++) {
                        fvalues[j] = readFloat(stream);
                    }
                    obj = fvalues;
                    break;
                    
                case TIFFField.TIFF_DOUBLE:
                    double[] dvalues = new double[count];
                    for (j = 0; j < count; j++) {
                        dvalues[j] = readDouble(stream);
                    }
                    obj = dvalues;
                    break;
                    
                default:
                    break;
            }
            
            fields[i] = new TIFFField(tag, type, count, obj);
            }
            
            stream.seek(nextTagOffset);
        }
        
        
        try {
            nextIFDOffset = readUnsignedInt(stream);
        }
        catch (Exception e) {
            
            nextIFDOffset = 0;
        }
    }
    
    
    public int getNumEntries() {
        return numEntries;
    }
    
    
    public TIFFField getField(int tag) {
        Integer i = fieldIndex.get(new Integer(tag));
        if (i == null) {
            return null;
        } else {
            return fields[i.intValue()];
        }
    }
    
    
    public boolean isTagPresent(int tag) {
        return fieldIndex.containsKey(new Integer(tag));
    }
    
    
    public int[] getTags() {
        int[] tags = new int[fieldIndex.size()];
        Enumeration<Integer> e = fieldIndex.keys();
        int i = 0;
        
        while (e.hasMoreElements()) {
            tags[i++] = e.nextElement().intValue();
        }
        
        return tags;
    }
    
    
    public TIFFField[] getFields() {
        return fields;
    }
    
    
    public byte getFieldAsByte(int tag, int index) {
        Integer i = fieldIndex.get(new Integer(tag));
        byte [] b = fields[i.intValue()].getAsBytes();
        return b[index];
    }
    
    
    public byte getFieldAsByte(int tag) {
        return getFieldAsByte(tag, 0);
    }
    
    
    public long getFieldAsLong(int tag, int index) {
        Integer i = fieldIndex.get(new Integer(tag));
        return fields[i.intValue()].getAsLong(index);
    }
    
    
    public long getFieldAsLong(int tag) {
        return getFieldAsLong(tag, 0);
    }
    
    
    public float getFieldAsFloat(int tag, int index) {
        Integer i = fieldIndex.get(new Integer(tag));
        return fields[i.intValue()].getAsFloat(index);
    }
    
    
    public float getFieldAsFloat(int tag) {
        return getFieldAsFloat(tag, 0);
    }
    
    
    public double getFieldAsDouble(int tag, int index) {
        Integer i = fieldIndex.get(new Integer(tag));
        return fields[i.intValue()].getAsDouble(index);
    }
    
    
    public double getFieldAsDouble(int tag) {
        return getFieldAsDouble(tag, 0);
    }
    
    
    
    private short readShort(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readShort();
        } else {
            return stream.readShortLE();
        }
    }
    
    private int readUnsignedShort(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readUnsignedShort();
        } else {
            return stream.readUnsignedShortLE();
        }
    }
    
    private int readInt(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readInt();
        } else {
            return stream.readIntLE();
        }
    }
    
    private long readUnsignedInt(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readUnsignedInt();
        } else {
            return stream.readUnsignedIntLE();
        }
    }
    
    private long readLong(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readLong();
        } else {
            return stream.readLongLE();
        }
    }
    
    private float readFloat(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readFloat();
        } else {
            return stream.readFloatLE();
        }
    }
    
    private double readDouble(RandomAccessFileOrArray stream)
    throws IOException {
        if (isBigEndian) {
            return stream.readDouble();
        } else {
            return stream.readDoubleLE();
        }
    }
    
    private static int readUnsignedShort(RandomAccessFileOrArray stream,
    boolean isBigEndian)
    throws IOException {
        if (isBigEndian) {
            return stream.readUnsignedShort();
        } else {
            return stream.readUnsignedShortLE();
        }
    }
    
    private static long readUnsignedInt(RandomAccessFileOrArray stream,
    boolean isBigEndian)
    throws IOException {
        if (isBigEndian) {
            return stream.readUnsignedInt();
        } else {
            return stream.readUnsignedIntLE();
        }
    }
    
    
    
    
    public static int getNumDirectories(RandomAccessFileOrArray stream)
    throws IOException{
        long pointer = stream.getFilePointer(); 
        
        stream.seek(0L);
        int endian = stream.readUnsignedShort();
        if (!isValidEndianTag(endian)) {
            throw new
            IllegalArgumentException("Bad endianness tag (not 0x4949 or 0x4d4d).");
        }
        boolean isBigEndian = (endian == 0x4d4d);
        int magic = readUnsignedShort(stream, isBigEndian);
        if (magic != 42) {
            throw new
            IllegalArgumentException("Bad magic number, should be 42.");
        }
        
        stream.seek(4L);
        long offset = readUnsignedInt(stream, isBigEndian);
        
        int numDirectories = 0;
        while (offset != 0L) {
            ++numDirectories;
            
            
            try {
                stream.seek(offset);
                int entries = readUnsignedShort(stream, isBigEndian);
                stream.skip(12*entries);
                offset = readUnsignedInt(stream, isBigEndian);
            } catch(EOFException eof) {
                
                break;
            }
        }
        
        stream.seek(pointer); 
        return numDirectories;
    }
    
    
    public boolean isBigEndian() {
        return isBigEndian;
    }
    
    
    public long getIFDOffset() {
        return IFDOffset;
    }
    
    
    public long getNextIFDOffset() {
        return nextIFDOffset;
    }
}
