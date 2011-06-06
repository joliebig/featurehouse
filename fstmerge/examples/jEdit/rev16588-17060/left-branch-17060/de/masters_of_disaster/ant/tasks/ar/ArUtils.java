package de.masters_of_disaster.ant.tasks.ar;


public class ArUtils {
    
    public static long parseOctal(byte[] header, int offset, int length) {
        long    result = 0;
        int     end = offset + length;

        for (int i=offset ; i<end ; i++) {
            if (header[i] == (byte) ' ') {
                break;
            }
            result = (result << 3) + (header[i] - '0');
        }

        return result;
    }

    
    public static StringBuffer parseName(byte[] header, int offset, int length) {
        StringBuffer result = new StringBuffer(length);
        int          end = offset + length;

        for (int i=offset ; i<end ; i++) {
            if (header[i] == ' ') {
                break;
            }

            result.append((char) header[i]);
        }

        return result;
    }

    
    public static int getNameBytes(StringBuffer name, byte[] buf, int offset, int length) {
        int i;
        int c = name.length();

        for (i=0 ; i<length && i<c ; i++) {
            buf[offset+i] = (byte) name.charAt(i);
        }

        while (i<length) {
            buf[offset+i] = (byte) ' ';
            i++;
        }

        return offset + length;
    }

    
    public static int getLongBytes(long value, byte[] buf, int offset, int length) {
        int i;
        String tmp = Long.toString(value);
        int c = tmp.length();

        for (i=0 ; i<length && i<c ; i++) {
            buf[offset+i] = (byte) tmp.charAt(i);
        }

        while (i<length) {
            buf[offset+i] = (byte) ' ';
            i++;
        }

        return offset + length;
    }

    
    public static int getIntegerBytes(int value, byte[] buf, int offset, int length) {
        int i;
        String tmp = Integer.toString(value);
        int c = tmp.length();

        for (i=0 ; i<length && i<c ; i++) {
            buf[offset+i] = (byte) tmp.charAt(i);
        }

        while (i<length) {
            buf[offset+i] = (byte) ' ';
            i++;
        }

        return offset + length;
    }

    
    public static int getOctalBytes(long value, byte[] buf, int offset, int length) {
        int i;
        String tmp = Long.toOctalString(value);
        int c = tmp.length();

        for (i=0 ; i<length && i<c ; i++) {
            buf[offset+i] = (byte) tmp.charAt(i);
        }

        while (i<length) {
            buf[offset+i] = (byte) ' ';
            i++;
        }

        return offset + length;
    }
}
