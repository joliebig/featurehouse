
package com.lowagie.text.pdf.crypto;


public class IVGenerator {
    
    private static ARCFOUREncryption arcfour;
    
    static {
        arcfour = new ARCFOUREncryption();
        long time = System.currentTimeMillis();
        long mem = Runtime.getRuntime().freeMemory();
        String s = time + "+" + mem;
        arcfour.prepareARCFOURKey(s.getBytes());
    }
    
    
    private IVGenerator() {
    }
    
    
    public static byte[] getIV() {
        return getIV(16);
    }
    
    
    public static byte[] getIV(int len) {
        byte[] b = new byte[len];
        synchronized (arcfour) {
            arcfour.encryptARCFOUR(b);
        }
        return b;
    }    
}