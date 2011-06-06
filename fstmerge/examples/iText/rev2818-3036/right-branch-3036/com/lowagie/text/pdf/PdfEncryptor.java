

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;

import com.lowagie.text.DocumentException;


public class PdfEncryptor {
    
    private PdfEncryptor(){
    }
    
    
    public static void encrypt(PdfReader reader, OutputStream os, byte userPassword[], byte ownerPassword[], int permissions, boolean strength128Bits) throws DocumentException, IOException {
        PdfStamper stamper = new PdfStamper(reader, os);
        stamper.setEncryption(userPassword, ownerPassword, permissions, strength128Bits);
        stamper.close();
    }
    
    
    public static void encrypt(PdfReader reader, OutputStream os, byte userPassword[], byte ownerPassword[], int permissions, boolean strength128Bits, HashMap<String, String> newInfo) throws DocumentException, IOException {
        PdfStamper stamper = new PdfStamper(reader, os);
        stamper.setEncryption(userPassword, ownerPassword, permissions, strength128Bits);
        stamper.setMoreInfo(newInfo);
        stamper.close();
    }
    
    
    public static void encrypt(PdfReader reader, OutputStream os, boolean strength, String userPassword, String ownerPassword, int permissions) throws DocumentException, IOException {
        PdfStamper stamper = new PdfStamper(reader, os);
        stamper.setEncryption(strength, userPassword, ownerPassword, permissions);
        stamper.close();
    }
    
    
    public static void encrypt(PdfReader reader, OutputStream os, boolean strength, String userPassword, String ownerPassword, int permissions, HashMap<String, String> newInfo) throws DocumentException, IOException {
        PdfStamper stamper = new PdfStamper(reader, os);
        stamper.setEncryption(strength, userPassword, ownerPassword, permissions);
        stamper.setMoreInfo(newInfo);
        stamper.close();
    }

    
    
    public static void encrypt(PdfReader reader, OutputStream os, int type, String userPassword, String ownerPassword, int permissions, HashMap<String, String> newInfo) throws DocumentException, IOException {
        PdfStamper stamper = new PdfStamper(reader, os);
        stamper.setEncryption(type, userPassword, ownerPassword, permissions);
        stamper.setMoreInfo(newInfo);
        stamper.close();
    }
    
    
    public static void encrypt(PdfReader reader, OutputStream os, int type, String userPassword, String ownerPassword, int permissions) throws DocumentException, IOException {
        PdfStamper stamper = new PdfStamper(reader, os);
        stamper.setEncryption(type, userPassword, ownerPassword, permissions);
        stamper.close();
    }
    
    
    public static String getPermissionsVerbose(int permissions) {
        StringBuffer buf = new StringBuffer("Allowed:");
        if ((PdfWriter.AllowPrinting & permissions) == PdfWriter.AllowPrinting) buf.append(" Printing");
        if ((PdfWriter.AllowModifyContents & permissions) == PdfWriter.AllowModifyContents) buf.append(" Modify contents");
        if ((PdfWriter.AllowCopy & permissions) == PdfWriter.AllowCopy) buf.append(" Copy");
        if ((PdfWriter.AllowModifyAnnotations & permissions) == PdfWriter.AllowModifyAnnotations) buf.append(" Modify annotations");
        if ((PdfWriter.AllowFillIn & permissions) == PdfWriter.AllowFillIn) buf.append(" Fill in");
        if ((PdfWriter.AllowScreenReaders & permissions) == PdfWriter.AllowScreenReaders) buf.append(" Screen readers");
        if ((PdfWriter.AllowAssembly & permissions) == PdfWriter.AllowAssembly) buf.append(" Assembly");
        if ((PdfWriter.AllowDegradedPrinting & permissions) == PdfWriter.AllowDegradedPrinting) buf.append(" Degraded printing");
        return buf.toString();
    }
}