

package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.security.cert.Certificate;
import java.util.List;

import com.lowagie.text.DocWriter;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.interfaces.PdfEncryptionSettings;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;


public class PdfCopyForms
    implements PdfViewerPreferences, PdfEncryptionSettings {
    
    
    private PdfCopyFormsImp fc;
    
        
    public PdfCopyForms(OutputStream os) throws DocumentException {
        fc = new PdfCopyFormsImp(os);
    }
    
        
    public void addDocument(PdfReader reader) throws DocumentException, IOException {
        fc.addDocument(reader);
    }
    
        
    public void addDocument(PdfReader reader, List pagesToKeep) throws DocumentException, IOException {
        fc.addDocument(reader, pagesToKeep);
    }

        
    public void addDocument(PdfReader reader, String ranges) throws DocumentException, IOException {
        fc.addDocument(reader, SequenceList.expand(ranges, reader.getNumberOfPages()));
    }

    
    public void copyDocumentFields(PdfReader reader) throws DocumentException{
        fc.copyDocumentFields(reader);
    }

    
    public void setEncryption(byte userPassword[], byte ownerPassword[], int permissions, boolean strength128Bits) throws DocumentException {
        fc.setEncryption(userPassword, ownerPassword, permissions, strength128Bits ? PdfWriter.STANDARD_ENCRYPTION_128 : PdfWriter.STANDARD_ENCRYPTION_40);
    }
    
    
    public void setEncryption(boolean strength, String userPassword, String ownerPassword, int permissions) throws DocumentException {
        setEncryption(DocWriter.getISOBytes(userPassword), DocWriter.getISOBytes(ownerPassword), permissions, strength);
    }
 
        
    public void close() {
        fc.close();
    }

        
    public void open() {
        fc.openDoc();
    }

        
    public void addJavaScript(String js) {
        fc.addJavaScript(js, !PdfEncodings.isPdfDocEncoding(js));
    }

        
    public void setOutlines(List outlines) {
        fc.setOutlines(outlines);
    }
    
        
    public PdfWriter getWriter() {
        return fc;
    }

    
    public boolean isFullCompression() {
        return fc.isFullCompression();
    }
    
    
    public void setFullCompression() {
        fc.setFullCompression();
    }

    
    public void setEncryption(byte[] userPassword, byte[] ownerPassword, int permissions, int encryptionType) throws DocumentException {
        fc.setEncryption(userPassword, ownerPassword, permissions, encryptionType);
    }

    
    public void addViewerPreference(PdfName key, PdfObject value) {
        fc.addViewerPreference(key, value);    
    }

    
    public void setViewerPreferences(int preferences) {
        fc.setViewerPreferences(preferences);
    }

    
    public void setEncryption(Certificate[] certs, int[] permissions, int encryptionType) throws DocumentException {
        fc.setEncryption(certs, permissions, encryptionType);
    }    
}