
package com.lowagie.text.pdf;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.SignatureException;
import java.util.HashMap;
import java.util.List;

import com.lowagie.text.DocWriter;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.collection.PdfCollection;
import com.lowagie.text.pdf.interfaces.PdfEncryptionSettings;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;
import java.security.cert.Certificate;


public class PdfStamper
    implements PdfViewerPreferences, PdfEncryptionSettings {
        
    protected PdfStamperImp stamper;
    private HashMap moreInfo;
    private boolean hasSignature;
    private PdfSignatureAppearance sigApp;

    
    public PdfStamper(PdfReader reader, OutputStream os) throws DocumentException, IOException {
        stamper = new PdfStamperImp(reader, os, '\0', false);
    }

    
    public PdfStamper(PdfReader reader, OutputStream os, char pdfVersion) throws DocumentException, IOException {
        stamper = new PdfStamperImp(reader, os, pdfVersion, false);
    }

    
    public PdfStamper(PdfReader reader, OutputStream os, char pdfVersion, boolean append) throws DocumentException, IOException {
        stamper = new PdfStamperImp(reader, os, pdfVersion, append);
    }

    
    public HashMap getMoreInfo() {
        return this.moreInfo;
    }

    
    public void setMoreInfo(HashMap moreInfo) {
        this.moreInfo = moreInfo;
    }

        
    public void insertPage(int pageNumber, Rectangle mediabox) {
        stamper.insertPage(pageNumber, mediabox);
    }
    
        
    public PdfSignatureAppearance getSignatureAppearance() {
        return sigApp;
    }

    
    public void close() throws DocumentException, IOException {
        if (!hasSignature) {
            stamper.close(moreInfo);
            return;
        }
        sigApp.preClose();
        PdfSigGenericPKCS sig = sigApp.getSigStandard();
        PdfLiteral lit = (PdfLiteral)sig.get(PdfName.CONTENTS);
        int totalBuf = (lit.getPosLength() - 2) / 2;
        byte buf[] = new byte[8192];
        int n;
        InputStream inp = sigApp.getRangeStream();
        try {
            while ((n = inp.read(buf)) > 0) {
                sig.getSigner().update(buf, 0, n);
            }
        }
        catch (SignatureException se) {
            throw new ExceptionConverter(se);
        }
        buf = new byte[totalBuf];
        byte[] bsig = sig.getSignerContents();
        System.arraycopy(bsig, 0, buf, 0, bsig.length);
        PdfString str = new PdfString(buf);
        str.setHexWriting(true);
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.CONTENTS, str);
        sigApp.close(dic);
        stamper.reader.close();
    }

    
    public PdfContentByte getUnderContent(int pageNum) {
        return stamper.getUnderContent(pageNum);
    }

    
    public PdfContentByte getOverContent(int pageNum) {
        return stamper.getOverContent(pageNum);
    }

    
    public boolean isRotateContents() {
        return stamper.isRotateContents();
    }

    
    public void setRotateContents(boolean rotateContents) {
        stamper.setRotateContents(rotateContents);
    }

    
    public void setEncryption(byte userPassword[], byte ownerPassword[], int permissions, boolean strength128Bits) throws DocumentException {
        if (stamper.isAppend())
            throw new DocumentException("Append mode does not support changing the encryption status.");
        if (stamper.isContentWritten())
            throw new DocumentException("Content was already written to the output.");
        stamper.setEncryption(userPassword, ownerPassword, permissions, strength128Bits ? PdfWriter.STANDARD_ENCRYPTION_128 : PdfWriter.STANDARD_ENCRYPTION_40);
    }

    
    public void setEncryption(byte userPassword[], byte ownerPassword[], int permissions, int encryptionType) throws DocumentException {
        if (stamper.isAppend())
            throw new DocumentException("Append mode does not support changing the encryption status.");
        if (stamper.isContentWritten())
            throw new DocumentException("Content was already written to the output.");
        stamper.setEncryption(userPassword, ownerPassword, permissions, encryptionType);
    }

    
    public void setEncryption(boolean strength, String userPassword, String ownerPassword, int permissions) throws DocumentException {
        setEncryption(DocWriter.getISOBytes(userPassword), DocWriter.getISOBytes(ownerPassword), permissions, strength);
    }

    
    public void setEncryption(int encryptionType, String userPassword, String ownerPassword, int permissions) throws DocumentException {
        setEncryption(DocWriter.getISOBytes(userPassword), DocWriter.getISOBytes(ownerPassword), permissions, encryptionType);
    }

    
     public void setEncryption(Certificate[] certs, int[] permissions, int encryptionType) throws DocumentException {
        if (stamper.isAppend())
            throw new DocumentException("Append mode does not support changing the encryption status.");
        if (stamper.isContentWritten())
            throw new DocumentException("Content was already written to the output.");
        stamper.setEncryption(certs, permissions, encryptionType);
     }
     
    
    public PdfImportedPage getImportedPage(PdfReader reader, int pageNumber) {
        return stamper.getImportedPage(reader, pageNumber);
    }

    
    public PdfWriter getWriter() {
        return stamper;
    }

    
    public PdfReader getReader() {
        return stamper.reader;
    }

    
    public AcroFields getAcroFields() {
        return stamper.getAcroFields();
    }

    
    public void setFormFlattening(boolean flat) {
        stamper.setFormFlattening(flat);
    }

    
    public void setFreeTextFlattening(boolean flat) {
        stamper.setFreeTextFlattening(flat);
    }

    
    public void addAnnotation(PdfAnnotation annot, int page) {
        stamper.addAnnotation(annot, page);
    }

        
    public void addComments(FdfReader fdf) throws IOException {
        stamper.addComments(fdf);
    }
    
    
    public void setOutlines(List outlines) {
        stamper.setOutlines(outlines);
    }

        
    public void setThumbnail(Image image, int page) throws PdfException, DocumentException {
        stamper.setThumbnail(image, page);
    }
    
    
    public boolean partialFormFlattening(String name) {
        return stamper.partialFormFlattening(name);
    }

    
    public void addJavaScript(String js) {
        stamper.addJavaScript(js, !PdfEncodings.isPdfDocEncoding(js));
    }

        
    public void addFileAttachment(String description, byte fileStore[], String file, String fileDisplay) throws IOException {
        addFileAttachment(description, PdfFileSpecification.fileEmbedded(stamper, file, fileDisplay, fileStore));
    }

        
    public void addFileAttachment(String description, PdfFileSpecification fs) throws IOException {
        stamper.addFileAttachment(description, fs);
    }

    
    public void makePackage( PdfName initialView ) {
        PdfCollection collection = new PdfCollection(0);
        collection.put(PdfName.VIEW, initialView);
        stamper.makePackage( collection );
    }

    
    public void makePackage(PdfCollection collection) {
        stamper.makePackage(collection);        
    }
    
    
    public void setViewerPreferences(int preferences) {
        stamper.setViewerPreferences(preferences);
    }
    
    
    
    public void addViewerPreference(PdfName key, PdfObject value) {
        stamper.addViewerPreference(key, value);
    }

    
    public void setXmpMetadata(byte[] xmp) {
        stamper.setXmpMetadata(xmp);
    }

    
    public boolean isFullCompression() {
        return stamper.isFullCompression();
    }

    
    public void setFullCompression() {
        if (stamper.isAppend())
            return;
        stamper.setFullCompression();
    }

        
    public void setPageAction(PdfName actionType, PdfAction action, int page) throws PdfException {
        stamper.setPageAction(actionType, action, page);
    }

    
    public void setDuration(int seconds, int page) {
        stamper.setDuration(seconds, page);
    }
    
    
    public void setTransition(PdfTransition transition, int page) {
        stamper.setTransition(transition, page);
    }

    
    public static PdfStamper createSignature(PdfReader reader, OutputStream os, char pdfVersion, File tempFile, boolean append) throws DocumentException, IOException {
        PdfStamper stp;
        if (tempFile == null) {
            ByteBuffer bout = new ByteBuffer();
            stp = new PdfStamper(reader, bout, pdfVersion, append);
            stp.sigApp = new PdfSignatureAppearance(stp.stamper);
            stp.sigApp.setSigout(bout);
        }
        else {
            if (tempFile.isDirectory())
                tempFile = File.createTempFile("pdf", null, tempFile);
            FileOutputStream fout = new FileOutputStream(tempFile);
            stp = new PdfStamper(reader, fout, pdfVersion, append);
            stp.sigApp = new PdfSignatureAppearance(stp.stamper);
            stp.sigApp.setTempFile(tempFile);
        }
        stp.sigApp.setOriginalout(os);
        stp.sigApp.setStamper(stp);
        stp.hasSignature = true;
        PdfDictionary catalog = reader.getCatalog();
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), catalog);
        if (acroForm != null) {
            acroForm.remove(PdfName.NEEDAPPEARANCES);
            stp.stamper.markUsed(acroForm);
        }
        return stp;
    }

    
    public static PdfStamper createSignature(PdfReader reader, OutputStream os, char pdfVersion) throws DocumentException, IOException {
        return createSignature(reader, os, pdfVersion, null, false);
    }
    
    
    public static PdfStamper createSignature(PdfReader reader, OutputStream os, char pdfVersion, File tempFile) throws DocumentException, IOException 
    {
        return createSignature(reader, os, pdfVersion, tempFile, false);
    }
}