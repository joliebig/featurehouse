

package com.lowagie.rups.model;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.swing.JOptionPane;
import javax.swing.JPasswordField;

import com.lowagie.text.DocumentException;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.RandomAccessFileOrArray;
import com.sun.pdfview.PDFFile;


public class PdfFile {

    
    
    
    protected File directory = null;
    
    
    protected String filename = null;
    
    
    protected PdfReader reader = null;
    
    
    protected PDFFile PDFFile = null;
    
    
    protected Permissions permissions = null;
    
    
    
    public PdfFile(File file) throws IOException, DocumentException {
        if (file == null)
            throw new IOException("No file selected.");
        RandomAccessFileOrArray pdf = new RandomAccessFileOrArray(file.getAbsolutePath());
        directory = file.getParentFile();
        filename = file.getName();
        readFile(pdf);
    }
    
    
    public PdfFile(byte[] file) throws IOException, DocumentException {
        RandomAccessFileOrArray pdf = new RandomAccessFileOrArray(file);
        readFile(pdf);
    }
    
    
    protected void readFile(RandomAccessFileOrArray pdf) throws IOException, DocumentException {
        
        permissions = new Permissions();
        try {
            reader = new PdfReader(pdf, null);
            permissions.setEncrypted(false);
        } catch(BadPasswordException bpe) {
            JPasswordField passwordField = new JPasswordField(32);
            JOptionPane.showConfirmDialog(null, passwordField, "Enter the User or Owner Password of this PDF file", JOptionPane.OK_CANCEL_OPTION);
            byte[] password = new String(passwordField.getPassword()).getBytes();
            reader = new PdfReader(pdf, password);
            permissions.setEncrypted(true);
            permissions.setCryptoMode(reader.getCryptoMode());
            permissions.setPermissions(reader.getPermissions());
            if (reader.isOpenedWithFullPermissions()) {
                permissions.setOwnerPassword(password);
                permissions.setUserPassword(reader.computeUserPassword());
            }
            else {
                throw new IOException("You need the owner password of this file to open it in iText Trapeze.");
            }
        }
        
        if (permissions.isEncrypted()) {
            pdf = workAround();
        }
        
        pdf.reOpen();
        try {
            PDFFile = new PDFFile(pdf.getNioByteBuffer());
        }
        catch(IOException ioe) {
            PDFFile = new PDFFile(workAround().getNioByteBuffer());
        }
        pdf.close();
        
    }
    
    protected RandomAccessFileOrArray workAround() throws DocumentException, IOException {
        if (directory == null) {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            PdfStamper stamper = new PdfStamper(reader, baos);
            stamper.close();
            return new RandomAccessFileOrArray(baos.toByteArray());
        }
        else {
            File temp = File.createTempFile(filename.substring(0, filename.lastIndexOf(".pdf")) + "~", ".pdf", directory);
            temp.deleteOnExit();
            FileOutputStream fos = new FileOutputStream(temp);
            PdfStamper stamper = new PdfStamper(reader, fos);
            stamper.close();
            return new RandomAccessFileOrArray(temp.getAbsolutePath());
        }        
    }

    
    public PdfReader getPdfReader() {
        return reader;
    }

    
    public PDFFile getPDFFile() {
        return PDFFile;
    }
}