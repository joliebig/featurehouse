

package com.lowagie.text.pdf.internal;

import java.io.IOException;

import com.lowagie.text.DocWriter;
import com.lowagie.text.pdf.OutputStreamCounter;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.text.pdf.interfaces.PdfVersion;



public class PdfVersionImp implements PdfVersion {
    
    
    public static final byte[][] HEADER = {
        DocWriter.getISOBytes("\n"),
        DocWriter.getISOBytes("%PDF-"),
        DocWriter.getISOBytes("\n%\u\u\u\u\n")
    };
    
    
    protected boolean headerWasWritten = false;
    
    protected boolean appendmode = false;
    
    protected char header_version = PdfWriter.VERSION_1_4;
    
    protected PdfName catalog_version = null;
    
    
    public void setPdfVersion(char version) {
        if (headerWasWritten || appendmode) {
            setPdfVersion(getVersionAsName(version));
        }
        else {
            this.header_version = version;
        }
    }
    
    
    public void setAtLeastPdfVersion(char version) {
        if (version > header_version) {
            setPdfVersion(version);
        }
    }
    
    
    public void setPdfVersion(PdfName version) {
        if (catalog_version == null || catalog_version.compareTo(version) < 0) {
            this.catalog_version = version;
        }
    }
    
    
    public void setAppendmode(boolean appendmode) {
        this.appendmode = appendmode;
    }
    
    
    public void writeHeader(OutputStreamCounter os) throws IOException {
        if (appendmode) {
            os.write(HEADER[0]);
        }
        else {
            os.write(HEADER[1]);
            os.write(getVersionAsByteArray(header_version));
            os.write(HEADER[2]);
            headerWasWritten = true;
        }
    }
    
    
    public PdfName getVersionAsName(char version) {
        switch(version) {
        case PdfWriter.VERSION_1_2:
            return PdfWriter.PDF_VERSION_1_2;
        case PdfWriter.VERSION_1_3:
            return PdfWriter.PDF_VERSION_1_3;
        case PdfWriter.VERSION_1_4:
            return PdfWriter.PDF_VERSION_1_4;
        case PdfWriter.VERSION_1_5:
            return PdfWriter.PDF_VERSION_1_5;
        case PdfWriter.VERSION_1_6:
            return PdfWriter.PDF_VERSION_1_6;
        case PdfWriter.VERSION_1_7:
            return PdfWriter.PDF_VERSION_1_7;
        default:
            return PdfWriter.PDF_VERSION_1_4;
        }
    }
    
    
    public byte[] getVersionAsByteArray(char version) {
        return DocWriter.getISOBytes(getVersionAsName(version).toString().substring(1));
    }

    
    public void addToCatalog(PdfDictionary catalog) {
        if(catalog_version != null) {
            catalog.put(PdfName.VERSION, catalog_version);
        }
    }
}