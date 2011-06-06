

package com.lowagie.text.pdf;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

class PdfReaderInstance {
    static final PdfLiteral IDENTITYMATRIX = new PdfLiteral("[1 0 0 1 0 0]");
    static final PdfNumber ONE = new PdfNumber(1);
    int myXref[];
    PdfReader reader;
    RandomAccessFileOrArray file;
    HashMap<Integer, PdfImportedPage> importedPages = new HashMap<Integer, PdfImportedPage>();
    PdfWriter writer;
    HashSet<Integer> visited = new HashSet<Integer>();
    ArrayList<Integer> nextRound = new ArrayList<Integer>();
    
    PdfReaderInstance(PdfReader reader, PdfWriter writer) {
        this.reader = reader;
        this.writer = writer;
        file = reader.getSafeFile();
        myXref = new int[reader.getXrefSize()];
    }
    
    PdfReader getReader() {
        return reader;
    }
    
    PdfImportedPage getImportedPage(int pageNumber) {
        if (!reader.isOpenedWithFullPermissions())
            throw new IllegalArgumentException("PdfReader not opened with owner password");
        if (pageNumber < 1 || pageNumber > reader.getNumberOfPages())
            throw new IllegalArgumentException("Invalid page number: " + pageNumber);
        Integer i = new Integer(pageNumber);
        PdfImportedPage pageT = importedPages.get(i);
        if (pageT == null) {
            pageT = new PdfImportedPage(this, writer, pageNumber);
            importedPages.put(i, pageT);
        }
        return pageT;
    }
    
    int getNewObjectNumber(int number, int generation) {
        if (myXref[number] == 0) {
            myXref[number] = writer.getIndirectReferenceNumber();
            nextRound.add(new Integer(number));
        }
        return myXref[number];
    }
    
    RandomAccessFileOrArray getReaderFile() {
        return file;
    }
    
    PdfObject getResources(int pageNumber) {
        PdfObject obj = PdfReader.getPdfObjectRelease(reader.getPageNRelease(pageNumber).get(PdfName.RESOURCES));
        return obj;
    }
    
    
    PdfStream getFormXObject(int pageNumber) throws IOException {
        PdfDictionary page = reader.getPageNRelease(pageNumber);
        PdfObject contents = PdfReader.getPdfObjectRelease(page.get(PdfName.CONTENTS));
        PdfDictionary dic = new PdfDictionary();
        byte bout[] = null;
        if (contents != null) {
            if (contents.isStream())
                dic.putAll((PRStream)contents);
            else
                bout = reader.getPageContent(pageNumber, file);
        }
        else
            bout = new byte[0];
        dic.put(PdfName.RESOURCES, PdfReader.getPdfObjectRelease(page.get(PdfName.RESOURCES)));
        dic.put(PdfName.TYPE, PdfName.XOBJECT);
        dic.put(PdfName.SUBTYPE, PdfName.FORM);
        PdfImportedPage impPage = importedPages.get(new Integer(pageNumber));
        dic.put(PdfName.BBOX, new PdfRectangle(impPage.getBoundingBox()));
        PdfArray matrix = impPage.getMatrix();
        if (matrix == null)
            dic.put(PdfName.MATRIX, IDENTITYMATRIX);
        else
            dic.put(PdfName.MATRIX, matrix);
        dic.put(PdfName.FORMTYPE, ONE);
        PRStream stream;
        if (bout == null) {
            stream = new PRStream((PRStream)contents, dic);
        }
        else {
            stream = new PRStream(reader, bout);
            stream.putAll(dic);
        }
        return stream;
    }
    
    void writeAllVisited() throws IOException {
        while (!nextRound.isEmpty()) {
            ArrayList<Integer> vec = nextRound;
            nextRound = new ArrayList<Integer>();
            for (int k = 0; k < vec.size(); ++k) {
                Integer i = vec.get(k);
                if (!visited.contains(i)) {
                    visited.add(i);
                    int n = i.intValue();
                    writer.addToBody(reader.getPdfObjectRelease(n), myXref[n]);
                }
            }
        }
    }
    
    void writeAllPages() throws IOException {
        try {
            file.reOpen();
            for (PdfImportedPage ip: importedPages.values()) {
                writer.addToBody(ip.getFormXObject(), ip.getIndirectReference());
            }
            writeAllVisited();
        }
        finally {
            try {
                reader.close();
                file.close();
            }
            catch (Exception e) {
                
            }
        }
    }
}
