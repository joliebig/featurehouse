
package com.lowagie.text.pdf.parser;

import java.io.IOException;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.RandomAccessFileOrArray;


public class PdfTextExtractor {

    
    private final PdfReader reader;
    
    private final SimpleTextExtractingPdfContentStreamProcessor extractionProcessor;
    
    
    public PdfTextExtractor(PdfReader reader) {
        this.reader = reader;
        extractionProcessor = new SimpleTextExtractingPdfContentStreamProcessor();
    }

    
    private byte[] getContentBytesForPage(int pageNum) throws IOException {
        RandomAccessFileOrArray f = reader.getSafeFile();
        byte[] contentBytes = reader.getPageContent(pageNum, f);
        f.close();
        return contentBytes;
    }
    
    
    public String getTextFromPage(int page) throws IOException {
        PdfDictionary pageDic = reader.getPageN(page);
        PdfDictionary resourcesDic = pageDic.getAsDict(PdfName.RESOURCES);
        extractionProcessor.processContent(getContentBytesForPage(page), resourcesDic);        
        return extractionProcessor.getResultantText();
    }
}
