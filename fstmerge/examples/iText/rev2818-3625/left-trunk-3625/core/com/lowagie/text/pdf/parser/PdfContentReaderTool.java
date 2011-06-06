
package com.lowagie.text.pdf.parser;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.RandomAccessFileOrArray;


public class PdfContentReaderTool {

    
    static public String getDictionaryDetail(PdfDictionary dic){
        return getDictionaryDetail(dic, 0);
    }
    
    
    static public String getDictionaryDetail(PdfDictionary dic, int depth){
        StringBuffer builder = new StringBuffer();
        builder.append('(');
        List subDictionaries = new ArrayList();
        for (Iterator i = dic.getKeys().iterator(); i.hasNext(); ) {
            PdfName key = (PdfName)i.next();
            PdfObject val = dic.getDirectObject(key);
            if (val.isDictionary())
                subDictionaries.add(key);
            builder.append(key);
            builder.append('=');
            builder.append(val);
            builder.append(", ");
        }
        builder.setLength(builder.length()-2);
        builder.append(')');
        PdfName pdfSubDictionaryName;
        for (Iterator it = subDictionaries.iterator(); it.hasNext(); ) {
            pdfSubDictionaryName = (PdfName)it.next();
            builder.append('\n');
            for(int i = 0; i < depth+1; i++){
                builder.append('\t');
            }
            builder.append("Subdictionary ");
            builder.append(pdfSubDictionaryName);
            builder.append(" = ");
            builder.append(getDictionaryDetail(dic.getAsDict(pdfSubDictionaryName), depth+1));
        }
        return builder.toString();
    }
    
    
    static public void listContentStream(File pdfFile) throws IOException {
        int maxPage = -1;    
        
        PdfReader reader = new PdfReader(pdfFile.getCanonicalPath());

            
            
            
            int maxPageNum = reader.getNumberOfPages();
            if (maxPage != -1)
                maxPageNum = maxPage;
            
            for (int pageNum = 1; pageNum <= maxPageNum; pageNum++){
                System.out.println("==============Page " + pageNum + "====================");
                System.out.println("- - - - - Dictionary - - - - - -");
                PdfDictionary pageDictionary = reader.getPageN(pageNum);
                System.out.println(getDictionaryDetail(pageDictionary));
                System.out.println("- - - - - Content Stream - - - - - -");
                RandomAccessFileOrArray f = reader.getSafeFile();
                
                byte[] contentBytes = reader.getPageContent(pageNum, f);
                f.close();

                
                
                InputStream is = new ByteArrayInputStream(contentBytes);
                int ch;
                while ((ch = is.read()) != -1){
                    System.out.print((char)ch);
                }

                System.out.println("- - - - - Text Extraction - - - - - -");
                PdfTextExtractor extractor = new PdfTextExtractor(reader);
                System.out.println(extractor.getTextFromPage(pageNum));
                
                System.out.println();
            }       
            
            
    }

    
    
    public static void main(String[] args) throws Exception {
        if (args.length != 1){
            System.out.println("Usage:  PdfContentReaderTool <pdf file path>");
            return;
        }
        listContentStream(new File(args[0]));
        
    }

}
