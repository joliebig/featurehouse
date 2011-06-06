

package com.lowagie.text.pdf;

import com.lowagie.text.ExceptionConverter;
import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.factories.RomanAlphabetFactory;
import com.lowagie.text.factories.RomanNumberFactory;
import java.util.Arrays;


public class PdfPageLabels {

        
    public static final int DECIMAL_ARABIC_NUMERALS = 0;
        
    public static final int UPPERCASE_ROMAN_NUMERALS = 1;
        
    public static final int LOWERCASE_ROMAN_NUMERALS = 2;
        
    public static final int UPPERCASE_LETTERS = 3;
        
    public static final int LOWERCASE_LETTERS = 4;
        
    public static final int EMPTY = 5;
        
    static PdfName numberingStyle[] = new PdfName[]{PdfName.D, PdfName.R,
                new PdfName("r"), PdfName.A, new PdfName("a")};
        
    private HashMap map;
    
    
    public PdfPageLabels() {
        map = new HashMap();
        addPageLabel(1, DECIMAL_ARABIC_NUMERALS, null, 1);
    }

        
    public void addPageLabel(int page, int numberStyle, String text, int firstPage) {
        if (page < 1 || firstPage < 1)
            throw new IllegalArgumentException("In a page label the page numbers must be greater or equal to 1.");
        PdfDictionary dic = new PdfDictionary();
        if (numberStyle >= 0 && numberStyle < numberingStyle.length)
            dic.put(PdfName.S, numberingStyle[numberStyle]);
        if (text != null)
            dic.put(PdfName.P, new PdfString(text, PdfObject.TEXT_UNICODE));
        if (firstPage != 1)
            dic.put(PdfName.ST, new PdfNumber(firstPage));
        map.put(new Integer(page - 1), dic);
    }

        
    public void addPageLabel(int page, int numberStyle, String text) {
        addPageLabel(page, numberStyle, text, 1);
    }
    
        
    public void addPageLabel(int page, int numberStyle) {
        addPageLabel(page, numberStyle, null, 1);
    }
    
    
    public void addPageLabel(PdfPageLabelFormat format) {
        addPageLabel(format.physicalPage, format.numberStyle, format.prefix, format.logicalPage);
    }
    
        
    public void removePageLabel(int page) {
        if (page <= 1)
            return;
        map.remove(new Integer(page - 1));
    }

        
    PdfDictionary getDictionary(PdfWriter writer) {
        try {
            return PdfNumberTree.writeTree(map, writer);
        }
        catch (IOException e) {
            throw new ExceptionConverter(e);
        }
    }
    
    
    public static String[] getPageLabels(PdfReader reader) {
        
        int n = reader.getNumberOfPages();
        
        PdfDictionary dict = reader.getCatalog();
        PdfDictionary labels = (PdfDictionary)PdfReader.getPdfObjectRelease(dict.get(PdfName.PAGELABELS));
        if (labels == null)
            return null;
        
        String[] labelstrings = new String[n];
        
        HashMap numberTree = PdfNumberTree.readTree(labels);
        
        int pagecount = 1;
        Integer current;
        String prefix = "";
        char type = 'D';
        for (int i = 0; i < n; i++) {
            current = new Integer(i);
            if (numberTree.containsKey(current)) {
                PdfDictionary d = (PdfDictionary)PdfReader.getPdfObjectRelease((PdfObject)numberTree.get(current));
                if (d.contains(PdfName.ST)) {
                    pagecount = ((PdfNumber)d.get(PdfName.ST)).intValue();
                }
                else {
                    pagecount = 1;
                }
                if (d.contains(PdfName.P)) {
                    prefix = ((PdfString)d.get(PdfName.P)).toUnicodeString();
                }
                if (d.contains(PdfName.S)) {
                    type = ((PdfName)d.get(PdfName.S)).toString().charAt(1);
                }
            }
            switch(type) {
            default:
                labelstrings[i] = prefix + pagecount;
                break;
            case 'R':
                labelstrings[i] = prefix + RomanNumberFactory.getUpperCaseString(pagecount);
                break;
            case 'r':
                labelstrings[i] = prefix + RomanNumberFactory.getLowerCaseString(pagecount);
                break;
            case 'A':
                labelstrings[i] = prefix + RomanAlphabetFactory.getUpperCaseString(pagecount);
                break;
            case 'a':
                labelstrings[i] = prefix + RomanAlphabetFactory.getLowerCaseString(pagecount);
                break;
            }
            pagecount++;
        }
        return labelstrings;
    }
    
    
    public static PdfPageLabelFormat[] getPageLabelFormats(PdfReader reader) {
        PdfDictionary dict = reader.getCatalog();
        PdfDictionary labels = (PdfDictionary)PdfReader.getPdfObjectRelease(dict.get(PdfName.PAGELABELS));
        if (labels == null) 
            return null;
        HashMap numberTree = PdfNumberTree.readTree(labels);
        Integer numbers[] = new Integer[numberTree.size()];
        numbers = (Integer[])numberTree.keySet().toArray(numbers);
        Arrays.sort(numbers);
        PdfPageLabelFormat[] formats = new PdfPageLabelFormat[numberTree.size()];
        String prefix;
        int numberStyle;
        int pagecount;
        for (int k = 0; k < numbers.length; ++k) {
            Integer key = numbers[k];
            PdfDictionary d = (PdfDictionary)PdfReader.getPdfObjectRelease((PdfObject)numberTree.get(key));
            if (d.contains(PdfName.ST)) {
                pagecount = ((PdfNumber)d.get(PdfName.ST)).intValue();
            } else {
                pagecount = 1;
            }
            if (d.contains(PdfName.P)) {
                prefix = ((PdfString)d.get(PdfName.P)).toUnicodeString();
            } else {
                prefix = "";
            }
            if (d.contains(PdfName.S)) {
                char type = ((PdfName)d.get(PdfName.S)).toString().charAt(1);
                switch(type) {
                    case 'R': numberStyle = UPPERCASE_ROMAN_NUMERALS; break;
                    case 'r': numberStyle = LOWERCASE_ROMAN_NUMERALS; break;
                    case 'A': numberStyle = UPPERCASE_LETTERS; break;
                    case 'a': numberStyle = LOWERCASE_LETTERS; break;
                    default: numberStyle = DECIMAL_ARABIC_NUMERALS; break;
                }
            } else {
                numberStyle = EMPTY;
            }
            formats[k] = new PdfPageLabelFormat(key.intValue()+1, numberStyle, prefix, pagecount);
        }
        return formats;
    }

    public static class PdfPageLabelFormat {
        
        public int physicalPage;
        public int numberStyle;
        public String prefix;
        public int logicalPage;
        
        
        public PdfPageLabelFormat(int physicalPage, int numberStyle, String prefix, int logicalPage) {
            this.physicalPage = physicalPage;
            this.numberStyle = numberStyle;
            this.prefix = prefix;
            this.logicalPage = logicalPage;
        }
    }
}