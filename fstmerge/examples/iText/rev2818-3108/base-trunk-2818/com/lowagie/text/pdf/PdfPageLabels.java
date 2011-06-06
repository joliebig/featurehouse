

package com.lowagie.text.pdf;

import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeMap;

import com.lowagie.text.factories.RomanAlphabetFactory;
import com.lowagie.text.factories.RomanNumberFactory;


public class PdfPageLabels {

        
    public static final int DECIMAL_ARABIC_NUMERALS = 0;
        
    public static final int UPPERCASE_ROMAN_NUMERALS = 1;
        
    public static final int LOWERCASE_ROMAN_NUMERALS = 2;
        
    public static final int UPPERCASE_LETTERS = 3;
        
    public static final int LOWERCASE_LETTERS = 4;
        
    public static final int EMPTY = 5;
        
    static PdfName numberingStyle[] = new PdfName[]{PdfName.D, PdfName.R,
                new PdfName("r"), PdfName.A, new PdfName("a")};
        
    TreeMap map;
    
    
    public PdfPageLabels() {
        map = new TreeMap();
        addPageLabel(1, DECIMAL_ARABIC_NUMERALS, null, 1);
    }

        
    public void addPageLabel(int page, int numberStyle, String text, int firstPage) {
        if (page < 1 || firstPage < 1)
            throw new IllegalArgumentException("In a page label the page numbers must be greater or equal to 1.");
        PdfName pdfName = null;
        if (numberStyle >= 0 && numberStyle < numberingStyle.length)
            pdfName = numberingStyle[numberStyle];
        Integer iPage = new Integer(page);
        Object obj = new Object[]{iPage, pdfName, text, new Integer(firstPage)};
        map.put(iPage, obj);
    }

        
    public void addPageLabel(int page, int numberStyle, String text) {
        addPageLabel(page, numberStyle, text, 1);
    }
    
        
    public void addPageLabel(int page, int numberStyle) {
        addPageLabel(page, numberStyle, null, 1);
    }
    
        
    public void removePageLabel(int page) {
        if (page <= 1)
            return;
        map.remove(new Integer(page));
    }

        
    PdfDictionary getDictionary() {
        PdfDictionary dic = new PdfDictionary();
        PdfArray array = new PdfArray();
        for (Iterator it = map.values().iterator(); it.hasNext();) {
            Object obj[] = (Object[])it.next();
            PdfDictionary subDic = new PdfDictionary();
            PdfName pName = (PdfName)obj[1];
            if (pName != null)
                subDic.put(PdfName.S, pName);
            String text = (String)obj[2];
            if (text != null)
                subDic.put(PdfName.P, new PdfString(text, PdfObject.TEXT_UNICODE));
            int st = ((Integer)obj[3]).intValue();
            if (st != 1)
                subDic.put(PdfName.ST, new PdfNumber(st));
            array.add(new PdfNumber(((Integer)obj[0]).intValue() - 1));
            array.add(subDic);
        }
        dic.put(PdfName.NUMS, array);
        return dic;
    }
    
    
    public static String[] getPageLabels(PdfReader reader) {
        
        int n = reader.getNumberOfPages();
        String[] labelstrings = new String[n];
        
        PdfDictionary dict = reader.getCatalog();
        PdfDictionary labels = (PdfDictionary)PdfReader.getPdfObject((PdfObject)dict.get(PdfName.PAGELABELS));
        PdfArray numbers = (PdfArray)PdfReader.getPdfObject((PdfObject)labels.get(PdfName.NUMS));
        
        PdfNumber pageIndex;
        PdfDictionary pageLabel;
        HashMap numberTree = new HashMap();
        for (Iterator i = numbers.listIterator(); i.hasNext(); ) {
            pageIndex = (PdfNumber)i.next();
            pageLabel = (PdfDictionary) PdfReader.getPdfObject((PdfObject)i.next());
            numberTree.put(new Integer(pageIndex.intValue()), pageLabel);
        }
        
        int pagecount = 1;
        Integer current;
        String prefix = "";
        char type = 'D';
        for (int i = 0; i < n; i++) {
            current = new Integer(i);
            if (numberTree.containsKey(current)) {
                PdfDictionary d = (PdfDictionary)numberTree.get(current);
                if (d.contains(PdfName.ST)) {
                    pagecount = ((PdfNumber)d.get(PdfName.ST)).intValue();
                }
                else {
                    pagecount = 1;
                }
                if (d.contains(PdfName.P)) {
                    prefix = ((PdfString)d.get(PdfName.P)).toString();
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
}