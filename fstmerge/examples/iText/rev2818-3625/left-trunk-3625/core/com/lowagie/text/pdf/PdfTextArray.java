

package com.lowagie.text.pdf;

import java.util.ArrayList;



public class PdfTextArray{
    ArrayList arrayList = new ArrayList();
    
    
    
    
    
    
    private String lastStr;
    private Float lastNum;
    
    
    public PdfTextArray(String str) {
        add(str);
    }
    
    public PdfTextArray() {
    }
    
    
    public void add(PdfNumber number) {
        add((float) number.doubleValue());
    }
    
    public void add(float number) {
        if (number != 0) {
            if (lastNum != null) {
                lastNum = new Float(number + lastNum.floatValue());
                if (lastNum.floatValue() != 0) {
                    replaceLast(lastNum);
                } else {
                    arrayList.remove(arrayList.size() - 1);
                }
            } else {
                lastNum = new Float(number);
                arrayList.add(lastNum);
            }
            
            lastStr = null;
        }
        
    }
    
    public void add(String str) {
        if (str.length() > 0) {
            if (lastStr != null) {
                lastStr = lastStr + str;
                replaceLast(lastStr);
            } else {
                lastStr = str;
                arrayList.add(lastStr);
            }
            lastNum = null;
        }
        
    }
    
    ArrayList getArrayList() {
        return arrayList;
    }
    
    private void replaceLast(Object obj) {
        
        arrayList.set(arrayList.size() - 1, obj);
    }
}
