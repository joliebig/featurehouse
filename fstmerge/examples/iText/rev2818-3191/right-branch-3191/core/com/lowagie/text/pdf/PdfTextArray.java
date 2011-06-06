

package com.lowagie.text.pdf;

import java.util.ArrayList;



public class PdfTextArray{
    ArrayList<Object> arrayList = new ArrayList<Object>();
    
    
    
    public PdfTextArray(String str) {
        arrayList.add(str);
    }
    
    public PdfTextArray() {
    }
    

    
    public void add(PdfNumber number)
    {
        arrayList.add(new Float(number.doubleValue()));
    }
    
    public void add(float number)
    {
        arrayList.add(new Float(number));
    }
    
    public void add(String str)
    {
        arrayList.add(str);
    }
    
    ArrayList<Object> getArrayList() {
        return arrayList;
    }
}