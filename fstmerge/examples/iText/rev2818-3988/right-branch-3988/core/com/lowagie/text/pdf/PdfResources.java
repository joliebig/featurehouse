

package com.lowagie.text.pdf;



class PdfResources extends PdfDictionary {
    
    
    

    
    PdfResources() {
        super();
    }
    
    
    
    void add(PdfName key, PdfDictionary resource) {
        if (resource.size() == 0)
            return;
        PdfDictionary dic = getAsDict(key);
        if (dic == null)
            put(key, resource);
        else
            dic.putAll(resource);
    }
}