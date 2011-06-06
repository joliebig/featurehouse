

 package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Iterator;


public class PdfLister {

    
    PrintStream out;

    
    public PdfLister(PrintStream out) {
        this.out = out;
    }

    
    public void listAnyObject(PdfObject object)
    {
        switch (object.type()) {
        case PdfObject.ARRAY:
            listArray((PdfArray)object);
            break;
        case PdfObject.DICTIONARY:
            listDict((PdfDictionary) object);
            break;
        case PdfObject.STRING:
            out.println("(" + object.toString() + ")");
            break;
        default:
            out.println(object.toString());
            break;
        }
    }
    
    public void listDict(PdfDictionary dictionary)
    {
        out.println("<<");
        PdfName key;
        PdfObject value;
        for (Iterator i = dictionary.getKeys().iterator(); i.hasNext(); ) {
            key = (PdfName) i.next();
            value = dictionary.get(key);
            out.print(key.toString());
            out.print(' ');
            listAnyObject(value);
        }
        out.println(">>");
    }

    
    public void listArray(PdfArray array)
    {
        out.println('[');
        for (Iterator i = array.getArrayList().iterator(); i.hasNext(); ) {
            PdfObject item = (PdfObject)i.next();
            listAnyObject(item);
        }
        out.println(']');
    }
    
    public void listStream(PRStream stream, PdfReaderInstance reader)
    {
        try {
            listDict(stream);
            out.println("startstream");
            byte[] b = PdfReader.getStreamBytes(stream);









            int len = b.length - 1;
            for (int k = 0; k < len; ++k) {
                if (b[k] == '\r' && b[k + 1] != '\n')
                    b[k] = (byte)'\n';
            }
            out.println(new String(b));
            out.println("endstream");
        } catch (IOException e) {
            System.err.println("I/O exception: " + e);


        }
    }
    
    public void listPage(PdfImportedPage iPage)
    {
        int pageNum = iPage.getPageNumber();
        PdfReaderInstance readerInst = iPage.getPdfReaderInstance();
        PdfReader reader = readerInst.getReader();

        PdfDictionary page = reader.getPageN(pageNum);
        listDict(page);
        PdfObject obj = PdfReader.getPdfObject(page.get(PdfName.CONTENTS));
        if (obj == null)
            return;
        switch (obj.type) {
        case PdfObject.STREAM:
            listStream((PRStream)obj, readerInst);
            break;
        case PdfObject.ARRAY:
            for (Iterator i = ((PdfArray)obj).getArrayList().iterator(); i.hasNext();) {
                PdfObject o = PdfReader.getPdfObject((PdfObject)i.next());
                listStream((PRStream)o, readerInst);
                out.println("-----------");
            }
            break;
        }
    }
}
