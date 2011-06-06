

package com.lowagie.tools;

import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.lowagie.text.Document;
import com.lowagie.text.pdf.PdfCopy;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.SimpleBookmark;


public class ConcatPdf {
    
    
    public static void main(String args[]) {
        if (args.length < 2) {
            System.err.println("arguments: file1 [file2 ...] destfile");
        }
        else {
            try {
                int pageOffset = 0;
                ArrayList<HashMap<String, Object>> master = new ArrayList<HashMap<String, Object>>();
                int f = 0;
                String outFile = args[args.length-1];
                Document document = null;
                PdfCopy  writer = null;
                while (f < args.length-1) {
                    
                    PdfReader reader = new PdfReader(args[f]);
                    reader.consolidateNamedDestinations();
                    
                    int n = reader.getNumberOfPages();
                    List<HashMap<String, Object>> bookmarks = SimpleBookmark.getBookmark(reader);
                    if (bookmarks != null) {
                        if (pageOffset != 0)
                            SimpleBookmark.shiftPageNumbers(bookmarks, pageOffset, null);
                        master.addAll(bookmarks);
                    }
                    pageOffset += n;
                    System.out.println("There are " + n + " pages in " + args[f]);
                    
                    if (f == 0) {
                        
                        document = new Document(reader.getPageSizeWithRotation(1));
                        
                        writer = new PdfCopy(document, new FileOutputStream(outFile));
                        
                        document.open();
                    }
                    
                    PdfImportedPage page;
                    for (int i = 0; i < n; ) {
                        ++i;
                        page = writer.getImportedPage(reader, i);
                        writer.addPage(page);
                        System.out.println("Processed page " + i);
                    }
                    writer.freeReader(reader);
                    f++;
                }
                if (!master.isEmpty())
                    writer.setOutlines(master);
                
                document.close();
            }
            catch(Exception e) {
                e.printStackTrace();
            }
        }
    }
}
