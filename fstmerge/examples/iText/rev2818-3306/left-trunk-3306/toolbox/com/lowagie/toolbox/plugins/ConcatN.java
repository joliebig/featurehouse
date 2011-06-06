



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.JInternalFrame;

import com.lowagie.text.Document;
import com.lowagie.text.pdf.PdfCopy;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.SimpleBookmark;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.FileArrayArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class ConcatN extends AbstractTool {

    static {
        addVersion("$Id: ConcatN.java 3271 2008-04-18 20:39:42Z xlv $");
    }

    
    public ConcatN() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
        arguments.add(new FileArrayArgument(this, "srcfiles",
                                            "The list of PDF files"));
        arguments.add(new FileArgument(this, "destfile",
                "The file to which the concatenated PDF has to be written", true,
                                       new PdfFilter()));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Concatenate n PDF files", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Concat OPENED ===");
    }

    
    public void execute() {
        try {
            File[] files;
            if (getValue("srcfiles") == null) {
                throw new InstantiationException(
                        "You need to choose a list of sourcefiles");
            }
            files = ((File[]) getValue("srcfiles"));
            if (getValue("destfile") == null) {
                throw new InstantiationException(
                        "You need to choose a destination file");
            }
            File pdf_file = (File) getValue("destfile");
            int pageOffset = 0;
            ArrayList<HashMap<String, Object>> master = new ArrayList<HashMap<String, Object>>();
            Document document = null;
            PdfCopy writer = null;
            for (int i = 0; i < files.length; i++) {
                
                PdfReader reader = new PdfReader(files[i].getAbsolutePath());
                reader.consolidateNamedDestinations();
                
                int n = reader.getNumberOfPages();
                List<HashMap<String, Object>> bookmarks = SimpleBookmark.getBookmark(reader);
                if (bookmarks != null) {
                    if (pageOffset != 0) {
                        SimpleBookmark.shiftPageNumbers(bookmarks, pageOffset, null);
                    }
                    master.addAll(bookmarks);
                }
                pageOffset += n;
                System.out.println("There are " + n + " pages in " + files[i]);
                if (i == 0) {
                    
                    document = new Document(reader.getPageSizeWithRotation(1));
                    
                    writer = new PdfCopy(document,
                                         new FileOutputStream(pdf_file));
                    
                    document.open();
                }
                
                PdfImportedPage page;
                for (int p = 0; p < n; ) {
                    ++p;
                    page = writer.getImportedPage(reader, p);
                    writer.addPage(page);
                    System.out.println("Processed page " + p);
                }
            }
            if (!master.isEmpty()) {
                writer.setOutlines(master);
            }
            
            document.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            return;
        }
        
    }


    
    public static void main(String[] args) {
        ConcatN tool = new ConcatN();
        if (args.length < 2) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File) getValue("destfile");
    }

}
