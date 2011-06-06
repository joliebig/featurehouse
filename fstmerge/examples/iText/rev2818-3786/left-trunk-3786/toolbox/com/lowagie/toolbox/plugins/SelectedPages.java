



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;

import com.lowagie.text.Document;
import com.lowagie.text.pdf.PRAcroForm;
import com.lowagie.text.pdf.PdfCopy;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class SelectedPages extends AbstractTool {

    static {
        addVersion("$Id: SelectedPages.java 3271 2008-04-18 20:39:42Z xlv $");
    }
    
    public SelectedPages() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
        arguments.add(new FileArgument(this, "srcfile", "The file you want to split", false, new PdfFilter()));
        arguments.add(new FileArgument(this, "destfile", "The file to which the first part of the original PDF has to be written", true, new PdfFilter()));
        arguments.add(new StringArgument(this, "selection", "A selection of pages (see Help for more info)"));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("SelectedPages", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== SelectedPages OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            File src = (File)getValue("srcfile");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination file for the first part of the PDF");
            File dest = (File)getValue("destfile");
            String selection = (String)getValue("selection");

            
            PdfReader reader = new PdfReader(src.getAbsolutePath());
            System.out.println("The original file had " + reader.getNumberOfPages() + " pages.");
            reader.selectPages(selection);
            int pages = reader.getNumberOfPages();
            System.err.println("The new file has " + pages + " pages.");
            Document document = new Document(reader.getPageSizeWithRotation(1));
            PdfCopy copy = new PdfCopy(document, new FileOutputStream(dest.getAbsolutePath()));
            document.open();
            PdfImportedPage page;
            for (int i = 0; i < pages; ) {
                ++i;
                System.out.println("Processed page " + i);
                page = copy.getImportedPage(reader, i);
                copy.addPage(page);
            }
            PRAcroForm form = reader.getAcroForm();
            if (form != null)
                copy.copyAcroForm(reader);
            document.close();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            return;
        }
        
    }


    
    public static void main(String[] args) {
        SelectedPages tool = new SelectedPages();
        if (args.length < 4) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File)getValue("destfile");
    }
}
