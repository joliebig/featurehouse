
package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.List;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.text.pdf.SimpleBookmark;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class XML2Bookmarks extends AbstractTool {

    static {
        addVersion("$Id: XML2Bookmarks.java 3393 2008-05-16 21:33:55Z xlv $");
    }

    
    public XML2Bookmarks() {
        arguments.add(new FileArgument(this, "xmlfile", "the bookmarks in XML", false));
        arguments.add(new FileArgument(this, "pdffile", "the PDF to which you want to add bookmarks", false, new PdfFilter()));
        arguments.add(new FileArgument(this, "destfile", "the resulting PDF", true, new PdfFilter()));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("XML + PDF = PDF", true, true, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== XML2Bookmarks OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("xmlfile") == null) throw new InstantiationException("You need to choose an xml file");
            if (getValue("pdffile") == null) throw new InstantiationException("You need to choose a source PDF file");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination PDF file");
            FileInputStream bmReader = new FileInputStream( (File) getValue("xmlfile") );
            List<HashMap<String, Object>> bookmarks = SimpleBookmark.importFromXML( bmReader );
            bmReader.close();
            PdfReader reader = new PdfReader(((File)getValue("pdffile")).getAbsolutePath());
            reader.consolidateNamedDestinations();
            PdfStamper stamper = new PdfStamper(reader, new FileOutputStream((File)getValue("destfile")));
            stamper.setOutlines(bookmarks);
            stamper.setViewerPreferences(reader.getSimpleViewerPreferences() | PdfWriter.PageModeUseOutlines);
            stamper.close();
        }
        catch(Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog(internalFrame,
                    e.getMessage(),
                    e.getClass().getName(),
                    JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
        }
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            return;
        }
        
    }

    
    public static void main(String[] args) {
        XML2Bookmarks tool = new XML2Bookmarks();
        if (args.length < 3) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File)getValue("destfile");
    }

}
