



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.List;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.SimpleBookmark;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class Bookmarks2XML extends AbstractTool {

    static {
        addVersion("$Id: Bookmarks2XML.java 3271 2008-04-18 20:39:42Z xlv $");
    }

    
    public Bookmarks2XML() {
        arguments.add(new FileArgument(this, "pdffile", "the PDF from which you want to extract bookmarks", false, new PdfFilter()));
        arguments.add(new FileArgument(this, "xmlfile", "the resulting bookmarks file in XML", true));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Bookmarks2XML", true, true, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Bookmarks2XML OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("xmlfile") == null) throw new InstantiationException("You need to choose an xml file");
            if (getValue("pdffile") == null) throw new InstantiationException("You need to choose a source PDF file");
            PdfReader reader = new PdfReader(((File)getValue("pdffile")).getAbsolutePath());
            reader.consolidateNamedDestinations();
            List<HashMap<String, Object>> bookmarks = SimpleBookmark.getBookmark( reader );
            
            FileOutputStream bmWriter = new FileOutputStream( (File)getValue("xmlfile") );
            SimpleBookmark.exportToXML(bookmarks, bmWriter, "UTF-8", false);
            bmWriter.close();
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
        Bookmarks2XML tool = new Bookmarks2XML();
        if (args.length < 2) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        throw new InstantiationException("There is no file to show.");
    }

}
