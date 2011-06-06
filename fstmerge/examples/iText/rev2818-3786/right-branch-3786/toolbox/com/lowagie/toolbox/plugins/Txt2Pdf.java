



package com.lowagie.toolbox.plugins;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.Document;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.OptionArgument;
import com.lowagie.toolbox.arguments.PageSizeArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class Txt2Pdf extends AbstractTool {

    static {
        addVersion("$Id: Txt2Pdf.java 3307 2008-05-01 19:55:48Z xlv $");
    }
    
    public Txt2Pdf() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW | MENU_EXECUTE_PRINT_SILENT;
        arguments.add(new FileArgument(this, "srcfile", "The file you want to convert", false));
        arguments.add(new FileArgument(this, "destfile", "The file to which the converted text has to be written", true, new PdfFilter()));
        PageSizeArgument oa1 = new PageSizeArgument(this, "pagesize", "Pagesize");
        arguments.add(oa1);
        OptionArgument oa2 = new OptionArgument(this, "orientation", "Orientation of the page");
        oa2.addOption("Portrait", "PORTRAIT");
        oa2.addOption("Landscape", "LANDSCAPE");
        arguments.add(oa2);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Txt2Pdf", true, true, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Txt2Pdf OPENED ===");
    }

    
    public void execute() {
        try {
            String line = null;
            Document document;
            Font f;
            Rectangle pagesize = (Rectangle)getValue("pagesize");
            if ("LANDSCAPE".equals(getValue("orientation"))) {
                f = FontFactory.getFont(FontFactory.COURIER, 10);
                document = new Document(pagesize.rotate(), 36, 9, 36, 36);
            }
            else {
                f = FontFactory.getFont(FontFactory.COURIER, 11);
                document = new Document(pagesize, 72, 36, 36, 36);
            }
            BufferedReader in = new BufferedReader(new FileReader((File)getValue("srcfile")));
            PdfWriter.getInstance(document, new FileOutputStream((File)getValue("destfile")));
            document.open();
            while ((line = in.readLine()) != null) {
                document.add(new Paragraph(12, line, f));
            }
            document.close();
        } catch (Exception e) {
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
        Txt2Pdf tool = new Txt2Pdf();
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
