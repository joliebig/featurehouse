



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.Document;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.OptionArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;
import com.lowagie.toolbox.swing.PdfInformationPanel;


public class CompressDecompressPageContent extends AbstractTool {

    static {
        addVersion("$Id: CompressDecompressPageContent.java 3307 2008-05-01 19:55:48Z xlv $");
    }

    
    public CompressDecompressPageContent() {
        FileArgument f = new FileArgument(this, "srcfile", "The file you want to compress/decompress", false, new PdfFilter());
        f.setLabel(new PdfInformationPanel());
        arguments.add(f);
        arguments.add(new FileArgument(this, "destfile", "The file to which the compressed/decompressed PDF has to be written", true, new PdfFilter()));
        OptionArgument oa = new OptionArgument(this, "compress", "compress");
        oa.addOption("Compress page content", "true");
        oa.addOption("Decompress page content", "false");
        arguments.add(oa);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Compress/Decompress", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Compress/Decompress OPENED ===");
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            return;
        }
        
    }

    
    public static void main(String[] args) {
        CompressDecompressPageContent tool = new CompressDecompressPageContent();
        if (args.length < 2) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File)getValue("destfile");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination file");
            boolean compress = "true".equals(getValue("compress"));
            PdfReader reader = new PdfReader(((File)getValue("srcfile")).getAbsolutePath());
            PdfStamper stamper = new PdfStamper(reader, new FileOutputStream(getDestPathPDF()));
            synchronized(arguments) {
                Document.compress = compress;
                int total = reader.getNumberOfPages() + 1;
                for (int i = 1; i < total; i++) {
                    reader.setPageContent(i, reader.getPageContent(i));
                }
                stamper.close();
                Document.compress = true;
            }
        }
        catch(Exception e) {
            JOptionPane.showMessageDialog(internalFrame,
                    e.getMessage(),
                    e.getClass().getName(),
                    JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
        }
    }
}
