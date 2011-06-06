



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class Decrypt extends AbstractTool {

    static {
        addVersion("$Id: Decrypt.java 3307 2008-05-01 19:55:48Z xlv $");
    }


    
    public Decrypt() {
        arguments.add(new FileArgument(this, "srcfile", "The file you want to decrypt", false, new PdfFilter()));
        arguments.add(new FileArgument(this, "destfile", "The file to which the decrypted PDF has to be written", true, new PdfFilter()));
        arguments.add(new StringArgument(this, "ownerpassword", "The ownerpassword you want to add to the PDF file"));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Decrypt", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Decrypt OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination file");
            byte[] ownerpassword = null;
            if (getValue("ownerpassword") != null) {
                ownerpassword = ((String)getValue("ownerpassword")).getBytes();
            }
            PdfReader reader = new PdfReader(((File)getValue("srcfile")).getAbsolutePath(), ownerpassword);
            PdfStamper stamper = new PdfStamper(reader, new FileOutputStream((File)getValue("destfile")));
            stamper.close();
        }
        catch(Exception e) {
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
        Decrypt tool = new Decrypt();
        if (args.length < 2) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File)getValue("destfile");
    }

}
