



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.pdf.PdfEncryptor;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.BitsetArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.OptionArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class Encrypt extends AbstractTool {

    static {
        addVersion("$Id: Encrypt.java 3307 2008-05-01 19:55:48Z xlv $");
    }
    private final static int PERMISSIONS[] = {
        PdfWriter.ALLOW_PRINTING,
        PdfWriter.ALLOW_MODIFY_CONTENTS,
        PdfWriter.ALLOW_COPY,
        PdfWriter.ALLOW_MODIFY_ANNOTATIONS,
        PdfWriter.ALLOW_FILL_IN,
        PdfWriter.ALLOW_SCREENREADERS,
        PdfWriter.ALLOW_ASSEMBLY,
        PdfWriter.ALLOW_DEGRADED_PRINTING};
    private final static String PERMISSION_OPTIONS[] = {
            "AllowPrinting",
            "AllowModifyContents",
            "AllowCopy",
            "AllowModifyAnnotations",
            "AllowFillIn (128 bit only)",
            "AllowScreenReaders (128 bit only)",
            "AllowAssembly (128 bit only)",
            "AllowDegradedPrinting (128 bit only)"
            };

    
    public Encrypt() {
        arguments.add(new FileArgument(this, "srcfile", "The file you want to encrypt", false, new PdfFilter()));
        arguments.add(new FileArgument(this, "destfile", "The file to which the encrypted PDF has to be written", true, new PdfFilter()));
        arguments.add(new StringArgument(this, "ownerpassword", "The ownerpassword you want to add to the PDF file"));
        arguments.add(new StringArgument(this, "userpassword", "The userpassword you want to add to the PDF file"));
        arguments.add(new BitsetArgument(this, "permissions", "Permissions on the file", PERMISSION_OPTIONS));
        OptionArgument oa = new OptionArgument(this, "strength", "Strength of the encryption");
        oa.addOption("40 bit encryption", "40");
        oa.addOption("128 bit encryption", "128");
        arguments.add(oa);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Encrypt", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Encrypt OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination file");
            int permissions = 0;
            String p = (String)getValue("permissions");
            if (p != null) {
                for (int k = 0; k < p.length(); ++k) {
                    permissions |= (p.charAt(k) == '0' ? 0 : PERMISSIONS[k]);
                }
            }
            byte[] userpassword = null;
            if (getValue("userpassword") != null) {
                userpassword = ((String)getValue("userpassword")).getBytes();
            }
            byte[] ownerpassword = null;
            if (getValue("ownerpassword") != null) {
                ownerpassword = ((String)getValue("ownerpassword")).getBytes();
            }
            PdfReader reader = new PdfReader(((File)getValue("srcfile")).getAbsolutePath());
            PdfEncryptor.encrypt(
                reader,
                new FileOutputStream((File)getValue("destfile")),
                userpassword,
                ownerpassword,
                permissions,
                "128".equals(getValue("strength"))
                );
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
        Encrypt tool = new Encrypt();
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
