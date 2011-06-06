



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.pdf.PdfEncryptor;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class InspectPDF extends AbstractTool {
    static {
        addVersion("$Id: InspectPDF.java 3989 2009-06-18 02:22:54Z xlv $");
    }

    
    public InspectPDF() {
        arguments.add(new FileArgument(this, "srcfile", "The file you want to inspect", false, new PdfFilter()));
        arguments.add(new StringArgument(this, "ownerpassword", "The owner password if the file is encrypt"));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Pdf Information", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Pdf Information OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            PdfReader reader;
            if (getValue("ownerpassword") == null) {
                reader = new PdfReader(((File)getValue("srcfile")).getAbsolutePath());
            }
            else {
                reader = new PdfReader(((File)getValue("srcfile")).getAbsolutePath(), ((String)getValue("ownerpassword")).getBytes());
            }
            
            System.out.println("=== Document Information ===");
            System.out.println("PDF Version: " + reader.getPdfVersion());
            System.out.println("Number of pages: " + reader.getNumberOfPages());
            System.out.println("Number of PDF objects: " + reader.getXrefSize());
            System.out.println("File length: " + reader.getFileLength());
            System.out.println("Encrypted? " + reader.isEncrypted());
            if (reader.isEncrypted()) {
                System.out.println("Permissions: " + PdfEncryptor.getPermissionsVerbose(reader.getPermissions()));
                System.out.println("128 bit? " + reader.is128Key());
            }
            System.out.println("Rebuilt? " + reader.isRebuilt());
            
            System.out.println("=== Metadata ===");
            HashMap<String, String> info = reader.getInfo();
            String key;
            String value;
            for (Map.Entry<String, String> entry: info.entrySet()) {
                key = entry.getKey();
                value = entry.getValue();
                System.out.println(key + ": " + value);
            }
            if (reader.getMetadata() == null) {
                System.out.println("There is no XML Metadata in the file");
            }
            else {
                System.out.println("XML Metadata: " + new String(reader.getMetadata()));
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

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            return;
        }
        
    }

    
    public static void main(String[] args) {
        InspectPDF tool = new InspectPDF();
        if (args.length < 1) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        throw new InstantiationException("There is no file to show.");
    }

}
