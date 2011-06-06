



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;

import javax.swing.JInternalFrame;

import com.lowagie.text.pdf.PRStream;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNameTree;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfString;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;
import com.lowagie.toolbox.swing.PdfInformationPanel;


public class ExtractAttachments extends AbstractTool {

    static {
        addVersion("$Id: ExtractAttachments.java 3271 2008-04-18 20:39:42Z xlv $");
    }

    
    public ExtractAttachments() {
        FileArgument f = new FileArgument(this, "srcfile",
                "The file you want to operate on", false, new PdfFilter());
        f.setLabel(new PdfInformationPanel());
        arguments.add(f);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("ExtractAttachments", true, false,
                true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== ExtractAttachments OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null)
                throw new InstantiationException(
                        "You need to choose a sourcefile");
            File src = (File) getValue("srcfile");

            
            PdfReader reader = new PdfReader(src.getAbsolutePath());
            final File parentFile = src.getParentFile();
            final String outPath;
            if (parentFile != null) {
                outPath = parentFile.getAbsolutePath();
            } else {
                outPath = "";
            }
            PdfDictionary catalog = reader.getCatalog();
            PdfDictionary names = (PdfDictionary) PdfReader
                    .getPdfObject(catalog.get(PdfName.NAMES));
            if (names != null) {
                PdfDictionary embFiles = (PdfDictionary) PdfReader
                        .getPdfObject(names.get(new PdfName("EmbeddedFiles")));
                if (embFiles != null) {
                    HashMap<String, PdfObject> embMap = PdfNameTree.readTree(embFiles);
                    for (Iterator<PdfObject> i = embMap.values().iterator(); i.hasNext();) {
                        PdfDictionary filespec = (PdfDictionary) PdfReader
                                .getPdfObject(i.next());
                        unpackFile(reader, filespec, outPath);
                    }
                }
            }
            for (int k = 1; k <= reader.getNumberOfPages(); ++k) {
                PdfArray annots = (PdfArray) PdfReader.getPdfObject(reader
                        .getPageN(k).get(PdfName.ANNOTS));
                if (annots == null)
                    continue;
                for (Iterator<PdfObject> i = annots.listIterator(); i.hasNext();) {
                    PdfDictionary annot = (PdfDictionary) PdfReader
                            .getPdfObject(i.next());
                    PdfName subType = (PdfName) PdfReader.getPdfObject(annot
                            .get(PdfName.SUBTYPE));
                    if (!PdfName.FILEATTACHMENT.equals(subType))
                        continue;
                    PdfDictionary filespec = (PdfDictionary) PdfReader
                            .getPdfObject(annot.get(PdfName.FS));
                    unpackFile(reader, filespec, outPath);
                }
            }

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
        ExtractAttachments tool = new ExtractAttachments();
        if (args.length < 1) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        throw new InstantiationException("There is more than one destfile.");
    }

    
    public static void unpackFile(PdfReader reader, PdfDictionary filespec,
            String outPath) throws IOException {
        if (filespec == null)
            return;
        PdfName type = (PdfName) PdfReader.getPdfObject(filespec
                .get(PdfName.TYPE));
        if (!PdfName.F.equals(type) && !PdfName.FILESPEC.equals(type))
            return;
        PdfDictionary ef = (PdfDictionary) PdfReader.getPdfObject(filespec
                .get(PdfName.EF));
        if (ef == null)
            return;
        PdfString fn = (PdfString) PdfReader.getPdfObject(filespec
                .get(PdfName.F));
        System.out.println("Unpacking file '" + fn + "' to " + outPath);
        if (fn == null)
            return;
        File fLast = new File(fn.toUnicodeString());
        File fullPath = new File(outPath, fLast.getName());
        if (fullPath.exists())
            return;
        PRStream prs = (PRStream) PdfReader.getPdfObject(ef.get(PdfName.F));
        if (prs == null)
            return;
        byte b[] = PdfReader.getStreamBytes(prs);
        FileOutputStream fout = new FileOutputStream(fullPath);
        fout.write(b);
        fout.close();
    }

}
