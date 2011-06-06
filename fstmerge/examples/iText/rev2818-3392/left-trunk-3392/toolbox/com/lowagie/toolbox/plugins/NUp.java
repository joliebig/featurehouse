



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;

import com.lowagie.text.Document;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.OptionArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class NUp extends AbstractTool {

    static {
        addVersion("$Id: NUp.java 3271 2008-04-18 20:39:42Z xlv $");
    }

    
    public NUp() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
        arguments.add(new FileArgument(this, "srcfile", "The file you want to N-up", false, new PdfFilter()));
        arguments.add(new FileArgument(this, "destfile", "The resulting PDF", true, new PdfFilter()));
        OptionArgument oa = new OptionArgument(this, "pow2", "The number of pages you want to copy to 1 page");
        oa.addOption("2", "1");
        oa.addOption("4", "2");
        oa.addOption("8", "3");
        oa.addOption("16", "4");
        oa.addOption("32", "5");
        oa.addOption("64", "6");
        arguments.add(oa);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("N-up", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== N-up OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) throw new InstantiationException("You need to choose a sourcefile");
            File src = (File)getValue("srcfile");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination file");
            File dest = (File)getValue("destfile");
            int pow2;
            try {
                pow2 = Integer.parseInt((String) getValue("pow2"));
            }
            catch(Exception e) {
                pow2 = 1;
            }
            
            PdfReader reader = new PdfReader(src.getAbsolutePath());
            
            int total = reader.getNumberOfPages();
            System.out.println("There are " + total + " pages in the original file.");
            Rectangle pageSize = reader.getPageSize(1);
            Rectangle newSize = (pow2 % 2) == 0 ? new Rectangle(pageSize.getWidth(), pageSize.getHeight()) : new Rectangle(pageSize.getHeight(), pageSize.getWidth());
            Rectangle unitSize = new Rectangle(pageSize.getWidth(), pageSize.getHeight());
            Rectangle currentSize;
            for (int i = 0; i < pow2; i++) {
                unitSize = new Rectangle(unitSize.getHeight() / 2, unitSize.getWidth());
            }
            int n = (int)Math.pow(2, pow2);
            int r = (int)Math.pow(2, (int)pow2 / 2);
            int c = n / r;
            
            Document document = new Document(newSize, 0, 0, 0, 0);
            
            PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream(dest));
            
            document.open();
            
            PdfContentByte cb = writer.getDirectContent();
            PdfImportedPage page;
            float offsetX, offsetY, factor;
            int p;
            for (int i = 0; i < total; i++) {
                if (i % n == 0) {
                    document.newPage();
                }
                p = i + 1;
                offsetX = unitSize.getWidth() * ((i % n) % c);
                offsetY = newSize.getHeight() - (unitSize.getHeight() * (((i % n) / c) + 1));
                currentSize = reader.getPageSize(p);
                factor = Math.min(unitSize.getWidth() / currentSize.getWidth(), unitSize.getHeight() / currentSize.getHeight());
                offsetX += (unitSize.getWidth() - (currentSize.getWidth() * factor)) / 2f;
                offsetY += (unitSize.getHeight() - (currentSize.getHeight() * factor)) / 2f;
                page = writer.getImportedPage(reader, p);
                cb.addTemplate(page, factor, 0, 0, factor, offsetX, offsetY);
            }
            
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
        NUp tool = new NUp();
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
