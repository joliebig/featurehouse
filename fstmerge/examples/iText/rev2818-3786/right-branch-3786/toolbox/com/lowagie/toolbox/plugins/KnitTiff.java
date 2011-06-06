



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;

import com.lowagie.text.Document;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.text.pdf.RandomAccessFileOrArray;
import com.lowagie.text.pdf.codec.TiffImage;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.ImageFilter;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class KnitTiff extends AbstractTool {

    static {
        addVersion("$Id: KnitTiff.java 3307 2008-05-01 19:55:48Z xlv $");
    }
    
    public KnitTiff() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
        arguments.add(new FileArgument(this, "odd", "The tiff file with the odd pages", false, new ImageFilter(false, false, false, false, false, true)));
        arguments.add(new FileArgument(this, "even", "The tiff file with the even pages", false, new ImageFilter(false, false, false, false, false, true)));
        arguments.add(new FileArgument(this, "destfile", "The file to which the converted TIFF has to be written", true, new PdfFilter()));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("KnitTiff", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== KnitTiff OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("odd") == null) throw new InstantiationException("You need to choose a sourcefile for the odd pages");
            File odd_file = (File)getValue("odd");
            if (getValue("even") == null) throw new InstantiationException("You need to choose a sourcefile for the even pages");
            File even_file = (File)getValue("even");
            if (getValue("destfile") == null) throw new InstantiationException("You need to choose a destination file");
            File pdf_file = (File)getValue("destfile");
            RandomAccessFileOrArray odd = new RandomAccessFileOrArray(odd_file.getAbsolutePath());
            RandomAccessFileOrArray even = new RandomAccessFileOrArray(even_file.getAbsolutePath());
            Image img = TiffImage.getTiffImage(odd, 1);
            Document document = new Document(new Rectangle(img.getScaledWidth(),
                    img.getScaledHeight()));
            PdfWriter writer = PdfWriter.getInstance(document,
                    new FileOutputStream(pdf_file));
            document.open();
            PdfContentByte cb = writer.getDirectContent();
            int count = Math.max(TiffImage.getNumberOfPages(odd), TiffImage
                    .getNumberOfPages(even));
            for (int c = 0; c < count; ++c) {
                try {
                    Image imgOdd = TiffImage.getTiffImage(odd, c + 1);
                    Image imgEven = TiffImage.getTiffImage(even, count - c);
                    document.setPageSize(new Rectangle(imgOdd.getScaledWidth(),
                            imgOdd.getScaledHeight()));
                    document.newPage();
                    imgOdd.setAbsolutePosition(0, 0);
                    cb.addImage(imgOdd);
                    document.setPageSize(new Rectangle(imgEven.getScaledWidth(),
                            imgEven.getScaledHeight()));
                    document.newPage();
                    imgEven.setAbsolutePosition(0, 0);
                    cb.addImage(imgEven);

                } catch (Exception e) {
                    System.out.println("Exception page " + (c + 1) + " "
                            + e.getMessage());
                }
            }
            odd.close();
            even.close();
            document.close();
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
        KnitTiff tool = new KnitTiff();
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
