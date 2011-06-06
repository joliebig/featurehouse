



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;
import java.util.TreeSet;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.Document;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPageLabels;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.DirFilter;
import com.lowagie.toolbox.arguments.filters.PdfFilter;
import com.lowagie.text.pdf.PdfGState;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.BaseFont;


public class PhotoAlbum extends AbstractTool {

    static {
        addVersion("$Id: PhotoAlbum.java 3307 2008-05-01 19:55:48Z xlv $");
    }

    
    public PhotoAlbum() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
        arguments.add(new FileArgument(this, "srcdir",
                "The directory containing the image files", false,
                                       new DirFilter()));
        arguments.add(new FileArgument(this, "destfile",
                "The file to which the converted TIFF has to be written", true,
                                       new PdfFilter()));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("PhotoAlbum", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== PhotoAlbum OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcdir") == null) {
                throw new InstantiationException(
                        "You need to choose a source directory");
            }
            File directory = (File) getValue("srcdir");
            if (directory.isFile()) {
                directory = directory.getParentFile();
            }
            if (getValue("destfile") == null) {
                throw new InstantiationException(
                        "You need to choose a destination file");
            }
            File pdf_file = (File) getValue("destfile");
            Document document = new Document();
            PdfWriter writer = PdfWriter.getInstance(document,
                    new FileOutputStream(pdf_file));
            writer.setViewerPreferences(PdfWriter.PageModeUseThumbs);
            PdfPageLabels pageLabels = new PdfPageLabels();
            int dpiX, dpiY;
            float imgWidthPica, imgHeightPica;
            TreeSet<File> images = new TreeSet<File>();
            File[] files = directory.listFiles();
            if (files == null) {
                throw new NullPointerException("listFiles() returns null");
            }
            for (int i = 0; i < files.length; i++) {
                if (files[i].isFile()) {
                    images.add(files[i]);
                }
            }
            String label;
            for (File image: images) {
                System.out.println("Testing image: " + image.getName());
                try {
                    Image img = Image.getInstance(image.getAbsolutePath());
                    String caption = "";
                    dpiX = img.getDpiX();
                    if (dpiX == 0) {
                        dpiX = 72;
                    }
                    dpiY = img.getDpiY();
                    if (dpiY == 0) {
                        dpiY = 72;
                    }
                    imgWidthPica = (72 * img.getPlainWidth()) / dpiX;
                    imgHeightPica = (72 * img.getPlainHeight()) / dpiY;
                    img.scaleAbsolute(imgWidthPica, imgHeightPica);
                    document.setPageSize(new Rectangle(imgWidthPica,
                            imgHeightPica));
                    if (document.isOpen()) {
                        document.newPage();
                    } else {
                        document.open();
                    }
                    img.setAbsolutePosition(0, 0);
                    document.add(img);
                    if (caption != null) {
                        BaseFont bf = BaseFont.createFont("Helvetica",
                                BaseFont.WINANSI,
                                false);
                        PdfGState gs1 = new PdfGState();
                        gs1.setBlendMode(PdfGState.BM_OVERLAY);
                        PdfContentByte cb = writer.getDirectContent();
                        cb.saveState();
                        cb.setGState(gs1);
                        cb.beginText();
                        cb.setFontAndSize(bf, 40);
                        cb.setTextMatrix(50, 50);
                        cb.showText(caption);
                        cb.endText();
                        cb.restoreState();
                    }

                    label = image.getName();
                    if (label.lastIndexOf('.') > 0) {
                        label = label.substring(0, label.lastIndexOf('.'));
                    }
                    pageLabels.addPageLabel(writer.getPageNumber(),
                                            PdfPageLabels.EMPTY, label);
                    System.out.println("Added image: " + image.getName());
                } catch (Exception e) {
                    System.err.println(e.getMessage());
                }
            }
            if (document.isOpen()) {
                writer.setPageLabels(pageLabels);
                document.close();
            } else {
                System.err.println("No images were found in directory " +
                                   directory.getAbsolutePath());
            }
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
        PhotoAlbum tool = new PhotoAlbum();
        if (args.length < 2) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File) getValue("destfile");
    }
}
