



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfGState;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.FloatArgument;
import com.lowagie.toolbox.arguments.IntegerArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class Watermarker extends AbstractTool {

    static {
        addVersion("$Id: Watermarker.java 3271 2008-04-18 20:39:42Z xlv $");
    }

    FileArgument destfile = null;
    
    public Watermarker() {
        super();
        FileArgument inputfile = new FileArgument(this, "srcfile",
                                                  "The file you want to watermark", false,
                                                  new PdfFilter());
        arguments.add(inputfile);
        arguments.add(new StringArgument(this, "watermark",
                                         "The text that can be used as watermark"));
        arguments.add(new IntegerArgument(this, "fontsize",
                                         "The fontsize of the watermark text"));
        arguments.add(new FloatArgument(this, "opacity",
                                         "The opacity of the watermark text"));
        destfile = new FileArgument(this, "destfile",
                                    "The file to which the watermarked PDF has to be written",
                                    true, new PdfFilter());
        arguments.add(destfile);
        inputfile.addPropertyChangeListener(destfile);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Watermark", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Watermark OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) {
                throw new InstantiationException(
                        "You need to choose a sourcefile");
            }
            if (getValue("destfile") == null) {
                throw new InstantiationException(
                        "You need to choose a destination file");
            }
            if (getValue("watermark") == null) {
                throw new InstantiationException(
                        "You need to add a text for the watermark");
            }
            int fontsize = Integer.parseInt((String) getValue("fontsize"));
            float opacity = Float.parseFloat((String) getValue("opacity"));
            BaseFont bf = BaseFont.createFont("Helvetica", BaseFont.WINANSI,
                                              false);
            PdfReader reader = new PdfReader(((File) getValue("srcfile"))
                                             .getAbsolutePath());
            int pagecount = reader.getNumberOfPages();
            PdfGState gs1 = new PdfGState();
            gs1.setFillOpacity(opacity);
            String text = (String) getValue("watermark");
            PdfStamper stamp = new PdfStamper(reader, new FileOutputStream(
                    (File) getValue("destfile")));
            float txtwidth = bf.getWidthPoint(text, fontsize);
            for (int i = 1; i <= pagecount; i++) {
                PdfContentByte seitex = stamp.getOverContent(i);
                Rectangle recc = reader.getCropBox(i);
                float winkel = (float) Math.atan(recc.getHeight() /
                                                 recc.getWidth());
                float m1 = (float) Math.cos(winkel);
                float m2 = (float) - Math.sin(winkel);
                float m3 = (float) Math.sin(winkel);
                float m4 = (float) Math.cos(winkel);
                float xoff = (float) ( -Math.cos(winkel) * txtwidth / 2 - Math
                                      .sin(winkel)
                                      * fontsize / 2);
                float yoff = (float) (Math.sin(winkel) * txtwidth / 2 - Math
                                      .cos(winkel)
                                      * fontsize / 2);
                seitex.saveState();
                seitex.setGState(gs1);
                seitex.beginText();
                seitex.setFontAndSize(bf, fontsize);
                seitex.setTextMatrix(m1, m2, m3, m4, xoff + recc.getWidth() / 2,
                                     yoff + recc.getHeight() / 2);
                seitex.showText(text);
                seitex.endText();
                seitex.restoreState();
            }
            stamp.close();
        } catch (Exception e) {
            JOptionPane.showMessageDialog(internalFrame, e.getMessage(), e
                                          .getClass().getName(),
                                          JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
        }
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File) getValue("destfile");
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        if (internalFrame == null) {
            
            
            return;
        }
        if (destfile.getValue() == null &&
            arg.getName().equalsIgnoreCase("srcfile")) {
            String filename = arg.getValue().toString();
            String filenameout = filename.substring(0, filename.indexOf(".",
                    filename.length() - 4)) + "_out.pdf";
            destfile.setValue(filenameout);
        }
    }

    
    public static void main(String[] args) {
        Watermarker watermarker = new Watermarker();
        if (args.length != 5) {
            System.err.println(watermarker.getUsage());
        }
        watermarker.setMainArguments(args);
        watermarker.execute();
    }
}
