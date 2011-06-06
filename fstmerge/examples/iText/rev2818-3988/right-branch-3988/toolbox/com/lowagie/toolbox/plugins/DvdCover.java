



package com.lowagie.toolbox.plugins;

import java.awt.Color;
import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.ColorArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.ImageArgument;
import com.lowagie.toolbox.arguments.StringArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class DvdCover extends AbstractTool {

    static {
        addVersion("$Id: DvdCover.java 3307 2008-05-01 19:55:48Z xlv $");
    }

    
    public DvdCover() {
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW | MENU_EXECUTE_PRINT;
        arguments.add(new FileArgument(this, "destfile", "The file to which the PDF has to be written", true, new PdfFilter()));
        arguments.add(new StringArgument(this, "title", "The title of the DVD"));
        arguments.add(new ColorArgument(this, "backgroundcolor", "The backgroundcolor of the DVD Cover (for instance 0xFFFFFF)"));
        arguments.add(new ImageArgument(this, "front", "The front image of the DVD Cover"));
        arguments.add(new ImageArgument(this, "back", "The back image of the DVD Cover"));
        arguments.add(new ImageArgument(this, "side", "The side image of the DVD Cover"));
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Make your own DVD Cover", true, false, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== DvdCover OPENED ===");
    }

    
    public void execute() {
        try {
            
            Rectangle pageSize = new Rectangle(780, 525);
            if (getValue("backgroundcolor") != null) pageSize.setBackgroundColor((Color)getValue("backgroundcolor"));
            Document document = new Document(pageSize);
            
            
            
            if (getValue("destfile") == null) throw new DocumentException("You must provide a destination file!");
            PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream((File)getValue("destfile")));

            
            document.open();

            
            PdfContentByte cb = writer.getDirectContent();
            if (getValue("title") != null) {
                cb.setFontAndSize(BaseFont.createFont(BaseFont.HELVETICA, BaseFont.WINANSI, false), 24);
                cb.beginText();
                if (getValue("front") == null) {
                    cb.showTextAligned(Element.ALIGN_CENTER, (String)getValue("title"), 595f, 262f, 0f);
                }
                if (getValue("side") == null) {
                    cb.showTextAligned(Element.ALIGN_CENTER, (String)getValue("title"), 385f, 262f, 270f);
                }
                cb.endText();
            }
            cb.moveTo(370, 0);
            cb.lineTo(370, 525);
            cb.moveTo(410, 525);
            cb.lineTo(410, 0);
            cb.stroke();
            if (getValue("front") != null) {
                Image front = (Image)getValue("front");
                front.scaleToFit(370, 525);
                front.setAbsolutePosition(410f + (370f - front.getScaledWidth()) / 2f, (525f - front.getScaledHeight()) / 2f);
                document.add(front);
            }
            if (getValue("back") != null) {
                Image back = (Image)getValue("back");
                back.scaleToFit(370, 525);
                back.setAbsolutePosition((370f - back.getScaledWidth()) / 2f, (525f - back.getScaledHeight()) / 2f);
                document.add(back);
            }
            if (getValue("side") != null) {
                Image side = (Image)getValue("side");
                side.scaleToFit(40, 525);
                side.setAbsolutePosition(370 + (40f - side.getScaledWidth()) / 2f, (525f - side.getScaledHeight()) / 2f);
                document.add(side);
            }

            
            document.close();
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
        DvdCover tool = new DvdCover();
        if (args.length == 0) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        return (File)getValue("destfile");
    }
}
