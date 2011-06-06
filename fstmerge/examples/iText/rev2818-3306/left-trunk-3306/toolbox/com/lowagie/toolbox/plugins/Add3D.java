

package com.lowagie.toolbox.plugins;

import java.io.*;

import javax.swing.*;

import com.lowagie.text.*;
import com.lowagie.text.pdf.*;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.*;
import com.lowagie.toolbox.arguments.filters.PdfFilter;
import com.lowagie.toolbox.arguments.filters.U3DFilter;

import java.net.*;


public class Add3D extends AbstractTool {
    static {
        addVersion("$Id: Add3D.java 3271 2008-04-18 20:39:42Z xlv $");
    }

    FileArgument destfile = null;
    public static final String PDF_NAME_3D = "3D";
    public static final String PDF_NAME_3DD = "3DD";
    public static final String PDF_NAME_3DV = "3DV";
    public static final String PDF_NAME_3DVIEW = "3DView";
    public static final String PDF_NAME_C2W = "C2W";
    public static final String PDF_NAME_IN = "IN";
    public static final String PDF_NAME_MS = "MS";
    public static final String PDF_NAME_U3D = "U3D";
    public static final String PDF_NAME_XN = "XN";

    
    public Add3D() {
        super();
        menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
        FileArgument inputfile = new FileArgument(this, "srcfile",
                                                  "The file you want to add the u3d File", false,
                                                  new PdfFilter());
        arguments.add(inputfile);
        FileArgument u3dinputfile = new FileArgument(this, "srcu3dfile",
                "The u3d file you want to add", false,
                new U3DFilter());
        arguments.add(u3dinputfile);
        StringArgument pagenumber = new StringArgument(this, "pagenumber",
                                   "The pagenumber where to add the u3d annotation");
                           pagenumber.setValue("1");
        arguments.add(pagenumber);
        destfile = new FileArgument(this, "destfile",
                                    "The file that contains the u3d annotation after processing",
                                    true, new PdfFilter());
        arguments.add(destfile);
        inputfile.addPropertyChangeListener(destfile);
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("Add3D", true, true, true);
        internalFrame.setSize(300, 80);
        internalFrame.setJMenuBar(getMenubar());
        System.out.println("=== Add3D OPENED ===");
    }

    
    public void execute() {
        try {
            if (getValue("srcfile") == null) {
                throw new InstantiationException(
                        "You need to choose a sourcefile");
            }
            if (getValue("srcu3dfile") == null) {
                throw new InstantiationException(
                        "You need to choose a u3d file");
            }
            if (getValue("destfile") == null) {
                throw new InstantiationException(
                        "You need to choose a destination file");
            }
            int pagenumber = Integer.parseInt( (String) getValue("pagenumber"));
            
            
            PdfIndirectReference streamRef;
            PdfIndirectObject objRef;
            PdfReader reader = new PdfReader(((File) getValue("srcfile"))
                                             .getAbsolutePath());

            String u3dFileName = ((File) getValue("srcu3dfile"))
                                 .getAbsolutePath();
            PdfStamper stamp = new PdfStamper(reader, new FileOutputStream(
                    (File) getValue("destfile")));
            PdfWriter wr = stamp.getWriter();
            PdfContentByte cb = stamp.getUnderContent(pagenumber);
            Rectangle rectori = reader.getCropBox(pagenumber);
            Rectangle rect = new Rectangle(new Rectangle(100,
                    rectori.getHeight() - 550, rectori.getWidth() - 100,
                    rectori.getHeight() - 150));
            PdfStream oni = new PdfStream(PdfEncodings.convertToBytes(
                    "runtime.setCurrentTool(\"Rotate\");", null));
            oni.flateCompress();


            PdfStream stream = new PdfStream(new FileInputStream(u3dFileName),
                                             wr);
            stream.put(new PdfName("OnInstantiate"),
                       wr.addToBody(oni).getIndirectReference());
            stream.put(PdfName.TYPE, new PdfName(PDF_NAME_3D)); 
            stream.put(PdfName.SUBTYPE, new PdfName(PDF_NAME_U3D));
            stream.flateCompress();

            streamRef = wr.addToBody(stream).getIndirectReference(); 
            stream.writeLength();

            
            
            
            
            PdfDictionary dict = new PdfDictionary(new PdfName(PDF_NAME_3DVIEW));

            dict.put(new PdfName(PDF_NAME_XN), new PdfString("Default"));
            dict.put(new PdfName(PDF_NAME_IN), new PdfString("Unnamed"));
            dict.put(new PdfName(PDF_NAME_MS), PdfName.M); 
            dict.put(new PdfName(PDF_NAME_C2W),
                     new PdfArray(new float[] {1, 0, 0, 0, 0, -1, 0, 1, 0, 3,
                                  -235, 28F})); 
            dict.put(PdfName.CO, new PdfNumber(235)); 

            objRef = wr.addToBody(dict); 

            
            PdfAppearance ap = cb.createAppearance(rect.getRight() - rect.getLeft(),
                    rect.getTop() - rect.getBottom());

            ap.setBoundingBox(rect);


            PdfAnnotation annot = new PdfAnnotation(wr, rect);

            annot.put(PdfName.CONTENTS, new PdfString("3D Model"));
            annot.put(PdfName.SUBTYPE, new PdfName(PDF_NAME_3D)); 
            annot.put(PdfName.TYPE, PdfName.ANNOT);
            annot.put(new PdfName(PDF_NAME_3DD), streamRef); 
            annot.put(new PdfName(PDF_NAME_3DV), objRef.getIndirectReference()); 
            annot.put(new PdfName("3DI"), PdfBoolean.PDFFALSE);

            PdfDictionary adi = new PdfDictionary();
            adi.put(PdfName.A, new PdfName("PO"));
            adi.put(new PdfName("DIS"), PdfName.I);
            annot.put(new PdfName("3DA"), adi);

            annot.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, ap); 
            annot.setPage();

            
            stamp.addAnnotation(annot, pagenumber);
            AddButton(100, 100, "Rotate",
                    "im = this.getAnnots3D(0)[0].context3D;\rim.runtime.setCurrentTool(\"Rotate\");",
                      "rotate.png", wr);
            AddButton(150, 100, "Pan",
                    "im = this.getAnnots3D(0)[0].context3D;\rim.runtime.setCurrentTool(\"Pan\");",
                      "translate.png", wr);
            AddButton(200, 100, "Zoom",
                    "im = this.getAnnots3D(0)[0].context3D;\rim.runtime.setCurrentTool(\"Zoom\");",
                      "zoom.png", wr);
            stamp.close();
        } catch (Exception e) {
            JOptionPane.showMessageDialog(internalFrame, e.getMessage(), e
                                          .getClass().getName(),
                                          JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
        }
    }

    public static void AddButton(float x, float y, String fname, String js,
                                 String image, PdfWriter wr) {
        try {





            Image img = Image.getInstance(image);
            PushbuttonField bt = new PushbuttonField(wr,
                    new Rectangle(x, y, x + img.getPlainWidth(),
                                  y + img.getPlainHeight()), fname);
            bt.setLayout(PushbuttonField.LAYOUT_ICON_ONLY);
            bt.setImage(img);
            PdfFormField ff = bt.getField();
            PdfAction ac = PdfAction.javaScript(js, wr);
            ff.setAction(ac);
            wr.addAnnotation(ff);
        } catch (MalformedURLException ex) {
        } catch (IOException ex) {
        } catch (BadElementException ex) {
        } catch (DocumentException ex) {
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
        Add3D add3d = new Add3D();
        if (args.length != 4) {
            System.err.println(add3d.getUsage());
        }
        add3d.setMainArguments(args);
        add3d.execute();
    }
}
