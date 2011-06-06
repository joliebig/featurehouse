
package com.lowagie.toolbox.plugins;

import java.io.*;
import java.util.*;

import javax.swing.*;

import com.lowagie.text.*;
import com.lowagie.text.pdf.*;
import com.lowagie.toolbox.*;
import com.lowagie.toolbox.arguments.*;
import com.lowagie.toolbox.arguments.filters.*;


public class Normalize
    extends AbstractTool {

  static {
    addVersion("$Id: Normalize.java 3393 2008-05-16 21:33:55Z xlv $");
  }

  FileArgument destfile = null;
  
  public Normalize() {
    menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
    FileArgument inputfile = new FileArgument(this, "srcfile",
                                   "The file you want to normalize", false,
                                   new PdfFilter());
    arguments.add(inputfile);
    destfile = new FileArgument(this, "destfile", "The resulting PDF", true,
                                   new PdfFilter());
    arguments.add(destfile);
    inputfile.addPropertyChangeListener(destfile);
  }


protected void createFrame() {
    internalFrame = new JInternalFrame("Normalize", true, false, true);
    internalFrame.setSize(300, 80);
    internalFrame.setJMenuBar(getMenubar());
    System.out.println("=== Normalize OPENED ===");
  }

  int pagecount;
  float width;
  float height;
  PdfDictionary lastpage = null;
  float tolerancex = 60;
  float tolerancey = 60;
  int pagecountinsertedpages;
  int pagecountrotatedpages;

  protected void iteratePages(PdfDictionary page, PdfReader pdfreader,
                              ArrayList<PdfDictionary> pageInh,
                              int count_in_leaf, PdfWriter writer) throws
      IOException {
    float curwidth;
    float curheight;
    PdfArray kidsPR = (PdfArray) PdfReader.getPdfObject(page.get(PdfName.KIDS));

    if (kidsPR == null) {
      PdfArray arr = (PdfArray) page.get(PdfName.MEDIABOX);
      ArrayList<PdfObject> arl = arr.getArrayList();
      curwidth = Float.parseFloat(arl.get(2).toString());
      curheight = Float.parseFloat(arl.get(3).toString());

      PdfNumber rotation = (PdfNumber) PdfReader.getPdfObject(page.get(PdfName.
          ROTATE));

      if (rotation == null) {
        System.out.println("optional rotation missing");
        rotation = new PdfNumber(0);
      }

      Ausrichtung ausr = new Ausrichtung(rotation.floatValue(),
                                         new Rectangle(curwidth, curheight));

      switch (ausr.type) {
        case Ausrichtung.A4Landscape:
        case Ausrichtung.A3Portrait:
          ausr.rotate();
          page.put(PdfName.ROTATE, new PdfNumber(ausr.getRotation()));
          System.out.println("rotate page:" + (pagecount + 1) + " targetformat: " +
                             ausr);
          this.pagecountrotatedpages++;

          break;
      }

      curwidth = ausr.getM5();
      curheight = ausr.getM6();

      if ( ( (pagecount + 1) % 2) == 0) {
        if ( (Math.abs(curwidth - width) > tolerancex) ||
            (Math.abs(curheight - height) > tolerancey)) {
          Seitehinzufuegen(page, count_in_leaf, writer, arl);
          this.pagecountinsertedpages++;
        }
      }

      
      if ( ( (pagecount + 1) % 2) == 1) {
        width = curwidth;
        height = curheight;
        lastpage = page;
      }

      pageInh.add(pagecount, page);
      pagecount++;
    }
    else {
      page.put(PdfName.TYPE, PdfName.PAGES);

      ArrayList<PdfObject> kids = kidsPR.getArrayList();

      for (int k = 0; k < kids.size(); ++k) {
        PdfDictionary kid = (PdfDictionary) PdfReader.getPdfObject(
                kids.get(k));
        iteratePages(kid, pdfreader, pageInh, k, writer);
      }
    }
  }

  private void Seitehinzufuegen(PdfDictionary page, int count_in_leaf,
                                PdfWriter writer,
                                ArrayList<PdfObject> arl) throws IOException {
    System.out.print("change!");

    PdfDictionary parent = (PdfDictionary) PdfReader.getPdfObject(page.get(
        PdfName.PARENT));
    PdfArray kids = (PdfArray) PdfReader.getPdfObject(parent.get(PdfName.KIDS));
    PdfIndirectReference ref = writer.getPdfIndirectReference();
    kids.getArrayList().add(count_in_leaf, ref);

    PdfDictionary newPage = new PdfDictionary(PdfName.PAGE);
    newPage.merge(lastpage);
    newPage.remove(PdfName.CONTENTS);
    newPage.remove(PdfName.ANNOTS);
    newPage.put(PdfName.RESOURCES, new PdfDictionary());
    writer.addToBody(newPage, ref);

    PdfNumber count = null;

    while (parent != null) {
      count = (PdfNumber) PdfReader.getPdfObject(parent.get(PdfName.COUNT));

      parent.put(PdfName.COUNT, new PdfNumber(count.intValue() + 1));
      parent = (PdfDictionary) PdfReader.getPdfObject(parent.get(PdfName.PARENT));
    }

    System.out.println("page:" + (pagecount + 1) + " nr in leaf:" +
                       count_in_leaf + " arl x:" +
                       arl.get(0) + " y:" + arl.get(1) + " width:" + arl.get(2) +
                       " height:" + arl.get(3));
  }


public void execute() {
    try {
      if (getValue("srcfile") == null) {
        throw new InstantiationException("You need to choose a sourcefile");
      }
      File src = (File) getValue("srcfile");
      if (getValue("destfile") == null) {
        throw new InstantiationException(
            "You need to choose a destination file");
      }
      File dest = (File) getValue("destfile");

      pagecountinsertedpages = 0;
      pagecountrotatedpages = 0;
      pagecount = 0;
      PdfReader reader = new PdfReader(src.getAbsolutePath());
      PdfStamper stp = new PdfStamper(reader, new FileOutputStream(dest));

      PdfWriter writer = stp.getWriter();

      ArrayList<PdfDictionary> pageInh = new ArrayList<PdfDictionary>();
      PdfDictionary catalog = reader.getCatalog();
      PdfDictionary rootPages = (PdfDictionary) PdfReader.getPdfObject(catalog.
          get(
              PdfName.PAGES));
      iteratePages(rootPages, reader, pageInh, 0, writer);

      if ( ( (pagecount) % 2) == 1) {
        appendemptypageatend(reader, writer);
        this.pagecountinsertedpages++;
      }

      stp.close();
      System.out.println("In " + dest.getAbsolutePath() + " pages= " +
                         pagecount +
                         " inserted pages=" + this.getPagecountinsertedpages() +
                         " rotated pages=" +
                         this.getPagecountrotatedpages());
    }
    catch (Exception e) {
      e.printStackTrace(System.out);
    }
  }

  private void appendemptypageatend(PdfReader reader, PdfWriter writer) throws
      IOException {
    System.out.println("last page odd. add page!");

    PdfDictionary page = reader.getPageN(reader.getNumberOfPages());
    PdfDictionary parent = (PdfDictionary) PdfReader.getPdfObject(page.get(
        PdfName.PARENT));
    PdfArray kids = (PdfArray) PdfReader.getPdfObject(parent.get(PdfName.KIDS));
    PdfIndirectReference ref = writer.getPdfIndirectReference();
    kids.add(ref);

    PdfDictionary newPage = new PdfDictionary(PdfName.PAGE);
    newPage.merge(lastpage);
    newPage.remove(PdfName.CONTENTS);
    newPage.remove(PdfName.ANNOTS);
    newPage.put(PdfName.RESOURCES, new PdfDictionary());
    writer.addToBody(newPage, ref);

    PdfNumber count = null;

    while (parent != null) {
      count = (PdfNumber) PdfReader.getPdfObject(parent.get(PdfName.COUNT));
      parent.put(PdfName.COUNT, new PdfNumber(count.intValue() + 1));
      parent = (PdfDictionary) PdfReader.getPdfObject(parent.get(PdfName.PARENT));
    }
  }

  public int getPagecountinsertedpages() {
    return pagecountinsertedpages;
  }

  public int getPagecountrotatedpages() {
    return pagecountrotatedpages;
  }

    
    public void valueHasChanged(AbstractArgument arg) {
    if (internalFrame == null) {
      
      return;
    }
    
    if (destfile.getValue() == null && arg.getName().equalsIgnoreCase("srcfile")) {
      String filename = arg.getValue().toString();
      String filenameout = filename.substring(0, filename.indexOf(".",
          filename.length() - 4)) + "_out.pdf";
      destfile.setValue(filenameout);
    }
  }

    
    public static void main(String[] args) {
    Normalize tool = new Normalize();
    if (args.length < 2) {
      System.err.println(tool.getUsage());
    }
    tool.setMainArguments(args);
    tool.execute();
  }

    
    protected File getDestPathPDF() throws InstantiationException {
    return (File) getValue("destfile");
  }

  public class Ausrichtung {
    static final float tolerance = 60;
    static final int UNKNOWN = 0;
    static final int A4Portrait = 1;
    static final int A4Landscape = 2;
    static final int A3Portrait = 3;
    static final int A3Landscape = 4;
    float rotation;
    Rectangle rect;
    float m5;
    float m6;
    int type;
    public Ausrichtung() {
      this(0, new Rectangle(1, 1));
    }

    public Ausrichtung(float rotation, Rectangle unrotatedoriginalrect) {
      this.rotation = rotation;
      if ( (rotation == 90) || (rotation == 270)) {
        rect = unrotatedoriginalrect.rotate();
      }
      else {
        rect = unrotatedoriginalrect;
      }

      m5 = rect.getWidth();
      m6 = rect.getHeight();
      klassifiziere();

    }

    private void klassifiziere() {
      if (Math.abs(rect.getWidth() - 595) < tolerance &&
          Math.abs(rect.getHeight() - 842) < tolerance) {
        this.type = A4Portrait;
      }
      else if (Math.abs(rect.getWidth() - 842) < tolerance &&
               Math.abs(rect.getHeight() - 595) < tolerance) {
        this.type = A4Landscape;
      }
      else if (Math.abs(rect.getWidth() - 1190) < tolerance &&
               Math.abs(rect.getHeight() - 842) < tolerance) {
        this.type = A3Landscape;
      }
      else if (Math.abs(rect.getWidth() - 842) < tolerance &&
               Math.abs(rect.getHeight() - 1190) < tolerance) {
        this.type = A3Portrait;
      }
      else {
        type = UNKNOWN;
      }
    }

    public float getM5() {
      return m5;
    }

    public float getM6() {
      return m6;
    }

    public String toString() {
      String back;
      switch (type) {
        case UNKNOWN:
          back = rect.getWidth() + "*" + rect.getHeight();
          break;
        case A3Landscape:
          back = "A3 Landscape";
          break;
        case A3Portrait:
          back = "A3 Portrait";
          break;
        case A4Landscape:
          back = "A4 Landscape";
          break;
        case A4Portrait:
          back = "A4 Portrait";
          break;
        default:
          back = "";
      }
      return back;
    }

    public void rotate() {
      rect = rect.rotate();
      m5 = rect.getWidth();
      m6 = rect.getHeight();
      rotation += 90;
      rotation = rotation % 360;
      klassifiziere();
    }

    public float getRotation() {
      return rotation;
    }
  }

}
