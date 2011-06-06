



package com.lowagie.toolbox.plugins;

import java.io.File;
import java.io.FileOutputStream;

import javax.swing.JInternalFrame;

import com.lowagie.text.pdf.PRIndirectReference;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;


public class RemoveLaunchApplication
    extends AbstractTool {

  static {
    addVersion(
        "$Id: RemoveLaunchApplication.java 3307 2008-05-01 19:55:48Z xlv $");
  }

  
  public RemoveLaunchApplication() {
    menuoptions = MENU_EXECUTE | MENU_EXECUTE_SHOW;
    arguments.add(new FileArgument(this, "srcfile",
                                   "The file from which you want to remove Launch Application actions", false,
                                   new PdfFilter()));
    arguments.add(new FileArgument(this, "destfile",
                                   "The file to which the cleaned up version of the original PDF has to be written", true,
                                   new PdfFilter()));
  }

  
  protected void createFrame() {
    internalFrame = new JInternalFrame("Remove Launch Applications", true, false, true);
    internalFrame.setSize(300, 80);
    internalFrame.setJMenuBar(getMenubar());
    System.out.println("=== Remove Launch Applications OPENED ===");
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

      
      PdfReader reader = new PdfReader(src.getAbsolutePath());
      PdfObject o;
      PdfDictionary d;
      PdfDictionary l;
      PdfName n;
      for (int i = 1; i < reader.getXrefSize(); i++) {
          o = reader.getPdfObject(i);
          if (o instanceof PdfDictionary) {
              d = (PdfDictionary)o;
              o = d.get(PdfName.A);
              if (o == null) continue;
              if (o instanceof PdfDictionary) {
                  l = (PdfDictionary)o;
              }
              else {
                  PRIndirectReference r =(PRIndirectReference)o;
                  l = (PdfDictionary)reader.getPdfObject(r.getNumber());
              }
              n = (PdfName)l.get(PdfName.S);
              if (PdfName.LAUNCH.equals(n)) {
                  if (l.get(PdfName.F) != null) {
                      System.out.println("Removed: " + l.get(PdfName.F));
                      l.remove(PdfName.F);
                  }
                  if (l.get(PdfName.WIN) != null) {
                      System.out.println("Removed: " + l.get(PdfName.WIN));
                      l.remove(PdfName.WIN);
                  }
                  l.put(PdfName.S, PdfName.JAVASCRIPT);
                  l.put(PdfName.JS, new PdfString("app.alert('Launch Application Action removed by iText');\r"));
              }
          }
      }
      PdfStamper stamper = new PdfStamper(reader, new FileOutputStream(dest));
      stamper.close();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

    
    public void valueHasChanged(AbstractArgument arg) {
    if (internalFrame == null) {
      
      return;
    }
    
  }

    
    public static void main(String[] args) {
    RemoveLaunchApplication tool = new RemoveLaunchApplication();
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
