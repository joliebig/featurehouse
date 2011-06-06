

package com.lowagie.tools;

import java.io.FileOutputStream;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.PageSize;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfWriter;


public class HandoutPdf extends java.lang.Object {
    
    
    public static void main (String args[]) {
        if (args.length != 3) {
            System.err.println("arguments: srcfile destfile pages");
        }
        else {
            try {
                int pages = Integer.parseInt(args[2]);
                if (pages < 2 || pages > 8) {
                    throw new DocumentException("You can't have " + pages + " pages on one page (minimum 2; maximum 8).");
                }
                
                float x1 = 30f;
                float x2 = 280f;
                float x3 = 320f;
                float x4 = 565f;
                
                float[] y1 = new float[pages];
                float[] y2 = new float[pages];
                
                float height = (778f - (20f * (pages - 1))) / pages;
                y1[0] = 812f;
                y2[0] = 812f - height;
                
                for (int i = 1; i < pages; i++) {
                    y1[i] = y2[i - 1] - 20f;
                    y2[i] = y1[i] - height;
                }
                
                
                PdfReader reader = new PdfReader(args[0]);
                
                int n = reader.getNumberOfPages();
                System.out.println("There are " + n + " pages in the original file.");
                
                
                Document document = new Document(PageSize.A4);
                
                PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream(args[1]));
                
                document.open();
                PdfContentByte cb = writer.getDirectContent();
                PdfImportedPage page;
                int rotation;
                int i = 0;
                int p = 0;
                
                while (i < n) {
                    i++;
                    Rectangle rect = reader.getPageSizeWithRotation(i);
                    float factorx = (x2 - x1) / rect.getWidth();
                    float factory = (y1[p] - y2[p]) / rect.getHeight();
                    float factor = (factorx < factory ? factorx : factory);
                    float dx = (factorx == factor ? 0f : ((x2 - x1) - rect.getWidth() * factor) / 2f);
                    float dy = (factory == factor ? 0f : ((y1[p] - y2[p]) - rect.getHeight() * factor) / 2f);
                    page = writer.getImportedPage(reader, i);
                    rotation = reader.getPageRotation(i);
                    if (rotation == 90 || rotation == 270) {
                        cb.addTemplate(page, 0, -factor, factor, 0, x1 + dx, y2[p] + dy + rect.getHeight() * factor);
                    }
                    else {
                        cb.addTemplate(page, factor, 0, 0, factor, x1 + dx, y2[p] + dy);
                    }
                    cb.setRGBColorStroke(0xC0, 0xC0, 0xC0);
                    cb.rectangle(x3 - 5f, y2[p] - 5f, x4 - x3 + 10f, y1[p] - y2[p] + 10f);
                    for (float l = y1[p] - 19; l > y2[p]; l -= 16) {
                        cb.moveTo(x3, l);
                        cb.lineTo(x4, l);
                    }
                    cb.rectangle(x1 + dx, y2[p] + dy, rect.getWidth() * factor, rect.getHeight() * factor);
                    cb.stroke();
                    System.out.println("Processed page " + i);
                    p++;
                    if (p == pages) {
                        p = 0;
                        document.newPage();
                    }
                }
                
                document.close();
            }
            catch(Exception e) {
                System.err.println(e.getClass().getName() + ": " + e.getMessage());
            }
        }
    }
}