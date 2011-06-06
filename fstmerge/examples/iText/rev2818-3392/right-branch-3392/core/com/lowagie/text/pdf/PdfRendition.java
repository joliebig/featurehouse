
package com.lowagie.text.pdf;

import java.io.IOException;


public class PdfRendition extends PdfDictionary {
     PdfRendition(String file, PdfFileSpecification fs, String mimeType) throws IOException{
         put(PdfName.S, new PdfName("MR"));
         put(PdfName.N, new PdfString("Rendition for "+file));
         put(PdfName.C, new PdfMediaClipData(file, fs, mimeType));
     }

}
