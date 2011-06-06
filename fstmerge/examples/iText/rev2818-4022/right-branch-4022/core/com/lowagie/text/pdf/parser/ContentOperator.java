
package com.lowagie.text.pdf.parser;

import java.util.ArrayList;

import com.lowagie.text.pdf.PdfLiteral;
import com.lowagie.text.pdf.PdfObject;


public interface ContentOperator {
    
    public void invoke(PdfContentStreamProcessor processor, PdfLiteral operator, ArrayList<PdfObject> operands);

}
