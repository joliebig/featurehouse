
package com.lowagie.text.pdf.parser;

import java.util.ArrayList;

import com.lowagie.text.pdf.PdfLiteral;


public interface ContentOperator {
    
    public void invoke(PdfContentStreamProcessor processor, PdfLiteral operator, ArrayList operands);

}
