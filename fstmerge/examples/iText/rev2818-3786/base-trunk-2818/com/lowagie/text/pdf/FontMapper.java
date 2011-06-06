

package com.lowagie.text.pdf;

import java.awt.Font;



public interface FontMapper {

    

    public BaseFont awtToPdf(Font font);

    

    public Font pdfToAwt(BaseFont font, int size);

}
