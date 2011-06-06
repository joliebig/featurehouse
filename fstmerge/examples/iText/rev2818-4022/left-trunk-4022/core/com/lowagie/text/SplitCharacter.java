

package com.lowagie.text;

import com.lowagie.text.pdf.PdfChunk;



public interface SplitCharacter {
    
    
    public boolean isSplitCharacter(int start, int current, int end, char cc[], PdfChunk ck[]);
}
