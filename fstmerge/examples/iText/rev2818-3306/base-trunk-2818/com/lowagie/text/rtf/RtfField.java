

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;



public interface RtfField {


    
    public void write( RtfWriter writer, OutputStream out ) throws IOException;
}


