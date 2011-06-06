

package com.lowagie.rups.io;

import java.io.IOException;
import java.io.OutputStream;


public interface OutputStreamResource {
    
    public void writeTo(OutputStream os) throws IOException;
}
