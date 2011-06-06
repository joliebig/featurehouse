

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;


public interface RtfExtendedElement extends RtfBasicElement
{
    
    public byte[] writeDefinition();

    
    public void writeDefinition(OutputStream out) throws IOException;
}
