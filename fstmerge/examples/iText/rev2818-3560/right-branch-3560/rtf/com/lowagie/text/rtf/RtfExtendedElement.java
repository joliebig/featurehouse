

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;


public interface RtfExtendedElement extends RtfBasicElement
{
    
    public void writeDefinition(OutputStream out) throws IOException;
}
