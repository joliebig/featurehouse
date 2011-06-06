
package com.lowagie.text.pdf.fonts.cmaps;


public class CodespaceRange
{

    private byte[] start;
    private byte[] end;

    
    public CodespaceRange()
    {
    }

    
    public byte[] getEnd()
    {
        return this.end;
    }

    
    public void setEnd(byte[] endBytes)
    {
        end = endBytes;
    }

    
    public byte[] getStart()
    {
        return this.start;
    }

    
    public void setStart(byte[] startBytes)
    {
        start = startBytes;
    }

}