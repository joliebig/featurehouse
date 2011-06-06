
package com.lowagie.text.pdf.fonts.cmaps;

import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class CMap
{
    private List<CodespaceRange> codeSpaceRanges = new ArrayList<CodespaceRange>();
    private Map<Integer, String> singleByteMappings = new HashMap<Integer, String>();
    private Map<Integer, String> doubleByteMappings = new HashMap<Integer, String>();

    
    public CMap()
    {
        
    }
    
    
    public boolean hasOneByteMappings()
    {
        return !singleByteMappings.isEmpty();
    }
    
    
    public boolean hasTwoByteMappings()
    {
        return !doubleByteMappings.isEmpty();
    }

    
    public String lookup( byte[] code, int offset, int length )
    {

        String result = null;
        Integer key = null;
        if( length == 1 )
        {
            
            key = new Integer( (code[offset]+256)%256 );
            result = singleByteMappings.get( key );
        }
        else if( length == 2 )
        {
            int intKey = (code[offset]+256)%256;
            intKey <<= 8;
            intKey += (code[offset+1]+256)%256;
            key = new Integer( intKey );

            result = doubleByteMappings.get( key );
        }

        return result;
    }

    
    public void addMapping( byte[] src, String dest ) throws IOException
    {
        if( src.length == 1 )
        {
            singleByteMappings.put( new Integer( src[0] ), dest );
        }
        else if( src.length == 2 )
        {
            int intSrc = src[0]&0xFF;
            intSrc <<= 8;
            intSrc |= (src[1]&0xFF);
            doubleByteMappings.put( new Integer( intSrc ), dest );
        }
        else
        {
            throw new IOException( "Mapping code should be 1 or two bytes and not " + src.length );
        }
    }


    
    public void addCodespaceRange( CodespaceRange range )
    {
        codeSpaceRanges.add( range );
    }

    
    public List<CodespaceRange> getCodeSpaceRanges()
    {
        return codeSpaceRanges;
    }

}