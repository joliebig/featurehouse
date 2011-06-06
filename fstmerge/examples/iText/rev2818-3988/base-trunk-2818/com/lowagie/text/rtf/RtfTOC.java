

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;


public class RtfTOC extends Chunk implements RtfField {


    private String      defaultText = "Klicken Sie mit der rechten Maustaste auf diesen Text, um das Inhaltsverzeichnis zu aktualisieren!";

    private boolean     addTOCAsTOCEntry = false;

    private Font        entryFont = null;
    private String      entryName = null;


    
    public RtfTOC( String tocName, Font tocFont ) {
        super( tocName, tocFont );
    }

    
    public void write( RtfWriter writer, OutputStream out ) throws IOException {

        writer.writeInitialFontSignature( out, this );
        out.write( RtfWriter.filterSpecialChar( getContent(), true ).getBytes() );
        writer.writeFinishingFontSignature( out, this );
        
        if (addTOCAsTOCEntry) {
            RtfTOCEntry entry = new RtfTOCEntry( entryName, entryFont );
            entry.hideText();
            try {
                writer.add( entry );
            } catch ( DocumentException de ) {
                throw new ExceptionConverter(de);
            }
        }

        
        out.write( RtfWriter.escape );
        out.write( RtfWriter.paragraph );
        out.write( RtfWriter.delimiter );

        
        out.write( RtfWriter.openGroup );
        out.write( RtfWriter.escape );
        out.write( RtfWriter.field );
            
            out.write( RtfWriter.openGroup );        
            out.write( RtfWriter.escape );
            out.write( RtfWriter.fieldContent );
            out.write( RtfWriter.delimiter );
            out.write( "TOC".getBytes() );
            
            out.write( RtfWriter.delimiter );
            out.write( RtfWriter.escape );        
            out.write( RtfWriter.escape );        
            out.write( "f".getBytes() );
            out.write( RtfWriter.delimiter );
            
            out.write( RtfWriter.escape );        
            out.write( RtfWriter.escape );        
            out.write( "h".getBytes() );
            out.write( RtfWriter.delimiter );
            
            out.write( RtfWriter.delimiter );
            out.write( RtfWriter.escape );        
            out.write( RtfWriter.escape );        
            out.write( "u".getBytes() );
            out.write( RtfWriter.delimiter );
            
            out.write( RtfWriter.delimiter );
            out.write( RtfWriter.escape );        
            out.write( RtfWriter.escape );        
            out.write( "o".getBytes() );
            out.write( RtfWriter.delimiter );
            out.write( "\"1-5\"".getBytes() );
            out.write( RtfWriter.delimiter );
            out.write( RtfWriter.closeGroup );

            
            out.write( RtfWriter.openGroup );        
            out.write( RtfWriter.escape );
            out.write( RtfWriter.fieldDisplay );
            out.write( RtfWriter.delimiter );
            out.write( defaultText.getBytes() );
            out.write( RtfWriter.delimiter );
            out.write( RtfWriter.closeGroup );
        out.write( RtfWriter.closeGroup );
    }

    
    
    public void addTOCAsTOCEntry( String entryName, Font entryFont ) {
        this.addTOCAsTOCEntry = true;
        this.entryFont = entryFont;
        this.entryName = entryName;        
    }

    
    
    public void setDefaultText( String text ) {
        this.defaultText = text;
    }
}
