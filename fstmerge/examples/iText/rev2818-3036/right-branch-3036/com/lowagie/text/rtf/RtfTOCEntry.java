

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;


public class RtfTOCEntry extends Chunk implements RtfField {


    private boolean         hideText = false;

    private boolean         hidePageNumber = false;    

    private String    entryName;

    private Font      entryFont;    

    private Font      contentFont;    


    
    public RtfTOCEntry( String content, Font contentFont ) {
        this( content, contentFont, content, contentFont );



    }


    
    public RtfTOCEntry( String content, Font contentFont, String entryName, Font entryFont ) {
        super( content, contentFont );
        
        this.entryName = entryName;
        this.entryFont = entryFont;
        this.contentFont = contentFont;
    }

    
    public void write( RtfWriter writer, OutputStream out ) throws IOException {

        if (!hideText) {
            writer.writeInitialFontSignature( out, new Chunk("", contentFont) );
            out.write( RtfWriter.filterSpecialChar( getContent(), true ).getBytes() );
            writer.writeFinishingFontSignature( out, new Chunk("", contentFont) );
        }

        if (!entryFont.equals( contentFont )) {
            writer.writeInitialFontSignature(out, new Chunk("", entryFont) );
            writeField( out );
            writer.writeFinishingFontSignature(out, new Chunk("", entryFont) );
        } else {
            writer.writeInitialFontSignature(out, new Chunk("", contentFont) );
            writeField( out );
            writer.writeFinishingFontSignature(out, new Chunk("", contentFont) );
        }
    }

    
    private void writeField( OutputStream out ) throws IOException {
        
        
        out.write( RtfWriter.openGroup );
        out.write( RtfWriter.escape );
        out.write( "v".getBytes() );

        
        out.write( RtfWriter.openGroup );
        out.write( RtfWriter.escape );
        if (!hidePageNumber) {
            out.write( "tc".getBytes() );
        } else {
            out.write( "tcn".getBytes() );
        }    
        out.write( RtfWriter.delimiter );
        out.write( RtfWriter.filterSpecialChar( entryName, true ).getBytes() );
        out.write( RtfWriter.delimiter );
        out.write( RtfWriter.closeGroup );        

        out.write( RtfWriter.closeGroup );        
    }

    
    public void hideText() {
        hideText = true;
    }

    
    public void hidePageNumber() {
        hidePageNumber = true;
    }
}


