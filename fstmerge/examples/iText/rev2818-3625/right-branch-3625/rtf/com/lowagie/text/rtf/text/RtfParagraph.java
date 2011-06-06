

package com.lowagie.text.rtf.text;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Paragraph;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.graphic.RtfImage;
import com.lowagie.text.rtf.style.RtfFont;
import com.lowagie.text.rtf.style.RtfParagraphStyle;



public class RtfParagraph extends RtfPhrase {

    
    public static final byte[] PARAGRAPH = "\\par".getBytes();
    
    
    protected RtfParagraphStyle paragraphStyle = null;
    
    
    public RtfParagraph(RtfDocument doc, Paragraph paragraph) {
        super(doc);
        RtfFont baseFont = null;
        if(paragraph.getFont() instanceof RtfParagraphStyle) {
            this.paragraphStyle = this.document.getDocumentHeader().getRtfParagraphStyle(((RtfParagraphStyle) paragraph.getFont()).getStyleName());
            baseFont = this.paragraphStyle;
        } else {
            baseFont = new RtfFont(this.document, paragraph.getFont());
            this.paragraphStyle = new RtfParagraphStyle(this.document, this.document.getDocumentHeader().getRtfParagraphStyle("Normal"));
            this.paragraphStyle.setAlignment(paragraph.getAlignment());
            this.paragraphStyle.setFirstLineIndent((int) (paragraph.getFirstLineIndent() * RtfElement.TWIPS_FACTOR));
            this.paragraphStyle.setIndentLeft((int) (paragraph.getIndentationLeft() * RtfElement.TWIPS_FACTOR));
            this.paragraphStyle.setIndentRight((int) (paragraph.getIndentationRight() * RtfElement.TWIPS_FACTOR));
            this.paragraphStyle.setSpacingBefore((int) (paragraph.spacingBefore() * RtfElement.TWIPS_FACTOR));
            this.paragraphStyle.setSpacingAfter((int) (paragraph.spacingAfter() * RtfElement.TWIPS_FACTOR));
            if(paragraph.hasLeading()) {
                this.paragraphStyle.setLineLeading((int) (paragraph.getLeading() * RtfElement.TWIPS_FACTOR));
            }
            this.paragraphStyle.setKeepTogether(paragraph.getKeepTogether());
        }        
        for(int i = 0; i < paragraph.size(); i++) {
            Element chunk = paragraph.get(i);
            if(chunk instanceof Chunk) {
                ((Chunk) chunk).setFont(baseFont.difference(((Chunk) chunk).getFont()));
            } else if(chunk instanceof RtfImage) {
                ((RtfImage) chunks.get(i)).setAlignment(this.paragraphStyle.getAlignment());
            }
            try {
                RtfBasicElement[] rtfElements = doc.getMapper().mapElement(chunk);
                for(int j = 0; j < rtfElements.length; j++) {
                    chunks.add(rtfElements[j]);
                }
            } catch(DocumentException de) {
            }
        }
    }
    
    
    public void setKeepTogetherWithNext(boolean keepTogetherWithNext) {
        this.paragraphStyle.setKeepTogetherWithNext(keepTogetherWithNext);
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(PARAGRAPH_DEFAULTS);
        result.write(PLAIN);

        if(inTable) {
            result.write(IN_TABLE);
        }
        
        if(this.paragraphStyle != null) {
            this.paragraphStyle.writeBegin(result);
        }
        result.write("\\plain".getBytes());
        
        for(int i = 0; i < chunks.size(); i++) {
            RtfBasicElement rbe = chunks.get(i);
            rbe.writeContent(result);
        }
        
        if(this.paragraphStyle != null) {
            this.paragraphStyle.writeEnd(result);
        }
        
        if(!inTable) {
            result.write(PARAGRAPH);
        }
        this.document.outputDebugLinebreak(result);
    }        
    
    
    public int getIndentLeft() {
        return this.paragraphStyle.getIndentLeft();
    }
    
    
    public void setIndentLeft(int indentLeft) {
        this.paragraphStyle.setIndentLeft(indentLeft);
    }
    
    
    public int getIndentRight()  {
        return this.paragraphStyle.getIndentRight();
    }
    
    
    public void setIndentRight(int indentRight) {
        this.paragraphStyle.setIndentRight(indentRight);
    }
}
