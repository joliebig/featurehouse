

package com.lowagie.text.rtf.text;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Section;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.field.RtfTOCEntry;



public class RtfSection extends RtfElement {

    
    protected RtfParagraph title = null;
    
    protected ArrayList<RtfBasicElement> items = null;
    
    
    public RtfSection(RtfDocument doc, Section section) {
        super(doc);
        items = new ArrayList<RtfBasicElement>();
        try {
            if(section.getTitle() != null) {
                this.title = (RtfParagraph) doc.getMapper().mapElement(section.getTitle());
            }
            if(document.getAutogenerateTOCEntries()) {
                StringBuffer titleText = new StringBuffer();
                for(Element element: section.getTitle()) {
                    if(element.type() == Element.CHUNK) {
                        titleText.append(((Chunk) element).getContent());
                    }
                }
                if(titleText.toString().trim().length() > 0) {
                    RtfTOCEntry tocEntry = new RtfTOCEntry(titleText.toString());
                    tocEntry.setRtfDocument(this.document);
                    this.items.add(tocEntry);
                }
            }
            for(Element element: section) {
                RtfBasicElement rtfElement = doc.getMapper().mapElement(element);
                if(rtfElement != null) {
                    items.add(rtfElement);
                }
            }
            
            updateIndentation(section.getIndentationLeft(), section.getIndentationRight(), section.getIndentation());
        } catch(DocumentException de) {
            de.printStackTrace();
        }
    }
    
    
    public byte[] write()
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(RtfParagraph.PARAGRAPH);
        if(this.title != null) {
            
            this.title.writeContent(result);
        }
        for(RtfBasicElement rbe: items) {
            
            rbe.writeContent(result);
        }
    }        

    
    
    public void setInTable(boolean inTable) {
        super.setInTable(inTable);
        for(int i = 0; i < this.items.size(); i++) {
            this.items.get(i).setInTable(inTable);
        }
    }
    
    
    public void setInHeader(boolean inHeader) {
        super.setInHeader(inHeader);
        for(int i = 0; i < this.items.size(); i++) {
            this.items.get(i).setInHeader(inHeader);
        }
    }

    
    private void updateIndentation(float indentLeft, float indentRight, float indentContent) {
        if(this.title != null) {
            this.title.setIndentLeft((int) (this.title.getIndentLeft() + indentLeft * RtfElement.TWIPS_FACTOR));
            this.title.setIndentRight((int) (this.title.getIndentRight() + indentRight * RtfElement.TWIPS_FACTOR));
        }
        for(int i = 0; i < this.items.size(); i++) {
            RtfBasicElement rtfElement = this.items.get(i);
            if(rtfElement instanceof RtfSection) {
                ((RtfSection) rtfElement).updateIndentation(indentLeft + indentContent, indentRight, 0);
            } else if(rtfElement instanceof RtfParagraph) {
                ((RtfParagraph) rtfElement).setIndentLeft((int) (((RtfParagraph) rtfElement).getIndentLeft() + (indentLeft + indentContent) * RtfElement.TWIPS_FACTOR));
                ((RtfParagraph) rtfElement).setIndentRight((int) (((RtfParagraph) rtfElement).getIndentRight() + indentRight * RtfElement.TWIPS_FACTOR));
            }
        }
    }
}
