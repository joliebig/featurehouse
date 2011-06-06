

package com.lowagie.text.rtf;

import com.lowagie.text.Anchor;
import com.lowagie.text.Annotation;
import com.lowagie.text.Chapter;
import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.Meta;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Section;
import com.lowagie.text.SimpleTable;
import com.lowagie.text.Table;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.document.RtfInfoElement;
import com.lowagie.text.rtf.field.RtfAnchor;
import com.lowagie.text.rtf.graphic.RtfImage;
import com.lowagie.text.rtf.list.RtfList;
import com.lowagie.text.rtf.list.RtfListItem;
import com.lowagie.text.rtf.table.RtfTable;
import com.lowagie.text.rtf.text.RtfAnnotation;
import com.lowagie.text.rtf.text.RtfChapter;
import com.lowagie.text.rtf.text.RtfChunk;
import com.lowagie.text.rtf.text.RtfNewPage;
import com.lowagie.text.rtf.text.RtfParagraph;
import com.lowagie.text.rtf.text.RtfPhrase;
import com.lowagie.text.rtf.text.RtfSection;



public class RtfMapper {

    
    RtfDocument rtfDoc;
    
    
    public RtfMapper(RtfDocument doc) {
        this.rtfDoc = doc;
    }
    
    
    public RtfBasicElement mapElement(Element element) throws DocumentException {
        RtfBasicElement rtfElement = null;

        if(element instanceof RtfBasicElement) {
            rtfElement = (RtfBasicElement) element;
            rtfElement.setRtfDocument(this.rtfDoc);
            return rtfElement;
        }
        switch(element.type()) {
            case Element.CHUNK:
                if(((Chunk) element).getImage() != null) {
                    rtfElement = new RtfImage(rtfDoc, ((Chunk) element).getImage());
                } else if(((Chunk) element).hasAttributes() && ((Chunk) element).getAttributes().containsKey(Chunk.NEWPAGE)) {
                    rtfElement = new RtfNewPage(rtfDoc);
                } else {
                    rtfElement = new RtfChunk(rtfDoc, (Chunk) element);
                }
                break;
            case Element.PHRASE:
                rtfElement = new RtfPhrase(rtfDoc, (Phrase) element);
                break;
            case Element.PARAGRAPH:
                rtfElement = new RtfParagraph(rtfDoc, (Paragraph) element);
                break;
            case Element.ANCHOR:
                rtfElement = new RtfAnchor(rtfDoc, (Anchor) element);
                break;
            case Element.ANNOTATION:
                rtfElement = new RtfAnnotation(rtfDoc, (Annotation) element);
                break;
            case Element.IMGRAW:
            case Element.IMGTEMPLATE:
            case Element.JPEG:
                rtfElement = new RtfImage(rtfDoc, (Image) element);
                break;
            case Element.AUTHOR: 
            case Element.SUBJECT:
            case Element.KEYWORDS:
            case Element.TITLE:
            case Element.PRODUCER:
            case Element.CREATIONDATE:
                rtfElement = new RtfInfoElement(rtfDoc, (Meta) element);
                break;
            case Element.LIST:
                rtfElement = new RtfList(rtfDoc, (List) element);
                break;
            case Element.LISTITEM:
                rtfElement = new RtfListItem(rtfDoc, (ListItem) element);
                break;
            case Element.SECTION:
                rtfElement = new RtfSection(rtfDoc, (Section) element);
                break;
            case Element.CHAPTER:
                rtfElement = new RtfChapter(rtfDoc, (Chapter) element);
                break;
            case Element.TABLE:
                try {
                    rtfElement = new RtfTable(rtfDoc, (Table) element);
                }
                catch(ClassCastException e) {
                    rtfElement = new RtfTable(rtfDoc, ((SimpleTable) element).createTable());
                }
                break;
        }
        
        return rtfElement;
    }
}
