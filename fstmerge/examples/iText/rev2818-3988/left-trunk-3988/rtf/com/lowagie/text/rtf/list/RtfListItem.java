

package com.lowagie.text.rtf.list;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.ListItem;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.text.RtfChunk;
import com.lowagie.text.rtf.text.RtfParagraph;



public class RtfListItem extends RtfParagraph {

    
    private RtfListLevel parentList = null;
    
    private boolean containsInnerList = false;
    
    
    public RtfListItem(RtfDocument doc, ListItem listItem) {
        super(doc, listItem);
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        if(this.paragraphStyle.getSpacingBefore() > 0) {
            result.write(RtfParagraphStyle.SPACING_BEFORE);
            result.write(intToByteArray(paragraphStyle.getSpacingBefore()));
        }
        if(this.paragraphStyle.getSpacingAfter() > 0) {
            result.write(RtfParagraphStyle.SPACING_AFTER);
            result.write(intToByteArray(this.paragraphStyle.getSpacingAfter()));
        }
        if(this.paragraphStyle.getLineLeading() > 0) {
            result.write(RtfParagraph.LINE_SPACING);
            result.write(intToByteArray(this.paragraphStyle.getLineLeading()));
        }
        for(int i = 0; i < chunks.size(); i++) {
            RtfBasicElement rtfElement = (RtfBasicElement) chunks.get(i);
            if(rtfElement instanceof RtfChunk) {
                ((RtfChunk) rtfElement).setSoftLineBreaks(true);
            } else if(rtfElement instanceof RtfList) {
                result.write(RtfParagraph.PARAGRAPH);
                this.containsInnerList = true;
            }
            rtfElement.writeContent(result);
            if(rtfElement instanceof RtfList) {
                switch(this.parentList.getLevelFollowValue()) {
                case RtfListLevel.LIST_LEVEL_FOLLOW_NOTHING:
                    break;
                case RtfListLevel.LIST_LEVEL_FOLLOW_TAB:
                    this.parentList.writeListBeginning(result);
                    result.write(RtfList.TAB);
                    break;
                case RtfListLevel.LIST_LEVEL_FOLLOW_SPACE:
                    this.parentList.writeListBeginning(result);
                    result.write(DocWriter.getISOBytes(" "));
                    break;
                }
            }
        }
    }        

    
    public boolean writeDefinition(OutputStream out) throws IOException
    {
        for(int i = 0; i < chunks.size(); i++) {
            RtfBasicElement rtfElement = (RtfBasicElement)chunks.get(i);
            if(rtfElement instanceof RtfList) {
                RtfList rl = (RtfList)rtfElement;
                rl.writeDefinition(out);
                return true;
            }
        }
        return false;
    }
    
    private int level=0;
    
    public void inheritListSettings(int listNumber, int listLevel) {
        for(int i = 0; i < chunks.size(); i++) {
            RtfBasicElement rtfElement = (RtfBasicElement) chunks.get(i);
            if(rtfElement instanceof RtfList) {
                ((RtfList) rtfElement).setListNumber(listNumber);
                setLevel(listLevel);

            }
        }
    }
        
    
    protected void correctIndentation() {
        for(int i = 0; i < chunks.size(); i++) {
            RtfBasicElement rtfElement = (RtfBasicElement) chunks.get(i);
            if(rtfElement instanceof RtfList) {
                ((RtfList) rtfElement).correctIndentation();
            }
        }
    }
    
    
    public void setParent(RtfListLevel parentList) {
        this.parentList = parentList;
    }
    
    public RtfListLevel getParent() {
        return this.parentList;
    }
    
    public boolean isContainsInnerList() {
        return this.containsInnerList;
    }

    
    public int getLevel() {
        return level;
    }

    
    public void setLevel(int level) {
        this.level = level;
    }
}
