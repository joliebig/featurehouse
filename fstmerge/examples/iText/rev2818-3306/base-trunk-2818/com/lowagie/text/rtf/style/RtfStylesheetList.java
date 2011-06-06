

package com.lowagie.text.rtf.style;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;


public class RtfStylesheetList extends RtfElement implements RtfExtendedElement {

    
    private HashMap styleMap = null;
    
    private boolean defaultsLoaded = false;
    
    
    public RtfStylesheetList(RtfDocument doc) {
        super(doc);
        this.styleMap = new HashMap();
    }

    
    public byte[] write()
    {
        return(new byte[0]);
    }
    
    public void writeContent(OutputStream out) throws IOException
    {    
    }
    
    
    public void registerParagraphStyle(RtfParagraphStyle rtfParagraphStyle) {
        RtfParagraphStyle tempStyle = new RtfParagraphStyle(this.document, rtfParagraphStyle);
        tempStyle.setStyleNumber(this.styleMap.size());
        tempStyle.handleInheritance();
        this.styleMap.put(tempStyle.getStyleName(), tempStyle);
    }

    
    private void registerDefaultStyles() {
        defaultsLoaded = true;
        if(!this.styleMap.containsKey(RtfParagraphStyle.STYLE_NORMAL.getStyleName())) {
            registerParagraphStyle(RtfParagraphStyle.STYLE_NORMAL);
        }
        if(!this.styleMap.containsKey(RtfParagraphStyle.STYLE_HEADING_1.getStyleName())) {
            registerParagraphStyle(RtfParagraphStyle.STYLE_HEADING_1);
        }
        if(!this.styleMap.containsKey(RtfParagraphStyle.STYLE_HEADING_2.getStyleName())) {
            registerParagraphStyle(RtfParagraphStyle.STYLE_HEADING_2);
        }
        if(!this.styleMap.containsKey(RtfParagraphStyle.STYLE_HEADING_3.getStyleName())) {
            registerParagraphStyle(RtfParagraphStyle.STYLE_HEADING_3);
        }
    }

    
    public RtfParagraphStyle getRtfParagraphStyle(String styleName) {
        if(!defaultsLoaded) {
            registerDefaultStyles();
        }
        if(this.styleMap.containsKey(styleName)) {
            return (RtfParagraphStyle) this.styleMap.get(styleName);
        } else {
            return null;
        }
    }
    
    
    public byte[] writeDefinition() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeDefinition(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write("{".getBytes());
        result.write("\\stylesheet".getBytes());
        result.write(RtfBasicElement.DELIMITER);
        if(this.document.getDocumentSettings().isOutputDebugLineBreaks()) {
            result.write("\n".getBytes());
        }
        Iterator it = this.styleMap.values().iterator();
        while(it.hasNext()) {
            RtfParagraphStyle rps = (RtfParagraphStyle)it.next();
            
            rps.writeDefinition(result);
        }
        result.write("}".getBytes());
        if(this.document.getDocumentSettings().isOutputDebugLineBreaks()) {
            result.write('\n');
        }        
    }
}
