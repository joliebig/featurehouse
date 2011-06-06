

package com.lowagie.text.rtf.style;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;


public class RtfStylesheetList extends RtfElement implements RtfExtendedElement {

    
    private HashMap<String, RtfParagraphStyle> styleMap = null;
    
    private boolean defaultsLoaded = false;
    
    
    public RtfStylesheetList(RtfDocument doc) {
        super(doc);
        this.styleMap = new HashMap<String, RtfParagraphStyle>();
    }

    
    public void writeContent(OutputStream out) throws IOException
    {    
    }
    
    
    public void registerParagraphStyle(RtfParagraphStyle rtfParagraphStyle) {
        RtfParagraphStyle tempStyle = new RtfParagraphStyle(this.document, rtfParagraphStyle);
        tempStyle.handleInheritance();
        tempStyle.setStyleNumber(this.styleMap.size());
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
            return this.styleMap.get(styleName);
        } else {
            return null;
        }
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write("{".getBytes());
        result.write("\\stylesheet".getBytes());
        result.write(RtfBasicElement.DELIMITER);
        this.document.outputDebugLinebreak(result);
        Iterator<RtfParagraphStyle> it = this.styleMap.values().iterator();
        while(it.hasNext()) {
            RtfParagraphStyle rps = it.next();
            rps.writeDefinition(result);
        }
        result.write("}".getBytes());
        this.document.outputDebugLinebreak(result);      
    }
}
