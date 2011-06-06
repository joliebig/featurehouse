
 
package com.lowagie.text.rtf.parser.destinations;

import com.lowagie.text.Document;
import com.lowagie.text.Meta;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.document.RtfInfoElement;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public class RtfDestinationInfo extends RtfDestination {
    private String elementName = "";
    private String text = "";

    
    public RtfDestinationInfo() {
        super(null);
    }
    
    public RtfDestinationInfo(RtfParser parser, String elementname) {
        super(parser);
        setToDefaults();
        this.elementName = elementname;
    }
    public void setParser(RtfParser parser) {
        this.rtfParser = parser;
        this.setToDefaults();
    }
    public void setElementName(String value) {
        this.elementName = value;
    }
    
    public boolean handleOpeningSubGroup() {
        return true;
    }

    
    public boolean closeDestination() {
        return true;
    }

    
    public boolean handleCloseGroup() {
        if (this.text.length() > 0) {        
            Document doc = this.rtfParser.getDocument();
            if(doc != null) {
                if(this.elementName.equals("author")){
                    doc.addAuthor(this.text);
                }
                if(this.elementName.equals("title")){
                    doc.addTitle(this.text);
                }
                if(this.elementName.equals("subject")){
                    doc.addSubject(this.text);
                }
            } else {
                RtfDocument rtfDoc = this.rtfParser.getRtfDocument();
                if(rtfDoc != null) {
                    if(this.elementName.equals("author")){
                        Meta meta = new Meta(this.elementName, this.text);
                        RtfInfoElement elem = new RtfInfoElement(rtfDoc, meta);
                        rtfDoc.getDocumentHeader().addInfoElement(elem);
                    }
                    if(this.elementName.equals("title")){
                        Meta meta = new Meta(this.elementName, this.text);
                        RtfInfoElement elem = new RtfInfoElement(rtfDoc, meta);
                        rtfDoc.getDocumentHeader().addInfoElement(elem);
                    }
                    if(this.elementName.equals("subject")){
                        Meta meta = new Meta(this.elementName, this.text);
                        RtfInfoElement elem = new RtfInfoElement(rtfDoc, meta);
                        rtfDoc.getDocumentHeader().addInfoElement(elem);
                    }
                }
            }
            this.setToDefaults();
        }
        return true;
    }

    
    public boolean handleOpenGroup() {

        return true;
    }
    
    public boolean handleCharacter(int ch) {
        this.text += (char)ch;
        return true;
    }

    
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        elementName = ctrlWordData.ctrlWord;
        return true;
    }

    
    public void setToDefaults() {
        this.text = "";
    }

}
