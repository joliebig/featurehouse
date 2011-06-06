
package com.lowagie.text.rtf.direct;

import java.io.IOException;
import java.io.Reader;
import java.util.Iterator;

import com.lowagie.text.rtf.document.RtfDocument;


public class RtfParser {
    
    private static final int PARSER_IN_HEADER = 0;
    
    private static final int PARSER_IN_FONT_TABLE = 1;
    
    private static final int PARSER_IN_COLOR_TABLE = 2;
    
    private static final int PARSER_IN_INFO_GROUP = 4;
    
    private static final int PARSER_IN_DOCUMENT = 8;
    
    
    private RtfDocument rtfDoc = null;
    
    private RtfTokeniser tokeniser = null;
    
    private RtfImportHeader importHeader = null;
    
    private RtfFontTableParser fontTableParser = null;
    
    private RtfColorTableParser colorTableParser = null;
    
    private int state = PARSER_IN_HEADER;
    
    
    public void importRtfDocument(Reader reader, RtfDocument rtfDoc) throws IOException {
        this.rtfDoc = rtfDoc;
        this.state = PARSER_IN_HEADER;
        this.importHeader = new RtfImportHeader(this.rtfDoc);
        this.fontTableParser = new RtfFontTableParser(this.importHeader);
        this.colorTableParser = new RtfColorTableParser(this.importHeader);
        this.tokeniser = new RtfTokeniser(this, 0);
        this.tokeniser.tokenise(reader);
    }
    
    
    public void importRtfFragment(Reader reader, RtfDocument rtfDoc, RtfImportMappings importMappings) throws IOException {
        this.rtfDoc = rtfDoc;
        this.state = PARSER_IN_DOCUMENT;
        this.importHeader = new RtfImportHeader(this.rtfDoc);
        this.fontTableParser = new RtfFontTableParser(this.importHeader);
        this.colorTableParser = new RtfColorTableParser(this.importHeader);
        handleImportMappings(importMappings);
        this.tokeniser = new RtfTokeniser(this, 1);
        this.tokeniser.tokenise(reader);
    }

    
    private void handleImportMappings(RtfImportMappings importMappings) {
        Iterator<String> it = importMappings.getFontMappings().keySet().iterator();
        while(it.hasNext()) {
            String fontNr = it.next();
            this.importHeader.importFont(fontNr, importMappings.getFontMappings().get(fontNr));
        }
        it = importMappings.getColorMappings().keySet().iterator();
        while(it.hasNext()) {
            String colorNr = it.next();
            this.importHeader.importColor(colorNr, importMappings.getColorMappings().get(colorNr));
        }
    }
    
    
    public void handleOpenGroup(int groupLevel) {
        if(this.state == PARSER_IN_DOCUMENT) {
            this.rtfDoc.add(new RtfDirectContent("{"));
        }
    }
    
    
    public void handleCloseGroup(int groupLevel) {
        if(this.state == PARSER_IN_DOCUMENT && groupLevel > 1) {
            this.rtfDoc.add(new RtfDirectContent("}"));
        } else if(this.state == PARSER_IN_INFO_GROUP && groupLevel == 2) {
            this.state = PARSER_IN_DOCUMENT;
        } else if(this.state == PARSER_IN_FONT_TABLE) {
            this.fontTableParser.handleCloseGroup(groupLevel);
            if(groupLevel == 2) {
                this.state = PARSER_IN_HEADER;
            }
        } else if(this.state == PARSER_IN_COLOR_TABLE) {
            this.state = PARSER_IN_HEADER;
        }
    }
    
    
    public void handleCtrlCharacter(String ctrlCharacter, int groupLevel) {
        if(this.state == PARSER_IN_DOCUMENT) {
            this.rtfDoc.add(new RtfDirectContent(ctrlCharacter));
        }
    }
    
    
    public void handleCtrlWord(String ctrlWord, int groupLevel) {
        if(this.state == PARSER_IN_DOCUMENT) {
            if(RtfColorTableParser.stringMatches(ctrlWord, "\\f")) {
                ctrlWord = "\\f" + this.importHeader.mapFontNr(ctrlWord.substring(2));
            } else if(RtfColorTableParser.stringMatches(ctrlWord, "\\cf")) {
                ctrlWord = "\\cf" + this.importHeader.mapColorNr(ctrlWord.substring(3));
            } else if(RtfColorTableParser.stringMatches(ctrlWord, "\\cb")) {
                ctrlWord = "\\cb" + this.importHeader.mapColorNr(ctrlWord.substring(3));
            } else if (RtfColorTableParser.stringMatches(ctrlWord, "\\clcbpat")){
                ctrlWord = "\\clcbpat" + this.importHeader.mapColorNr(ctrlWord.substring(8));
            } else if (RtfColorTableParser.stringMatches(ctrlWord, "\\clcbpatraw")) {
                ctrlWord = "\\clcbpatraw" + this.importHeader.mapColorNr(ctrlWord.substring(11));
            } else if (RtfColorTableParser.stringMatches(ctrlWord, "\\clcfpat")) {
                ctrlWord = "\\clcfpat" + this.importHeader.mapColorNr(ctrlWord.substring(8));
            } else if (RtfColorTableParser.stringMatches(ctrlWord, "\\clcfpatraw")) {
                ctrlWord = "\\clcfpatraw" + this.importHeader.mapColorNr(ctrlWord.substring(11));
            } else if (RtfColorTableParser.stringMatches(ctrlWord, "\\trcfpat")) {
                ctrlWord = "\\trcfpat" + this.importHeader.mapColorNr(ctrlWord.substring(8));
            } else if (RtfColorTableParser.stringMatches(ctrlWord, "\\trcbpat")) {
                ctrlWord = "\\trcbpat" + this.importHeader.mapColorNr(ctrlWord.substring(8));
            }
            this.rtfDoc.add(new RtfDirectContent(ctrlWord));
        } else if(this.state == PARSER_IN_FONT_TABLE) {
            this.fontTableParser.handleCtrlWord(ctrlWord, groupLevel);
        } else if(this.state == PARSER_IN_COLOR_TABLE) {
            this.colorTableParser.handleCtrlWord(ctrlWord, groupLevel);
        } else if(this.state == PARSER_IN_HEADER) {
            if(ctrlWord.equals("\\info")) {
                this.state = PARSER_IN_INFO_GROUP;
            } else if(ctrlWord.equals("\\fonttbl")) {
                this.state = PARSER_IN_FONT_TABLE;
            } else if(ctrlWord.equals("\\colortbl")) {
                this.state = PARSER_IN_COLOR_TABLE;
            }
        }
    }
    
    
    public void handleText(String text, int groupLevel) {
        if(this.state == PARSER_IN_DOCUMENT) {
            this.rtfDoc.add(new RtfDirectContent(text));
        } else if(this.state == PARSER_IN_FONT_TABLE) {
            this.fontTableParser.handleText(text, groupLevel);
        } else if(this.state == PARSER_IN_COLOR_TABLE) {
            this.colorTableParser.handleText(text, groupLevel);
        }
    }
}
