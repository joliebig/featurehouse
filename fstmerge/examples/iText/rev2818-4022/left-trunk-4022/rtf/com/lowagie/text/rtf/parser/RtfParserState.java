
package com.lowagie.text.rtf.parser;

import java.util.Stack;

import com.lowagie.text.rtf.parser.destinations.RtfDestination;
import com.lowagie.text.rtf.parser.destinations.RtfDestinationNull;
import com.lowagie.text.rtf.parser.properties.RtfProperty;


public class RtfParserState {
    
    public int parserState = RtfParser.PARSER_IN_UNKNOWN;
    
    public int tokeniserState = RtfParser.TOKENISER_STATE_IN_UNKOWN;
    
    public Object groupHandler = null;
    
    public StringBuffer text = null;
    
    public Stack ctrlWordHandlers = null;
    
    public Object ctrlWordHandler = null;
    
    public RtfDestination destination = null;
    
    public boolean isExtendedDestination = false;
    
    public boolean newGroup = false;
    
    public RtfProperty properties = null;
    
    public RtfParserState() {
        this.text = new StringBuffer();
        this.ctrlWordHandlers = new Stack();
        this.properties = new RtfProperty();
        this.destination = RtfDestinationNull.getInstance();
        this.newGroup = false;
    }
    
    public RtfParserState(RtfParserState orig) {
        this.properties = orig.properties;
        this.parserState = orig.parserState;
        this.tokeniserState = orig.tokeniserState;
        this.groupHandler = null;
        this.destination = orig.destination;
        this.text = new StringBuffer();
        this.ctrlWordHandlers = new Stack();
        this.destination = orig.destination;
        this.newGroup = false;
    }
    
}
