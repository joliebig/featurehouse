
package com.lowagie.text.rtf.parser.ctrlwords; 

import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.destinations.RtfDestination;
import com.lowagie.text.rtf.parser.destinations.RtfDestinationMgr;


public class RtfCtrlWordHandler implements Cloneable {
    
    private static final boolean debug = false;

    
    protected RtfParser rtfParser = null;
    
    protected String ctrlWord = "";
    
    protected int defaultParameterValue = 0;
    
    protected boolean passDefaultParameterValue = false;
    
    protected int ctrlWordType = RtfCtrlWordType.UNIDENTIFIED;
    
    protected String specialHandler = "";
    
    protected float rtfVersionSupported = -1.0f;    
    
    protected RtfCtrlWordData ctrlWordData = null;
    
    protected String groupPrefix = "";
    
    protected String ctrlWordPrefix = "\\";
    
    protected String ctrlWordSuffix = " ";
    
    private RtfCtrlWordHandler(){};
    
    
    public RtfCtrlWordHandler(RtfParser rtfParser, String ctrlWord, int defaultParameterValue, boolean passDefaultParameterValue, 
            int ctrlWordType, String prefix, String suffix, String specialHandler) {
        super();
        this.rtfParser = rtfParser;
        this.ctrlWord = ctrlWord;
        this.defaultParameterValue = defaultParameterValue;
        this.passDefaultParameterValue = passDefaultParameterValue;
        this.ctrlWordType = ctrlWordType;
        this.ctrlWordPrefix = prefix;
        this.ctrlWordSuffix = suffix;
        this.specialHandler = specialHandler;

        if(this.ctrlWordType == RtfCtrlWordType.DESTINATION || this.ctrlWordType == RtfCtrlWordType.DESTINATION_EX){
            if(this.specialHandler == null) {
                this.specialHandler = "RtfDestinationNull";
            }
            String arg1 = ""; 
            RtfDestinationMgr.addDestination(this.ctrlWord, new Object[] { this.specialHandler, arg1 });            
        } else {
            if(this.ctrlWordType == RtfCtrlWordType.SYMBOL){
                
            } else {
                if(this.specialHandler == null) {
                    this.specialHandler = this.ctrlWord;    
                } else {
                    if(this.specialHandler.length() > 1 && this.specialHandler.endsWith(".")) {
                        this.specialHandler += this.ctrlWord;    
                    }
                }
            }
        }
    }
    
    
    public final boolean handleControlword(RtfCtrlWordData ctrlWordDataIn){
        boolean result = false;
        this.ctrlWordData = ctrlWordDataIn;
        RtfDestination dest = null;
        boolean handled = false;
        
        this.ctrlWordData.prefix  = this.ctrlWordPrefix;
        this.ctrlWordData.suffix  = this.ctrlWordSuffix;
        this.ctrlWordData.newGroup = this.rtfParser.getState().newGroup;
        this.ctrlWordData.ctrlWordType = this.ctrlWordType;
        this.ctrlWordData.specialHandler = this.specialHandler;
        
        if(!this.ctrlWordData.hasParam && this.passDefaultParameterValue) {
            this.ctrlWordData.hasParam = true;
            this.ctrlWordData.param = Integer.toString(this.defaultParameterValue);
        }

        if(debug) {
            printDebug("handleKeyword: [" + this.ctrlWordData.ctrlWord + "] param=" + ctrlWordDataIn.param);
            RtfParser.outputDebug(this.rtfParser.getRtfDocument(), this.rtfParser.getLevel()+1, "RtfCtrlWordHandler debug Start: " + this.ctrlWordData.ctrlWord + " ");
        }
        if(this.ctrlWordData.ctrlWord.equals("*")) {
            return true;
        }
        
        if(!beforeControlWord()) {
            return true;
        }
        
        switch(this.ctrlWordType) {
        case RtfCtrlWordType.FLAG:
        case RtfCtrlWordType.TOGGLE:
        case RtfCtrlWordType.VALUE:
            dest = this.rtfParser.getCurrentDestination();
            if(dest != null) {
                handled = dest.handleControlWord(this.ctrlWordData);
            }
            break;
        
        case RtfCtrlWordType.SYMBOL:
            dest = this.rtfParser.getCurrentDestination();
            if(dest != null) {
                String data = null;
                
                if(this.rtfParser.isImport()) {
                    data = this.ctrlWordPrefix + this.ctrlWordData.ctrlWord + this.ctrlWordSuffix;
                }
                if(this.rtfParser.isConvert()) {
                    data = this.specialHandler;
                }
                
                
                
                if(data != null) {
                    for(int idx=0; idx< data.length(); idx++) {
                        handled = dest.handleCharacter(data.charAt(idx));
                    }
                } else {
                    handled = dest.handleControlWord(this.ctrlWordData);
                }
            }
            break;

        case RtfCtrlWordType.DESTINATION_EX:
        case RtfCtrlWordType.DESTINATION:
            
            int x=0;
            if("shppict".equals(this.ctrlWord) || "nonshppict".equals(this.ctrlWord)) {
                x++;
            }
            handled = this.rtfParser.setCurrentDestination(this.ctrlWord);
            
            dest = this.rtfParser.getCurrentDestination();
            if(dest != null) {
                if(dest.getNewTokeniserState() == RtfParser.TOKENISER_IGNORE_RESULT) {
                    handled = dest.handleControlWord(this.ctrlWordData);
                }
                else {
                    this.rtfParser.setTokeniserState(dest.getNewTokeniserState());
                }
            }

            break;
        }

        afterControlWord();
        
        if(debug) {
            RtfParser.outputDebug(this.rtfParser.getRtfDocument(), this.rtfParser.getLevel()+1, "RtfCtrlWordHandler debug End: " + this.ctrlWordData.ctrlWord + " ");
        }

        return result;
    }
    
    
    
    protected boolean beforeControlWord() {
        if(debug) printDebug("beforeControlWord");
        
        return true;
    }
    
    protected boolean onControlWord() {
        if(debug) printDebug("onCtrlWord");
        
        return false;
    }
    
    protected boolean afterControlWord() {
        if(debug) printDebug("afterControlWord");
        
        return true;
    }
    


















    
    
    private final void printDebug(final String txt) {
         System.out.println(this.getClass().getName() + " : " + txt);
    }
}
