
package com.lowagie.text.rtf.parser.destinations;

import java.util.ArrayList;

import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;

 
public abstract class RtfDestination {
    
    protected RtfParser rtfParser = null;
    
    
    protected boolean modified = false;

    
    protected RtfCtrlWordData lastCtrlWord = null;;
    
    
    private static ArrayList<RtfDestinationListener> listeners = new ArrayList<RtfDestinationListener>();
    
    
    public RtfDestination() {
        rtfParser = null;
    }
    
    public RtfDestination(RtfParser parser) {
        this.rtfParser = parser;
    }
    
    public void setParser(RtfParser parser) {
        if(this.rtfParser != null && this.rtfParser.equals(parser)) return;
        this.rtfParser = parser;
    }
    
    public abstract boolean closeDestination();
    
    public abstract boolean handleOpeningSubGroup();
    
    public abstract boolean handleCloseGroup();

    
    public abstract boolean handleOpenGroup();
    
    public abstract boolean handleCharacter(int ch);
    
    public abstract boolean handleControlWord(RtfCtrlWordData ctrlWordData);
    
    public abstract void setToDefaults();

    
    public boolean isModified() {
        return modified;
    }

    

    
    public boolean addListener(RtfDestinationListener listener) {
        return listeners.add(listener);
    }

    
    public boolean removeListener(RtfDestinationListener listener) {
        return listeners.remove(listener);
    }
    
    protected RtfCtrlWordData beforeCtrlWord(RtfCtrlWordData ctrlWordData) {
        for (RtfDestinationListener listener: listeners) {
            listener.beforeCtrlWord(ctrlWordData);
        }
        return null;
    }
    
    protected  RtfCtrlWordData onCtrlWord(RtfCtrlWordData ctrlWordData){
        for (RtfDestinationListener listener: listeners) {
            listener.onCtrlWord(ctrlWordData);
        }
        return null;
    }

    
    protected  RtfCtrlWordData afterCtrlWord(RtfCtrlWordData ctrlWordData){
        for (RtfDestinationListener listener: listeners) {
            listener.afterCtrlWord(ctrlWordData);
        }
        return null;
    }

    
    protected  int beforeCharacter(int ch){
        for (RtfDestinationListener listener: listeners) {
            listener.beforeCharacter(ch);
        }
        return 0;
    }

    
    protected  int onCharacter(int ch){
        for (RtfDestinationListener listener: listeners) {
            listener.onCharacter(ch);
        }
        return 0;
    }

    
    protected  int afterCharacter(int ch){
        for (RtfDestinationListener listener: listeners) {
            listener.afterCharacter(ch);
        }
        return 0;
    }

    
    protected  boolean onOpenGroup(){
        for (RtfDestinationListener listener: listeners) {
            listener.onOpenGroup();
        }
        return true;
    }

    
    protected  boolean onCloseGroup(){
        for (RtfDestinationListener listener: listeners) {
            listener.onCloseGroup();
        }
        return true;
    }
    
    public int getNewTokeniserState() {
        return RtfParser.TOKENISER_IGNORE_RESULT;
    }
}
