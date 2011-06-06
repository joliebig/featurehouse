

package com.lowagie.text.rtf.parser.ctrlwords;

import java.io.PushbackInputStream;
import java.util.ArrayList;

import com.lowagie.text.rtf.parser.RtfParser;


public final class RtfCtrlWordMgr {
    public static final boolean debug = false;
    public static final boolean debugFound = false;
    public static final boolean debugNotFound = true;
    private PushbackInputStream reader = null;
    private RtfParser rtfParser = null;
    private RtfCtrlWordMap ctrlWordMap = null;
    
    
    private ArrayList<RtfCtrlWordListener> listeners = new ArrayList<RtfCtrlWordListener>();









    
    
    public RtfCtrlWordMgr(RtfParser rtfParser, PushbackInputStream reader) {
        this.rtfParser = rtfParser;    
        this.reader = reader;    
        ctrlWordMap = new RtfCtrlWordMap(rtfParser);
        










    }
    
    
    public int handleKeyword(RtfCtrlWordData ctrlWordData, int groupLevel) {
        
        int result = RtfParser.errOK;
        
        
        beforeCtrlWord(ctrlWordData);
        
        result = dispatchKeyword(ctrlWordData, groupLevel);
        
        
        afterCtrlWord(ctrlWordData);
        
        return result;
    }
    
    
    private int dispatchKeyword(RtfCtrlWordData ctrlWordData, int groupLevel) {
        int result = RtfParser.errOK;
        if(ctrlWordData != null) {
            RtfCtrlWordHandler ctrlWord = ctrlWordMap.getCtrlWordHandler(ctrlWordData.ctrlWord);
            if(ctrlWord != null) {
                ctrlWord.handleControlword(ctrlWordData);
                if(debug && debugFound) {
                    System.out.println("Keyword found:" +
                        " New:" + ctrlWordData.ctrlWord + 
                        " Param:" + ctrlWordData.param + 
                        " bParam=" + ctrlWordData.hasParam);
                }
            } else {
                result = RtfParser.errCtrlWordNotFound;
                
                if(debug && debugNotFound) {
                    System.out.println("Keyword unknown:" + 
                        " New:" + ctrlWordData.ctrlWord + 
                        " Param:" + ctrlWordData.param + 
                        " bParam=" + ctrlWordData.hasParam);
                }
            }    
        }
        return result;
    }
    

    

    
    public void addRtfCtrlWordListener(RtfCtrlWordListener listener) {
        listeners.add(listener);
    }

    
    public void removeRtfCtrlWordListener(RtfCtrlWordListener listener) {
        listeners.remove(listener);
    }
    
    private boolean beforeCtrlWord(RtfCtrlWordData ctrlWordData) {
        for (RtfCtrlWordListener listener: listeners) {
            listener.beforeCtrlWord(ctrlWordData);
        }
        return true;
    }
    
    private boolean onCtrlWord(RtfCtrlWordData ctrlWordData) {
        for (RtfCtrlWordListener listener: listeners) {
            listener.onCtrlWord(ctrlWordData);
        }
        return true;
    }
    
    private boolean afterCtrlWord(RtfCtrlWordData ctrlWordData) {
        for (RtfCtrlWordListener listener: listeners) {
            listener.afterCtrlWord(ctrlWordData);
        }
        return true;
    }
}
