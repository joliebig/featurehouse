
package com.lowagie.text.rtf.parser.destinations;

import java.util.EventListener;

import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public interface RtfDestinationListener extends EventListener {
    
    public RtfCtrlWordData beforeCtrlWord(RtfCtrlWordData ctrlWordData);
    
    public RtfCtrlWordData onCtrlWord(RtfCtrlWordData ctrlWordData);
    
    public RtfCtrlWordData afterCtrlWord(RtfCtrlWordData ctrlWordData);
    
    public int beforeCharacter(int ch);
    
    public int onCharacter(int ch);
    
    public int afterCharacter(int ch);
    
    public boolean onOpenGroup();
    
    public boolean onCloseGroup();
    

}
