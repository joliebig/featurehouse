
package com.lowagie.text.rtf.parser.ctrlwords;

import java.util.EventListener;


public interface RtfCtrlWordListener extends EventListener {
    
    public RtfCtrlWordData beforeCtrlWord(RtfCtrlWordData ctrlWordData);
    
    public RtfCtrlWordData onCtrlWord(RtfCtrlWordData ctrlWordData);
    
    public RtfCtrlWordData afterCtrlWord(RtfCtrlWordData ctrlWordData);
}
