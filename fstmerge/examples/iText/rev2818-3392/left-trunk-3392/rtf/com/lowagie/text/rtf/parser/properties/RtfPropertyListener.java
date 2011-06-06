
package com.lowagie.text.rtf.parser.properties;

import java.util.EventListener;


public interface RtfPropertyListener extends EventListener {
    
    public void beforePropertyChange(String propertyName);
    
    public void afterPropertyChange(String propertyName);
}
