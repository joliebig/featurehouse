

package com.lowagie.text.rtf.text;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.rtf.RtfAddableElement;


public class RtfTabGroup extends RtfAddableElement {
    
    private ArrayList tabs = null;

    
    public RtfTabGroup() {
        this.tabs = new ArrayList();
    }
    
    
    public RtfTabGroup(ArrayList tabs) {
        this.tabs = new ArrayList();
        for(int i = 0; i < tabs.size(); i++) {
            if(tabs.get(i) instanceof RtfTab) {
                this.tabs.add(tabs.get(i));
            }
        }
    }
    
    
    public void add(RtfTab tab) {
        this.tabs.add(tab);
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        for(int i = 0; i < this.tabs.size(); i++) {
            RtfTab rt = (RtfTab) this.tabs.get(i);
            rt.writeContent(result);
        }
    }        
    
}
