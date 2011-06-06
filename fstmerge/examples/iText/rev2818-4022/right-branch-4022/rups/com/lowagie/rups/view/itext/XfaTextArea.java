package com.lowagie.rups.view.itext;

import java.io.IOException;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import com.lowagie.rups.io.OutputStreamResource;
import com.lowagie.rups.io.TextAreaOutputStream;


public class XfaTextArea extends JScrollPane {
    
    
    protected JTextArea text;
    
    
    public XfaTextArea() {
        super();
        text = new JTextArea();
        setViewportView(text);
    }
    
    public void clear() {
        text.setText("");
    }
    
    public void load(OutputStreamResource xml) throws IOException {
        TextAreaOutputStream stream = new TextAreaOutputStream(text);
        xml.writeTo(stream);
    }

    
    private static final long serialVersionUID = -8275229961781669457L;
}
