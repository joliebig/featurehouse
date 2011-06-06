

package com.lowagie.rups.view.itext;
import java.io.IOException;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import com.lowagie.rups.io.TextAreaOutputStream;
import com.lowagie.text.pdf.PRStream;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;

public class StreamTextArea extends JScrollPane implements Observer {
    
    
    protected JTextArea text;
    
    
    public StreamTextArea() {
        super();
        text = new JTextArea();
        setViewportView(text);
    }
    
    
    public void update(Observable observable, Object obj) {
        text.setText(null);
    }
    
    
    public void render(PdfObject object) {
        if (object instanceof PRStream) {
            PRStream stream = (PRStream)object;
            try {
                TextAreaOutputStream taos = new TextAreaOutputStream(text);
                taos.write(PdfReader.getStreamBytes(stream));
                
            }
            catch(IOException e) {
                text.setText("The stream could not be read: " + e.getMessage());
            }
        }
        else {
            update(null, null);
            return;
        }
        text.repaint();
        repaint();
    }

    
    private static final long serialVersionUID = 1302283071087762494L;

}
