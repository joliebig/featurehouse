

package com.lowagie.rups.io;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.JTextArea;


public class TextAreaOutputStream extends OutputStream {
    
    protected JTextArea text;
    
    protected int offset;
    
    
    public TextAreaOutputStream(JTextArea text) throws IOException {
        this.text = text;
        clear();
    }

    
    public void clear() {
        text.setText(null);
        offset = 0;
    }
    
    
    @Override
    public void write(int i) throws IOException {
        byte[] b = { (byte)i };
        write(b, 0, 1);
    }

    
    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        String snippet = new String(b, off, len);
        text.insert(snippet, offset);
        offset += len - off;
    }

    
    @Override
    public void write(byte[] b) throws IOException {
        ByteArrayInputStream bais = new ByteArrayInputStream(b);
        byte[] snippet = new byte[1024];
        int bytesread;
        while ((bytesread = bais.read(snippet)) > 0) {
            write(snippet, 0, bytesread);
        }
    }
    
}
