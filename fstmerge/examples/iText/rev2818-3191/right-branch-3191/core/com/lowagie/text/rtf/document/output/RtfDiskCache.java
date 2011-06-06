

package com.lowagie.text.rtf.document.output;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;



public class RtfDiskCache implements RtfDataCache {

    
    private BufferedOutputStream data = null;
    
    private File tempFile = null;
    
    
    public RtfDiskCache() throws IOException {
        this.tempFile = File.createTempFile("iText", null);
        this.data = new BufferedOutputStream(new FileOutputStream(tempFile));
    }

    
    public OutputStream getOutputStream() {
        return this.data;
    }

    
    public void writeTo(OutputStream target) throws IOException {
        this.data.close();
        BufferedInputStream tempIn = new BufferedInputStream(new FileInputStream(this.tempFile));
        byte[] buffer = new byte[8192];
        int bytesRead = -1;
        while((bytesRead = tempIn.read(buffer)) >= 0) {
            target.write(buffer, 0, bytesRead);
        }
        tempIn.close();
        this.tempFile.delete();
    }

}
