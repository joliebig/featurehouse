

package com.lowagie.rups.model;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

import com.lowagie.rups.io.OutputStreamResource;


public class XfaFile implements OutputStreamResource {

    
    protected Document xfaDocument;
    
    
    public XfaFile(OutputStreamResource resource) throws IOException, DocumentException {
        
        
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        resource.writeTo(baos);
        ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        SAXReader reader = new SAXReader();
        xfaDocument = reader.read(bais);
    }

    
    public Document getXfaDocument() {
        return xfaDocument;
    }

    
    public void writeTo(OutputStream os) throws IOException {
        if (xfaDocument == null)
            return;
        OutputFormat format = new OutputFormat("   ", true);
        XMLWriter writer = new XMLWriter(os, format);
        writer.write(xfaDocument);
    }
}
