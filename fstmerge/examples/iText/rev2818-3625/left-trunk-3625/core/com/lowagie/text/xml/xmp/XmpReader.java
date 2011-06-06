
package com.lowagie.text.xml.xmp;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.xml.XmlDomWriter;



public class XmpReader {

    private Document domDocument;
    
    
    public XmpReader(byte[] bytes) throws SAXException, IOException {
        try {
            DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
            fact.setNamespaceAware(true);
            DocumentBuilder db = fact.newDocumentBuilder();
            ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
            domDocument = db.parse(bais);
        } catch (ParserConfigurationException e) {
            throw new ExceptionConverter(e);
        }
    }
    
    
    public void replace(String namespaceURI, String localName, String value) {
        NodeList nodes = domDocument.getElementsByTagNameNS(namespaceURI, localName);
        Node node;
        for (int i = 0; i < nodes.getLength(); i++) {
            node = nodes.item(i);
            setNodeText(domDocument, node, value);
        }
    }    
    
    
    public boolean setNodeText(Document domDocument, Node n, String value) {
        if (n == null)
            return false;
        Node nc = null;
        while ((nc = n.getFirstChild()) != null) {
            n.removeChild(nc);
        }
        n.appendChild(domDocument.createTextNode(value));
        return true;
    }
    
    
    public byte[] serializeDoc() throws IOException {
        XmlDomWriter xw = new XmlDomWriter();
        ByteArrayOutputStream fout = new ByteArrayOutputStream();
        xw.setOutput(fout, null);
        Node first = domDocument.getFirstChild();
        xw.write(first);
        fout.write('\n');
        xw.write(first.getNextSibling());
        fout.flush();
        for (int i = 0; i < 20; i++) {
            fout.write(XmpWriter.EXTRASPACE.getBytes());
        }
        xw.write(domDocument.getLastChild());
        fout.close();
        return fout.toByteArray();
    }
}
