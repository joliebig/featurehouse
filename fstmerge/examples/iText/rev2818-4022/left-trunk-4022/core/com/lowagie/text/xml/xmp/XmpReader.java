
package com.lowagie.text.xml.xmp;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
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
    
    
    public boolean replace(String namespaceURI, String localName, String value) {
        NodeList nodes = domDocument.getElementsByTagNameNS(namespaceURI, localName);
        Node node;
        if (nodes.getLength() == 0)
            return false;
        for (int i = 0; i < nodes.getLength(); i++) {
            node = nodes.item(i);
            setNodeText(domDocument, node, value);
        }
        return true;
    }    
    
    
    public boolean add(String parent, String namespaceURI, String localName, String value) {
        NodeList nodes = domDocument.getElementsByTagName(parent);
        if (nodes.getLength() == 0)
            return false;
        Node pNode;
        Node node;
        for (int i = 0; i < nodes.getLength(); i++) {
            pNode = nodes.item(i);
            NamedNodeMap attrs = pNode.getAttributes();
            for (int j = 0; j < attrs.getLength(); j++) {
                node = attrs.item(j);
                if (namespaceURI.equals(node.getNodeValue())) {
                    node = domDocument.createElement(localName);
                    node.appendChild(domDocument.createTextNode(value));
                    pNode.appendChild(node);
                    return true;
                }
            }
        }
        return false;
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
        fout.write(XmpWriter.XPACKET_PI_BEGIN.getBytes("UTF-8"));
        fout.flush();
        NodeList xmpmeta = domDocument.getElementsByTagName("x:xmpmeta");
        xw.write(xmpmeta.item(0));
        fout.flush();
        for (int i = 0; i < 20; i++) {
            fout.write(XmpWriter.EXTRASPACE.getBytes());
        }
        fout.write(XmpWriter.XPACKET_PI_END_W.getBytes());
        fout.close();
        return fout.toByteArray();
    }
}
