

package com.lowagie.text.html;

import java.util.HashMap;
import java.util.Properties;

import org.xml.sax.Attributes;

import com.lowagie.text.DocListener;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ElementTags;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.xml.SAXiTextHandler;
import com.lowagie.text.xml.XmlPeer;



public class SAXmyHtmlHandler extends SAXiTextHandler 
{

    
    private Properties bodyAttributes = new Properties();

    
    private boolean tableBorder = false;

    

    public SAXmyHtmlHandler(DocListener document) {
        super(document, new HtmlTagMap());
    }
    

    public SAXmyHtmlHandler(DocListener document, BaseFont bf) {
        super(document, new HtmlTagMap(), bf);
    }

    

    public SAXmyHtmlHandler(DocListener document, HashMap<String, HtmlPeer> htmlTags) {
        super(document, htmlTags);
    }

    

    public void startElement(String uri, String lname, String name,
            Attributes attrs) {
        

        
        
        name = name.toLowerCase();
        if (((HtmlTagMap) myTags).isHtml(name)) {
            
            return;
        }
        if (((HtmlTagMap) myTags).isHead(name)) {
            
            return;
        }
        if (((HtmlTagMap) myTags).isTitle(name)) {
            
            return;
        }
        if (((HtmlTagMap) myTags).isMeta(name)) {
            
            String meta = null;
            String content = null;
            if (attrs != null) {
                for (int i = 0; i < attrs.getLength(); i++) {
                    String attribute = attrs.getQName(i);
                    if (attribute.equalsIgnoreCase(HtmlTags.CONTENT))
                        content = attrs.getValue(i);
                    else if (attribute.equalsIgnoreCase(HtmlTags.NAME))
                        meta = attrs.getValue(i);
                }
            }
            if (meta != null && content != null) {
                bodyAttributes.put(meta, content);
            }
            return;
        }
        if (((HtmlTagMap) myTags).isLink(name)) {
            
            
            return;
        }
        if (((HtmlTagMap) myTags).isBody(name)) {
            
            
            
            XmlPeer peer = new XmlPeer(ElementTags.ITEXT, name);
            peer.addAlias(ElementTags.TOP, HtmlTags.TOPMARGIN);
            peer.addAlias(ElementTags.BOTTOM, HtmlTags.BOTTOMMARGIN);
            peer.addAlias(ElementTags.RIGHT, HtmlTags.RIGHTMARGIN);
            peer.addAlias(ElementTags.LEFT, HtmlTags.LEFTMARGIN);
            bodyAttributes.putAll(peer.getAttributes(attrs));
            handleStartingTags(peer.getTag(), bodyAttributes);
            return;
        }
        if (myTags.containsKey(name)) {
            XmlPeer peer = myTags.get(name);
            if (ElementTags.TABLE.equals(peer.getTag()) || ElementTags.CELL.equals(peer.getTag())) {
                Properties p = peer.getAttributes(attrs);
                String value;
                if (ElementTags.TABLE.equals(peer.getTag())
                        && (value = p.getProperty(ElementTags.BORDERWIDTH)) != null) {
                    if (Float.parseFloat(value + "f") > 0) {
                        tableBorder = true;
                    }
                }
                if (tableBorder) {
                    p.put(ElementTags.LEFT, String.valueOf(true));
                    p.put(ElementTags.RIGHT, String.valueOf(true));
                    p.put(ElementTags.TOP, String.valueOf(true));
                    p.put(ElementTags.BOTTOM, String.valueOf(true));
                }
                handleStartingTags(peer.getTag(), p);
                return;
            }
            handleStartingTags(peer.getTag(), peer.getAttributes(attrs));
            return;
        }
        Properties attributes = new Properties();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                String attribute = attrs.getQName(i).toLowerCase();
                attributes.setProperty(attribute, attrs.getValue(i).toLowerCase());
            }
        }
        handleStartingTags(name, attributes);
    }

    

    public void endElement(String uri, String lname, String name) {
        
        name = name.toLowerCase();
        if (ElementTags.PARAGRAPH.equals(name)) {
            try {
                document.add(stack.pop());
                return;
            } catch (DocumentException e) {
                throw new ExceptionConverter(e);
            }
        }
        if (((HtmlTagMap) myTags).isHead(name)) {
            
            return;
        }
        if (((HtmlTagMap) myTags).isTitle(name)) {
            if (currentChunk != null) {
                bodyAttributes.put(ElementTags.TITLE, currentChunk.getContent());
            }
            return;
        }
        if (((HtmlTagMap) myTags).isMeta(name)) {
            
            return;
        }
        if (((HtmlTagMap) myTags).isLink(name)) {
            
            return;
        }
        if (((HtmlTagMap) myTags).isBody(name)) {
            
            return;
        }
        if (myTags.containsKey(name)) {
            XmlPeer peer = myTags.get(name);
            if (ElementTags.TABLE.equals(peer.getTag())) {
                tableBorder = false;
            }
            super.handleEndingTags(peer.getTag());
            return;
        }
        
        
        handleEndingTags(name);
    }
}