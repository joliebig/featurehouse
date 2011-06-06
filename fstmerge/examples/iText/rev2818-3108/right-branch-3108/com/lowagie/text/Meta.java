

package com.lowagie.text;

import java.util.ArrayList;



public class Meta implements Element {
    
    
    
    
    private int type;
    
    
    private StringBuffer content;
    
    
    
    
    Meta(int type, String content) {
        this.type = type;
        this.content = new StringBuffer(content);
    }
    
    
    public Meta(String tag, String content) {
        this.type = Meta.getType(tag);
        this.content = new StringBuffer(content);
    }
    
    
    
    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
    
    public int type() {
        return type;
    }
    
    
    public ArrayList<Chunk> getChunks() {
        return new ArrayList<Chunk>();
    }
    
    
    
    
    public StringBuffer append(String string) {
        return content.append(string);
    }
    
    

    
    public String getContent() {
        return content.toString();
    }

    
    
    public String getName() {
        switch (type) {
            case Element.SUBJECT:
                return ElementTags.SUBJECT;
            case Element.KEYWORDS:
                return ElementTags.KEYWORDS;
            case Element.AUTHOR:
                return ElementTags.AUTHOR;
            case Element.TITLE:
                return ElementTags.TITLE;
            case Element.PRODUCER:
                return ElementTags.PRODUCER;
            case Element.CREATIONDATE:
                return ElementTags.CREATIONDATE;
                default:
                    return ElementTags.UNKNOWN;
        }
    }
    
    
    public static int getType(String tag) {
        if (ElementTags.SUBJECT.equals(tag)) {
            return Element.SUBJECT;
        }
        if (ElementTags.KEYWORDS.equals(tag)) {
            return Element.KEYWORDS;
        }
        if (ElementTags.AUTHOR.equals(tag)) {
            return Element.AUTHOR;
        }
        if (ElementTags.TITLE.equals(tag)) {
            return Element.TITLE;
        }
        if (ElementTags.PRODUCER.equals(tag)) {
            return Element.PRODUCER;
        }
        if (ElementTags.CREATIONDATE.equals(tag)) {
            return Element.CREATIONDATE;
        }
        return Element.HEADER;
    }
    
    
    
    
    public String name() {
        return getName();
    }
    
    
    public String content() {
        return getContent();
    }
}