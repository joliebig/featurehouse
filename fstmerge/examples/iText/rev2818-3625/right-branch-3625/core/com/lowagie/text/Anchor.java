

package com.lowagie.text;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;



public class Anchor extends Phrase {

    
    private static final long serialVersionUID = -852278536049236911L;
    
    
    
    
    protected String name = null;
    
    
    protected String reference = null;
    
    
    
    
    public Anchor() {
        super(16);
    }
    
    
    
    public Anchor(float leading) {
        super(leading);
    }
    
    
    public Anchor(Chunk chunk) {
        super(chunk);
    }
    
    
    public Anchor(String string) {
        super(string);
    }
    
    
    public Anchor(String string, Font font) {
        super(string, font);
    }
    
    
    public Anchor(float leading, Chunk chunk) {
        super(leading, chunk);
    }
    
    
    public Anchor(float leading, String string) {
        super(leading, string);
    }
    
    
    public Anchor(float leading, String string, Font font) {
        super(leading, string, font);
    }
    
        
    public Anchor(Phrase phrase) {
        super(phrase);
        if (phrase instanceof Anchor) {
            Anchor a = (Anchor) phrase;
            setName(a.name);
            setReference(a.reference);
        }
    }
    
    
    
    
    public boolean process(ElementListener listener) {
        try {
            Chunk chunk;
            Iterator<Chunk> i = getChunks().iterator();
            boolean localDestination = (reference != null && reference.startsWith("#"));
            boolean notGotoOK = true;
            while (i.hasNext()) {
                chunk = i.next();
                if (name != null && notGotoOK && !chunk.isEmpty()) {
                    chunk.setLocalDestination(name);
                    notGotoOK = false;
                }
                if (localDestination) {
                    chunk.setLocalGoto(reference.substring(1));
                }
                listener.add(chunk);
            }
            return true;
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
    
    public ArrayList<Chunk> getChunks() {
        ArrayList<Chunk> tmp = new ArrayList<Chunk>();
        Chunk chunk;
        Iterator<Element> i = iterator();
        boolean localDestination = (reference != null && reference.startsWith("#"));
        boolean notGotoOK = true;
        while (i.hasNext()) {
            chunk = (Chunk) i.next();
            if (name != null && notGotoOK && !chunk.isEmpty()) {
                chunk.setLocalDestination(name);
                notGotoOK = false;
            }
            if (localDestination) {
                chunk.setLocalGoto(reference.substring(1));
            }
            else if (reference != null)
                chunk.setAnchor(reference);
            tmp.add(chunk);
        }
        return tmp;
    }
    
    
    public int type() {
        return Element.ANCHOR;
    }
    
    
    
    
    public void setName(String name) {
        this.name = name;
    }
    
    
    public void setReference(String reference) {
        this.reference = reference;
    }
    
    

       
    public String getName() {
        return name;
    }

    
    public String getReference() {
        return reference;
    }

    
    public URL getUrl() {
        try {
            return new URL(reference);
        }
        catch(MalformedURLException mue) {
            return null;
        }
    }

}
