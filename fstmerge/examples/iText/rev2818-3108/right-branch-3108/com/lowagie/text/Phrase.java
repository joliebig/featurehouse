

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;



public class Phrase extends ArrayList<Element> implements TextElementArray {
    
    
    private static final long serialVersionUID = 2643594602455068231L;

    
    
    protected float leading = Float.NaN;
    
    
    protected Font font;
    
    
    
    
    public Phrase() {
        this(16);
    }
    
    
    public Phrase(Phrase phrase) {
        super();
        this.addAll(phrase);
        leading = phrase.getLeading();
        font = phrase.getFont();
    }
    
    
    public Phrase(float leading) {
        this.leading = leading;
        font = new Font();
    }
    
    
    public Phrase(Chunk chunk) {
        super.add(chunk);
        font = chunk.getFont();
    }
    
    
    public Phrase(float leading, Chunk chunk) {
        this.leading = leading;
        super.add(chunk);
        font = chunk.getFont();
    }
    
    
    public Phrase(String string) {
        this(Float.NaN, string, new Font());
    }
    
    
    public Phrase(String string, Font font) {
        this(Float.NaN, string, font);
    }
    
    
    public Phrase(float leading, String string) {
        this(leading, string, new Font());
    }
    
    
    public Phrase(float leading, String string, Font font) {
        this.leading = leading;
        this.font = font;
        
        if (string != null && string.length() != 0) {
            super.add(new Chunk(string, font));
        }
    }
    
    
    
    
    public boolean process(ElementListener listener) {
        try {
            for (Element e: this) {
                listener.add(e);
            }
            return true;
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
        
    public int type() {
        return Element.PHRASE;
    }
    
     
    public ArrayList<Chunk> getChunks() {
        ArrayList<Chunk> tmp = new ArrayList<Chunk>();
        for (Element e: this) {
            tmp.addAll(e.getChunks());
        }
        return tmp;
    }
    
    
    
    
    public void add(int index, Element element) {
        if (element == null) return;
        try {
            if (element.type() == Element.CHUNK) {
                Chunk chunk = (Chunk) element;
                if (!font.isStandardFont()) {
                    chunk.setFont(font.difference(chunk.getFont()));
                }
                super.add(index, chunk);
            }
            else if (element.type() == Element.PHRASE ||
            element.type() == Element.ANCHOR ||
            element.type() == Element.ANNOTATION ||
            element.type() == Element.TABLE || 
            element.type() == Element.MARKED) {
                super.add(index, element);
            }
            else {
                throw new ClassCastException(String.valueOf(element.type()));
            }
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("Insertion of illegal Element: " + cce.getMessage());
        }
    }
    
    
    public boolean addObject(Object o) {
        if (o == null) return false;
        if (o instanceof String) {
            return super.add(new Chunk((String) o, font));
        }
        if (o instanceof com.lowagie.text.rtf.RtfBasicElement) {
            
            throw new UnsupportedOperationException("Rtf elements not supported in iText-jdk15");
        }
        try {
            return add((Element)o);
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("Insertion of illegal Element: " + cce.getMessage());
        }
    }

    
    public boolean add(Element element) {
        if (element == null) return false;

        try {
            switch(element.type()) {
                case Element.CHUNK:
                    return addChunk((Chunk) element);
                case Element.PHRASE:
                case Element.PARAGRAPH:
                    Phrase phrase = (Phrase) element;
                    boolean success = true;
                    Element e;
                    for (Iterator<Element> i = phrase.iterator(); i.hasNext(); ) {
                        e = i.next();
                        if (e instanceof Chunk) {
                            success &= addChunk((Chunk)e);
                        }
                        else {
                            success &= this.add(e);
                        }
                    }
                    return success;
                case Element.MARKED:
                case Element.ANCHOR:
                case Element.ANNOTATION:
                case Element.TABLE: 
                case Element.PTABLE: 
                    
                case Element.LIST:
                    return super.add(element);
                    default:
                        throw new ClassCastException(String.valueOf(element.type()));
            }
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("Insertion of illegal Element: " + cce.getMessage());
        }
    }
    
    
    public boolean addAll(Collection<? extends Element> collection) {
        for (Iterator<? extends Element> iterator = collection.iterator(); iterator.hasNext(); ) {
            this.add(iterator.next());
        }
        return true;
    }
    
    
    protected boolean addChunk(Chunk chunk) {
        Font f = chunk.getFont();
        String c = chunk.getContent();
        if (font != null && !font.isStandardFont()) {
            f = font.difference(chunk.getFont());
        }
        if (size() > 0 && !chunk.hasAttributes()) {
            try {
                Chunk previous = (Chunk) get(size() - 1);
                if (!previous.hasAttributes()
                        && (f == null
                        || f.compareTo(previous.getFont()) == 0)
                        && !"".equals(previous.getContent().trim())
                        && !"".equals(c.trim())) {
                    previous.append(c);
                    return true;
                }
            }
            catch(ClassCastException cce) {
            }
        }
        Chunk newChunk = new Chunk(c, f);
        newChunk.setAttributes(chunk.getAttributes());
        return super.add(newChunk);
    }
    
    
    protected void addSpecial(Element object) {
        super.add(object);
    }
    
    
    
    
    
    public void setLeading(float leading) {
        this.leading = leading;
    }
    
    
    public void setFont(Font font) {
        this.font = font;
    }
    
    

    
    public float getLeading() {
        if (Float.isNaN(leading) && font != null) {
            return font.getCalculatedLeading(1.5f);
        }
        return leading;
    }

     
    public boolean hasLeading() {
        if (Float.isNaN(leading)) {
            return false;
        }
        return true;
    }

      
    public Font getFont() {
        return font;
    }

    
    public String getContent() {
        StringBuffer buf = new StringBuffer();
        for (Iterator<Chunk> i = getChunks().iterator(); i.hasNext(); ) {
            buf.append(i.next().toString());
        }
        return buf.toString();
    }
    
    
    public boolean isEmpty() {
        switch(size()) {
            case 0:
                return true;
            case 1:
                Element element = get(0);
                if (element.type() == Element.CHUNK && ((Chunk) element).isEmpty()) {
                    return true;
                }
                return false;
                default:
                    return false;
        }
    }
    
    
    
    
    
    private Phrase(boolean dummy) {
    }
    
    
    public static final Phrase getInstance(String string) {
        return getInstance(16, string, new Font());
    }
    
    
    public static final Phrase getInstance(int leading, String string) {
        return getInstance(leading, string, new Font());
    }
    
    
    public static final Phrase getInstance(int leading, String string, Font font) {
        Phrase p = new Phrase(true);
        p.setLeading(leading);
        p.font = font;
        if (font.getFamily() != Font.SYMBOL && font.getFamily() != Font.ZAPFDINGBATS && font.getBaseFont() == null) {
            int index;
            while((index = SpecialSymbol.index(string)) > -1) {
                if (index > 0) {
                    String firstPart = string.substring(0, index);
                    p.add(new Chunk(firstPart, font));
                    string = string.substring(index);
                }
                Font symbol = new Font(Font.SYMBOL, font.getSize(), font.getStyle(), font.getColor());
                StringBuffer buf = new StringBuffer();
                buf.append(SpecialSymbol.getCorrespondingSymbol(string.charAt(0)));
                string = string.substring(1);
                while (SpecialSymbol.index(string) == 0) {
                    buf.append(SpecialSymbol.getCorrespondingSymbol(string.charAt(0)));
                    string = string.substring(1);
                }
                p.add(new Chunk(buf.toString(), symbol));
            }
        }
        if (string != null && string.length() != 0) {
            p.add(new Chunk(string, font));
        }
        return p;
    }
    
    
    
    
    public Phrase(Properties attributes) {
        this(com.lowagie.text.factories.ElementFactory.getPhrase(attributes));
    }
      
    public Font font() {
        return getFont();
    }    
    
    public float leading() {
        return getLeading();
    }
    
    public boolean leadingDefined() {
        return hasLeading();
    }
    
    
    public String content() {
        return getContent();
    }
}
