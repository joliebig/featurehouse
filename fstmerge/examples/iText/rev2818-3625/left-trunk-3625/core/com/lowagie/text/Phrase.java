

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import com.lowagie.text.pdf.HyphenationEvent;



public class Phrase extends ArrayList implements TextElementArray {
    
    
    private static final long serialVersionUID = 2643594602455068231L;

    
    
    protected float leading = Float.NaN;
    
    
    protected Font font;
    
    
    protected HyphenationEvent hyphenation = null;
    
    
    
    
    public Phrase() {
        this(16);
    }
    
    
    public Phrase(Phrase phrase) {
        super();
        this.addAll(phrase);
        leading = phrase.getLeading();
        font = phrase.getFont();
        setHyphenation(phrase.getHyphenation());
    }

    
    public Phrase(float leading) {
        this.leading = leading;
        font = new Font();
    }
    
    
    public Phrase(Chunk chunk) {
        super.add(chunk);
        font = chunk.getFont();
        setHyphenation(chunk.getHyphenation());
    }

    
    public Phrase(float leading, Chunk chunk) {
        this.leading = leading;
        super.add(chunk);
        font = chunk.getFont();
        setHyphenation(chunk.getHyphenation());
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
            for (Iterator i = iterator(); i.hasNext(); ) {
                listener.add((Element) i.next());
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
    
     
    public ArrayList getChunks() {
        ArrayList tmp = new ArrayList();
        for (Iterator i = iterator(); i.hasNext(); ) {
            tmp.addAll(((Element) i.next()).getChunks());
        }
        return tmp;
    }
    
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return true;
    }
    
    
    
    
    public void add(int index, Object o) {
        if (o == null) return;
        try {
            Element element = (Element) o;
            if (element.type() == Element.CHUNK) {
                Chunk chunk = (Chunk) element;
                if (!font.isStandardFont()) {
                    chunk.setFont(font.difference(chunk.getFont()));
                }
                if (hyphenation != null) {
                    chunk.setHyphenation(hyphenation);
                }
                super.add(index, chunk);
            }
            else if (element.type() == Element.PHRASE ||
            element.type() == Element.ANCHOR ||
            element.type() == Element.ANNOTATION ||
            element.type() == Element.TABLE || 
            element.type() == Element.YMARK || 
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
    
    
    public boolean add(Object o) {
        if (o == null) return false;
        if (o instanceof String) {
            return super.add(new Chunk((String) o, font));
        }
        if (o instanceof RtfElementInterface) {
            return super.add(o);
        }
        try {
            Element element = (Element) o;
            switch(element.type()) {
                case Element.CHUNK:
                    return addChunk((Chunk) o);
                case Element.PHRASE:
                case Element.PARAGRAPH:
                    Phrase phrase = (Phrase) o;
                    boolean success = true;
                    Element e;
                    for (Iterator i = phrase.iterator(); i.hasNext(); ) {
                        e = (Element) i.next();
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
                case Element.YMARK:
                    return super.add(o);
                    default:
                        throw new ClassCastException(String.valueOf(element.type()));
            }
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("Insertion of illegal Element: " + cce.getMessage());
        }
    }
    
    
    public boolean addAll(Collection collection) {
        for (Iterator iterator = collection.iterator(); iterator.hasNext(); ) {
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
        if (newChunk.getHyphenation() == null) {
            newChunk.setHyphenation(hyphenation);
        }
        return super.add(newChunk);
    }
    
    
    protected void addSpecial(Object object) {
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
        for (Iterator i = getChunks().iterator(); i.hasNext(); ) {
            buf.append(i.next().toString());
        }
        return buf.toString();
    }
    
    
    public boolean isEmpty() {
        switch(size()) {
            case 0:
                return true;
            case 1:
                Element element = (Element) get(0);
                if (element.type() == Element.CHUNK && ((Chunk) element).isEmpty()) {
                    return true;
                }
                return false;
                default:
                    return false;
        }
    }
    
    
    public HyphenationEvent getHyphenation() {
        return hyphenation;
    }

    
    public void setHyphenation(HyphenationEvent hyphenation) {
        this.hyphenation = hyphenation;
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
                    ((ArrayList)p).add(new Chunk(firstPart, font));
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
                ((ArrayList)p).add(new Chunk(buf.toString(), symbol));
            }
        }
        if (string != null && string.length() != 0) {
            ((ArrayList)p).add(new Chunk(string, font));
        }
        return p;
    }

}