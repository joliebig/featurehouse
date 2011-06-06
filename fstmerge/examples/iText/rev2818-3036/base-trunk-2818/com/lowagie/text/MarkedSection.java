

package com.lowagie.text;

import java.util.Collection;
import java.util.Iterator;



public class MarkedSection extends MarkedObject {

    
    protected MarkedObject title = null;
        
    
    public MarkedSection(Section section) {
        super();
        if (section.title != null) {
            title = new MarkedObject(section.title);
            section.setTitle(null);
        }
        this.element = section;
    }
    
    
    public void add(int index, Object o) {
        ((Section)element).add(index, o);
    }
        
     
    public boolean add(Object o) {
        return ((Section)element).add(o);
    }

    
    public boolean process(ElementListener listener) {
        try {
            Element element;
            for (Iterator i = ((Section)this.element).iterator(); i.hasNext(); ) {
                element = (Element)i.next();
                listener.add(element);
            }
            return true;
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
     
    public boolean addAll(Collection collection) {
        return ((Section)element).addAll(collection);
    }
      
       
    public MarkedSection addSection(float indentation, int numberDepth) {
        MarkedSection section = ((Section)element).addMarkedSection();
        section.setIndentation(indentation);
        section.setNumberDepth(numberDepth);
        return section;
    }
        
    
    public MarkedSection addSection(float indentation) {
        MarkedSection section = ((Section)element).addMarkedSection();
        section.setIndentation(indentation);
        return section;
    }
        
    
    public MarkedSection addSection(int numberDepth) {
        MarkedSection section = ((Section)element).addMarkedSection();
        section.setNumberDepth(numberDepth);
        return section;
    }
        
    
    public MarkedSection addSection() {
        return ((Section)element).addMarkedSection();
    }
        
    
        
    
    public void setTitle(MarkedObject title) {
        if (title.element instanceof Paragraph)
            this.title = title;
    }
       
    
    public MarkedObject title() {
        if (title == null) {
            return null;
        }
        int depth = Math.min(((Section)element).numbers.size(), ((Section)element).numberDepth);
        if (depth < 1) {
            return title;
        }
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < depth; i++) {
            buf.insert(0, ".");
            buf.insert(0, ((Integer) ((Section)element).numbers.get(i)).intValue());
        }
        if (buf.length() > 0) buf.append(' ');
        Paragraph result = new Paragraph((Paragraph)title.element);
        result.add(0, new Chunk(buf.toString(), ((Paragraph)title.element).getFont()));
        MarkedObject mo = new MarkedObject(result);
        mo.markupAttributes = title.markupAttributes;
        return mo;
    }
    
    
    public void setNumberDepth(int numberDepth) {
        ((Section)element).setNumberDepth(numberDepth);
    }
        
    
    public void setIndentationLeft(float indentation) {
        ((Section)element).setIndentationLeft(indentation);
    }
        
       
    public void setIndentationRight(float indentation) {
        ((Section)element).setIndentationRight(indentation);
    }
        
    
    public void setIndentation(float indentation) {
        ((Section)element).setIndentation(indentation);
    }
        
    
    public void setBookmarkOpen(boolean bookmarkOpen) {
         ((Section)element).setBookmarkOpen(bookmarkOpen);
    }
        
    
    public void setTriggerNewPage(boolean triggerNewPage) {
          ((Section)element).setTriggerNewPage(triggerNewPage);
    }
        
        
    public void setBookmarkTitle(String bookmarkTitle) {
          ((Section)element).setBookmarkTitle(bookmarkTitle);
    }
}