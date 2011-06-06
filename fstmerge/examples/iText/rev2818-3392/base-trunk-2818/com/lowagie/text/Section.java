

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;



public class Section extends ArrayList implements TextElementArray {
    
    
    private static final long serialVersionUID = 3324172577544748043L;

    
    
    
    protected Paragraph title;
    
    
    protected String bookmarkTitle;
    
    
    protected int numberDepth;
    
    
    protected float indentationLeft;
    
    
    protected float indentationRight;
    
    
    protected float indentation;
    
    
    protected boolean bookmarkOpen = true;
    
    
    protected boolean triggerNewPage = false;
    
    
    protected int subsections = 0;
    
    
    protected ArrayList numbers = null;
    
    
    
        
    protected Section() {
        title = new Paragraph();
        numberDepth = 1;
    }
    
    
    protected Section(Paragraph title, int numberDepth) {
        this.numberDepth = numberDepth;
        this.title = title;
    }
    
    
    
    
    public boolean process(ElementListener listener) {
        try {
            Element element;
            for (Iterator i = iterator(); i.hasNext(); ) {
                element = (Element)i.next();
                listener.add(element);
            }
            return true;
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
        
    public int type() {
        return Element.SECTION;
    }
    
    
    public boolean isChapter() {
        return type() == Element.CHAPTER;
    }
    
    
    public boolean isSection() {
        return type() == Element.SECTION;
    }
    
    
    public ArrayList getChunks() {
        ArrayList tmp = new ArrayList();
        for (Iterator i = iterator(); i.hasNext(); ) {
            tmp.addAll(((Element) i.next()).getChunks());
        }
        return tmp;
    }
    
    
    
    
    public void add(int index, Object o) {
        try {
            Element element = (Element) o;
            if (element.type() == Element.PARAGRAPH ||
            element.type() == Element.LIST ||
            element.type() == Element.CHUNK ||
            element.type() == Element.PHRASE ||
            element.type() == Element.ANCHOR ||
            element.type() == Element.ANNOTATION ||
            element.type() == Element.TABLE ||
            element.type() == Element.PTABLE ||
            element.type() == Element.IMGTEMPLATE ||
            element.type() == Element.JPEG ||
            element.type() == Element.IMGRAW) {
                super.add(index, element);
            }
            else {
                throw new ClassCastException("You can add a " + element.getClass().getName() + " to a Section.");
            }
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("Insertion of illegal Element: " + cce.getMessage());
        }
    }
    
    
    public boolean add(Object o) {
        try {
            Element element = (Element) o;
            if (element.type() == Element.PARAGRAPH ||
            element.type() == Element.LIST ||
            element.type() == Element.CHUNK ||
            element.type() == Element.PHRASE ||
            element.type() == Element.ANCHOR ||
            element.type() == Element.ANNOTATION ||
            element.type() == Element.TABLE ||
            element.type() == Element.IMGTEMPLATE ||
            element.type() == Element.PTABLE ||
            element.type() == Element.JPEG ||
            element.type() == Element.IMGRAW) {
                return super.add(o);
            }
            else if (element.type() == Element.SECTION) {
                Section section = (Section) o;
                section.setNumbers(++subsections, numbers);
                return super.add(section);
            }
            else if (o instanceof MarkedSection && ((MarkedObject)o).element.type() == Element.SECTION) {
                MarkedSection mo = (MarkedSection)o;
                Section section = (Section)mo.element;
                section.setNumbers(++subsections, numbers);
                return super.add(mo);
            }
            else if (element instanceof MarkedObject) {
                return super.add(o);
            }
            else {
                throw new ClassCastException("You can add a " + element.getClass().getName() + " to a Section.");
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
    
    
    
    
    public Section addSection(float indentation, Paragraph title, int numberDepth) {
        Section section = new Section(title, numberDepth);
        section.setIndentation(indentation);
        add(section);
        return section;
    }
    
    
    public Section addSection(float indentation, Paragraph title) {
        return addSection(indentation, title, numberDepth + 1);
    }
    
    
    public Section addSection(Paragraph title, int numberDepth) {
        return addSection(0, title, numberDepth);
    }
    
    
    public MarkedSection addMarkedSection() {
        MarkedSection section = new MarkedSection(new Section(null, numberDepth + 1));
        add(section);
        return section;
    }
    
    
    public Section addSection(Paragraph title) {
        return addSection(0, title, numberDepth + 1);
    }
    
    
    public Section addSection(float indentation, String title, int numberDepth) {
        return addSection(indentation, new Paragraph(title), numberDepth);
    }
    
    
    public Section addSection(String title, int numberDepth) {
        return addSection(new Paragraph(title), numberDepth);
    }
    
    
    public Section addSection(float indentation, String title) {
        return addSection(indentation, new Paragraph(title));
    }
    
    
    public Section addSection(String title) {
        return addSection(new Paragraph(title));
    }
    
    
    
    
    public void setTitle(Paragraph title) {
        this.title = title;
    }

    
    public Paragraph getTitle() {
        if (title == null) {
            return null;
        }
        int depth = Math.min(numbers.size(), numberDepth);
        if (depth < 1) {
            return title;
        }
        StringBuffer buf = new StringBuffer(" ");
        for (int i = 0; i < depth; i++) {
            buf.insert(0, ".");
            buf.insert(0, ((Integer) numbers.get(i)).intValue());
        }
        Paragraph result = new Paragraph(title);
        result.add(0, new Chunk(buf.toString(), title.getFont()));
        return result;
    }
    
    
    public void setNumberDepth(int numberDepth) {
        this.numberDepth = numberDepth;
    }
    
    
    public int getNumberDepth() {
        return numberDepth;
    }
    
    
    public void setIndentationLeft(float indentation) {
        indentationLeft = indentation;
    }

    
    public float getIndentationLeft() {
        return indentationLeft;
    }
    
    
    public void setIndentationRight(float indentation) {
        indentationRight = indentation;
    }

    
    public float getIndentationRight() {
        return indentationRight;
    }
    
    
    public void setIndentation(float indentation) {
        this.indentation = indentation;
    }

    
    public float getIndentation() {
        return indentation;
    }
    
    
    public void setBookmarkOpen(boolean bookmarkOpen) {
        this.bookmarkOpen = bookmarkOpen;
    }
    
    
    public boolean isBookmarkOpen() {
        return bookmarkOpen;
    }
    
    
    public void setTriggerNewPage(boolean triggerNewPage) {
        this.triggerNewPage = triggerNewPage;
    }

    
    public boolean isTriggerNewPage() {
        return triggerNewPage;
    }
    
        
    public void setBookmarkTitle(String bookmarkTitle) {
        this.bookmarkTitle = bookmarkTitle;
    }

        
    public Paragraph getBookmarkTitle() {
        if (bookmarkTitle == null)
            return getTitle();
        else
            return new Paragraph(bookmarkTitle);
    }
    
    
    public void setChapterNumber(int number) {
        numbers.set(numbers.size() - 1, new Integer(number));
        Object s;
        for (Iterator i = iterator(); i.hasNext(); ) {
            s = i.next();
            if (s instanceof Section) {
                ((Section)s).setChapterNumber(number);
            }
        }
    }

    
    public int getDepth() {
        return numbers.size();
    }
    
    
    
    
    private void setNumbers(int number, ArrayList numbers) {
        this.numbers = new ArrayList();
        this.numbers.add(new Integer(number));
        this.numbers.addAll(numbers);
    }
    
    
    
    
    public Paragraph title() {
        return getTitle();
    }
    
    
    public int numberDepth() {
        return getNumberDepth();
    }
    
    
    public float indentationLeft() {
        return getIndentationLeft();
    }
    
    
    public float indentationRight() {
        return getIndentationRight();
    }
    
    
    public float indentation() {
        return getIndentation();
    }
    
    
    public int depth() {
        return getDepth();
    }
    
    
    public Section addSection(java.util.Properties attributes) {
        return com.lowagie.text.factories.ElementFactory.getSection(this, attributes);
    }
}