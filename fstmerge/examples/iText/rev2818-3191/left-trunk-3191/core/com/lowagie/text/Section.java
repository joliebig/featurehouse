

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;



public class Section extends ArrayList implements TextElementArray, LargeElement {
    
    
    public static final int NUMBERSTYLE_DOTTED = 0;
    
    public static final int NUMBERSTYLE_DOTTED_WITHOUT_FINAL_DOT = 1;
    
    
    private static final long serialVersionUID = 3324172577544748043L;

    
    
    
    protected Paragraph title;
    
    
    protected String bookmarkTitle;

    
    protected int numberDepth;
    
    
    protected int numberStyle = NUMBERSTYLE_DOTTED;
    
    
    protected float indentationLeft;
    
    
    protected float indentationRight;
    
    
    protected float indentation;
    
    
    protected boolean bookmarkOpen = true;
    
    
    protected boolean triggerNewPage = false;
    
    
    protected int subsections = 0;
    
    
    protected ArrayList numbers = null;
    
    
    protected boolean complete = true;
    
    
    protected boolean addedCompletely = false;
    
    
    protected boolean notAddedYet = true;
    
    
    
        
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
    
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return false;
    }
    
    
    
    
    public void add(int index, Object o) {
        if (isAddedCompletely()) {
            throw new IllegalStateException("This LargeElement has already been added to the Document.");
        }
        try {
            Element element = (Element) o;
            if (element.isNestable()) {
                super.add(index, element);
            }
            else {
                throw new ClassCastException("You can't add a " + element.getClass().getName() + " to a Section.");
            }
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("Insertion of illegal Element: " + cce.getMessage());
        }
    }
    
    
    public boolean add(Object o) {
        if (isAddedCompletely()) {
            throw new IllegalStateException("This LargeElement has already been added to the Document.");
        }
        try {
            Element element = (Element) o;
            if (element.type() == Element.SECTION) {
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
            else if (element.isNestable()) {
                return super.add(o);
            }
            else {
                throw new ClassCastException("You can't add a " + element.getClass().getName() + " to a Section.");
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
        if (isAddedCompletely()) {
            throw new IllegalStateException("This LargeElement has already been added to the Document.");
        }
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
        return constructTitle(title, numbers, numberDepth, numberStyle);
    }
    
    
    public static Paragraph constructTitle(Paragraph title, ArrayList numbers, int numberDepth, int numberStyle) {
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
        if (numberStyle == NUMBERSTYLE_DOTTED_WITHOUT_FINAL_DOT) {
            buf.deleteCharAt(buf.length() - 2);
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

    
    public void setNumberStyle(int numberStyle) {
        this.numberStyle = numberStyle;
    }
    
    
    public int getNumberStyle() {
        return numberStyle;
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
        return triggerNewPage && notAddedYet;
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

    
    public boolean isNotAddedYet() {
        return notAddedYet;
    }

    
    public void setNotAddedYet(boolean notAddedYet) {
        this.notAddedYet = notAddedYet;
    }
    
    
    protected boolean isAddedCompletely() {
        return addedCompletely;
    }
    
    
    protected void setAddedCompletely(boolean addedCompletely) {
        this.addedCompletely = addedCompletely;
    }
    
    
    public void flushContent() {
        setNotAddedYet(false);
        title = null;
        Element element;
        for (Iterator i = iterator(); i.hasNext(); ) {
            element = (Element)i.next();
            if (element instanceof Section) {
                Section s = (Section)element;
                if (!s.isComplete() && size() == 1) {
                    s.flushContent();
                    return;
                }
                else {
                    s.setAddedCompletely(true);
                }
            }
            i.remove();
        }
    }

    
    public boolean isComplete() {
        return complete;
    }

    
    public void setComplete(boolean complete) {
        this.complete = complete;
    }

}