

package com.lowagie.text;


public class ChapterAutoNumber extends Chapter {

    
    private static final long serialVersionUID = -9217457637987854167L;

    
    public ChapterAutoNumber(final Paragraph para) {
        super(para, 0);
    }

    
    public ChapterAutoNumber(final String title) {
        super(title, 0);
    }

    
    public Section addSection(final String title) {
        if (isAddedCompletely()) {
            throw new IllegalStateException("This LargeElement has already been added to the Document.");
        }
        return addSection(title, 2);
    }

    
    public Section addSection(final Paragraph title) {
        if (isAddedCompletely()) {
            throw new IllegalStateException("This LargeElement has already been added to the Document.");
        }
        return addSection(title, 2);
    }

}
