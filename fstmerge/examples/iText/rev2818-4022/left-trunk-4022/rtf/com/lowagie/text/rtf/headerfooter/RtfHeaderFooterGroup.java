

package com.lowagie.text.rtf.headerfooter;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Phrase;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfHeaderFooterGroup extends HeaderFooter implements RtfBasicElement {
    
    
    private static final int MODE_NONE = 0;
    
    private static final int MODE_SINGLE = 1;
    
    private static final int MODE_MULTIPLE = 2;
    
    
    private int mode = MODE_NONE;
    
    private int type = RtfHeaderFooter.TYPE_HEADER;
    
    
    private RtfHeaderFooter headerAll = null;
    
    private RtfHeaderFooter headerFirst = null;
    
    private RtfHeaderFooter headerLeft = null;
    
    private RtfHeaderFooter headerRight = null;
    
    private RtfDocument document = null;

    
    public RtfHeaderFooterGroup() {
        super(new Phrase(""), false);
        this.mode = MODE_NONE;
    }
    
    
    public RtfHeaderFooterGroup(RtfDocument doc, int type) {
        super(new Phrase(""), false);
        this.document = doc;
        this.type = type;
    }
    
    
    public RtfHeaderFooterGroup(RtfDocument doc, RtfHeaderFooterGroup headerFooter, int type) {
        super(new Phrase(""), false);
        this.document = doc;
        this.mode = headerFooter.getMode();
        this.type = type;
        if(headerFooter.getHeaderAll() != null) {
            this.headerAll = new RtfHeaderFooter(this.document, headerFooter.getHeaderAll(), RtfHeaderFooter.DISPLAY_ALL_PAGES);
        }
        if(headerFooter.getHeaderFirst() != null) {
            this.headerFirst = new RtfHeaderFooter(this.document, headerFooter.getHeaderFirst(), RtfHeaderFooter.DISPLAY_FIRST_PAGE);
        }
        if(headerFooter.getHeaderLeft() != null) {
            this.headerLeft = new RtfHeaderFooter(this.document, headerFooter.getHeaderLeft(), RtfHeaderFooter.DISPLAY_LEFT_PAGES);
        }
        if(headerFooter.getHeaderRight() != null) {
            this.headerRight = new RtfHeaderFooter(this.document, headerFooter.getHeaderRight(), RtfHeaderFooter.DISPLAY_RIGHT_PAGES);
        }
        setType(this.type);
    }
    
    
    public RtfHeaderFooterGroup(RtfDocument doc, RtfHeaderFooter headerFooter, int type) {
        super(new Phrase(""), false);
        this.document = doc;
        this.type = type;
        this.mode = MODE_SINGLE;
        headerAll = new RtfHeaderFooter(doc, headerFooter, RtfHeaderFooter.DISPLAY_ALL_PAGES);
        headerAll.setType(this.type);
    }
    
    
    public RtfHeaderFooterGroup(RtfDocument doc, HeaderFooter headerFooter, int type) {
        super(new Phrase(""), false);
        this.document = doc;
        this.type = type;
        this.mode = MODE_SINGLE;
        headerAll = new RtfHeaderFooter(doc, headerFooter, type, RtfHeaderFooter.DISPLAY_ALL_PAGES);
        headerAll.setType(this.type);
    }
    
    
    public void setRtfDocument(RtfDocument doc) {
        this.document = doc;
        if(headerAll != null) {
            headerAll.setRtfDocument(this.document);
        }
        if(headerFirst != null) {
            headerFirst.setRtfDocument(this.document);
        }
        if(headerLeft != null) {
            headerLeft.setRtfDocument(this.document);
        }
        if(headerRight != null) {
            headerRight.setRtfDocument(this.document);
        }
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        if(this.mode == MODE_SINGLE) {
            headerAll.writeContent(result);
        } else if(this.mode == MODE_MULTIPLE) {
            if(headerFirst != null) {
                headerFirst.writeContent(result);
            }
            if(headerLeft != null) {
                headerLeft.writeContent(result);
            }
            if(headerRight != null) {
                headerRight.writeContent(result);
            }
            if(headerAll != null) {
                headerAll.writeContent(result);
            }
        }
    }        
    
    
    public void setHeaderFooter(RtfHeaderFooter headerFooter, int displayAt) {
        this.mode = MODE_MULTIPLE;
        headerFooter.setRtfDocument(this.document);
        headerFooter.setType(this.type);
        headerFooter.setDisplayAt(displayAt);
        switch(displayAt) {
            case RtfHeaderFooter.DISPLAY_ALL_PAGES:
                headerAll = headerFooter;
                break;
            case RtfHeaderFooter.DISPLAY_FIRST_PAGE:
                headerFirst = headerFooter;
                break;
            case RtfHeaderFooter.DISPLAY_LEFT_PAGES:
                headerLeft = headerFooter;
                break;
            case RtfHeaderFooter.DISPLAY_RIGHT_PAGES:
                headerRight = headerFooter;
                break;
        }
    }
    
    
    public void setHeaderFooter(HeaderFooter headerFooter, int displayAt) {
        this.mode = MODE_MULTIPLE;
        switch(displayAt) {
            case RtfHeaderFooter.DISPLAY_ALL_PAGES:
                headerAll = new RtfHeaderFooter(this.document, headerFooter, this.type, displayAt);
                break;
            case RtfHeaderFooter.DISPLAY_FIRST_PAGE:
                headerFirst = new RtfHeaderFooter(this.document, headerFooter, this.type, displayAt);
                break;
            case RtfHeaderFooter.DISPLAY_LEFT_PAGES:
                headerLeft = new RtfHeaderFooter(this.document, headerFooter, this.type, displayAt);
                break;
            case RtfHeaderFooter.DISPLAY_RIGHT_PAGES:
                headerRight = new RtfHeaderFooter(this.document, headerFooter, this.type, displayAt);
                break;
        }
    }
    
    
    public void setHasTitlePage() {
        if(this.mode == MODE_SINGLE) {
            this.mode = MODE_MULTIPLE;
            headerFirst = new RtfHeaderFooter(this.document, headerAll, RtfHeaderFooter.DISPLAY_FIRST_PAGE);
            headerFirst.setType(this.type);
        }
    }
    
    
    public void setHasFacingPages() {
        if(this.mode == MODE_SINGLE) {
            this.mode = MODE_MULTIPLE;
            this.headerLeft = new RtfHeaderFooter(this.document, this.headerAll, RtfHeaderFooter.DISPLAY_LEFT_PAGES);
            this.headerLeft.setType(this.type);
            this.headerRight = new RtfHeaderFooter(this.document, this.headerAll, RtfHeaderFooter.DISPLAY_RIGHT_PAGES);
            this.headerRight.setType(this.type);
            this.headerAll = null;
        } else if(this.mode == MODE_MULTIPLE) {
            if(this.headerLeft == null && this.headerAll != null) {
                this.headerLeft = new RtfHeaderFooter(this.document, this.headerAll, RtfHeaderFooter.DISPLAY_LEFT_PAGES);
                this.headerLeft.setType(this.type);
            }
            if(this.headerRight == null && this.headerAll != null) {
                this.headerRight = new RtfHeaderFooter(this.document, this.headerAll, RtfHeaderFooter.DISPLAY_RIGHT_PAGES);
                this.headerRight.setType(this.type);
            }
            this.headerAll = null;
        }
    }
    
    
    public boolean hasTitlePage() {
        return (headerFirst != null);
    }
    
    
    public boolean hasFacingPages() {
        return (headerLeft != null || headerRight != null);
    }

    
    public void setInTable(boolean inTable) {
    }
    
    
    public void setInHeader(boolean inHeader) {
    }
    
    
    public void setType(int type) {
        this.type = type;
        if(headerAll != null) {
            headerAll.setType(this.type);
        }
        if(headerFirst != null) {
            headerFirst.setType(this.type);
        }
        if(headerLeft != null) {
            headerLeft.setType(this.type);
        }
        if(headerRight != null) {
            headerRight.setType(this.type);
        }
    }
    
    
    protected int getMode() {
        return this.mode;
    }
    
    
    protected RtfHeaderFooter getHeaderAll() {
        return headerAll;
    }

    
    protected RtfHeaderFooter getHeaderFirst() {
        return headerFirst;
    }

    
    protected RtfHeaderFooter getHeaderLeft() {
        return headerLeft;
    }

    
    protected RtfHeaderFooter getHeaderRight() {
        return headerRight;
    }
}
