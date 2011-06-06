

package com.lowagie.text.rtf.headerfooter;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Table;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.field.RtfPageNumber;



public class RtfHeaderFooter extends HeaderFooter implements RtfBasicElement {

    
    public static final int TYPE_HEADER = 1;
    
    public static final int TYPE_FOOTER = 2;
    
    public static final int DISPLAY_FIRST_PAGE = 0;
    
    public static final int DISPLAY_ALL_PAGES = 1;
    
    public static final int DISPLAY_LEFT_PAGES = 2;
    
    public static final int DISPLAY_RIGHT_PAGES = 4;

    
    private static final byte[] HEADER_ALL = "\\header".getBytes();
    
    private static final byte[] HEADER_FIRST = "\\headerf".getBytes();
    
    private static final byte[] HEADER_LEFT = "\\headerl".getBytes();
    
    private static final byte[] HEADER_RIGHT = "\\headerr".getBytes();
    
    private static final byte[] FOOTER_ALL = "\\footer".getBytes();
    
    private static final byte[] FOOTER_FIRST = "\\footerf".getBytes();
    
    private static final byte[] FOOTER_LEFT = "\\footerl".getBytes();
    
    private static final byte[] FOOTER_RIGHT = "\\footerr".getBytes();
    
    
    private RtfDocument document = null;
    
    private Object[] content = null;
    
    private int type = TYPE_HEADER;
    
    private int displayAt = DISPLAY_ALL_PAGES;
   
    
    protected RtfHeaderFooter(RtfDocument doc, HeaderFooter headerFooter, int type, int displayAt) {
        super(new Phrase(""), false);
        this.document = doc;
        this.type = type;
        this.displayAt = displayAt;
        Paragraph par = new Paragraph();
        par.setAlignment(headerFooter.alignment());
        if (headerFooter.getBefore() != null) {
            par.add(headerFooter.getBefore());
        }
        if (headerFooter.isNumbered()) {
            par.add(new RtfPageNumber(this.document));
        }
        if (headerFooter.getAfter() != null) {
            par.add(headerFooter.getAfter());
        }
        try {
            this.content = new Object[1];
            if(this.document != null) {
                this.content[0] = this.document.getMapper().mapElement(par);
                ((RtfBasicElement) this.content[0]).setInHeader(true);
            } else {
                this.content[0] = par;
            }
        } catch(DocumentException de) {
            de.printStackTrace();
        }
    }
    
    
    protected RtfHeaderFooter(RtfDocument doc, RtfHeaderFooter headerFooter, int displayAt) {
        super(new Phrase(""), false);
        this.document = doc;
        this.content = headerFooter.getContent();
        this.displayAt = displayAt;
        for(int i = 0; i < this.content.length; i++) {
            if(this.content[i] instanceof Element) {
                try {
                    this.content[i] = this.document.getMapper().mapElement((Element) this.content[i]);
                } catch(DocumentException de) {
                    de.printStackTrace();
                }
            }
            if(this.content[i] instanceof RtfBasicElement) {
                ((RtfBasicElement) this.content[i]).setInHeader(true);
            }
        }
    }
    
    
    protected RtfHeaderFooter(RtfDocument doc, HeaderFooter headerFooter) {
        super(new Phrase(""), false);
        this.document = doc;
        Paragraph par = new Paragraph();
        par.setAlignment(headerFooter.alignment());
        if (headerFooter.getBefore() != null) {
            par.add(headerFooter.getBefore());
        }
        if (headerFooter.isNumbered()) {
            par.add(new RtfPageNumber(this.document));
        }
        if (headerFooter.getAfter() != null) {
            par.add(headerFooter.getAfter());
        }
        try {
            this.content = new Object[1];
            this.content[0] = doc.getMapper().mapElement(par);
            ((RtfBasicElement) this.content[0]).setInHeader(true);
        } catch(DocumentException de) {
            de.printStackTrace();
        }
    }
    
    
    public RtfHeaderFooter(Element element) {
        this(new Element[]{element});
    }

    
    public RtfHeaderFooter(Element[] elements) {
        super(new Phrase(""), false);
        this.content = new Object[elements.length];
        for(int i = 0; i < elements.length; i++) {
            this.content[i] = elements[i];
        }
    }
    
    
    public void setRtfDocument(RtfDocument doc) {
        this.document = doc;
        if(this.document != null) {
            for(int i = 0; i < this.content.length; i++) {
                try {
                    if(this.content[i] instanceof Element) {
                        this.content[i] = this.document.getMapper().mapElement((Element) this.content[i]);
                        ((RtfBasicElement) this.content[i]).setInHeader(true);
                    } else if(this.content[i] instanceof RtfBasicElement){
                        ((RtfBasicElement) this.content[i]).setRtfDocument(this.document);
                        ((RtfBasicElement) this.content[i]).setInHeader(true);
                    }
                } catch(DocumentException de) {
                    de.printStackTrace();
                }
            }
        }
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        if(this.type == TYPE_HEADER) {
            if(this.displayAt == DISPLAY_ALL_PAGES) {
                result.write(HEADER_ALL);
            } else if(this.displayAt == DISPLAY_FIRST_PAGE) {
                result.write(HEADER_FIRST);
            } else if(this.displayAt == DISPLAY_LEFT_PAGES) {
                result.write(HEADER_LEFT);
            } else if(this.displayAt == DISPLAY_RIGHT_PAGES) {
                result.write(HEADER_RIGHT);
            }
        } else {
            if(this.displayAt == DISPLAY_ALL_PAGES) {
                result.write(FOOTER_ALL);
            } else if(this.displayAt == DISPLAY_FIRST_PAGE) {
                result.write(FOOTER_FIRST);
            } else if(this.displayAt == DISPLAY_LEFT_PAGES) {
                result.write(FOOTER_LEFT);
            } else if(this.displayAt == DISPLAY_RIGHT_PAGES) {
                result.write(FOOTER_RIGHT);
            }
        }
        result.write(DELIMITER);
        for(int i = 0; i < this.content.length; i++) {
            if(this.content[i] instanceof RtfBasicElement) {
                RtfBasicElement rbe = (RtfBasicElement)this.content[i];
                rbe.writeContent(result);
            }
        }
        result.write(CLOSE_GROUP);
    }        
    
    
    
    public void setDisplayAt(int displayAt) {
        this.displayAt = displayAt;
    }
    
    
    public void setType(int type) {
        this.type = type;
    }
    
    
    private Object[] getContent() {
        return this.content;
    }

    
    public void setInTable(boolean inTable) {
    }
    
    
    public void setInHeader(boolean inHeader) {
    }
    
    
    public void setAlignment(int alignment) {
        super.setAlignment(alignment);
        for(int i = 0; i < this.content.length; i++) {
            if(this.content[i] instanceof Paragraph) {
                ((Paragraph) this.content[i]).setAlignment(alignment);
            } else if(this.content[i] instanceof Table) {
                ((Table) this.content[i]).setAlignment(alignment);
            } else if(this.content[i] instanceof Image) {
                ((Image) this.content[i]).setAlignment(alignment);
            }     
        }
    }
}
