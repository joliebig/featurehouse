

package com.lowagie.text.html;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;
import java.util.EmptyStackException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;
import java.util.Stack;

import com.lowagie.text.Anchor;
import com.lowagie.text.Annotation;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Cell;
import com.lowagie.text.Chunk;
import com.lowagie.text.DocWriter;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.Header;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.MarkedObject;
import com.lowagie.text.MarkedSection;
import com.lowagie.text.Meta;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Row;
import com.lowagie.text.Section;
import com.lowagie.text.SimpleTable;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.BaseFont;



public class HtmlWriter extends DocWriter {
    
    
    

    public static final byte[] BEGINCOMMENT = getISOBytes("<!-- ");
    

    public static final byte[] ENDCOMMENT = getISOBytes(" -->");
    

    public static final String NBSP = "&nbsp;";
    
    
    

    protected Stack<Font> currentfont = new Stack<Font>();
    

    protected Font standardfont = new Font();
    

    protected String imagepath = null;
    

    protected int pageN = 0;
    

    protected HeaderFooter header = null;
    

    protected HeaderFooter footer = null;
    

    protected Properties markup = new Properties();
    
    
    

    
    protected HtmlWriter(Document doc, OutputStream os) {
        super(doc, os);
        
        document.addDocListener(this);
        this.pageN = document.getPageNumber();
        try {
            os.write(LT);
            os.write(getISOBytes(HtmlTags.HTML));
            os.write(GT);
            os.write(NEWLINE);
            os.write(TAB);
            os.write(LT);
            os.write(getISOBytes(HtmlTags.HEAD));
            os.write(GT);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    
    
    

    
    public static HtmlWriter getInstance(Document document, OutputStream os) {
        return new HtmlWriter(document, os);
    }
    
    
    

    
    public boolean newPage() {
        try {
            writeStart(HtmlTags.DIV);
            write(" ");
            write(HtmlTags.STYLE);
            write("=\"");
            writeCssProperty(Markup.CSS_KEY_PAGE_BREAK_BEFORE, Markup.CSS_VALUE_ALWAYS);
            write("\" /");
            os.write(GT);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
        return true;
    }
    

    
    public boolean add(Element element) throws DocumentException {
        if (pause) {
            return false;
        }
        if (open && !element.isContent()) {
                throw new DocumentException(
                        "The document is open; you can only add Elements with content.");
        }
        try {
            switch(element.type()) {
                case Element.HEADER:
                    try {
                        Header h = (Header) element;
                        if (HtmlTags.STYLESHEET.equals(h.getName())) {
                            writeLink(h);
                        }
                        else if (HtmlTags.JAVASCRIPT.equals(h.getName())) {
                            writeJavaScript(h);
                        }
                        else {
                            writeHeader(h);
                        }
                    }
                    catch(ClassCastException cce) {
                    }
                    return true;
                case Element.SUBJECT:
                case Element.KEYWORDS:
                case Element.AUTHOR:
                    Meta meta = (Meta) element;
                    writeHeader(meta);
                    return true;
                case Element.TITLE:
                    addTabs(2);
                    writeStart(HtmlTags.TITLE);
                    os.write(GT);
                    addTabs(3);
                    write(HtmlEncoder.encode(((Meta)element).getContent()));
                    addTabs(2);
                    writeEnd(HtmlTags.TITLE);
                    return true;
                case Element.CREATOR:
                    writeComment("Creator: " + HtmlEncoder.encode(((Meta)element).getContent()));
                    return true;
                case Element.PRODUCER:
                    writeComment("Producer: " + HtmlEncoder.encode(((Meta)element).getContent()));
                    return true;
                case Element.CREATIONDATE:
                    writeComment("Creationdate: " + HtmlEncoder.encode(((Meta)element).getContent()));
                    return true;
                case Element.MARKED:
                    if (element instanceof MarkedSection) {
                        MarkedSection ms = (MarkedSection)element;
                        addTabs(1);
                        writeStart(HtmlTags.DIV);
                        writeMarkupAttributes(ms.getMarkupAttributes());
                        os.write(GT);
                        MarkedObject mo = ((MarkedSection)element).getTitle();
                        if (mo != null) {
                            markup = mo.getMarkupAttributes();
                            mo.process(this);
                        }
                        ms.process(this);
                        writeEnd(HtmlTags.DIV);
                        return true;
                    }
                    else {
                        MarkedObject mo = (MarkedObject) element;
                        markup = mo.getMarkupAttributes();
                        return mo.process(this);
                    }
                default:
                    write(element, 2);
                    return true;
            }
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public void open() {
        super.open();
        try {
            writeComment(Document.getVersion());
            writeComment("CreationDate: " + new Date().toString());
            addTabs(1);
            writeEnd(HtmlTags.HEAD);
            addTabs(1);
            writeStart(HtmlTags.BODY);
            if (document.leftMargin() > 0) {
                write(HtmlTags.LEFTMARGIN, String.valueOf(document.leftMargin()));
            }
            if (document.rightMargin() > 0) {
                write(HtmlTags.RIGHTMARGIN, String.valueOf(document.rightMargin()));
            }
            if (document.topMargin() > 0) {
                write(HtmlTags.TOPMARGIN, String.valueOf(document.topMargin()));
            }
            if (document.bottomMargin() > 0) {
                write(HtmlTags.BOTTOMMARGIN, String.valueOf(document.bottomMargin()));
            }
            if (pageSize.getBackgroundColor() != null) {
                write(HtmlTags.BACKGROUNDCOLOR, HtmlEncoder.encode(pageSize.getBackgroundColor()));
            }
            if (document.getJavaScript_onLoad() != null) {
                write(HtmlTags.JAVASCRIPT_ONLOAD, HtmlEncoder.encode(document.getJavaScript_onLoad()));
            }
            if (document.getJavaScript_onUnLoad() != null) {
                write(HtmlTags.JAVASCRIPT_ONUNLOAD, HtmlEncoder.encode(document.getJavaScript_onUnLoad()));
            }
            if (document.getHtmlStyleClass() != null) {
                write(Markup.HTML_ATTR_CSS_CLASS, document.getHtmlStyleClass());
            }
            os.write(GT);
            initHeader(); 
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    public void close() {
        try {
            initFooter(); 
            addTabs(1);
            writeEnd(HtmlTags.BODY);
            os.write(NEWLINE);
            writeEnd(HtmlTags.HTML);
            super.close();
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    
    
    

    
    protected void initHeader() {
        if (header != null) {
            try {
                add(header.paragraph());
            }
            catch(Exception e) {
                throw new ExceptionConverter(e);
            }
        }
    }
    

    
    protected void initFooter() {
        if (footer != null) {
            try {
                
                
                footer.setPageNumber(pageN + 1);
                add(footer.paragraph());
            }
            catch(Exception e) {
                throw new ExceptionConverter(e);
            }
        }
    }
    

    
    protected void writeHeader(Meta meta) throws IOException {
        addTabs(2);
        writeStart(HtmlTags.META);
        switch(meta.type()) {
            case Element.HEADER:
                write(HtmlTags.NAME, ((Header) meta).getName());
                break;
            case Element.SUBJECT:
                write(HtmlTags.NAME, HtmlTags.SUBJECT);
                break;
            case Element.KEYWORDS:
                write(HtmlTags.NAME, HtmlTags.KEYWORDS);
                break;
            case Element.AUTHOR:
                write(HtmlTags.NAME, HtmlTags.AUTHOR);
                break;
        }
        write(HtmlTags.CONTENT, HtmlEncoder.encode(meta.getContent()));
        writeEnd();
    }
    

    
    protected void writeLink(Header header) throws IOException {
        addTabs(2);
        writeStart(HtmlTags.LINK);
        write(HtmlTags.REL, header.getName());
        write(HtmlTags.TYPE, HtmlTags.TEXT_CSS);
        write(HtmlTags.REFERENCE, header.getContent());
        writeEnd();
    }
    

    
    protected void writeJavaScript(Header header) throws IOException {
        addTabs(2);
        writeStart(HtmlTags.SCRIPT);
        write(HtmlTags.LANGUAGE, HtmlTags.JAVASCRIPT);
        if (markup.size() > 0) {
           
          writeMarkupAttributes(markup);
          os.write(GT);
          writeEnd(HtmlTags.SCRIPT);
        }
        else {
           
          write(HtmlTags.TYPE, Markup.HTML_VALUE_JAVASCRIPT);
          os.write(GT);
          addTabs(2);
          write(new String(BEGINCOMMENT) + "\n");
          write(header.getContent());
          addTabs(2);
          write("//" + new String(ENDCOMMENT));
          addTabs(2);
          writeEnd(HtmlTags.SCRIPT);
        }
    }
    

    
    protected void writeComment(String comment) throws IOException {
        addTabs(2);
        os.write(BEGINCOMMENT);
        write(comment);
        os.write(ENDCOMMENT);
    }
    
    
    

    
    public void setStandardFont(Font standardfont) {
        this.standardfont = standardfont;
    }
    

    
    public boolean isOtherFont(Font font) {
        try {
            Font cFont = currentfont.peek();
            if (cFont.compareTo(font) == 0) return false;
            return true;
        }
        catch(EmptyStackException ese) {
            if (standardfont.compareTo(font) == 0) return false;
            return true;
        }
    }
    

    
    public void setImagepath(String imagepath) {
        this.imagepath = imagepath;
    }
    

    
    public void resetImagepath() {
        imagepath = null;
    }
    

    
    public void setHeader(HeaderFooter header) {
        this.header = header;
    }
    

    
    public void setFooter(HeaderFooter footer) {
        this.footer = footer;
    }
    

    
    public boolean add(String string) {
        if (pause) {
            return false;
        }
        try
        {
            write(string);
            return true;
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }
    

    
    protected void write(Element element, int indent) throws IOException {
        Properties styleAttributes = null;
        switch(element.type()) {
            case Element.MARKED: {
                try {
                    add(element);
                } catch (DocumentException e) {
                    e.printStackTrace();
                }
                return;
            }
            case Element.CHUNK:
            {
                Chunk chunk = (Chunk) element;
                
                Image image = chunk.getImage();
                if (image != null) {
                    write(image, indent);
                    return;
                }
                
                if (chunk.isEmpty()) return;
                HashMap<String, Object> attributes = chunk.getAttributes();
                if (attributes != null && attributes.get(Chunk.NEWPAGE) != null) {
                    return;
                }
                boolean tag = isOtherFont(chunk.getFont()) || markup.size() > 0;
                if (tag) {
                    
                    addTabs(indent);
                    writeStart(HtmlTags.SPAN);
                    if (isOtherFont(chunk.getFont())) {
                        write(chunk.getFont(), null);
                    }
                    writeMarkupAttributes(markup);
                    os.write(GT);
                }
                if (attributes != null && attributes.get(Chunk.SUBSUPSCRIPT) != null) {
                    
                    if (((Float)attributes.get(Chunk.SUBSUPSCRIPT)).floatValue() > 0) {
                        writeStart(HtmlTags.SUP);
                    }
                    else {
                        writeStart(HtmlTags.SUB);
                    }
                    os.write(GT);
                }
                
                write(HtmlEncoder.encode(chunk.getContent()));
                if (attributes != null && attributes.get(Chunk.SUBSUPSCRIPT) != null) {
                    
                    os.write(LT);
                    os.write(FORWARD);
                    if (((Float)attributes.get(Chunk.SUBSUPSCRIPT)).floatValue() > 0) {
                        write(HtmlTags.SUP);
                    }
                    else {
                        write(HtmlTags.SUB);
                    }
                    os.write(GT);
                }
                if (tag) {
                    
                    writeEnd(Markup.HTML_TAG_SPAN);
                }
                return;
            }
            case Element.PHRASE:
            {
                Phrase phrase = (Phrase) element;
                styleAttributes = new Properties();
                if (phrase.hasLeading()) styleAttributes.setProperty(Markup.CSS_KEY_LINEHEIGHT, phrase.getLeading() + "pt");
                
                
                addTabs(indent);
                writeStart(Markup.HTML_TAG_SPAN);
                writeMarkupAttributes(markup);
                write(phrase.getFont(), styleAttributes);
                os.write(GT);
                currentfont.push(phrase.getFont());
                
                for (Element e: phrase) {
                    write(e, indent + 1);
                }
                
                addTabs(indent);
                writeEnd(Markup.HTML_TAG_SPAN);
                currentfont.pop();
                return;
            }
            case Element.ANCHOR:
            {
                Anchor anchor = (Anchor) element;
                styleAttributes = new Properties();
                if (anchor.hasLeading()) styleAttributes.setProperty(Markup.CSS_KEY_LINEHEIGHT, anchor.getLeading() + "pt");
                
                
                addTabs(indent);
                writeStart(HtmlTags.ANCHOR);
                if (anchor.getName() != null) {
                    write(HtmlTags.NAME, anchor.getName());
                }
                if (anchor.getReference() != null) {
                    write(HtmlTags.REFERENCE, anchor.getReference());
                }
                writeMarkupAttributes(markup);
                write(anchor.getFont(), styleAttributes);
                os.write(GT);
                currentfont.push(anchor.getFont());
                
                for (Element e: anchor) {
                    write(e, indent + 1);
                }
                
                addTabs(indent);
                writeEnd(HtmlTags.ANCHOR);
                currentfont.pop();
                return;
            }
            case Element.PARAGRAPH:
            {
                Paragraph paragraph = (Paragraph) element;
                styleAttributes = new Properties();
                if (paragraph.hasLeading()) styleAttributes.setProperty(Markup.CSS_KEY_LINEHEIGHT, paragraph.getTotalLeading() + "pt");
                
                addTabs(indent);
                writeStart(HtmlTags.DIV);
                writeMarkupAttributes(markup);
                String alignment = HtmlEncoder.getAlignment(paragraph.getAlignment());
                if (!"".equals(alignment)) {
                    write(HtmlTags.ALIGN, alignment);
                }
                write(paragraph.getFont(), styleAttributes);
                os.write(GT);
                currentfont.push(paragraph.getFont());
                
                for (Element e: paragraph) {
                    write(e, indent + 1);
                }
                
                addTabs(indent);
                writeEnd(HtmlTags.DIV);
                currentfont.pop();
                return;
            }
            case Element.SECTION:
            case Element.CHAPTER:
            {
                
                writeSection((Section) element, indent);
                return;
            }
            case Element.LIST:
            {
                List list = (List) element;
                
                addTabs(indent);
                if (list.isNumbered()) {
                    writeStart(HtmlTags.ORDEREDLIST);
                }
                else {
                    writeStart(HtmlTags.UNORDEREDLIST);
                }
                writeMarkupAttributes(markup);
                os.write(GT);
                
                for (Element e: list.getItems()) {
                    write(e, indent + 1);
                }
                
                addTabs(indent);
                if (list.isNumbered()) {
                    writeEnd(HtmlTags.ORDEREDLIST);
                }
                else {
                    writeEnd(HtmlTags.UNORDEREDLIST);
                }
                return;
            }
            case Element.LISTITEM:
            {
                ListItem listItem = (ListItem) element;
                styleAttributes = new Properties();
                if (listItem.hasLeading()) styleAttributes.setProperty(Markup.CSS_KEY_LINEHEIGHT, listItem.getTotalLeading() + "pt");
                
                
                addTabs(indent);
                writeStart(HtmlTags.LISTITEM);
                writeMarkupAttributes(markup);
                write(listItem.getFont(), styleAttributes);
                os.write(GT);
                currentfont.push(listItem.getFont());
                
                for (Element e: listItem) {
                    write(e, indent + 1);
                }
                
                addTabs(indent);
                writeEnd(HtmlTags.LISTITEM);
                currentfont.pop();
                return;
            }
            case Element.CELL:
            {
                Cell cell = (Cell) element;
                
                
                addTabs(indent);
                if (cell.isHeader()) {
                    writeStart(HtmlTags.HEADERCELL);
                }
                else {
                    writeStart(HtmlTags.CELL);
                }
                writeMarkupAttributes(markup);
                if (cell.getBorderWidth() != Rectangle.UNDEFINED) {
                    write(HtmlTags.BORDERWIDTH, String.valueOf(cell.getBorderWidth()));
                }
                if (cell.getBorderColor() != null) {
                    write(HtmlTags.BORDERCOLOR, HtmlEncoder.encode(cell.getBorderColor()));
                }
                if (cell.getBackgroundColor() != null) {
                    write(HtmlTags.BACKGROUNDCOLOR, HtmlEncoder.encode(cell.getBackgroundColor()));
                }
                String alignment = HtmlEncoder.getAlignment(cell.getHorizontalAlignment());
                if (!"".equals(alignment)) {
                    write(HtmlTags.HORIZONTALALIGN, alignment);
                }
                alignment = HtmlEncoder.getAlignment(cell.getVerticalAlignment());
                if (!"".equals(alignment)) {
                    write(HtmlTags.VERTICALALIGN, alignment);
                }
                if (cell.getWidthAsString() != null) {
                    write(HtmlTags.WIDTH, cell.getWidthAsString());
                }
                if (cell.getColspan() != 1) {
                    write(HtmlTags.COLSPAN, String.valueOf(cell.getColspan()));
                }
                if (cell.getRowspan() != 1) {
                    write(HtmlTags.ROWSPAN, String.valueOf(cell.getRowspan()));
                }
                if (cell.getMaxLines() == 1) {
                    write(HtmlTags.NOWRAP, String.valueOf(true));
                }
                os.write(GT);
                
                if (cell.isEmpty()) {
                    write(NBSP);
                } else {
                    for (Iterator<Element> i = cell.getElements(); i.hasNext(); ) {
                        write(i.next(), indent + 1);
                    }
                }
                
                addTabs(indent);
                if (cell.isHeader()) {
                    writeEnd(HtmlTags.HEADERCELL);
                }
                else {
                    writeEnd(HtmlTags.CELL);
                }
                return;
            }
            case Element.ROW:
            {
                Row row = (Row) element;
                
                
                addTabs(indent);
                writeStart(HtmlTags.ROW);
                writeMarkupAttributes(markup);
                os.write(GT);
                
                Element cell;
                for (int i = 0; i < row.getColumns(); i++) {
                    if ((cell = (Element)row.getCell(i)) != null) {
                        write(cell, indent + 1);
                    }
                }
                
                addTabs(indent);
                writeEnd(HtmlTags.ROW);
                return;
            }
            case Element.TABLE:
            {
                Table table;
                try {
                    table = (Table) element;
                }
                catch(ClassCastException cce) {
                    try {
                        table = ((SimpleTable)element).createTable();
                    } catch (BadElementException e) {
                        throw new ExceptionConverter(e);
                    }
                }
                table.complete();
                
                addTabs(indent);
                writeStart(HtmlTags.TABLE);
                writeMarkupAttributes(markup);
                os.write(SPACE);
                write(HtmlTags.WIDTH);
                os.write(EQUALS);
                os.write(QUOTE);
                write(String.valueOf(table.getWidth()));
                if (!table.isLocked()){
                    write("%");
                }
                os.write(QUOTE);
                String alignment = HtmlEncoder.getAlignment(table.getAlignment());
                if (!"".equals(alignment)) {
                    write(HtmlTags.ALIGN, alignment);
                }
                write(HtmlTags.CELLPADDING, String.valueOf(table.getPadding()));
                write(HtmlTags.CELLSPACING, String.valueOf(table.getSpacing()));
                if (table.getBorderWidth() != Rectangle.UNDEFINED) {
                    write(HtmlTags.BORDERWIDTH, String.valueOf(table.getBorderWidth()));
                }
                if (table.getBorderColor() != null) {
                    write(HtmlTags.BORDERCOLOR, HtmlEncoder.encode(table.getBorderColor()));
                }
                if (table.getBackgroundColor() != null) {
                    write(HtmlTags.BACKGROUNDCOLOR, HtmlEncoder.encode(table.getBackgroundColor()));
                }
                os.write(GT);
                
                Row row;
                for (Iterator<Row> iterator = table.iterator(); iterator.hasNext(); ) {
                    row = iterator.next();
                    write(row, indent + 1);
                }
                
                addTabs(indent);
                writeEnd(HtmlTags.TABLE);
                return;
            }
            case Element.ANNOTATION:
            {
                Annotation annotation = (Annotation) element;
                writeComment(annotation.title() + ": " + annotation.content());
                return;
            }
            case Element.IMGRAW:
            case Element.JPEG:
            case Element.JPEG2000:
            case Element.IMGTEMPLATE:
            {
                Image image = (Image) element;
                if (image.getUrl() == null) {
                    return;
                }
                
                
                addTabs(indent);
                writeStart(HtmlTags.IMAGE);
                String path = image.getUrl().toString();
                if (imagepath != null) {
                    if (path.indexOf('/') > 0) {
                        path = imagepath + path.substring(path.lastIndexOf('/') + 1);
                    }
                    else {
                        path = imagepath + path;
                    }
                }
                write(HtmlTags.URL, path);
                if ((image.getAlignment() & Image.RIGHT) > 0) {
                    write(HtmlTags.ALIGN, HtmlTags.ALIGN_RIGHT);
                }
                else if ((image.getAlignment() & Image.MIDDLE) > 0) {
                    write(HtmlTags.ALIGN, HtmlTags.ALIGN_MIDDLE);
                }
                else {
                    write(HtmlTags.ALIGN, HtmlTags.ALIGN_LEFT);
                }
                if (image.getAlt() != null) {
                    write(HtmlTags.ALT, image.getAlt());
                }
                write(HtmlTags.PLAINWIDTH, String.valueOf(image.getScaledWidth()));
                write(HtmlTags.PLAINHEIGHT, String.valueOf(image.getScaledHeight()));
                writeMarkupAttributes(markup);
                writeEnd();
                return;
            }
            
            default:
                return;
        }
    }
    

    
    protected void writeSection(Section section, int indent) throws IOException {
        if (section.getTitle() != null) {
            int depth = section.getDepth() - 1;
            if (depth > 5) {
                depth = 5;
            }
            Properties styleAttributes = new Properties();
            if (section.getTitle().hasLeading()) styleAttributes.setProperty(Markup.CSS_KEY_LINEHEIGHT, section.getTitle().getTotalLeading() + "pt");
            
            addTabs(indent);
            writeStart(HtmlTags.H[depth]);
            write(section.getTitle().getFont(), styleAttributes);
            String alignment = HtmlEncoder.getAlignment(section.getTitle().getAlignment());
            if (!"".equals(alignment)) {
                write(HtmlTags.ALIGN, alignment);
            }
            writeMarkupAttributes(markup);
            os.write(GT);
            currentfont.push(section.getTitle().getFont());
            
            for (Element e: section.getTitle()) {
                write(e, indent + 1);
            }
            
            addTabs(indent);
            writeEnd(HtmlTags.H[depth]);
            currentfont.pop();
        }
        for (Element e: section) {
            write(e, indent);
        }
    }
    
    
    
    protected void write(Font font, Properties styleAttributes) throws IOException {
        if (font == null || !isOtherFont(font) ) return;
        write(" ");
        write(HtmlTags.STYLE);
        write("=\"");
        if (styleAttributes != null) {
            String key;
            for (Enumeration<?> e = styleAttributes.propertyNames(); e.hasMoreElements(); ) {
                key = (String)e.nextElement();
                writeCssProperty(key, styleAttributes.getProperty(key));
            }
        }
        if (isOtherFont(font)) {
            writeCssProperty(Markup.CSS_KEY_FONTFAMILY, font.getFamilyname());
            
            if (font.getSize() != Font.UNDEFINED) {
                writeCssProperty(Markup.CSS_KEY_FONTSIZE, font.getSize() + "pt");
            }
            if (font.getColor() != null) {
                writeCssProperty(Markup.CSS_KEY_COLOR, HtmlEncoder.encode(font.getColor()));
            }
            
            int fontstyle = font.getStyle();
            BaseFont bf = font.getBaseFont();
            if (bf != null) {
                String ps = bf.getPostscriptFontName().toLowerCase();
                if (ps.indexOf("bold") >= 0) {
                    if (fontstyle == Font.UNDEFINED)
                        fontstyle = 0;
                    fontstyle |= Font.BOLD;
                }
                if (ps.indexOf("italic") >= 0 || ps.indexOf("oblique") >= 0) {
                    if (fontstyle == Font.UNDEFINED)
                        fontstyle = 0;
                    fontstyle |= Font.ITALIC;
                }
            }
            if (fontstyle != Font.UNDEFINED && fontstyle != Font.NORMAL) {
                switch (fontstyle & Font.BOLDITALIC) {
                    case Font.BOLD:
                        writeCssProperty(Markup.CSS_KEY_FONTWEIGHT, Markup.CSS_VALUE_BOLD);
                        break;
                    case Font.ITALIC:
                        writeCssProperty(Markup.CSS_KEY_FONTSTYLE, Markup.CSS_VALUE_ITALIC);
                        break;
                    case Font.BOLDITALIC:
                        writeCssProperty(Markup.CSS_KEY_FONTWEIGHT, Markup.CSS_VALUE_BOLD);
                        writeCssProperty(Markup.CSS_KEY_FONTSTYLE, Markup.CSS_VALUE_ITALIC);
                        break;
                }
                
                
                
                if ((fontstyle & Font.UNDERLINE) > 0) {
                    writeCssProperty(Markup.CSS_KEY_TEXTDECORATION, Markup.CSS_VALUE_UNDERLINE);
                }
                if ((fontstyle & Font.STRIKETHRU) > 0) {
                    writeCssProperty(Markup.CSS_KEY_TEXTDECORATION, Markup.CSS_VALUE_LINETHROUGH);
                }
            }
        }
        write("\"");
    }
    
    
    protected void writeCssProperty(String prop, String value) throws IOException {
        write(new StringBuffer(prop).append(": ").append(value).append("; ").toString());
    }
}