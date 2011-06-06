

package com.lowagie.text.xml;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import com.lowagie.text.Anchor;
import com.lowagie.text.Annotation;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Cell;
import com.lowagie.text.Chapter;
import com.lowagie.text.Chunk;
import com.lowagie.text.DocListener;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ElementTags;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.Meta;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Section;
import com.lowagie.text.Table;
import com.lowagie.text.TextElementArray;
import com.lowagie.text.factories.ElementFactory;
import com.lowagie.text.html.HtmlTagMap;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.draw.LineSeparator;
import com.lowagie.text.xml.simpleparser.EntitiesToSymbol;



public class SAXiTextHandler extends DefaultHandler {

    
    protected DocListener document;

    
    protected Stack stack;

    
    protected int chapters = 0;

    
    protected Chunk currentChunk = null;

    
    protected boolean ignore = false;

    
    protected boolean controlOpenClose = true;

    
    float topMargin = 36;

    
    float rightMargin = 36;

    
    float leftMargin = 36;

    
    float bottomMargin = 36;

    
    public SAXiTextHandler(DocListener document) {
        super();
        this.document = document;
        stack = new Stack();
    }

    
    protected HashMap myTags;

    
    public SAXiTextHandler(DocListener document, HtmlTagMap myTags) {
        this(document);
        this.myTags = myTags;
    }

    
    public SAXiTextHandler(DocListener document, HtmlTagMap myTags,
            BaseFont bf){
        this(document, myTags);
        this.bf = bf;
    }

    
    public SAXiTextHandler(DocListener document, HashMap myTags) {
        this(document);
        this.myTags = myTags;
    }

    

    public void setControlOpenClose(boolean controlOpenClose) {
        this.controlOpenClose = controlOpenClose;
    }

    

    public void startElement(String uri, String lname, String name,
            Attributes attrs) {

        Properties attributes = new Properties();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                String attribute = attrs.getQName(i);
                attributes.setProperty(attribute, attrs.getValue(i));
            }
        }
        handleStartingTags(name, attributes);
    }

    

    public void handleStartingTags(String name, Properties attributes) {
        
        if (ignore || ElementTags.IGNORE.equals(name)) {
            ignore = true;
            return;
        }

        
        if (currentChunk != null) {
            TextElementArray current;
            try {
                current = (TextElementArray) stack.pop();
            } catch (EmptyStackException ese) {
                if (bf == null) {
                    current = new Paragraph("", new Font());
                }
                else {
                    current = new Paragraph("", new Font(this.bf));
                }
            }
            current.add(currentChunk);
            stack.push(current);
            currentChunk = null;
        }

        
        if (ElementTags.CHUNK.equals(name)) {
            currentChunk = ElementFactory.getChunk(attributes);
            if (bf != null) {
                currentChunk.setFont(new Font(this.bf));
            }
            return;
        }

        
        if (ElementTags.ENTITY.equals(name)) {
            Font f = new Font();
            if (currentChunk != null) {
                handleEndingTags(ElementTags.CHUNK);
                f = currentChunk.getFont();
            }
            currentChunk = EntitiesToSymbol.get(attributes.getProperty(ElementTags.ID),
                    f);
            return;
        }

        
        if (ElementTags.PHRASE.equals(name)) {
            stack.push(ElementFactory.getPhrase(attributes));
            return;
        }

        
        if (ElementTags.ANCHOR.equals(name)) {
            stack.push(ElementFactory.getAnchor(attributes));
            return;
        }

        
        if (ElementTags.PARAGRAPH.equals(name) || ElementTags.TITLE.equals(name)) {
            stack.push(ElementFactory.getParagraph(attributes));
            return;
        }

        
        if (ElementTags.LIST.equals(name)) {
            stack.push(ElementFactory.getList(attributes));
            return;
        }
        
        
        if (ElementTags.LISTITEM.equals(name)) {
            stack.push(ElementFactory.getListItem(attributes));
            return;
        }

        
        if (ElementTags.CELL.equals(name)) {
            stack.push(ElementFactory.getCell(attributes));
            return;
        }

        
        if (ElementTags.TABLE.equals(name)) {
            Table table = ElementFactory.getTable(attributes);
            float widths[] = table.getProportionalWidths();
            for (int i = 0; i < widths.length; i++) {
                if (widths[i] == 0) {
                    widths[i] = 100.0f / (float) widths.length;
                }
            }
            try {
                table.setWidths(widths);
            } catch (BadElementException bee) {
                
                throw new ExceptionConverter(bee);
            }
            stack.push(table);
            return;
        }

        
        if (ElementTags.SECTION.equals(name)) {
            Element previous = (Element) stack.pop();
            Section section;
            try {
                section = ElementFactory.getSection((Section) previous, attributes);
            } catch (ClassCastException cce) {
                throw new ExceptionConverter(cce);
            }
            stack.push(previous);
            stack.push(section);
            return;
        }

        
        if (ElementTags.CHAPTER.equals(name)) {
            stack.push(ElementFactory.getChapter(attributes));
            return;
        }

        
        if (ElementTags.IMAGE.equals(name)) {
            try {
                Image img = ElementFactory.getImage(attributes);
                try {
                    addImage(img);
                    return;
                } catch (EmptyStackException ese) {
                    
                    
                    try {
                        document.add(img);
                    } catch (DocumentException de) {
                        throw new ExceptionConverter(de);
                    }
                    return;
                }
            } catch (Exception e) {
                throw new ExceptionConverter(e);
            }
        }

        
        if (ElementTags.ANNOTATION.equals(name)) {
            Annotation annotation = ElementFactory.getAnnotation(attributes);
            TextElementArray current;
            try {
                try {
                    current = (TextElementArray) stack.pop();
                    try {
                        current.add(annotation);
                    } catch (Exception e) {
                        document.add(annotation);
                    }
                    stack.push(current);
                } catch (EmptyStackException ese) {
                    document.add(annotation);
                }
                return;
            } catch (DocumentException de) {
                throw new ExceptionConverter(de);
            }
        }

        
        if (isNewline(name)) {
            TextElementArray current;
            try {
                current = (TextElementArray) stack.pop();
                current.add(Chunk.NEWLINE);
                stack.push(current);
            } catch (EmptyStackException ese) {
                if (currentChunk == null) {
                    try {
                        document.add(Chunk.NEWLINE);
                    } catch (DocumentException de) {
                        throw new ExceptionConverter(de);
                    }
                } else {
                    currentChunk.append("\n");
                }
            }
            return;
        }

        
        if (isNewpage(name)) {
            TextElementArray current;
            try {
                current = (TextElementArray) stack.pop();
                Chunk newPage = new Chunk("");
                newPage.setNewPage();
                if (bf != null) {
                    newPage.setFont(new Font(this.bf));
                }
                current.add(newPage);
                stack.push(current);
            } catch (EmptyStackException ese) {
                document.newPage();
            }
            return;
        }

        if (ElementTags.HORIZONTALRULE.equals(name)) {
            TextElementArray current;
            LineSeparator hr = new LineSeparator(1.0f, 100.0f, null, Element.ALIGN_CENTER, 0);
            try {
                current = (TextElementArray) stack.pop();
                current.add(hr);
                stack.push(current);
            } catch (EmptyStackException ese) {
                try {
                    document.add(hr);
                } catch (DocumentException de) {
                    throw new ExceptionConverter(de);
                }
            }
            return;
        }
        
        
        if (isDocumentRoot(name)) {
            String key;
            String value;
            
            
            Rectangle pageSize = null;
            String orientation = null;
            for (Iterator i = attributes.keySet().iterator(); i.hasNext();) {
                key = (String) i.next();
                value = attributes.getProperty(key);
                try {
                    
                    if (ElementTags.LEFT.equalsIgnoreCase(key))
                        leftMargin = Float.parseFloat(value + "f");
                    if (ElementTags.RIGHT.equalsIgnoreCase(key))
                        rightMargin = Float.parseFloat(value + "f");
                    if (ElementTags.TOP.equalsIgnoreCase(key))
                        topMargin = Float.parseFloat(value + "f");
                    if (ElementTags.BOTTOM.equalsIgnoreCase(key))
                        bottomMargin = Float.parseFloat(value + "f");
                } catch (Exception ex) {
                    throw new ExceptionConverter(ex);
                }
                if (ElementTags.PAGE_SIZE.equals(key)) {
                    try {
                        String pageSizeName = value;
                        Field pageSizeField = PageSize.class
                                .getField(pageSizeName);
                        pageSize = (Rectangle) pageSizeField.get(null);
                    } catch (Exception ex) {
                        throw new ExceptionConverter(ex);
                    }
                } else if (ElementTags.ORIENTATION.equals(key)) {
                    try {
                        if ("landscape".equals(value)) {
                            orientation = "landscape";
                        }
                    } catch (Exception ex) {
                        throw new ExceptionConverter(ex);
                    }
                } else {
                    try {
                        document.add(new Meta(key, value));
                    } catch (DocumentException de) {
                        throw new ExceptionConverter(de);
                    }
                }
            }
            if(pageSize != null) {
                if ("landscape".equals(orientation)) {
                    pageSize = pageSize.rotate();
                }
                document.setPageSize(pageSize);
            }
            document.setMargins(leftMargin, rightMargin, topMargin,
                    bottomMargin);

            if (controlOpenClose)
                document.open();
        }

    }

    protected void addImage(Image img) throws EmptyStackException {
        
        Object current = stack.pop();
        
        
        if (current instanceof Chapter
                || current instanceof Section
                || current instanceof Cell) {
            ((TextElementArray) current).add(img);
            stack.push(current);
            return;
        }
        
        else {
            Stack newStack = new Stack();
            while (!(current instanceof Chapter
                    || current instanceof Section || current instanceof Cell)) {
                newStack.push(current);
                if (current instanceof Anchor) {
                    img.setAnnotation(new Annotation(0, 0, 0,
                            0, ((Anchor) current).getReference()));
                }
                current = stack.pop();
            }
            ((TextElementArray) current).add(img);
            stack.push(current);
            while (!newStack.empty()) {
                stack.push(newStack.pop());
            }
            return;
        }
    }
    
    

    public void ignorableWhitespace(char[] ch, int start, int length) {
        characters(ch, start, length);
    }

    

    public void characters(char[] ch, int start, int length) {

        if (ignore)
            return;

        String content = new String(ch, start, length);
        

        if (content.trim().length() == 0 && content.indexOf(' ') < 0) {
            return;
        }

        StringBuffer buf = new StringBuffer();
        int len = content.length();
        char character;
        boolean newline = false;
        for (int i = 0; i < len; i++) {
            switch (character = content.charAt(i)) {
            case ' ':
                if (!newline) {
                    buf.append(character);
                }
                break;
            case '\n':
                if (i > 0) {
                    newline = true;
                    buf.append(' ');
                }
                break;
            case '\r':
                break;
            case '\t':
                break;
            default:
                newline = false;
                buf.append(character);
            }
        }
        if (currentChunk == null) {
            if (bf == null) {
                currentChunk = new Chunk(buf.toString());
            }
            else {
                currentChunk = new Chunk(buf.toString(), new Font(this.bf));
            }
        } else {
            currentChunk.append(buf.toString());
        }
    }

    private BaseFont bf = null;
    
    
    public void setBaseFont(BaseFont bf) {
        this.bf = bf;
    }

    

    public void endElement(String uri, String lname, String name) {
        handleEndingTags(name);
    }

    

    public void handleEndingTags(String name) {

        

        if (ElementTags.IGNORE.equals(name)) {
            ignore = false;
            return;
        }
        if (ignore)
            return;
        
        if (isNewpage(name) || ElementTags.ANNOTATION.equals(name) || ElementTags.IMAGE.equals(name)
                || isNewline(name)) {
            return;
        }

        try {
            
            if (ElementTags.TITLE.equals(name)) {
                Paragraph current = (Paragraph) stack.pop();
                if (currentChunk != null) {
                    current.add(currentChunk);
                    currentChunk = null;
                }
                Section previous = (Section) stack.pop();
                previous.setTitle(current);
                stack.push(previous);
                return;
            }

            
            if (currentChunk != null) {
                TextElementArray current;
                try {
                    current = (TextElementArray) stack.pop();
                } catch (EmptyStackException ese) {
                    current = new Paragraph();
                }
                current.add(currentChunk);
                stack.push(current);
                currentChunk = null;
            }

            
            if (ElementTags.CHUNK.equals(name)) {
                return;
            }

            
            if (ElementTags.PHRASE.equals(name) || ElementTags.ANCHOR.equals(name) || ElementTags.LIST.equals(name)
                    || ElementTags.PARAGRAPH.equals(name)) {
                Element current = (Element) stack.pop();
                try {
                    TextElementArray previous = (TextElementArray) stack.pop();
                    previous.add(current);
                    stack.push(previous);
                } catch (EmptyStackException ese) {
                    document.add(current);
                }
                return;
            }

            
            if (ElementTags.LISTITEM.equals(name)) {
                ListItem listItem = (ListItem) stack.pop();
                List list = (List) stack.pop();
                list.add(listItem);
                stack.push(list);
            }

            
            if (ElementTags.TABLE.equals(name)) {
                Table table = (Table) stack.pop();
                try {
                    TextElementArray previous = (TextElementArray) stack.pop();
                    previous.add(table);
                    stack.push(previous);
                } catch (EmptyStackException ese) {
                    document.add(table);
                }
                return;
            }

            
            if (ElementTags.ROW.equals(name)) {
                ArrayList cells = new ArrayList();
                int columns = 0;
                Table table;
                Cell cell;
                while (true) {
                    Element element = (Element) stack.pop();
                    if (element.type() == Element.CELL) {
                        cell = (Cell) element;
                        columns += cell.getColspan();
                        cells.add(cell);
                    } else {
                        table = (Table) element;
                        break;
                    }
                }
                if (table.getColumns() < columns) {
                    table.addColumns(columns - table.getColumns());
                }
                Collections.reverse(cells);
                String width;
                float[] cellWidths = new float[columns];
                boolean[] cellNulls = new boolean[columns];
                for (int i = 0; i < columns; i++) {
                    cellWidths[i] = 0;
                    cellNulls[i] = true;
                }
                float total = 0;
                int j = 0;
                for (Iterator i = cells.iterator(); i.hasNext();) {
                    cell = (Cell) i.next();
                    width = cell.getWidthAsString();
                    if (cell.getWidth() == 0) {
                        if (cell.getColspan() == 1 && cellWidths[j] == 0) {
                            try {
                                cellWidths[j] = 100f / columns;
                                total += cellWidths[j];
                            } catch (Exception e) {
                                
                            }
                        } else if (cell.getColspan() == 1) {
                            cellNulls[j] = false;
                        }
                    } else if (cell.getColspan() == 1 && width.endsWith("%")) {
                        try {
                            cellWidths[j] = Float.parseFloat(
                                    width.substring(0, width.length() - 1)
                                            + "f");
                            total += cellWidths[j];
                        } catch (Exception e) {
                            
                        }
                    }
                    j += cell.getColspan();
                    table.addCell(cell);
                }
                float widths[] = table.getProportionalWidths();
                if (widths.length == columns) {
                    float left = 0.0f;
                    for (int i = 0; i < columns; i++) {
                        if (cellNulls[i] && widths[i] != 0) {
                            left += widths[i];
                            cellWidths[i] = widths[i];
                        }
                    }
                    if (100.0 >= total) {
                        for (int i = 0; i < widths.length; i++) {
                            if (cellWidths[i] == 0 && widths[i] != 0) {
                                cellWidths[i] = (widths[i] / left)
                                        * (100.0f - total);
                            }
                        }
                    }
                    table.setWidths(cellWidths);
                }
                stack.push(table);
            }

            
            if (ElementTags.CELL.equals(name)) {
                return;
            }

            
            if (ElementTags.SECTION.equals(name)) {
                stack.pop();
                return;
            }

            
            if (ElementTags.CHAPTER.equals(name)) {
                document.add((Element) stack.pop());
                return;
            }

            
            if (isDocumentRoot(name)) {
                try {
                    while (true) {
                        Element element = (Element) stack.pop();
                        try {
                            TextElementArray previous = (TextElementArray) stack
                                    .pop();
                            previous.add(element);
                            stack.push(previous);
                        } catch (EmptyStackException es) {
                            document.add(element);
                        }
                    }
                } catch (EmptyStackException ese) {
                    
                }
                if (controlOpenClose)
                    document.close();
                return;
            }
        } catch (DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }

    

    private boolean isNewpage(String tag) {
        return ElementTags.NEWPAGE.equals(tag);
    }

    

    private boolean isNewline(String tag) {
        return ElementTags.NEWLINE.equals(tag);
    }

    

    protected boolean isDocumentRoot(String tag) {
        return ElementTags.ITEXT.equals(tag);
    }
}