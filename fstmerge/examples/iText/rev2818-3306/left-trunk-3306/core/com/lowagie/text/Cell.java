

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfPCell;



public class Cell extends Rectangle implements TextElementArray {

    

    
    protected ArrayList arrayList = null;

    
    protected int horizontalAlignment = Element.ALIGN_UNDEFINED;

    
    protected int verticalAlignment = Element.ALIGN_UNDEFINED;

    
    protected float width;
    protected boolean percentage = false;

    
    protected int colspan = 1;

    
    protected int rowspan = 1;

    
    float leading = Float.NaN;

    
    protected boolean header;

    
    protected int maxLines = Integer.MAX_VALUE;
    
    
    String showTruncation;

    
    protected boolean useAscender = false;

    
    protected boolean useDescender = false;

    
    protected boolean useBorderPadding;
    
    
    protected boolean groupChange = true;

    

    
    public Cell() {
        
        super(0, 0, 0, 0);
        setBorder(UNDEFINED);
        setBorderWidth(0.5f);
        
        arrayList = new ArrayList();
    }

    
    public Cell(boolean dummy) {
        this();
        arrayList.add(new Paragraph(0));
    }

    
    public Cell(String content) {
        this();
        try {
            addElement(new Paragraph(content));
        }
        catch(BadElementException bee) {
        }
    }

    
    public Cell(Element element) throws BadElementException {
        this();
         if(element instanceof Phrase) {
            setLeading(((Phrase)element).getLeading());
        }
        addElement(element);
    }

    

    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        }
        catch(DocumentException de) {
            return false;
        }
    }

    
    public int type() {
        return Element.CELL;
    }

    
    public ArrayList getChunks() {
        ArrayList tmp = new ArrayList();
        for (Iterator i = arrayList.iterator(); i.hasNext(); ) {
            tmp.addAll(((Element) i.next()).getChunks());
        }
        return tmp;
    }

    

    
       public int getHorizontalAlignment() {
           return horizontalAlignment;
       }

    
    public void setHorizontalAlignment(int value) {
        horizontalAlignment = value;
    }

    
    public void setHorizontalAlignment(String alignment) {
        setHorizontalAlignment(ElementTags.alignmentValue(alignment));
    }

    
    public int getVerticalAlignment() {
        return verticalAlignment;
    }

    
    public void setVerticalAlignment(int value) {
        verticalAlignment = value;
    }

    
    public void setVerticalAlignment(String alignment) {
        setVerticalAlignment(ElementTags.alignmentValue(alignment));
    }

    
    public void setWidth(float value) {
        this.width = value;
    }
    
    
    public void setWidth(String value) {
        if (value.endsWith("%")) {
            value = value.substring(0, value.length() - 1);
            percentage = true;
        }
        width = Integer.parseInt(value);
    }
    
    
    public float getWidth() {
        return width;
    }

    
    public String getWidthAsString() {
        String w = String.valueOf(width);
        if (w.endsWith(".0")) w = w.substring(0, w.length() - 2);
        if (percentage) w += "%";
        return w;
    }

    
    public void setColspan(int value) {
        colspan = value;
    }

    
    public int getColspan() {
        return colspan;
    }

    
    public void setRowspan(int value) {
        rowspan = value;
    }

    
    public int getRowspan() {
        return rowspan;
    }

    
    public void setLeading(float value) {
        leading = value;
    }

    
    public float getLeading() {
        if (Float.isNaN(leading)) {
            return 16;
        }
        return leading;
    }

    
    public void setHeader(boolean value) {
        header = value;
    }

    
    public boolean isHeader() {
        return header;
    }
    
    
    public void setMaxLines(int value) {
        maxLines = value;
    }
    
    
    public int getMaxLines() {
        return maxLines;
    }
        
    
    public void setShowTruncation(String value) {
        showTruncation = value;
    }
    
    
    public String getShowTruncation() {
        return showTruncation;
    }

    
    public void setUseAscender(boolean use) {
        useAscender = use;
    }

    
    public boolean isUseAscender() {
        return useAscender;
    }

    
    public void setUseDescender(boolean use) {
        useDescender = use;
    }

    
    public boolean isUseDescender() {
        return useDescender;
    }

    
    public void setUseBorderPadding(boolean use) {
        useBorderPadding = use;
    }

    
    public boolean isUseBorderPadding() {
        return useBorderPadding;
    }

    
    public boolean getGroupChange() {
        return groupChange;
    }

    
    public void setGroupChange(boolean value) {
        groupChange = value;
    }
    


    
    public int size() {
        return arrayList.size();
    }

    
    public Iterator getElements() {
        return arrayList.iterator();
    }
    
    
    public void clear() {
        arrayList.clear();
    }

    
    public boolean isEmpty() {
        switch(size()) {
            case 0:
                return true;
            case 1:
                Element element = (Element) arrayList.get(0);
                switch (element.type()) {
                    case Element.CHUNK:
                        return ((Chunk) element).isEmpty();
                    case Element.ANCHOR:
                    case Element.PHRASE:
                    case Element.PARAGRAPH:
                        return ((Phrase) element).isEmpty();
                    case Element.LIST:
                        return ((List) element).isEmpty();
                }
            return false;
            default:
                return false;
        }
    }
    
    
    void fill() {
        if (size() == 0) arrayList.add(new Paragraph(0));
    }

    
    public boolean isTable() {
        return (size() == 1)
            && (((Element)arrayList.get(0)).type() == Element.TABLE);
    }
    
    
    public void addElement(Element element) throws BadElementException {
        if (isTable()) {
            Table table = (Table) arrayList.get(0);
            Cell tmp = new Cell(element);
            tmp.setBorder(NO_BORDER);
            tmp.setColspan(table.getColumns());
            table.addCell(tmp);
            return;
        }
        switch(element.type()) {
            case Element.LISTITEM:
            case Element.ROW:
            case Element.CELL:
                throw new BadElementException("You can't add listitems, rows or cells to a cell.");
            case Element.LIST:
                List list = (List)element;
                if (Float.isNaN(leading)) {
                    setLeading(list.getTotalLeading());
                }
                if (list.isEmpty()) return;
                arrayList.add(element);
                return;
            case Element.ANCHOR:
            case Element.PARAGRAPH:
            case Element.PHRASE:
                Phrase p = (Phrase)element;
                if (Float.isNaN(leading)) {
                    setLeading(p.getLeading());
                }
                if (p.isEmpty()) return;
                arrayList.add(element);
                return;
            case Element.CHUNK:
                if (((Chunk) element).isEmpty()) return;
                arrayList.add(element);
                return;
            case Element.TABLE:
                Table table = new Table(3);
                float[] widths = new float[3];
                widths[1] = ((Table)element).getWidth();
                switch(((Table)element).getAlignment()) {
                    case Element.ALIGN_LEFT:
                        widths[0] = 0f;
                        widths[2] = 100f - widths[1];
                        break;
                    case Element.ALIGN_CENTER:
                        widths[0] = (100f - widths[1]) / 2f;
                        widths[2] = widths[0];
                        break;
                    case Element.ALIGN_RIGHT:
                        widths[0] = 100f - widths[1];
                        widths[2] = 0f;
                }
                table.setWidths(widths);
                Cell tmp;
                if (arrayList.isEmpty()) {
                    table.addCell(getDummyCell());
                }
                else {
                    tmp = new Cell();
                    tmp.setBorder(NO_BORDER);
                    tmp.setColspan(3);
                    for (Iterator i = arrayList.iterator(); i.hasNext(); ) {
                        tmp.add(i.next());
                    }
                    table.addCell(tmp);
                }
                tmp = new Cell();
                tmp.setBorder(NO_BORDER);
                table.addCell(tmp);
                table.insertTable((Table)element);
                tmp = new Cell();
                tmp.setBorder(NO_BORDER);
                table.addCell(tmp);
                table.addCell(getDummyCell());
                clear();
                arrayList.add(table);
                return;
            default:
                arrayList.add(element);
        }
    }

    
    public boolean add(Object o) {
        try {
            this.addElement((Element) o);
            return true;
        }
        catch(ClassCastException cce) {
            throw new ClassCastException("You can only add objects that implement the Element interface.");
        }
        catch(BadElementException bee) {
            throw new ClassCastException(bee.getMessage());
        }
    }

    
    
    
    private static Cell getDummyCell() {
        Cell cell = new Cell(true);
        cell.setColspan(3);
        cell.setBorder(NO_BORDER);
        return cell;
    }

    
    public PdfPCell createPdfPCell() throws BadElementException {
        if (rowspan > 1) throw new BadElementException("PdfPCells can't have a rowspan > 1");
        if (isTable()) return new PdfPCell(((Table)arrayList.get(0)).createPdfPTable());
        PdfPCell cell = new PdfPCell();
        cell.setVerticalAlignment(verticalAlignment);
        cell.setHorizontalAlignment(horizontalAlignment);
        cell.setColspan(colspan);
        cell.setUseBorderPadding(useBorderPadding);
        cell.setUseDescender(useDescender);
        cell.setLeading(getLeading(), 0);
        cell.cloneNonPositionParameters(this);
        cell.setNoWrap(getMaxLines() == 1);
        for (Iterator i = getElements(); i.hasNext(); ) {
            Element e = (Element)i.next();
            if (e.type() == Element.PHRASE || e.type() == Element.PARAGRAPH) {
                Paragraph p = new Paragraph((Phrase)e);
                p.setAlignment(horizontalAlignment);
                e = p;
            }
            cell.addElement(e);
        }
        return cell;
    }

    

    
    public float getTop() {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float getBottom() {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float getLeft() {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float getRight() {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float top(int margin) {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float bottom(int margin) {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float left(int margin) {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public float right(int margin) {
        throw new UnsupportedOperationException("Dimensions of a Cell can't be calculated. See the FAQ.");
    }

    
    public void setTop(int value) {
        throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
    }

    
    public void setBottom(int value) {
        throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
    }

    
    public void setLeft(int value) {
        throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
    }

    
    public void setRight(int value) {
        throw new UnsupportedOperationException("Dimensions of a Cell are attributed automagically. See the FAQ.");
    }

}
