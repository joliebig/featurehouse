

package com.lowagie.text.rtf.table;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Cell;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfColor;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.text.RtfParagraph;



public class RtfCell extends Cell implements RtfExtendedElement {

    
    private static final int MERGE_NONE = 0;
    
    private static final int MERGE_VERT_PARENT = 1;
    
    private static final int MERGE_VERT_CHILD = 2;

    
    private RtfRow parentRow = null;
    
    private ArrayList content = null;
    
    private int cellRight = 0;
    
    private int cellWidth = 0;
    
    private RtfBorderGroup borders = null;
    
    
    private RtfColor backgroundColor = null;
    
    private int cellPadding = 0;
    
    private int mergeType = MERGE_NONE;
    
    private RtfDocument document = null;
    
    private boolean inHeader = false;
    
    private boolean deleted = false;

    
    private boolean usePadding = false;
    
    private float cellPaddingLeft = 0;
    
    private float cellPaddingTop = 0;
    
    private float cellPaddingBottom = 0;
    
    private float cellPaddingRight = 0;

    
    public RtfCell() {
        super();
        this.borders = new RtfBorderGroup();
        verticalAlignment = ALIGN_MIDDLE;
    }
    
    
    public RtfCell(String content) {
        super(content);
        this.borders = new RtfBorderGroup();
        verticalAlignment = ALIGN_MIDDLE;
    }
    
    
    public RtfCell(Element element) throws BadElementException {
        super(element);
        this.borders = new RtfBorderGroup();
        verticalAlignment = ALIGN_MIDDLE;
    }
    
    
    protected RtfCell(boolean deleted) {
        super();
        this.deleted = deleted;
        verticalAlignment = ALIGN_MIDDLE;
    }
    
    
    protected RtfCell(RtfDocument doc, RtfRow row, Cell cell) {
        this.document = doc;
        this.parentRow = row;
        importCell(cell);
    }
    
    
    protected RtfCell(RtfDocument doc, RtfRow row, PdfPCell cell) {
        this.document = doc;
        this.parentRow = row;
        importCell(cell);
    }
    
    private void importCell(Cell cell) {
        this.content = new ArrayList();
        
        if(cell == null) {
            this.borders = new RtfBorderGroup(this.document, RtfBorder.CELL_BORDER, this.parentRow.getParentTable().getBorders());
            return;
        }
        
        this.colspan = cell.getColspan();
        this.rowspan = cell.getRowspan();
        if(cell.getRowspan() > 1) {
            this.mergeType = MERGE_VERT_PARENT;
        }
        if(cell instanceof RtfCell) {
            this.borders = new RtfBorderGroup(this.document, RtfBorder.CELL_BORDER, ((RtfCell) cell).getBorders());
        } else {
            this.borders = new RtfBorderGroup(this.document, RtfBorder.CELL_BORDER, cell.getBorder(), cell.getBorderWidth(), cell.getBorderColor());
        }
        this.verticalAlignment = cell.getVerticalAlignment();
        if(cell.getBackgroundColor() == null) {
            this.backgroundColor = new RtfColor(this.document, 255, 255, 255);
        } else {
            this.backgroundColor = new RtfColor(this.document, cell.getBackgroundColor());
        }
        
        this.cellPadding = (int) this.parentRow.getParentTable().getCellPadding();
        
        Iterator cellIterator = cell.getElements();
        Paragraph container = null;
        while(cellIterator.hasNext()) {
            try {
                Element element = (Element) cellIterator.next();
                
                if(!(element instanceof Paragraph) && !(element instanceof List)) {
                    if(container != null) {
                        container.add(element);
                    } else {
                        container = new Paragraph();
                        container.setAlignment(cell.getHorizontalAlignment());
                        container.add(element);
                    }
                } else {
                    if(container != null) {
                        RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(container);
                        for(int i = 0; i < rtfElements.length; i++) {
                            rtfElements[i].setInTable(true);
                            this.content.add(rtfElements[i]);
                        }
                        container = null;
                    }
                    
                    
                    if (element instanceof Paragraph && ((Paragraph) element).getAlignment() == Element.ALIGN_UNDEFINED) {
                        ((Paragraph) element).setAlignment(cell.getHorizontalAlignment());
                    }

                    RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(element);
                    for(int i = 0; i < rtfElements.length; i++) {
                        rtfElements[i].setInTable(true);
                        this.content.add(rtfElements[i]);
                    }
                }
            } catch(DocumentException de) {
                de.printStackTrace();
            }
        }
        if(container != null) {
            try {
                RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(container);
                for(int i = 0; i < rtfElements.length; i++) {
                    rtfElements[i].setInTable(true);
                    this.content.add(rtfElements[i]);
                }
            } catch(DocumentException de) {
                de.printStackTrace();
            }
        }
    }
    
    private void importCell(PdfPCell cell) {
        this.content = new ArrayList();
        
        if(cell == null) {
            this.borders = new RtfBorderGroup(this.document, RtfBorder.CELL_BORDER, this.parentRow.getParentTable().getBorders());
            return;
        }
        
        this.colspan = cell.getColspan();
        this.rowspan = 1; 




        this.borders = new RtfBorderGroup(this.document, RtfBorder.CELL_BORDER, cell.getBorder(), cell.getBorderWidth(), cell.getBorderColor());
        
        this.verticalAlignment = cell.getVerticalAlignment();
        if(cell.getBackgroundColor() == null) {
            this.backgroundColor = new RtfColor(this.document, 255, 255, 255);
        } else {
            this.backgroundColor = new RtfColor(this.document, cell.getBackgroundColor());
        }
        
        this.cellPadding = (int) this.parentRow.getParentTable().getCellPadding();
        
        
        java.util.List compositeElements = cell.getCompositeElements();
        if(compositeElements != null) {
            Iterator cellIterator = compositeElements.iterator();
            
            Paragraph container = null;
            while(cellIterator.hasNext()) {
                try {
                    Element element = (Element) cellIterator.next();
                    
                    if(!(element instanceof Paragraph) && !(element instanceof List)) {
                        if(container != null) {
                            container.add(element);
                        } else {
                            container = new Paragraph();
                            container.setAlignment(cell.getHorizontalAlignment());
                            container.add(element);
                        }
                    } else {
                        if(container != null) {
                            RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(container);
                            for(int i = 0; i < rtfElements.length; i++) {
                                rtfElements[i].setInTable(true);
                                this.content.add(rtfElements[i]);
                            }
                            container = null;
                        }
                        
                        
                        if (element instanceof Paragraph && ((Paragraph) element).getAlignment() == Element.ALIGN_UNDEFINED) {
                            ((Paragraph) element).setAlignment(cell.getHorizontalAlignment());
                        }
    
                        RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(element);
                        for(int i = 0; i < rtfElements.length; i++) {
                            rtfElements[i].setInTable(true);
                            this.content.add(rtfElements[i]);
                        }
                    }
                } catch(DocumentException de) {
                    de.printStackTrace();
                }
            }
            if(container != null) {
                try {
                    RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(container);
                    for(int i = 0; i < rtfElements.length; i++) {
                        rtfElements[i].setInTable(true);
                        this.content.add(rtfElements[i]);
                    }
                } catch(DocumentException de) {
                    de.printStackTrace();
                }
            }
        }

        

        Image img = cell.getImage();
        if(img != null) {
            try {
                RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(img);
                for (int i = 0; i < rtfElements.length; i++) {
                    rtfElements[i].setInTable(true);
                    this.content.add(rtfElements[i]);
                }
            } catch (DocumentException e) {
                
                e.printStackTrace();
            }
        }
        
        Phrase phrase = cell.getPhrase();
        if(phrase != null) {
            try {
                RtfBasicElement[] rtfElements = this.document.getMapper().mapElement(phrase);
                for (int i = 0; i < rtfElements.length; i++) {
                    rtfElements[i].setInTable(true);
                    this.content.add(rtfElements[i]);
                }
            } catch (DocumentException e) {
                
                e.printStackTrace();
            }
        }
        
        PdfPTable table = cell.getTable();
        if(table != null) {
            this.add(table);










        }

    }
    
    public void writeDefinition(final OutputStream result) throws IOException 
    {
        if(this.mergeType == MERGE_VERT_PARENT) {
            result.write("\\clvmgf".getBytes());
        } else if(this.mergeType == MERGE_VERT_CHILD) {
            result.write("\\clvmrg".getBytes());
        }
        switch (verticalAlignment) {
            case Element.ALIGN_BOTTOM:
                result.write("\\clvertalb".getBytes());
                break;
            case Element.ALIGN_CENTER:
            case Element.ALIGN_MIDDLE:
                result.write("\\clvertalc".getBytes());
                break;
            case Element.ALIGN_TOP:
                result.write("\\clvertalt".getBytes());
                break;
        }
        this.borders.writeContent(result);

        if(this.backgroundColor != null) {
            result.write("\\clcbpat".getBytes());
            result.write(intToByteArray(this.backgroundColor.getColorNumber()));
        }
        this.document.outputDebugLinebreak(result);
        
        result.write("\\clftsWidth3".getBytes());
        this.document.outputDebugLinebreak(result);
        
        result.write("\\clwWidth".getBytes());
        result.write(intToByteArray(this.cellWidth));
        this.document.outputDebugLinebreak(result);
        
        if(this.cellPadding > 0) {
            result.write("\\clpadl".getBytes());
            result.write(intToByteArray(this.cellPadding / 2));
            result.write("\\clpadt".getBytes());
            result.write(intToByteArray(this.cellPadding / 2));
            result.write("\\clpadr".getBytes());
            result.write(intToByteArray(this.cellPadding / 2));
            result.write("\\clpadb".getBytes());
            result.write(intToByteArray(this.cellPadding / 2));
            result.write("\\clpadfl3".getBytes());
            result.write("\\clpadft3".getBytes());
            result.write("\\clpadfr3".getBytes());
            result.write("\\clpadfb3".getBytes());
        }
        result.write("\\cellx".getBytes());
        result.write(intToByteArray(this.cellRight));
    }

    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        if(this.content.size() == 0) {
            result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
            if(this.parentRow.getParentTable().getTableFitToPage()) {
                result.write(RtfParagraphStyle.KEEP_TOGETHER_WITH_NEXT);
            }
            result.write(RtfParagraph.IN_TABLE);
        } else {
            for(int i = 0; i < this.content.size(); i++) {
                RtfBasicElement rtfElement = (RtfBasicElement) this.content.get(i);
                if(rtfElement instanceof RtfParagraph) {
                    ((RtfParagraph) rtfElement).setKeepTogetherWithNext(this.parentRow.getParentTable().getTableFitToPage());
                }
                rtfElement.writeContent(result);
                if(rtfElement instanceof RtfParagraph && i < (this.content.size() - 1)) {
                    result.write(RtfParagraph.PARAGRAPH);
                }
            }
        }
        result.write("\\cell".getBytes());
    }        

    
    protected void setCellRight(int cellRight) {
        this.cellRight = cellRight;
    }
    
    
    protected int getCellRight() {
        return this.cellRight;
    }
    
    
    protected void setCellWidth(int cellWidth) {
        this.cellWidth = cellWidth;
    }
    
    
    protected int getCellWidth() {
        return this.cellWidth;
    }
    
    
    protected int getCellpadding() {
        return this.cellPadding;
    }

    
    protected RtfBorderGroup getBorders() {
        return this.borders;
    }
    
    
    public void setBorders(RtfBorderGroup borderGroup) {
        this.borders = new RtfBorderGroup(this.document, RtfBorder.CELL_BORDER, borderGroup);
    }

    
    protected RtfColor getRtfBackgroundColor() {
        return this.backgroundColor;
    }

    
    protected void setCellMergeChild(RtfCell mergeParent) {
        this.mergeType = MERGE_VERT_CHILD;
        this.cellWidth = mergeParent.getCellWidth();
        this.cellRight = mergeParent.getCellRight();
        this.cellPadding = mergeParent.getCellpadding();
        this.borders = mergeParent.getBorders();
        this.verticalAlignment = mergeParent.getVerticalAlignment();
        this.backgroundColor = mergeParent.getRtfBackgroundColor();
    }

    
    public void setRtfDocument(RtfDocument doc) {
        this.document = doc;
    }
    
    
    public void setInTable(boolean inTable) {
    }
    
    
    public void setInHeader(boolean inHeader) {
        this.inHeader = inHeader;
        for(int i = 0; i < this.content.size(); i++) {
            ((RtfBasicElement) this.content.get(i)).setInHeader(inHeader);
        }
    }
    
    
    public boolean isInHeader() {
        return this.inHeader;
    }

    
    private byte[] intToByteArray(int i) {
        return Integer.toString(i).getBytes();
    }
    
    
    public boolean isDeleted() {
        return this.deleted;
    }
}
