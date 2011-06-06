

package com.lowagie.text.pdf;

import java.util.List;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.events.PdfPCellEventForwarder;



public class PdfPCell extends Rectangle{
    
    private ColumnText column = new ColumnText(null);
    
    
    private int verticalAlignment = Element.ALIGN_TOP;
    
    
    private float paddingLeft = 2;
    
    
    private float paddingRight = 2;
    
    
    private float paddingTop = 2;
    
    
    private float paddingBottom = 2;
    
    
    private float fixedHeight = 0;
    
    
    private float minimumHeight;
    
    
    private boolean noWrap = false;
    
    
    private PdfPTable table;

    
    private int colspan = 1;

    
    private int rowspan = 1;
    
    
    private Image image;
    
    
    private PdfPCellEvent cellEvent;

    
    private boolean useDescender;

    
    private boolean useBorderPadding = false;

    
    protected Phrase phrase;

    
    private int rotation;

    
    public PdfPCell() {
        super(0, 0, 0, 0);
        borderWidth = 0.5f;
        border = BOX;
        column.setLeading(0, 1);
    }

    
    public PdfPCell(Phrase phrase) {
        super(0, 0, 0, 0);
        borderWidth = 0.5f;
        border = BOX;
        column.addText(this.phrase = phrase);
        column.setLeading(0, 1);
    }
    
    
    public PdfPCell(Image image) {
        this(image, false);
    }
    
    
    public PdfPCell(Image image, boolean fit) {
        super(0, 0, 0, 0);
        borderWidth = 0.5f;
        border = BOX;
        if (fit) {
            this.image = image;
            column.setLeading(0, 1);
            setPadding(borderWidth / 2);
        }
        else {
            column.addText(this.phrase = new Phrase(new Chunk(image, 0, 0)));
            column.setLeading(0, 1);
            setPadding(0);
        }
    }
    
    
    public PdfPCell(PdfPTable table) {
        this(table, null);
    }
    
    
    public PdfPCell(PdfPTable table, PdfPCell style) {
        super(0, 0, 0, 0);
        borderWidth = 0.5f;
        border = BOX;
        column.setLeading(0, 1);
        this.table = table;
        table.setWidthPercentage(100);
        table.setExtendLastRow(true);
        column.addElement(table);
        if (style != null) {
            cloneNonPositionParameters(style);
            verticalAlignment = style.verticalAlignment;
            paddingLeft = style.paddingLeft;
            paddingRight = style.paddingRight;
            paddingTop = style.paddingTop;
            paddingBottom = style.paddingBottom;
            colspan = style.colspan;
            rowspan = style.rowspan;
            cellEvent = style.cellEvent;
            useDescender = style.useDescender;
            useBorderPadding = style.useBorderPadding;
            rotation = style.rotation;
        }
        else
            setPadding(0);
    }
    
    
    public PdfPCell(PdfPCell cell) {
        super(cell.llx, cell.lly, cell.urx, cell.ury);
        cloneNonPositionParameters(cell);
        verticalAlignment = cell.verticalAlignment;
        paddingLeft = cell.paddingLeft;
        paddingRight = cell.paddingRight;
        paddingTop = cell.paddingTop;
        paddingBottom = cell.paddingBottom;
        phrase = cell.phrase;
        fixedHeight = cell.fixedHeight;
        minimumHeight = cell.minimumHeight;
        noWrap = cell.noWrap;
        colspan = cell.colspan;
        rowspan = cell.rowspan;
        if (cell.table != null)
            table = new PdfPTable(cell.table);
        image = Image.getInstance(cell.image);
        cellEvent = cell.cellEvent;
        useDescender = cell.useDescender;
        column = ColumnText.duplicate(cell.column);
        useBorderPadding = cell.useBorderPadding;
        rotation = cell.rotation;
    }
    
    
    public void addElement(Element element) {
        if (table != null) {
            table = null;
            column.setText(null);
        }
        column.addElement(element);
    }
    
    
    public Phrase getPhrase() {
        return phrase;
    }
    
    
    public void setPhrase(Phrase phrase) {
        table = null;
        image = null;
        column.setText(this.phrase = phrase);
    }
    
    
    public int getHorizontalAlignment() {
        return column.getAlignment();
    }
    
    
    public void setHorizontalAlignment(int horizontalAlignment) {
        column.setAlignment(horizontalAlignment);
    }
    
    
    public int getVerticalAlignment() {
        return verticalAlignment;
    }
    
    
    public void setVerticalAlignment(int verticalAlignment) {
        if (table != null)
            table.setExtendLastRow(verticalAlignment == Element.ALIGN_TOP);
        this.verticalAlignment = verticalAlignment;
    }
    
    
    public float getEffectivePaddingLeft() {
        if (isUseBorderPadding()) {
            float border = getBorderWidthLeft() / (isUseVariableBorders() ? 1f : 2f);
            return paddingLeft + border;
        }
        return paddingLeft;
    }
    
    
    public float getPaddingLeft() {
        return paddingLeft;
    }

    
    public void setPaddingLeft(float paddingLeft) {
        this.paddingLeft = paddingLeft;
    }
    
    
    public float getEffectivePaddingRight() {
        if (isUseBorderPadding()) {
            float border = getBorderWidthRight() / (isUseVariableBorders() ? 1f : 2f);
            return paddingRight + border;
        }
        return paddingRight;
    }
    
    
    public float getPaddingRight() {
        return paddingRight;
    }

    
    public void setPaddingRight(float paddingRight) {
        this.paddingRight = paddingRight;
    }
    
    
    public float getEffectivePaddingTop() {
        if (isUseBorderPadding()) {
            float border = getBorderWidthTop()/(isUseVariableBorders()?1f:2f);
            return paddingTop + border;
        }
        return paddingTop;
    }
    
    
    public float getPaddingTop() {
        return paddingTop;
    }

    
    public void setPaddingTop(float paddingTop) {
        this.paddingTop = paddingTop;
    }
    
    
    public float getEffectivePaddingBottom() {
        if (isUseBorderPadding()) {
            float border = getBorderWidthBottom()/(isUseVariableBorders()?1f:2f);
            return paddingBottom + border;
        }
        return paddingBottom;
    }
    
    
    public float getPaddingBottom() {
        return paddingBottom;
    }

    
    public void setPaddingBottom(float paddingBottom) {
        this.paddingBottom = paddingBottom;
    }
    
    
    public void setPadding(float padding) {
        paddingBottom = padding;
        paddingTop = padding;
        paddingLeft = padding;
        paddingRight = padding;
    }

    
    public boolean isUseBorderPadding() {
        return useBorderPadding;
    }

    
    public void setUseBorderPadding(boolean use) {
        useBorderPadding = use;
    }

    
    public void setLeading(float fixedLeading, float multipliedLeading) {
        column.setLeading(fixedLeading, multipliedLeading);
    }
    
    
    public float getLeading() {
        return column.getLeading();
    }
    
    
    public float getMultipliedLeading() {
        return column.getMultipliedLeading();
    }
    
    
    public void setIndent(float indent) {
        column.setIndent(indent);
    }
    
    
    public float getIndent() {
        return column.getIndent();
    }
    
    
    public float getExtraParagraphSpace() {
        return column.getExtraParagraphSpace();
    }
    
    
    public void setExtraParagraphSpace(float extraParagraphSpace) {
        column.setExtraParagraphSpace(extraParagraphSpace);
    }
    
    
    public void setFixedHeight(float fixedHeight) {
        this.fixedHeight = fixedHeight;
        minimumHeight = 0;
    }
    
    
    public float getFixedHeight() {
        return fixedHeight;
    }

    
    public boolean hasFixedHeight() {
        return getFixedHeight() > 0;
    }
    
    
    public void setMinimumHeight(float minimumHeight) {
        this.minimumHeight = minimumHeight;
        fixedHeight = 0;
    }
    
    
    public float getMinimumHeight() {
        return minimumHeight;
    }

    
    public boolean hasMinimumHeight() {
        return getMinimumHeight() > 0;
    }
    
    
    public boolean isNoWrap() {
        return noWrap;
    }
    
    
    public void setNoWrap(boolean noWrap) {
        this.noWrap = noWrap;
    }
    
    
    public PdfPTable getTable() {
        return table;
    }
    
    void setTable(PdfPTable table) {
        this.table = table;
        column.setText(null);
        image = null;
        if (table != null) {
            table.setExtendLastRow(verticalAlignment == Element.ALIGN_TOP);
            column.addElement(table);
            table.setWidthPercentage(100);
        }
    }

    
    public int getColspan() {
        return colspan;
    }
    
    
    public void setColspan(int colspan) {
        this.colspan = colspan;
    }
    
    
    public int getRowspan() {
        return rowspan;
    }
    
    
    public void setRowspan(int rowspan) {
        this.rowspan = rowspan;
    }

    
    public void setFollowingIndent(float indent) {
        column.setFollowingIndent(indent);
    }
    
    
    public float getFollowingIndent() {
        return column.getFollowingIndent();
    }
    
    
    public void setRightIndent(float indent) {
        column.setRightIndent(indent);
    }
    
    
    public float getRightIndent() {
        return column.getRightIndent();
    }
    
    
    public float getSpaceCharRatio() {
        return column.getSpaceCharRatio();
    }
    
    
    public void setSpaceCharRatio(float spaceCharRatio) {
        column.setSpaceCharRatio(spaceCharRatio);
    }
    
    
    public void setRunDirection(int runDirection) {
        column.setRunDirection(runDirection);
    }
    
    
    public int getRunDirection() {
        return column.getRunDirection();
    }
    
    
    public Image getImage() {
        return image;
    }
    
    
    public void setImage(Image image) {
        column.setText(null);
        table = null;
        this.image = image;
    }
    
    
    public PdfPCellEvent getCellEvent() {
        return cellEvent;
    }
    
    
    public void setCellEvent(PdfPCellEvent cellEvent) {
        if (cellEvent == null)
            this.cellEvent = null;
        else if (this.cellEvent == null)
            this.cellEvent = cellEvent;
        else if (this.cellEvent instanceof PdfPCellEventForwarder)
            ((PdfPCellEventForwarder)this.cellEvent).addCellEvent(cellEvent);
        else {
            PdfPCellEventForwarder forward = new PdfPCellEventForwarder();
            forward.addCellEvent(this.cellEvent);
            forward.addCellEvent(cellEvent);
            this.cellEvent = forward;
        }
    }
    
    
    public int getArabicOptions() {
        return column.getArabicOptions();
    }
    
    
    public void setArabicOptions(int arabicOptions) {
        column.setArabicOptions(arabicOptions);
    }
    
    
    public boolean isUseAscender() {
        return column.isUseAscender();
    }

    
    public void setUseAscender(boolean useAscender) {
        column.setUseAscender(useAscender);
    }


    
    public boolean isUseDescender() {
        return useDescender;
    }

    
    public void setUseDescender(boolean useDescender) {
        this.useDescender = useDescender;
    }

    
    public ColumnText getColumn() {
        return column;
    }
    
    
    public List<Element> getCompositeElements() {
        return getColumn().compositeElements;
    }
    
    
    public void setColumn(ColumnText column) {
        this.column = column;
    }

    
    public int getRotation() {
        return rotation;
    }

    
    public void setRotation(int rotation) {
        if (rotation < 0)
            rotation += 360;
        else
            rotation %= 360;
        if ((rotation % 90) != 0)
            throw new IllegalArgumentException("Rotation must be a multiple of 90.");
        this.rotation = rotation;
    }
    
    
    void consumeHeight(float height) {
        float rightLimit = getRight() - getEffectivePaddingRight();
        float leftLimit = getLeft() + getEffectivePaddingLeft();
        float bry = height - getEffectivePaddingTop() - getEffectivePaddingBottom();
        if (getRotation() != 90 && getRotation() != 270) {
            column.setSimpleColumn(leftLimit, bry + 0.001f,    rightLimit, 0);
        }
        else {
            column.setSimpleColumn(0, leftLimit, bry + 0.001f, rightLimit);
        }
        try {
            column.go(true);
        } catch (DocumentException e) {
            
        }
    }
}