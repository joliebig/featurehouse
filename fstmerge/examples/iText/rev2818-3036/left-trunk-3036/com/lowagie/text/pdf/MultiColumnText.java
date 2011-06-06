

package com.lowagie.text.pdf;

import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ElementListener;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;


public class MultiColumnText implements Element {

    
    public static final float AUTOMATIC = -1f;

    
    private float desiredHeight;

    
    private float totalHeight;

    
    private boolean overflow;

    
    private float top;

    
    private float pageBottom;

    
    private ColumnText columnText;

    
    private ArrayList columnDefs;

    
    private boolean simple = true;

    private int currentColumn = 0;
    
    private float nextY = AUTOMATIC;
    
    private boolean columnsRightToLeft = false;
    
    private PdfDocument document;
    
    public MultiColumnText() {
        this(AUTOMATIC);
    }

    
    public MultiColumnText(float height) {
        columnDefs = new ArrayList();
        desiredHeight = height;
        top = AUTOMATIC;
        
        columnText = new ColumnText(null);
        totalHeight = 0f;
    }

    
    public MultiColumnText(float top, float height) {
        columnDefs = new ArrayList();
        desiredHeight = height;
        this.top = top;
        nextY = top;
        
        columnText = new ColumnText(null);
        totalHeight = 0f;
    }
    
    
    public boolean isOverflow() {
        return overflow;
    }

    
    public void useColumnParams(ColumnText sourceColumn) {
        
        columnText.setSimpleVars(sourceColumn);
    }

    
    public void addColumn(float[] left, float[] right) {
        ColumnDef nextDef = new ColumnDef(left, right);
        simple = nextDef.isSimple();
        columnDefs.add(nextDef);
    }

    
    public void addSimpleColumn(float left, float right) {
        ColumnDef newCol = new ColumnDef(left, right);
        columnDefs.add(newCol);
    }

    
    public void addRegularColumns(float left, float right, float gutterWidth, int numColumns) {
        float currX = left;
        float width = right - left;
        float colWidth = (width - (gutterWidth * (numColumns - 1))) / numColumns;
        for (int i = 0; i < numColumns; i++) {
            addSimpleColumn(currX, currX + colWidth);
            currX += colWidth + gutterWidth;
        }
    }

    
    public void addElement(Element element) throws DocumentException {
        if (simple) {
            columnText.addElement(element);
        } else if (element instanceof Phrase) {
            columnText.addText((Phrase) element);
        } else if (element instanceof Chunk) {
            columnText.addText((Chunk) element);
        } else {
            throw new DocumentException("Can't add " + element.getClass() + " to MultiColumnText with complex columns");
        }
    }


    
    public float write(PdfContentByte canvas, PdfDocument document, float documentY) throws DocumentException {
        this.document = document;
        columnText.setCanvas(canvas);
        if (columnDefs.isEmpty()) {
            throw new DocumentException("MultiColumnText has no columns");
        }
        overflow = false;
        pageBottom = document.bottom();
        float currentHeight = 0;
        boolean done = false;
        try {
            while (!done) {
                if (nextY == AUTOMATIC) {
                    nextY = document.getVerticalPosition(true); 
                }
                if (top == AUTOMATIC) {
                    top = document.getVerticalPosition(true); 
                }

                ColumnDef currentDef = (ColumnDef) columnDefs.get(getCurrentColumn());
                columnText.setYLine(top);

                float[] left = currentDef.resolvePositions(Rectangle.LEFT);
                float[] right = currentDef.resolvePositions(Rectangle.RIGHT);
                if (document.isMarginMirroring() && document.getPageNumber() % 2 == 0){
                    float delta = document.rightMargin() - document.left();
                    left = (float[])left.clone();
                    right = (float[])right.clone();
                    for (int i = 0; i < left.length; i += 2) {
                        left[i] -= delta;
                    }
                    for (int i = 0; i < right.length; i += 2) {
                        right[i] -= delta;
                    }
                }
                
                currentHeight = Math.max(currentHeight, getHeight(left, right));

                if (currentDef.isSimple()) {
                    columnText.setSimpleColumn(left[2], left[3], right[0], right[1]);
                } else {
                    columnText.setColumns(left, right);
                }

                int result = columnText.go();
                if ((result & ColumnText.NO_MORE_TEXT) != 0) {
                    done = true;
                    top = columnText.getYLine();
                } else if (shiftCurrentColumn()) {
                    top = nextY;
                } else {  
                    totalHeight += currentHeight;
                    if ((desiredHeight != AUTOMATIC) && (totalHeight >= desiredHeight)) {
                        overflow = true;
                        break;
                    } else {  
                        documentY = nextY;
                        newPage();
                        currentHeight = 0;
                    }
                }
            }
        } catch (DocumentException ex) {
            ex.printStackTrace();
            throw ex;
        }
        if (desiredHeight == AUTOMATIC && columnDefs.size() == 1) {
            currentHeight = documentY - columnText.getYLine();
        }
        return currentHeight;
    }

    private void newPage() throws DocumentException {
        resetCurrentColumn();
        if (desiredHeight == AUTOMATIC) {
            top = nextY = AUTOMATIC;
        }
        else {
            top = nextY;
        }
        totalHeight = 0;
        if (document != null) {
            document.newPage();
        }
    }
    
    
    private float getHeight(float[] left, float[] right) {
        float max = Float.MIN_VALUE;
        float min = Float.MAX_VALUE;
        for (int i = 0; i < left.length; i += 2) {
            min = Math.min(min, left[i + 1]);
            max = Math.max(max, left[i + 1]);
        }
        for (int i = 0; i < right.length; i += 2) {
            min = Math.min(min, right[i + 1]);
            max = Math.max(max, right[i + 1]);
        }
        return max - min;
    }


    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        } catch (DocumentException de) {
            return false;
        }
    }

    

    public int type() {
        return Element.MULTI_COLUMN_TEXT;
    }

    

    public ArrayList getChunks() {
        return null;
    }

    
    private float getColumnBottom() {
        if (desiredHeight == AUTOMATIC) {
            return pageBottom;
        } else {
            return Math.max(top - (desiredHeight - totalHeight), pageBottom);
        }
    }

        
    public void nextColumn() throws DocumentException {
        currentColumn = (currentColumn + 1) % columnDefs.size();
        top = nextY;
        if (currentColumn == 0) {
            newPage();
        }
    }

    
    public int getCurrentColumn() {
        if (columnsRightToLeft) {
            return (columnDefs.size() - currentColumn - 1);
        } 
        return currentColumn;
    }
    
    
    public void resetCurrentColumn() {
        currentColumn = 0;
    }
    
    
    public boolean shiftCurrentColumn() {
        if (currentColumn + 1 < columnDefs.size()) {
            currentColumn++;
            return true;
        }
        return false;
    }
    
    
    public void setColumnsRightToLeft(boolean direction) {
        columnsRightToLeft = direction;
    }
    
    
    public void setSpaceCharRatio(float spaceCharRatio) {
        columnText.setSpaceCharRatio(spaceCharRatio);
    }

        
    public void setRunDirection(int runDirection) {
        columnText.setRunDirection(runDirection);
    }
    
    
    public void setArabicOptions(int arabicOptions) {
        columnText.setArabicOptions(arabicOptions);
    }
    
    
    public void setAlignment(int alignment) {
        columnText.setAlignment(alignment);
    }
    
    
    private class ColumnDef {
        private float[] left;
        private float[] right;

        ColumnDef(float[] newLeft, float[] newRight) {
            left = newLeft;
            right = newRight;
        }

        ColumnDef(float leftPosition, float rightPosition) {
            left = new float[4];
            left[0] = leftPosition; 
            left[1] = top;          
            left[2] = leftPosition; 
            if (desiredHeight == AUTOMATIC || top == AUTOMATIC) {
                left[3] = AUTOMATIC;
            } else {
                left[3] = top - desiredHeight;
            }

            right = new float[4];
            right[0] = rightPosition; 
            right[1] = top;           
            right[2] = rightPosition; 
            if (desiredHeight == AUTOMATIC || top == AUTOMATIC) {
                right[3] = AUTOMATIC;
            } else {
                right[3] = top - desiredHeight;
            }
        }

        
        float[] resolvePositions(int side) {
            if (side == Rectangle.LEFT) {
                return resolvePositions(left);
            } else {
                return resolvePositions(right);
            }
        }

        private float[] resolvePositions(float[] positions) {
            if (!isSimple()) {
                return positions;
            }
            if (top == AUTOMATIC) {
                
                throw new RuntimeException("resolvePositions called with top=AUTOMATIC (-1).  " +
                        "Top position must be set befure lines can be resolved");
            }
            positions[1] = top;
            positions[3] = getColumnBottom();
            return positions;
        }

        
        private boolean isSimple() {
            return (left.length == 4 && right.length == 4) && (left[0] == left[2] && right[0] == right[2]);
        }

    }
}
