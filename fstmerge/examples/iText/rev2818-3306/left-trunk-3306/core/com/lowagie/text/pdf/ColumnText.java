

package com.lowagie.text.pdf;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Stack;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ListItem;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.SimpleTable;



public class ColumnText {
        
    public static final int AR_NOVOWEL = ArabicLigaturizer.ar_novowel;
        
    public static final int AR_COMPOSEDTASHKEEL = ArabicLigaturizer.ar_composedtashkeel;
        
    public static final int AR_LIG = ArabicLigaturizer.ar_lig;
    
    public static final int DIGITS_EN2AN = ArabicLigaturizer.DIGITS_EN2AN;
    
    
    public static final int DIGITS_AN2EN = ArabicLigaturizer.DIGITS_AN2EN;
    
    
    public static final int DIGITS_EN2AN_INIT_LR = ArabicLigaturizer.DIGITS_EN2AN_INIT_LR;
    
    
    public static final int DIGITS_EN2AN_INIT_AL = ArabicLigaturizer.DIGITS_EN2AN_INIT_AL;
    
    
    public static final int DIGIT_TYPE_AN = ArabicLigaturizer.DIGIT_TYPE_AN;
    
    
    public static final int DIGIT_TYPE_AN_EXTENDED = ArabicLigaturizer.DIGIT_TYPE_AN_EXTENDED;
    
    protected int runDirection = PdfWriter.RUN_DIRECTION_DEFAULT;
    
    
    public static final float GLOBAL_SPACE_CHAR_RATIO = 0;
    
    
    public static final int START_COLUMN = 0;
    
    
    public static final int NO_MORE_TEXT = 1;
    
    
    public static final int NO_MORE_COLUMN = 2;
    
    
    protected static final int LINE_STATUS_OK = 0;
    
    
    protected static final int LINE_STATUS_OFFLIMITS = 1;
    
    
    protected static final int LINE_STATUS_NOLINE = 2;
    
    
    protected float maxY;
    
    
    protected float minY;
    
    protected float leftX;
    
    protected float rightX;
    
    
    protected int alignment = Element.ALIGN_LEFT;
    
    
    protected ArrayList leftWall;
    
    
    protected ArrayList rightWall;
    
    

    protected BidiLine bidiLine;
    
    
    protected float yLine;
    
    
    protected float currentLeading = 16;
    
    
    protected float fixedLeading = 16;
    
    
    protected float multipliedLeading = 0;
    
    
    protected PdfContentByte canvas;
    
    protected PdfContentByte[] canvases;
    
    
    protected int lineStatus;
    
    
    protected float indent = 0;
    
    
    protected float followingIndent = 0;
    
    
    protected float rightIndent = 0;
    
    
    protected float extraParagraphSpace = 0;
    
    
    protected float rectangularWidth = -1;
    
    protected boolean rectangularMode = false;
    
    private float spaceCharRatio = GLOBAL_SPACE_CHAR_RATIO;

    private boolean lastWasNewline = true;
    
    
    private int linesWritten;
    
    private float firstLineY;
    private boolean firstLineYDone = false;
    
    
    private int arabicOptions = 0;
    
    protected float descender;
    
    protected boolean composite = false;
    
    protected ColumnText compositeColumn;
    
    protected LinkedList compositeElements;
    
    protected int listIdx = 0;
    
    private boolean splittedRow;
    
    protected Phrase waitPhrase;
    
    
    private boolean useAscender = false;

    
    public ColumnText(PdfContentByte canvas) {
        this.canvas = canvas;
    }
    
        
    public static ColumnText duplicate(ColumnText org) {
        ColumnText ct = new ColumnText(null);
        ct.setACopy(org);
        return ct;
    }
    
        
    public ColumnText setACopy(ColumnText org) {
        setSimpleVars(org);
        if (org.bidiLine != null)
            bidiLine = new BidiLine(org.bidiLine);
        return this;
    }
    
    protected void setSimpleVars(ColumnText org) {
        maxY = org.maxY;
        minY = org.minY;
        alignment = org.alignment;
        leftWall = null;
        if (org.leftWall != null)
            leftWall = new ArrayList(org.leftWall);
        rightWall = null;
        if (org.rightWall != null)
            rightWall = new ArrayList(org.rightWall);
        yLine = org.yLine;
        currentLeading = org.currentLeading;
        fixedLeading = org.fixedLeading;
        multipliedLeading = org.multipliedLeading;
        canvas = org.canvas;
        canvases = org.canvases;
        lineStatus = org.lineStatus;
        indent = org.indent;
        followingIndent = org.followingIndent;
        rightIndent = org.rightIndent;
        extraParagraphSpace = org.extraParagraphSpace;
        rectangularWidth = org.rectangularWidth;
        rectangularMode = org.rectangularMode;
        spaceCharRatio = org.spaceCharRatio;
        lastWasNewline = org.lastWasNewline;
        linesWritten = org.linesWritten;
        arabicOptions = org.arabicOptions;
        runDirection = org.runDirection;
        descender = org.descender;
        composite = org.composite;
        splittedRow = org.splittedRow;
        if (org.composite) {
            compositeElements = new LinkedList(org.compositeElements);
            if (splittedRow) {
                PdfPTable table = (PdfPTable)compositeElements.getFirst();
                compositeElements.set(0, new PdfPTable(table));
            }
            if (org.compositeColumn != null)
                compositeColumn = duplicate(org.compositeColumn);
        }
        listIdx = org.listIdx;
        firstLineY = org.firstLineY;
        leftX = org.leftX;
        rightX = org.rightX;
        firstLineYDone = org.firstLineYDone;
        waitPhrase = org.waitPhrase;
        useAscender = org.useAscender;
        filledWidth = org.filledWidth;
        adjustFirstLine = org.adjustFirstLine;
    }
    
    private void addWaitingPhrase() {
        if (bidiLine == null && waitPhrase != null) {
            bidiLine = new BidiLine();
            for (Iterator j = waitPhrase.getChunks().iterator(); j.hasNext();) {
                bidiLine.addChunk(new PdfChunk((Chunk)j.next(), null));
            }
            waitPhrase = null;
        }
    }
    
    
    public void addText(Phrase phrase) {
        if (phrase == null || composite)
            return;
        addWaitingPhrase();
        if (bidiLine == null) {
            waitPhrase = phrase;
            return;
        }
        for (Iterator j = phrase.getChunks().iterator(); j.hasNext();) {
            bidiLine.addChunk(new PdfChunk((Chunk)j.next(), null));
        }
    }
    
    
    public void setText(Phrase phrase) {
        bidiLine = null;
        composite = false;
        compositeColumn = null;
        compositeElements = null;
        listIdx = 0;
        splittedRow = false;
        waitPhrase = phrase;
    }
    
    
    public void addText(Chunk chunk) {
        if (chunk == null || composite)
            return;
        addText(new Phrase(chunk));
    }
    
        
    public void addElement(Element element) {
        if (element == null)
            return;
        if (element instanceof Image) {
            Image img = (Image)element;
            PdfPTable t = new PdfPTable(1);
            float w = img.getWidthPercentage();
            if (w == 0) {
                t.setTotalWidth(img.getScaledWidth());
                t.setLockedWidth(true);
            }
            else
                t.setWidthPercentage(w);
            t.setSpacingAfter(img.getSpacingAfter());
            t.setSpacingBefore(img.getSpacingBefore());
            switch (img.getAlignment()) {
                case Image.LEFT:
                    t.setHorizontalAlignment(Element.ALIGN_LEFT);
                    break;
                case Image.RIGHT:
                    t.setHorizontalAlignment(Element.ALIGN_RIGHT);
                    break;
                default:
                    t.setHorizontalAlignment(Element.ALIGN_CENTER);
                    break;
            }
            PdfPCell c = new PdfPCell(img, true);
            c.setPadding(0);
            c.setBorder(img.getBorder());
            c.setBorderColor(img.getBorderColor());
            c.setBorderWidth(img.getBorderWidth());
            c.setBackgroundColor(img.getBackgroundColor());
            t.addCell(c);
            element = t;
        }
        if (element.type() == Element.CHUNK) {
            element = new Paragraph((Chunk)element);
        }
        else if (element.type() == Element.PHRASE) {
            element = new Paragraph((Phrase)element);
        }
        if (element instanceof SimpleTable) {
            try {
                element = ((SimpleTable)element).createPdfPTable();
            } catch (DocumentException e) {
                throw new IllegalArgumentException("Element not allowed.");
            }
        }
        else if (element.type() != Element.PARAGRAPH && element.type() != Element.LIST && element.type() != Element.PTABLE)
            throw new IllegalArgumentException("Element not allowed.");
        if (!composite) {
            composite = true;
            compositeElements = new LinkedList();
            bidiLine = null;
            waitPhrase = null;
        }
        compositeElements.add(element);
    }
    
    
    protected ArrayList convertColumn(float cLine[]) {
        if (cLine.length < 4)
            throw new RuntimeException("No valid column line found.");
        ArrayList cc = new ArrayList();
        for (int k = 0; k < cLine.length - 2; k += 2) {
            float x1 = cLine[k];
            float y1 = cLine[k + 1];
            float x2 = cLine[k + 2];
            float y2 = cLine[k + 3];
            if (y1 == y2)
                continue;
            
            float a = (x1 - x2) / (y1 - y2);
            float b = x1 - a * y1;
            float r[] = new float[4];
            r[0] = Math.min(y1, y2);
            r[1] = Math.max(y1, y2);
            r[2] = a;
            r[3] = b;
            cc.add(r);
            maxY = Math.max(maxY, r[1]);
            minY = Math.min(minY, r[0]);
        }
        if (cc.isEmpty())
            throw new RuntimeException("No valid column line found.");
        return cc;
    }
    
    
    protected float findLimitsPoint(ArrayList wall) {
        lineStatus = LINE_STATUS_OK;
        if (yLine < minY || yLine > maxY) {
            lineStatus = LINE_STATUS_OFFLIMITS;
            return 0;
        }
        for (int k = 0; k < wall.size(); ++k) {
            float r[] = (float[])wall.get(k);
            if (yLine < r[0] || yLine > r[1])
                continue;
            return r[2] * yLine + r[3];
        }
        lineStatus = LINE_STATUS_NOLINE;
        return 0;
    }
    
    
    protected float[] findLimitsOneLine() {
        float x1 = findLimitsPoint(leftWall);
        if (lineStatus == LINE_STATUS_OFFLIMITS || lineStatus == LINE_STATUS_NOLINE)
            return null;
        float x2 = findLimitsPoint(rightWall);
        if (lineStatus == LINE_STATUS_NOLINE)
            return null;
        return new float[]{x1, x2};
    }
    
    
    protected float[] findLimitsTwoLines() {
        boolean repeat = false;
        for (;;) {
            if (repeat && currentLeading == 0)
                return null;
            repeat = true;
            float x1[] = findLimitsOneLine();
            if (lineStatus == LINE_STATUS_OFFLIMITS)
                return null;
            yLine -= currentLeading;
            if (lineStatus == LINE_STATUS_NOLINE) {
                continue;
            }
            float x2[] = findLimitsOneLine();
            if (lineStatus == LINE_STATUS_OFFLIMITS)
                return null;
            if (lineStatus == LINE_STATUS_NOLINE) {
                yLine -= currentLeading;
                continue;
            }
            if (x1[0] >= x2[1] || x2[0] >= x1[1])
                continue;
            return new float[]{x1[0], x1[1], x2[0], x2[1]};
        }
    }
    
    
    public void setColumns(float leftLine[], float rightLine[]) {
        maxY = -10e20f;
        minY = 10e20f;
        rightWall = convertColumn(rightLine);
        leftWall = convertColumn(leftLine);
        rectangularWidth = -1;
        rectangularMode = false;
    }
    
    
    public void setSimpleColumn(Phrase phrase, float llx, float lly, float urx, float ury, float leading, int alignment) {
        addText(phrase);
        setSimpleColumn(llx, lly, urx, ury, leading, alignment);
    }
    
    
    public void setSimpleColumn(float llx, float lly, float urx, float ury, float leading, int alignment) {
        setLeading(leading);
        this.alignment = alignment;
        setSimpleColumn(llx, lly, urx, ury);
    }
    
    
    public void setSimpleColumn(float llx, float lly, float urx, float ury) {
        leftX = Math.min(llx, urx);
        maxY = Math.max(lly, ury);
        minY = Math.min(lly, ury);
        rightX = Math.max(llx, urx);
        yLine = maxY;
        rectangularWidth = rightX - leftX;
        if (rectangularWidth < 0)
            rectangularWidth = 0;
        rectangularMode = true;
    }
    
    public void setLeading(float leading) {
        fixedLeading = leading;
        multipliedLeading = 0;
    }
    
    
    public void setLeading(float fixedLeading, float multipliedLeading) {
        this.fixedLeading = fixedLeading;
        this.multipliedLeading = multipliedLeading;
    }
    
    
    public float getLeading() {
        return fixedLeading;
    }
    
    
    public float getMultipliedLeading() {
        return multipliedLeading;
    }
    
    
    public void setYLine(float yLine) {
        this.yLine = yLine;
    }
    
    
    public float getYLine() {
        return yLine;
    }
    
    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }
    
    
    public int getAlignment() {
        return alignment;
    }
    
    
    public void setIndent(float indent) {
        this.indent = indent;
        lastWasNewline = true;
    }
    
    
    public float getIndent() {
        return indent;
    }
    
    
    public void setFollowingIndent(float indent) {
        this.followingIndent = indent;
        lastWasNewline = true;
    }
    
    
    public float getFollowingIndent() {
        return followingIndent;
    }
    
    
    public void setRightIndent(float indent) {
        this.rightIndent = indent;
        lastWasNewline = true;
    }
    
    
    public float getRightIndent() {
        return rightIndent;
    }
    
    
    public int go() throws DocumentException {
        return go(false);
    }
    
    
    public int go(boolean simulate) throws DocumentException {
        if (composite)
            return goComposite(simulate);
        addWaitingPhrase();
        if (bidiLine == null)
            return NO_MORE_TEXT;
        descender = 0;
        linesWritten = 0;
        boolean dirty = false;
        float ratio = spaceCharRatio;
        Object currentValues[] = new Object[2];
        PdfFont currentFont = null;
        Float lastBaseFactor = new Float(0);
        currentValues[1] = lastBaseFactor;
        PdfDocument pdf = null;
        PdfContentByte graphics = null;
        PdfContentByte text = null;
        firstLineY = Float.NaN;
        int localRunDirection = PdfWriter.RUN_DIRECTION_NO_BIDI;
        if (runDirection != PdfWriter.RUN_DIRECTION_DEFAULT)
            localRunDirection = runDirection;
        if (canvas != null) {
            graphics = canvas;
            pdf = canvas.getPdfDocument();
            text = canvas.getDuplicate();
        }
        else if (!simulate)
            throw new NullPointerException("ColumnText.go with simulate==false and text==null.");
        if (!simulate) {
            if (ratio == GLOBAL_SPACE_CHAR_RATIO)
                ratio = text.getPdfWriter().getSpaceCharRatio();
            else if (ratio < 0.001f)
                ratio = 0.001f;
        }
        float firstIndent = 0;
        
        int status = 0;
        if (rectangularMode) {
            for (;;) {
                firstIndent = (lastWasNewline ? indent : followingIndent);
                if (rectangularWidth <= firstIndent + rightIndent) {
                    status = NO_MORE_COLUMN;
                    if (bidiLine.isEmpty())
                        status |= NO_MORE_TEXT;
                    break;
                }
                if (bidiLine.isEmpty()) {
                    status = NO_MORE_TEXT;
                    break;
                }
                PdfLine line = bidiLine.processLine(rectangularWidth - firstIndent - rightIndent, alignment, localRunDirection, arabicOptions);
                if (line == null) {
                    status = NO_MORE_TEXT;
                    break;
                }
                float maxSize = line.getMaxSizeSimple();
                if (isUseAscender() && Float.isNaN(firstLineY)) {
                    currentLeading = line.getAscender();
                }
                else {
                    currentLeading = fixedLeading + maxSize * multipliedLeading;
                }
                if (yLine > maxY || yLine - currentLeading < minY ) {
                    status = NO_MORE_COLUMN;
                    bidiLine.restore();
                    break;
                }
                yLine -= currentLeading;
                if (!simulate && !dirty) {
                    text.beginText();
                    dirty = true;
                }
                if (Float.isNaN(firstLineY)) {
                    firstLineY = yLine;
                }
                updateFilledWidth(rectangularWidth - line.widthLeft());
                if (!simulate) {
                    currentValues[0] = currentFont;
                    text.setTextMatrix(leftX + (line.isRTL() ? rightIndent : firstIndent) + line.indentLeft(), yLine);
                    pdf.writeLineToContent(line, text, graphics, currentValues, ratio);
                    currentFont = (PdfFont)currentValues[0];
                }
                lastWasNewline = line.isNewlineSplit();
                yLine -= line.isNewlineSplit() ? extraParagraphSpace : 0;
                ++linesWritten;
                descender = line.getDescender();
            }
        }
        else {
            currentLeading = fixedLeading;
            for (;;) {
                firstIndent = (lastWasNewline ? indent : followingIndent);
                float yTemp = yLine;
                float xx[] = findLimitsTwoLines();
                if (xx == null) {
                    status = NO_MORE_COLUMN;
                    if (bidiLine.isEmpty())
                        status |= NO_MORE_TEXT;
                    yLine = yTemp;
                    break;
                }
                if (bidiLine.isEmpty()) {
                    status = NO_MORE_TEXT;
                    yLine = yTemp;
                    break;
                }
                float x1 = Math.max(xx[0], xx[2]);
                float x2 = Math.min(xx[1], xx[3]);
                if (x2 - x1 <= firstIndent + rightIndent)
                    continue;
                if (!simulate && !dirty) {
                    text.beginText();
                    dirty = true;
                }
                PdfLine line = bidiLine.processLine(x2 - x1 - firstIndent - rightIndent, alignment, localRunDirection, arabicOptions);
                if (line == null) {
                    status = NO_MORE_TEXT;
                    yLine = yTemp;
                    break;
                }
                if (!simulate) {
                    currentValues[0] = currentFont;
                    text.setTextMatrix(x1 + (line.isRTL() ? rightIndent : firstIndent) + line.indentLeft(), yLine);
                    pdf.writeLineToContent(line, text, graphics, currentValues, ratio);
                    currentFont = (PdfFont)currentValues[0];
                }
                lastWasNewline = line.isNewlineSplit();
                yLine -= line.isNewlineSplit() ? extraParagraphSpace : 0;
                ++linesWritten;
                descender = line.getDescender();
            }
        }
        if (dirty) {
            text.endText();
            canvas.add(text);
        }
        return status;
    }
    
    
    public float getExtraParagraphSpace() {
        return extraParagraphSpace;
    }
    
    
    public void setExtraParagraphSpace(float extraParagraphSpace) {
        this.extraParagraphSpace = extraParagraphSpace;
    }
    
    
    public void clearChunks() {
        if (bidiLine != null)
            bidiLine.clearChunks();
    }
    
        
    public float getSpaceCharRatio() {
        return spaceCharRatio;
    }
    
    
    public void setSpaceCharRatio(float spaceCharRatio) {
        this.spaceCharRatio = spaceCharRatio;
    }

        
    public void setRunDirection(int runDirection) {
        if (runDirection < PdfWriter.RUN_DIRECTION_DEFAULT || runDirection > PdfWriter.RUN_DIRECTION_RTL)
            throw new RuntimeException("Invalid run direction: " + runDirection);
        this.runDirection = runDirection;
    }
    
        
    public int getRunDirection() {
        return runDirection;
    }
    
    
    public int getLinesWritten() {
        return this.linesWritten;
    }
    
    
    public int getArabicOptions() {
        return this.arabicOptions;
    }
    
    
    public void setArabicOptions(int arabicOptions) {
        this.arabicOptions = arabicOptions;
    }
    
        
    public float getDescender() {
        return descender;
    }
    
        
    public static float getWidth(Phrase phrase, int runDirection, int arabicOptions) {
        ColumnText ct = new ColumnText(null);
        ct.addText(phrase);
        ct.addWaitingPhrase();
        PdfLine line = ct.bidiLine.processLine(20000, Element.ALIGN_LEFT, runDirection, arabicOptions);
        if (line == null)
            return 0;
        else
            return 20000 - line.widthLeft();
    }
    
        
    public static float getWidth(Phrase phrase) {
        return getWidth(phrase, PdfWriter.RUN_DIRECTION_NO_BIDI, 0);
    }
    
        
    public static void showTextAligned(PdfContentByte canvas, int alignment, Phrase phrase, float x, float y, float rotation, int runDirection, int arabicOptions) {
        if (alignment != Element.ALIGN_LEFT && alignment != Element.ALIGN_CENTER
            && alignment != Element.ALIGN_RIGHT)
            alignment = Element.ALIGN_LEFT;
        canvas.saveState();
        ColumnText ct = new ColumnText(canvas);
        if (rotation == 0) {
            if (alignment == Element.ALIGN_LEFT)
                ct.setSimpleColumn(phrase, x, y - 1, 20000 + x, y + 2, 2, alignment);
            else if (alignment == Element.ALIGN_RIGHT)
                ct.setSimpleColumn(phrase, x-20000, y-1, x, y+2, 2, alignment);
            else
                ct.setSimpleColumn(phrase, x-20000, y-1, x+20000, y+2, 2, alignment);
        }
        else {
            double alpha = rotation * Math.PI / 180.0;
            float cos = (float)Math.cos(alpha);
            float sin = (float)Math.sin(alpha);
            canvas.concatCTM(cos, sin, -sin, cos, x, y);
            if (alignment == Element.ALIGN_LEFT)
                ct.setSimpleColumn(phrase, 0, -1, 20000, 2, 2, alignment);
            else if (alignment == Element.ALIGN_RIGHT)
                ct.setSimpleColumn(phrase, -20000, -1, 0, 2, 2, alignment);
            else
                ct.setSimpleColumn(phrase, -20000, -1, 20000, 2, 2, alignment);
        }
        if (runDirection == PdfWriter.RUN_DIRECTION_RTL) {
            if (alignment == Element.ALIGN_LEFT)
                alignment = Element.ALIGN_RIGHT;
            else if (alignment == Element.ALIGN_RIGHT)
                alignment = Element.ALIGN_LEFT;
        }
        ct.setAlignment(alignment);
        ct.setArabicOptions(arabicOptions);
        ct.setRunDirection(runDirection);
        try {
            ct.go();
        }
        catch (DocumentException e) {
            throw new ExceptionConverter(e);
        }
        canvas.restoreState();
    }

        
    public static void showTextAligned(PdfContentByte canvas, int alignment, Phrase phrase, float x, float y, float rotation) {
        showTextAligned(canvas, alignment, phrase, x, y, rotation, PdfWriter.RUN_DIRECTION_NO_BIDI, 0);
    }

    protected int goComposite(boolean simulate) throws DocumentException {
        if (!rectangularMode)
            throw new DocumentException("Irregular columns are not supported in composite mode.");
        linesWritten = 0;
        descender = 0;
        boolean firstPass = adjustFirstLine;
        main_loop:
        while (true) {
            if (compositeElements.isEmpty())
                return NO_MORE_TEXT;
            Element element = (Element)compositeElements.getFirst();
            if (element.type() == Element.PARAGRAPH) {
                Paragraph para = (Paragraph)element;
                int status = 0;
                for (int keep = 0; keep < 2; ++keep) {
                    float lastY = yLine;
                    boolean createHere = false;
                    if (compositeColumn == null) {
                        compositeColumn = new ColumnText(canvas);
                        compositeColumn.setUseAscender(firstPass ? useAscender : false);
                        compositeColumn.setAlignment(para.getAlignment());
                        compositeColumn.setIndent(para.getIndentationLeft() + para.getFirstLineIndent());
                        compositeColumn.setExtraParagraphSpace(para.getExtraParagraphSpace());
                        compositeColumn.setFollowingIndent(para.getIndentationLeft());
                        compositeColumn.setRightIndent(para.getIndentationRight());
                        compositeColumn.setLeading(para.getLeading(), para.getMultipliedLeading());
                        compositeColumn.setRunDirection(runDirection);
                        compositeColumn.setArabicOptions(arabicOptions);
                        compositeColumn.setSpaceCharRatio(spaceCharRatio);
                        compositeColumn.addText(para);
                        if (!firstPass) {
                            yLine -= para.spacingBefore();
                        }
                        createHere = true;
                    }
                    compositeColumn.leftX = leftX;
                    compositeColumn.rightX = rightX;
                    compositeColumn.yLine = yLine;
                    compositeColumn.rectangularWidth = rectangularWidth;
                    compositeColumn.rectangularMode = rectangularMode;
                    compositeColumn.minY = minY;
                    compositeColumn.maxY = maxY;
                    boolean keepCandidate = (para.getKeepTogether() && createHere && !firstPass);
                    status = compositeColumn.go(simulate || (keepCandidate && keep == 0));
                    updateFilledWidth(compositeColumn.filledWidth);
                    if ((status & NO_MORE_TEXT) == 0 && keepCandidate) {
                        compositeColumn = null;
                        yLine = lastY;
                        return NO_MORE_COLUMN;
                    }
                    if (simulate || !keepCandidate)
                        break;
                    if (keep == 0) {
                        compositeColumn = null;
                        yLine = lastY;
                    }
                }
                firstPass = false;
                yLine = compositeColumn.yLine;
                linesWritten += compositeColumn.linesWritten;
                descender = compositeColumn.descender;
                if ((status & NO_MORE_TEXT) != 0) {
                    compositeColumn = null;
                    compositeElements.removeFirst();
                    yLine -= para.spacingAfter();
                }
                if ((status & NO_MORE_COLUMN) != 0) {
                    return NO_MORE_COLUMN;
                }
            }
            else if (element.type() == Element.LIST) {
                com.lowagie.text.List list = (com.lowagie.text.List)element;
                ArrayList items = list.getItems();
                ListItem item = null;
                float listIndentation = list.getIndentationLeft();
                int count = 0;
                Stack stack = new Stack();
                for (int k = 0; k < items.size(); ++k) {
                    Object obj = items.get(k);
                    if (obj instanceof ListItem) {
                        if (count == listIdx) {
                            item = (ListItem)obj;
                            break;
                        }
                        else ++count;
                    }
                    else if (obj instanceof com.lowagie.text.List) {
                        stack.push(new Object[]{list, new Integer(k), new Float(listIndentation)});
                        list = (com.lowagie.text.List)obj;
                        items = list.getItems();
                        listIndentation += list.getIndentationLeft();
                        k = -1;
                        continue;
                    }
                    if (k == items.size() - 1) {
                        if (!stack.isEmpty()) {
                            Object objs[] = (Object[])stack.pop();
                            list = (com.lowagie.text.List)objs[0];
                            items = list.getItems();
                            k = ((Integer)objs[1]).intValue();
                            listIndentation = ((Float)objs[2]).floatValue();
                        }
                    }
                }
                int status = 0;
                for (int keep = 0; keep < 2; ++keep) {
                    float lastY = yLine;
                    boolean createHere = false;
                    if (compositeColumn == null) {
                        if (item == null) {
                            listIdx = 0;
                            compositeElements.removeFirst();
                            continue main_loop;
                        }
                        compositeColumn = new ColumnText(canvas);
                        compositeColumn.setUseAscender(firstPass ? useAscender : false);
                        compositeColumn.setAlignment(item.getAlignment());
                        compositeColumn.setIndent(item.getIndentationLeft() + listIndentation + item.getFirstLineIndent());
                        compositeColumn.setExtraParagraphSpace(item.getExtraParagraphSpace());
                        compositeColumn.setFollowingIndent(compositeColumn.getIndent());
                        compositeColumn.setRightIndent(item.getIndentationRight() + list.getIndentationRight());
                        compositeColumn.setLeading(item.getLeading(), item.getMultipliedLeading());
                        compositeColumn.setRunDirection(runDirection);
                        compositeColumn.setArabicOptions(arabicOptions);
                        compositeColumn.setSpaceCharRatio(spaceCharRatio);
                        compositeColumn.addText(item);
                        if (!firstPass) {
                            yLine -= item.spacingBefore();
                        }
                        createHere = true;
                    }
                    compositeColumn.leftX = leftX;
                    compositeColumn.rightX = rightX;
                    compositeColumn.yLine = yLine;
                    compositeColumn.rectangularWidth = rectangularWidth;
                    compositeColumn.rectangularMode = rectangularMode;
                    compositeColumn.minY = minY;
                    compositeColumn.maxY = maxY;
                    boolean keepCandidate = (item.getKeepTogether() && createHere && !firstPass);
                    status = compositeColumn.go(simulate || (keepCandidate && keep == 0));
                    updateFilledWidth(compositeColumn.filledWidth);
                    if ((status & NO_MORE_TEXT) == 0 && keepCandidate) {
                        compositeColumn = null;
                        yLine = lastY;
                        return NO_MORE_COLUMN;
                    }
                    if (simulate || !keepCandidate)
                        break;
                    if (keep == 0) {
                        compositeColumn = null;
                        yLine = lastY;
                    }
                }
                firstPass = false;
                yLine = compositeColumn.yLine;
                linesWritten += compositeColumn.linesWritten;
                descender = compositeColumn.descender;
                if (!Float.isNaN(compositeColumn.firstLineY) && !compositeColumn.firstLineYDone) {
                    if (!simulate)
                        showTextAligned(canvas, Element.ALIGN_LEFT, new Phrase(item.getListSymbol()), compositeColumn.leftX + listIndentation, compositeColumn.firstLineY, 0);
                    compositeColumn.firstLineYDone = true;
                }
                if ((status & NO_MORE_TEXT) != 0) {
                    compositeColumn = null;
                    ++listIdx;
                    yLine -= item.spacingAfter();
                }
                if ((status & NO_MORE_COLUMN) != 0) {
                    return NO_MORE_COLUMN;
                }
            }
            else if (element.type() == Element.PTABLE) {
                
                if (yLine < minY || yLine > maxY)
                    return NO_MORE_COLUMN;
                
                
                PdfPTable table = (PdfPTable)element;
                
                
                if (table.size() <= table.getHeaderRows()) {
                    compositeElements.removeFirst();
                    continue;
                }
                
                
                float yTemp = yLine;
                if (!firstPass && listIdx == 0) {
                    yTemp -= table.spacingBefore();
                }
                float yLineWrite = yTemp;
                
                
                if (yTemp < minY || yTemp > maxY) {
                    return NO_MORE_COLUMN;
                }
                
                
                currentLeading = 0;
                float x1 = leftX;
                float tableWidth;
                if (table.isLockedWidth()) {
                    tableWidth = table.getTotalWidth();
                    updateFilledWidth(tableWidth);
                }
                else {
                    tableWidth = rectangularWidth * table.getWidthPercentage() / 100f;
                    table.setTotalWidth(tableWidth);
                }
                
                
                int headerRows = table.getHeaderRows();
                int footerRows = table.getFooterRows();
                if (footerRows > headerRows)
                    footerRows = headerRows;
                int realHeaderRows = headerRows - footerRows;
                float headerHeight = table.getHeaderHeight();
                float footerHeight = table.getFooterHeight();

                
                boolean skipHeader = (!firstPass && table.isSkipFirstHeader() && listIdx <= headerRows);
                if (!skipHeader) {
                    yTemp -= headerHeight;
                    if (yTemp < minY || yTemp > maxY) {
                        if (firstPass) {
                            compositeElements.removeFirst();
                            continue;
                        }
                        return NO_MORE_COLUMN;
                    }
                }
                
                
                int k;
                if (listIdx < headerRows) {
                    listIdx = headerRows;
                }
                if (!table.isComplete()) {
                    yTemp -= footerHeight;
                }
                for (k = listIdx; k < table.size(); ++k) {
                    float rowHeight = table.getRowHeight(k);
                    if (yTemp - rowHeight < minY)
                        break;
                    yTemp -= rowHeight;
                }
                if (!table.isComplete()) {
                    yTemp += footerHeight;
                }
                
                if (k < table.size()) {
                    if (table.isSplitRows() && (!table.isSplitLate() || (k == listIdx && firstPass))) {
                        if (!splittedRow) {
                            splittedRow = true;
                            table = new PdfPTable(table);
                            compositeElements.set(0, table);
                            ArrayList rows = table.getRows();
                            for (int i = headerRows; i < listIdx; ++i)
                                rows.set(i, null);
                        }
                        float h = yTemp - minY;
                        PdfPRow newRow = table.getRow(k).splitRow(h);
                        if (newRow == null) {
                            if (k == listIdx) {
                                return NO_MORE_COLUMN;
                            }
                        }
                        else {
                            yTemp = minY;
                            table.getRows().add(++k, newRow);
                        }
                    }
                    else if (!table.isSplitRows() && k == listIdx && firstPass) {
                        compositeElements.removeFirst();
                        splittedRow = false;
                        continue;
                    }
                    else if (k == listIdx && !firstPass && (!table.isSplitRows() || table.isSplitLate()) && (table.getFooterRows() == 0 || table.isComplete())) {
                        return NO_MORE_COLUMN;
                    }
                }
                
                firstPass = false;
                
                if (!simulate) {
                    
                    switch (table.getHorizontalAlignment()) {
                        case Element.ALIGN_LEFT:
                            break;
                        case Element.ALIGN_RIGHT:
                            x1 += rectangularWidth - tableWidth;
                            break;
                        default:
                            x1 += (rectangularWidth - tableWidth) / 2f;
                    }
                    
                    PdfPTable nt = PdfPTable.shallowCopy(table);
                    ArrayList rows = table.getRows();
                    ArrayList sub = nt.getRows();
                    
                    
                    if (!skipHeader) {
                        for (int j = 0; j < realHeaderRows; ++j) {
                            PdfPRow headerRow = (PdfPRow)rows.get(j);
                            sub.add(headerRow);
                        }
                    }
                    else {
                        nt.setHeaderRows(footerRows);
                    }
                    
                    for (int j = listIdx; j < k; ++j) {
                        sub.add(rows.get(j));
                    }
                    
                    
                    if (k < table.size()) {
                        nt.setComplete(true);
                    }
                    
                    for (int j = 0; j < footerRows && nt.isComplete(); ++j) {
                        sub.add(rows.get(j + realHeaderRows));
                    }

                    
                    float rowHeight = 0;
                    if (table.isExtendLastRow()) {
                        PdfPRow last = (PdfPRow)sub.get(sub.size() - 1 - footerRows);
                        rowHeight = last.getMaxHeights();
                        last.setMaxHeights(yTemp - minY + rowHeight);
                        yTemp = minY;
                    }
                    
                    
                    if (canvases != null)
                        nt.writeSelectedRows(0, -1, x1, yLineWrite, canvases);
                    else
                        nt.writeSelectedRows(0, -1, x1, yLineWrite, canvas);
                    if (table.isExtendLastRow()) {
                        PdfPRow last = (PdfPRow)sub.get(sub.size() - 1 - footerRows);
                        last.setMaxHeights(rowHeight);
                    }
                }
                else if (table.isExtendLastRow() && minY > PdfPRow.BOTTOM_LIMIT) {
                    yTemp = minY;
                }
                yLine = yTemp;
                if (!(skipHeader || table.isComplete())) {
                    yLine += footerHeight;
                }
                if (k >= table.size()) {
                    yLine -= table.spacingAfter();
                    compositeElements.removeFirst();
                    splittedRow = false;
                    listIdx = 0;
                }
                else {
                    if (splittedRow) {
                        ArrayList rows = table.getRows();
                        for (int i = listIdx; i < k; ++i)
                            rows.set(i, null);
                    }
                    listIdx = k;
                    return NO_MORE_COLUMN;
                }
            }
            else
                compositeElements.removeFirst();
        }
    }
    
    
    public PdfContentByte getCanvas() {
        return canvas;
    }
    
    
    public void setCanvas(PdfContentByte canvas) {
        this.canvas = canvas;
        this.canvases = null;
        if (compositeColumn != null)
            compositeColumn.setCanvas(canvas);
    }
    
    
    public void setCanvases(PdfContentByte[] canvases) {
        this.canvases = canvases;
        this.canvas = canvases[PdfPTable.TEXTCANVAS];
        if (compositeColumn != null)
            compositeColumn.setCanvases(canvases);
    }
    
    
    public PdfContentByte[] getCanvases() {
        return canvases;
    }
    
    
    public boolean isUseAscender() {
        return useAscender;
    }

    
    public void setUseAscender(boolean use) {
        useAscender = use;
    }
    
    
    public static boolean hasMoreText(int status) {
        return (status & ColumnText.NO_MORE_TEXT) == 0;
    }

    
    private float filledWidth;

    
    public float getFilledWidth() {

        return this.filledWidth;
    }

    
    public void setFilledWidth(float filledWidth) {

        this.filledWidth = filledWidth;
    }
    
    
    public void updateFilledWidth(float w) {
        if (w > filledWidth)
            filledWidth = w;
    }

    private boolean adjustFirstLine = true;

    
    public boolean isAdjustFirstLine() {
        return this.adjustFirstLine;
    }

    
    public void setAdjustFirstLine(boolean adjustFirstLine) {
        this.adjustFirstLine = adjustFirstLine;
    }
}
