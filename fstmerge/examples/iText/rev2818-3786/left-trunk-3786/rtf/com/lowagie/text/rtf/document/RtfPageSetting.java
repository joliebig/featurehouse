

package com.lowagie.text.rtf.document;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.PageSize;
import com.lowagie.text.Rectangle;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;



public class RtfPageSetting extends RtfElement implements RtfExtendedElement {

    
    private static final byte[] PAGE_WIDTH = DocWriter.getISOBytes("\\paperw");
    
    private static final byte[] PAGE_HEIGHT = DocWriter.getISOBytes("\\paperh");
    
    private static final byte[] MARGIN_LEFT = DocWriter.getISOBytes("\\margl");
    
    private static final byte[] MARGIN_RIGHT = DocWriter.getISOBytes("\\margr");
    
    private static final byte[] MARGIN_TOP = DocWriter.getISOBytes("\\margt");
    
    private static final byte[] MARGIN_BOTTOM = DocWriter.getISOBytes("\\margb");
    
    private static final byte[] LANDSCAPE = DocWriter.getISOBytes("\\lndscpsxn");
    
    private static final byte[] SECTION_PAGE_WIDTH = DocWriter.getISOBytes("\\pgwsxn");
    
    private static final byte[] SECTION_PAGE_HEIGHT = DocWriter.getISOBytes("\\pghsxn");
    
    private static final byte[] SECTION_MARGIN_LEFT = DocWriter.getISOBytes("\\marglsxn");
    
    private static final byte[] SECTION_MARGIN_RIGHT = DocWriter.getISOBytes("\\margrsxn");
    
    private static final byte[] SECTION_MARGIN_TOP = DocWriter.getISOBytes("\\margtsxn");
    
    private static final byte[] SECTION_MARGIN_BOTTOM = DocWriter.getISOBytes("\\margbsxn");
    
    
    private int pageWidth = 11906;
    
    private int pageHeight = 16840;
    
    private int marginLeft = 1800;
    
    private int marginRight = 1800;
    
    private int marginTop = 1440;
    
    private int marginBottom = 1440;
    
    private boolean landscape = false;

    
    public RtfPageSetting(RtfDocument doc) {
        super(doc);
    }
    
    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(PAGE_WIDTH);
        result.write(intToByteArray(pageWidth));
        result.write(PAGE_HEIGHT);
        result.write(intToByteArray(pageHeight));
        result.write(MARGIN_LEFT);
        result.write(intToByteArray(marginLeft));
        result.write(MARGIN_RIGHT);
        result.write(intToByteArray(marginRight));
        result.write(MARGIN_TOP);
        result.write(intToByteArray(marginTop));
        result.write(MARGIN_BOTTOM);
        result.write(intToByteArray(marginBottom));
        this.document.outputDebugLinebreak(result);        
    }
    
    
    public void writeSectionDefinition(final OutputStream result) throws IOException
    {
        if(landscape) {
            result.write(LANDSCAPE);
            result.write(SECTION_PAGE_WIDTH);
            result.write(intToByteArray(pageWidth));
            result.write(SECTION_PAGE_HEIGHT);
            result.write(intToByteArray(pageHeight));
            this.document.outputDebugLinebreak(result);
        } else {
            result.write(SECTION_PAGE_WIDTH);
            result.write(intToByteArray(pageWidth));
            result.write(SECTION_PAGE_HEIGHT);
            result.write(intToByteArray(pageHeight));
            this.document.outputDebugLinebreak(result);
        }
        result.write(SECTION_MARGIN_LEFT);
        result.write(intToByteArray(marginLeft));
        result.write(SECTION_MARGIN_RIGHT);
        result.write(intToByteArray(marginRight));
        result.write(SECTION_MARGIN_TOP);
        result.write(intToByteArray(marginTop));
        result.write(SECTION_MARGIN_BOTTOM);
        result.write(intToByteArray(marginBottom));        
    }

    
    public int getMarginBottom() {
        return marginBottom;
    }
    
    
    public void setMarginBottom(int marginBottom) {
        this.marginBottom = marginBottom;
    }
    
    
    public int getMarginLeft() {
        return marginLeft;
    }
    
    
    public void setMarginLeft(int marginLeft) {
        this.marginLeft = marginLeft;
    }
    
    
    public int getMarginRight() {
        return marginRight;
    }
    
    
    public void setMarginRight(int marginRight) {
        this.marginRight = marginRight;
    }
    
    
    public int getMarginTop() {
        return marginTop;
    }
    
    
    public void setMarginTop(int marginTop) {
        this.marginTop = marginTop;
    }
    
    
    public int getPageHeight() {
        return pageHeight;
    }
    
    
    public void setPageHeight(int pageHeight) {
        this.pageHeight = pageHeight;
    }
    
    
    public int getPageWidth() {
        return pageWidth;
    }
    
    
    public void setPageWidth(int pageWidth) {
        this.pageWidth = pageWidth;
    }
    
    
    public void setPageSize(Rectangle pageSize) {
        if(!guessFormat(pageSize, false)) {
            this.pageWidth = (int) (pageSize.getWidth() * RtfElement.TWIPS_FACTOR);
            this.pageHeight = (int) (pageSize.getHeight() * RtfElement.TWIPS_FACTOR);
            this.landscape = pageWidth > pageHeight;
        }
    }
    
    
    private boolean guessFormat(Rectangle pageSize, boolean rotate) {
        if (rotate) {
            pageSize = pageSize.rotate();
        }
        if (rectEquals(pageSize, PageSize.A3)) {
            pageWidth = 16837;
            pageHeight = 23811;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.A4)) {
            pageWidth = 11907;
            pageHeight = 16840;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.A5)) {
            pageWidth = 8391;
            pageHeight = 11907;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.A6)) {
            pageWidth = 5959;
            pageHeight = 8420;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.B4)) {
            pageWidth = 14570;
            pageHeight = 20636;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.B5)) {
            pageWidth = 10319;
            pageHeight = 14572;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.HALFLETTER)) {
            pageWidth = 7927;
            pageHeight = 12247;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.LETTER)) {
            pageWidth = 12242;
            pageHeight = 15842;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.LEGAL)) {
            pageWidth = 12252;
            pageHeight = 20163;
            landscape = rotate;
            return true;
        }
        if (!rotate && guessFormat(pageSize, true)) {
            int x = pageWidth;
            pageWidth = pageHeight;
            pageHeight = x;
            return true;
        }
        return false;
    }

    
    private boolean rectEquals(Rectangle rect1, Rectangle rect2) {
        return (rect1.getWidth() == rect2.getWidth()) && (rect1.getHeight() == rect2.getHeight());
    }
}
