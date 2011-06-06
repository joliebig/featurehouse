

package com.lowagie.text.pdf;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Rectangle;


public abstract class BaseField {
    
        
    public static final float BORDER_WIDTH_THIN = 1;
        
    public static final float BORDER_WIDTH_MEDIUM = 2;
        
    public static final float BORDER_WIDTH_THICK = 3;
        
    public static final int VISIBLE = 0;
        
    public static final int HIDDEN = 1;
        
    public static final int VISIBLE_BUT_DOES_NOT_PRINT = 2;
        
    public static final int HIDDEN_BUT_PRINTABLE = 3;
        
    public static final int READ_ONLY = PdfFormField.FF_READ_ONLY;
        
    public static final int REQUIRED = PdfFormField.FF_REQUIRED;
        
    public static final int MULTILINE = PdfFormField.FF_MULTILINE;
        
    public static final int DO_NOT_SCROLL = PdfFormField.FF_DONOTSCROLL;
        
    public static final int PASSWORD = PdfFormField.FF_PASSWORD;
        
    public static final int FILE_SELECTION = PdfFormField.FF_FILESELECT;
        
    public static final int DO_NOT_SPELL_CHECK = PdfFormField.FF_DONOTSPELLCHECK;
        
    public static final int EDIT = PdfFormField.FF_EDIT;

    
    public static final int COMB = PdfFormField.FF_COMB;

    protected float borderWidth = BORDER_WIDTH_THIN;
    protected int borderStyle = PdfBorderDictionary.STYLE_SOLID;
    protected Color borderColor;
    protected Color backgroundColor;
    protected Color textColor;
    protected BaseFont font;
    protected float fontSize = 0;
    protected int alignment = Element.ALIGN_LEFT;
    protected PdfWriter writer;
    protected String text;
    protected Rectangle box;
    
    
    protected int rotation = 0;
    
    
    protected int visibility;
    
    
    protected String fieldName;
    
    
    protected int options;
    
    
    protected int maxCharacterLength;
    
    private final static HashMap fieldKeys = new HashMap();
 
    static {
        fieldKeys.putAll(PdfCopyFieldsImp.fieldKeys);
        fieldKeys.put(PdfName.T, new Integer(1));
    }
    
    public BaseField(PdfWriter writer, Rectangle box, String fieldName) {
        this.writer = writer;
        setBox(box);
        this.fieldName = fieldName;
    }
    
    protected BaseFont getRealFont() throws IOException, DocumentException {
        if (font == null)
            return BaseFont.createFont(BaseFont.HELVETICA, BaseFont.WINANSI, false);
        else
            return font;
    }
    
    protected PdfAppearance getBorderAppearance() {
        PdfAppearance app = PdfAppearance.createAppearance(writer, box.getWidth(), box.getHeight());
        switch (rotation) {
            case 90:
                app.setMatrix(0, 1, -1, 0, box.getHeight(), 0);
                break;
            case 180:
                app.setMatrix(-1, 0, 0, -1, box.getWidth(), box.getHeight());
                break;
            case 270:
                app.setMatrix(0, -1, 1, 0, 0, box.getWidth());
                break;
        }
        
        if (backgroundColor != null) {
            app.setColorFill(backgroundColor);
            app.rectangle(0, 0, box.getWidth(), box.getHeight());
            app.fill();
        }
        
        if (borderStyle == PdfBorderDictionary.STYLE_UNDERLINE) {
            if (borderWidth != 0 && borderColor != null) {
                app.setColorStroke(borderColor);
                app.setLineWidth(borderWidth);
                app.moveTo(0, borderWidth / 2);
                app.lineTo(box.getWidth(), borderWidth / 2);
                app.stroke();
            }
        }
        else if (borderStyle == PdfBorderDictionary.STYLE_BEVELED) {
            if (borderWidth != 0 && borderColor != null) {
                app.setColorStroke(borderColor);
                app.setLineWidth(borderWidth);
                app.rectangle(borderWidth / 2, borderWidth / 2, box.getWidth() - borderWidth, box.getHeight() - borderWidth);
                app.stroke();
            }
            
            Color actual = backgroundColor;
            if (actual == null)
                actual = Color.white;
            app.setGrayFill(1);
            drawTopFrame(app);
            app.setColorFill(actual.darker());
            drawBottomFrame(app);
        }
        else if (borderStyle == PdfBorderDictionary.STYLE_INSET) {
            if (borderWidth != 0 && borderColor != null) {
                app.setColorStroke(borderColor);
                app.setLineWidth(borderWidth);
                app.rectangle(borderWidth / 2, borderWidth / 2, box.getWidth() - borderWidth, box.getHeight() - borderWidth);
                app.stroke();
            }
            
            app.setGrayFill(0.5f);
            drawTopFrame(app);
            app.setGrayFill(0.75f);
            drawBottomFrame(app);
        }
        else {
            if (borderWidth != 0 && borderColor != null) {
                if (borderStyle == PdfBorderDictionary.STYLE_DASHED)
                    app.setLineDash(3, 0);
                app.setColorStroke(borderColor);
                app.setLineWidth(borderWidth);
                app.rectangle(borderWidth / 2, borderWidth / 2, box.getWidth() - borderWidth, box.getHeight() - borderWidth);
                app.stroke();
                if ((options & COMB) != 0 && maxCharacterLength > 1) {
                    float step = box.getWidth() / maxCharacterLength;
                    float yb = borderWidth / 2;
                    float yt = box.getHeight() - borderWidth / 2;
                    for (int k = 1; k < maxCharacterLength; ++k) {
                        float x = step * k;
                        app.moveTo(x, yb);
                        app.lineTo(x, yt);
                    }
                    app.stroke();
                }
            }
        }
        return app;
    }
    
    protected static ArrayList getHardBreaks(String text) {
        ArrayList arr = new ArrayList();
        char cs[] = text.toCharArray();
        int len = cs.length;
        StringBuffer buf = new StringBuffer();
        for (int k = 0; k < len; ++k) {
            char c = cs[k];
            if (c == '\r') {
                if (k + 1 < len && cs[k + 1] == '\n')
                    ++k;
                arr.add(buf.toString());
                buf = new StringBuffer();
            }
            else if (c == '\n') {
                arr.add(buf.toString());
                buf = new StringBuffer();
            }
            else
                buf.append(c);
        }
        arr.add(buf.toString());
        return arr;
    }
    
    protected static void trimRight(StringBuffer buf) {
        int len = buf.length();
        while (true) {
            if (len == 0)
                return;
            if (buf.charAt(--len) != ' ')
                return;
            buf.setLength(len);
        }
    }
    
    protected static ArrayList breakLines(ArrayList breaks, BaseFont font, float fontSize, float width) {
        ArrayList lines = new ArrayList();
        StringBuffer buf = new StringBuffer();
        for (int ck = 0; ck < breaks.size(); ++ck) {
            buf.setLength(0);
            float w = 0;
            char cs[] = ((String)breaks.get(ck)).toCharArray();
            int len = cs.length;
            
            int state = 0;
            int lastspace = -1;
            char c = 0;
            int refk = 0;
            for (int k = 0; k < len; ++k) {
                c = cs[k];
                switch (state) {
                    case 0:
                        w += font.getWidthPoint(c, fontSize);
                        buf.append(c);
                        if (w > width) {
                            w = 0;
                            if (buf.length() > 1) {
                                --k;
                                buf.setLength(buf.length() - 1);
                            }
                            lines.add(buf.toString());
                            buf.setLength(0);
                            refk = k;
                            if (c == ' ')
                                state = 2;
                            else
                                state = 1;
                        }
                        else {
                            if (c != ' ')
                                state = 1;
                        }
                        break;
                    case 1:
                        w += font.getWidthPoint(c, fontSize);
                        buf.append(c);
                        if (c == ' ')
                            lastspace = k;
                        if (w > width) {
                            w = 0;
                            if (lastspace >= 0) {
                                k = lastspace;
                                buf.setLength(lastspace - refk);
                                trimRight(buf);
                                lines.add(buf.toString());
                                buf.setLength(0);
                                refk = k;
                                lastspace = -1;
                                state = 2;
                            }
                            else {
                                if (buf.length() > 1) {
                                    --k;
                                    buf.setLength(buf.length() - 1);
                                }
                                lines.add(buf.toString());
                                buf.setLength(0);
                                refk = k;
                                if (c == ' ')
                                    state = 2;
                            }
                        }
                        break;
                    case 2:
                        if (c != ' ') {
                            w = 0;
                            --k;
                            state = 1;
                        }
                        break;
                }
            }
            trimRight(buf);
            lines.add(buf.toString());
        }
        return lines;
    }
        
    private void drawTopFrame(PdfAppearance app) {
        app.moveTo(borderWidth, borderWidth);
        app.lineTo(borderWidth, box.getHeight() - borderWidth);
        app.lineTo(box.getWidth() - borderWidth, box.getHeight() - borderWidth);
        app.lineTo(box.getWidth() - 2 * borderWidth, box.getHeight() - 2 * borderWidth);
        app.lineTo(2 * borderWidth, box.getHeight() - 2 * borderWidth);
        app.lineTo(2 * borderWidth, 2 * borderWidth);
        app.lineTo(borderWidth, borderWidth);
        app.fill();
    }
    
    private void drawBottomFrame(PdfAppearance app) {
        app.moveTo(borderWidth, borderWidth);
        app.lineTo(box.getWidth() - borderWidth, borderWidth);
        app.lineTo(box.getWidth() - borderWidth, box.getHeight() - borderWidth);
        app.lineTo(box.getWidth() - 2 * borderWidth, box.getHeight() - 2 * borderWidth);
        app.lineTo(box.getWidth() - 2 * borderWidth, 2 * borderWidth);
        app.lineTo(2 * borderWidth, 2 * borderWidth);
        app.lineTo(borderWidth, borderWidth);
        app.fill();
    }
    
    public float getBorderWidth() {
        return this.borderWidth;
    }
    
    
    public void setBorderWidth(float borderWidth) {
        this.borderWidth = borderWidth;
    }
    
    
    public int getBorderStyle() {
        return this.borderStyle;
    }
    
    
    public void setBorderStyle(int borderStyle) {
        this.borderStyle = borderStyle;
    }
    
    
    public Color getBorderColor() {
        return this.borderColor;
    }
    
    
    public void setBorderColor(Color borderColor) {
        this.borderColor = borderColor;
    }
    
    
    public Color getBackgroundColor() {
        return this.backgroundColor;
    }
    
    
    public void setBackgroundColor(Color backgroundColor) {
        this.backgroundColor = backgroundColor;
    }
    
    
    public Color getTextColor() {
        return this.textColor;
    }
    
    
    public void setTextColor(Color textColor) {
        this.textColor = textColor;
    }
    
    
    public BaseFont getFont() {
        return this.font;
    }
    
    
    public void setFont(BaseFont font) {
        this.font = font;
    }
    
    
    public float getFontSize() {
        return this.fontSize;
    }
    
    
    public void setFontSize(float fontSize) {
        this.fontSize = fontSize;
    }
    
    
    public int getAlignment() {
        return this.alignment;
    }
    
    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }
    
    
    public String getText() {
        return this.text;
    }
    
    
    public void setText(String text) {
        this.text = text;
    }
    
    
    public Rectangle getBox() {
        return this.box;
    }
    
    
    public void setBox(Rectangle box) {
        if (box == null) {
            this.box = null;
        }
        else {
            this.box = new Rectangle(box);
            this.box.normalize();
        }
    }
    
    
    public int getRotation() {
        return this.rotation;
    }
    
    
    public void setRotation(int rotation) {
        if (rotation % 90 != 0)
            throw new IllegalArgumentException("Rotation must be a multiple of 90.");
        rotation %= 360;
        if (rotation < 0)
            rotation += 360;
        this.rotation = rotation;
    }
    
        
    public void setRotationFromPage(Rectangle page) {
        setRotation(page.getRotation());
    }
    
    
    public int getVisibility() {
        return this.visibility;
    }
    
    
    public void setVisibility(int visibility) {
        this.visibility = visibility;
    }
    
    
    public String getFieldName() {
        return this.fieldName;
    }
    
    
    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }
    
    
    public int getOptions() {
        return this.options;
    }
    
    
    public void setOptions(int options) {
        this.options = options;
    }
    
    
    public int getMaxCharacterLength() {
        return this.maxCharacterLength;
    }
    
    
    public void setMaxCharacterLength(int maxCharacterLength) {
        this.maxCharacterLength = maxCharacterLength;
    }
    
    
    public PdfWriter getWriter() {
        return writer;
    }
    
    
    public void setWriter(PdfWriter writer) {
        this.writer = writer;
    }
    
        
    public static void moveFields(PdfDictionary from, PdfDictionary to) {
        for (Iterator i = from.getKeys().iterator(); i.hasNext();) {
            PdfName key = (PdfName)i.next();
            if (fieldKeys.containsKey(key)) {
                if (to != null)
                    to.put(key, from.get(key));
                i.remove();
            }
        }
    }
}
