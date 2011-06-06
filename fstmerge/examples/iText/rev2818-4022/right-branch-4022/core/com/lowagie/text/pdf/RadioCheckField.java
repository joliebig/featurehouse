
package com.lowagie.text.pdf;

import java.io.IOException;

import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;


public class RadioCheckField extends BaseField {

    
    public static final int TYPE_CHECK = 1;
    
    public static final int TYPE_CIRCLE = 2;
    
    public static final int TYPE_CROSS = 3;
    
    public static final int TYPE_DIAMOND = 4;
    
    public static final int TYPE_SQUARE = 5;
    
    public static final int TYPE_STAR = 6;
    
    private static String typeChars[] = {"4", "l", "8", "u", "n", "H"};
    
    
    private int checkType;
    
    
    private String onValue;
    
    
    private boolean checked;
    
    
    public RadioCheckField(PdfWriter writer, Rectangle box, String fieldName, String onValue) {
        super(writer, box, fieldName);
        setOnValue(onValue);
        setCheckType(TYPE_CIRCLE);
    }
    
    
    public int getCheckType() {
        return this.checkType;
    }
    
    
    public void setCheckType(int checkType) {
        if (checkType < TYPE_CHECK || checkType > TYPE_STAR)
            checkType = TYPE_CIRCLE;
        this.checkType = checkType;
        setText(typeChars[checkType - 1]);
        try {
            setFont(BaseFont.createFont(BaseFont.ZAPFDINGBATS, BaseFont.WINANSI, false));
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    
    public String getOnValue() {
        return this.onValue;
    }
    
    
    public void setOnValue(String onValue) {
        this.onValue = onValue;
    }
    
    
    public boolean isChecked() {
        return this.checked;
    }
    
    
    public void setChecked(boolean checked) {
        this.checked = checked;
    }
    
        
    public PdfAppearance getAppearance(boolean isRadio, boolean on) throws IOException, DocumentException {
        if (isRadio && checkType == TYPE_CIRCLE)
            return getAppearanceRadioCircle(on);
        PdfAppearance app = getBorderAppearance();
        if (!on)
            return app;
        BaseFont ufont = getRealFont();
        boolean borderExtra = borderStyle == PdfBorderDictionary.STYLE_BEVELED || borderStyle == PdfBorderDictionary.STYLE_INSET;
        float h = box.getHeight() - borderWidth * 2;
        float bw2 = borderWidth;
        if (borderExtra) {
            h -= borderWidth * 2;
            bw2 *= 2;
        }
        float offsetX = (borderExtra ? 2 * borderWidth : borderWidth);
        offsetX = Math.max(offsetX, 1);
        float offX = Math.min(bw2, offsetX);
        float wt = box.getWidth() - 2 * offX;
        float ht = box.getHeight() - 2 * offX;
        float fsize = fontSize;
        if (fsize == 0) {
            float bw = ufont.getWidthPoint(text, 1);
            if (bw == 0)
                fsize = 12;
            else
                fsize = wt / bw;
            float nfsize = h / (ufont.getFontDescriptor(BaseFont.ASCENT, 1));
            fsize = Math.min(fsize, nfsize);
        }
        app.saveState();
        app.rectangle(offX, offX, wt, ht);
        app.clip();
        app.newPath();
        if (textColor == null)
            app.resetGrayFill();
        else
            app.setColorFill(textColor);
        app.beginText();
        app.setFontAndSize(ufont, fsize);
        app.setTextMatrix((box.getWidth() - ufont.getWidthPoint(text, fsize)) / 2, 
            (box.getHeight() - ufont.getAscentPoint(text, fsize)) / 2);
        app.showText(text);
        app.endText();
        app.restoreState();
        return app;
    }

        
    public PdfAppearance getAppearanceRadioCircle(boolean on) {
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
        Rectangle box = new Rectangle(app.getBoundingBox());
        float cx = box.getWidth() / 2;
        float cy = box.getHeight() / 2;
        float r = (Math.min(box.getWidth(), box.getHeight()) - borderWidth) / 2;
        if (r <= 0)
            return app;
        if (backgroundColor != null) {
            app.setColorFill(backgroundColor);
            app.circle(cx, cy, r + borderWidth / 2);
            app.fill();
        }
        if (borderWidth > 0 && borderColor != null) {
            app.setLineWidth(borderWidth);
            app.setColorStroke(borderColor);
            app.circle(cx, cy, r);
            app.stroke();
        }
        if (on) {
            if (textColor == null)
                app.resetGrayFill();
            else
                app.setColorFill(textColor);
            app.circle(cx, cy, r / 2);
            app.fill();
        }
        return app;
    }
    
        
    public PdfFormField getRadioGroup(boolean noToggleToOff, boolean radiosInUnison) {
        PdfFormField field = PdfFormField.createRadioButton(writer, noToggleToOff);
        if (radiosInUnison)
            field.setFieldFlags(PdfFormField.FF_RADIOSINUNISON);
        field.setFieldName(fieldName);
        if ((options & READ_ONLY) != 0)
            field.setFieldFlags(PdfFormField.FF_READ_ONLY);
        if ((options & REQUIRED) != 0)
            field.setFieldFlags(PdfFormField.FF_REQUIRED);
        field.setValueAsName(checked ? onValue : "Off");
        return field;
    }
    
        
    public PdfFormField getRadioField() throws IOException, DocumentException {
        return getField(true);
    }
    
        
    public PdfFormField getCheckField() throws IOException, DocumentException {
        return getField(false);
    }
    
        
    protected PdfFormField getField(boolean isRadio) throws IOException, DocumentException {
        PdfFormField field = null;
        if (isRadio)
            field = PdfFormField.createEmpty(writer);
        else
            field = PdfFormField.createCheckBox(writer);
        field.setWidget(box, PdfAnnotation.HIGHLIGHT_INVERT);
        if (!isRadio) {
            field.setFieldName(fieldName);
            if ((options & READ_ONLY) != 0)
                field.setFieldFlags(PdfFormField.FF_READ_ONLY);
            if ((options & REQUIRED) != 0)
                field.setFieldFlags(PdfFormField.FF_REQUIRED);
            field.setValueAsName(checked ? onValue : "Off");
        }
        if (text != null)
            field.setMKNormalCaption(text);
        if (rotation != 0)
            field.setMKRotation(rotation);
        field.setBorderStyle(new PdfBorderDictionary(borderWidth, borderStyle, new PdfDashPattern(3)));
        PdfAppearance tpon = getAppearance(isRadio, true);
        PdfAppearance tpoff = getAppearance(isRadio, false);
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, onValue, tpon);
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, "Off", tpoff);
        field.setAppearanceState(checked ? onValue : "Off");
        PdfAppearance da = (PdfAppearance)tpon.getDuplicate();
        da.setFontAndSize(getRealFont(), fontSize);
        if (textColor == null)
            da.setGrayFill(0);
        else
            da.setColorFill(textColor);
        field.setDefaultAppearanceString(da);
        if (borderColor != null)
            field.setMKBorderColor(borderColor);
        if (backgroundColor != null)
            field.setMKBackgroundColor(backgroundColor);
        switch (visibility) {
            case HIDDEN:
                field.setFlags(PdfAnnotation.FLAGS_PRINT | PdfAnnotation.FLAGS_HIDDEN);
                break;
            case VISIBLE_BUT_DOES_NOT_PRINT:
                break;
            case HIDDEN_BUT_PRINTABLE:
                field.setFlags(PdfAnnotation.FLAGS_PRINT | PdfAnnotation.FLAGS_NOVIEW);
                break;
            default:
                field.setFlags(PdfAnnotation.FLAGS_PRINT);
                break;
        }
        return field;
    }
}