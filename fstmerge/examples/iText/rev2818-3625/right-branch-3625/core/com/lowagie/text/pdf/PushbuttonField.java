
package com.lowagie.text.pdf;

import java.io.IOException;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;

public class PushbuttonField extends BaseField {
   
        
    public static final int LAYOUT_LABEL_ONLY = 1;
        
    public static final int LAYOUT_ICON_ONLY = 2;
        
    public static final int LAYOUT_ICON_TOP_LABEL_BOTTOM = 3;
        
    public static final int LAYOUT_LABEL_TOP_ICON_BOTTOM = 4;
        
    public static final int LAYOUT_ICON_LEFT_LABEL_RIGHT = 5;
        
    public static final int LAYOUT_LABEL_LEFT_ICON_RIGHT = 6;
        
    public static final int LAYOUT_LABEL_OVER_ICON = 7;
        
    public static final int SCALE_ICON_ALWAYS  = 1;
        
    public static final int SCALE_ICON_NEVER = 2;
        
    public static final int SCALE_ICON_IS_TOO_BIG = 3;
        
    public static final int SCALE_ICON_IS_TOO_SMALL = 4;

    
    private int layout = LAYOUT_LABEL_ONLY;
    
    
    private Image image;    
    
    
    private PdfTemplate template;
    
    
    private int scaleIcon = SCALE_ICON_ALWAYS;
    
    
    private boolean proportionalIcon = true;
    
    
    private float iconVerticalAdjustment = 0.5f;
    
    
    private float iconHorizontalAdjustment = 0.5f;
    
    
    private boolean iconFitToBounds;
    
    private PdfTemplate tp;
    
    
    public PushbuttonField(PdfWriter writer, Rectangle box, String fieldName) {
        super(writer, box, fieldName);
    }
    
    
    public int getLayout() {
        return this.layout;
    }
    
    
    public void setLayout(int layout) {
        if (layout < LAYOUT_LABEL_ONLY || layout > LAYOUT_LABEL_OVER_ICON)
            throw new IllegalArgumentException("Layout out of bounds.");
        this.layout = layout;
    }
    
    
    public Image getImage() {
        return this.image;
    }
    
    
    public void setImage(Image image) {
        this.image = image;
        template = null;
    }
    
    
    public PdfTemplate getTemplate() {
        return this.template;
    }
    
    
    public void setTemplate(PdfTemplate template) {
        this.template = template;
        image = null;
    }
    
    
    public int getScaleIcon() {
        return this.scaleIcon;
    }
    
    
    public void setScaleIcon(int scaleIcon) {
        if (scaleIcon < SCALE_ICON_ALWAYS || scaleIcon > SCALE_ICON_IS_TOO_SMALL)
            scaleIcon = SCALE_ICON_ALWAYS;
        this.scaleIcon = scaleIcon;
    }
    
    
    public boolean isProportionalIcon() {
        return this.proportionalIcon;
    }
    
    
    public void setProportionalIcon(boolean proportionalIcon) {
        this.proportionalIcon = proportionalIcon;
    }
    
    
    public float getIconVerticalAdjustment() {
        return this.iconVerticalAdjustment;
    }
    
    
    public void setIconVerticalAdjustment(float iconVerticalAdjustment) {
        if (iconVerticalAdjustment < 0)
            iconVerticalAdjustment = 0;
        else if (iconVerticalAdjustment > 1)
            iconVerticalAdjustment = 1;
        this.iconVerticalAdjustment = iconVerticalAdjustment;
    }
    
    
    public float getIconHorizontalAdjustment() {
        return this.iconHorizontalAdjustment;
    }
    
    
    public void setIconHorizontalAdjustment(float iconHorizontalAdjustment) {
        if (iconHorizontalAdjustment < 0)
            iconHorizontalAdjustment = 0;
        else if (iconHorizontalAdjustment > 1)
            iconHorizontalAdjustment = 1;
        this.iconHorizontalAdjustment = iconHorizontalAdjustment;
    }
    
    private float calculateFontSize(float w, float h) throws IOException, DocumentException {
        BaseFont ufont = getRealFont();
        float fsize = fontSize;
        if (fsize == 0) {
            float bw = ufont.getWidthPoint(text, 1);
            if (bw == 0)
                fsize = 12;
            else
                fsize = w / bw;
            float nfsize = h / (1 - ufont.getFontDescriptor(BaseFont.DESCENT, 1));
            fsize = Math.min(fsize, nfsize);
            if (fsize < 4)
                fsize = 4;
        }
        return fsize;
    }
    
        
    public PdfAppearance getAppearance() throws IOException, DocumentException {
        PdfAppearance app = getBorderAppearance();
        Rectangle box = new Rectangle(app.getBoundingBox());
        if ((text == null || text.length() == 0) && (layout == LAYOUT_LABEL_ONLY || (image == null && template == null && iconReference == null))) {
            return app;
        }
        if (layout == LAYOUT_ICON_ONLY && image == null && template == null && iconReference == null)
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
        tp = null;
        float textX = Float.NaN;
        float textY = 0;
        float fsize = fontSize;
        float wt = box.getWidth() - 2 * offX - 2;
        float ht = box.getHeight() - 2 * offX;
        float adj = (iconFitToBounds ? 0 : offX + 1);
        int nlayout = layout;
        if (image == null && template == null && iconReference == null)
            nlayout = LAYOUT_LABEL_ONLY;
        Rectangle iconBox = null;
        while (true) {
            switch (nlayout) {
                case LAYOUT_LABEL_ONLY:
                case LAYOUT_LABEL_OVER_ICON:
                    if (text != null && text.length() > 0 && wt > 0 && ht > 0) {
                        fsize = calculateFontSize(wt, ht);
                        textX = (box.getWidth() - ufont.getWidthPoint(text, fsize)) / 2;
                        textY = (box.getHeight() - ufont.getFontDescriptor(BaseFont.ASCENT, fsize)) / 2;
                    }
                case LAYOUT_ICON_ONLY:
                    if (nlayout == LAYOUT_LABEL_OVER_ICON || nlayout == LAYOUT_ICON_ONLY)
                        iconBox = new Rectangle(box.getLeft() + adj, box.getBottom() + adj, box.getRight() - adj, box.getTop() - adj);
                    break;
                case LAYOUT_ICON_TOP_LABEL_BOTTOM:
                    if (text == null || text.length() == 0 || wt <= 0 || ht <= 0) {
                        nlayout = LAYOUT_ICON_ONLY;
                        continue;
                    }
                    float nht = box.getHeight() * 0.35f - offX;
                    if (nht > 0)
                        fsize = calculateFontSize(wt, nht);
                    else
                        fsize = 4;
                    textX = (box.getWidth() - ufont.getWidthPoint(text, fsize)) / 2;
                    textY = offX - ufont.getFontDescriptor(BaseFont.DESCENT, fsize);
                    iconBox = new Rectangle(box.getLeft() + adj, textY + fsize, box.getRight() - adj, box.getTop() - adj);
                    break;
                case LAYOUT_LABEL_TOP_ICON_BOTTOM:
                    if (text == null || text.length() == 0 || wt <= 0 || ht <= 0) {
                        nlayout = LAYOUT_ICON_ONLY;
                        continue;
                    }
                    nht = box.getHeight() * 0.35f - offX;
                    if (nht > 0)
                        fsize = calculateFontSize(wt, nht);
                    else
                        fsize = 4;
                    textX = (box.getWidth() - ufont.getWidthPoint(text, fsize)) / 2;
                    textY = box.getHeight() - offX - fsize;
                    if (textY < offX)
                        textY = offX;
                    iconBox = new Rectangle(box.getLeft() + adj, box.getBottom() + adj, box.getRight() - adj, textY + ufont.getFontDescriptor(BaseFont.DESCENT, fsize));
                    break;
                case LAYOUT_LABEL_LEFT_ICON_RIGHT:
                    if (text == null || text.length() == 0 || wt <= 0 || ht <= 0) {
                        nlayout = LAYOUT_ICON_ONLY;
                        continue;
                    }
                    float nw = box.getWidth() * 0.35f - offX;
                    if (nw > 0)
                        fsize = calculateFontSize(wt, nw);
                    else
                        fsize = 4;
                    if (ufont.getWidthPoint(text, fsize) >= wt) {
                        nlayout = LAYOUT_LABEL_ONLY;
                        fsize = fontSize;
                        continue;
                    }
                    textX = offX + 1;
                    textY = (box.getHeight() - ufont.getFontDescriptor(BaseFont.ASCENT, fsize)) / 2;
                    iconBox = new Rectangle(textX + ufont.getWidthPoint(text, fsize), box.getBottom() + adj, box.getRight() - adj, box.getTop() - adj);
                    break;
                case LAYOUT_ICON_LEFT_LABEL_RIGHT:
                    if (text == null || text.length() == 0 || wt <= 0 || ht <= 0) {
                        nlayout = LAYOUT_ICON_ONLY;
                        continue;
                    }
                    nw = box.getWidth() * 0.35f - offX;
                    if (nw > 0)
                        fsize = calculateFontSize(wt, nw);
                    else
                        fsize = 4;
                    if (ufont.getWidthPoint(text, fsize) >= wt) {
                        nlayout = LAYOUT_LABEL_ONLY;
                        fsize = fontSize;
                        continue;
                    }
                    textX = box.getWidth() - ufont.getWidthPoint(text, fsize) - offX - 1;
                    textY = (box.getHeight() - ufont.getFontDescriptor(BaseFont.ASCENT, fsize)) / 2;
                    iconBox = new Rectangle(box.getLeft() + adj, box.getBottom() + adj, textX - 1, box.getTop() - adj);
                    break;
            }
            break;
        }
        if (textY < box.getBottom() + offX)
            textY = box.getBottom() + offX;
        if (iconBox != null && (iconBox.getWidth() <= 0 || iconBox.getHeight() <= 0))
            iconBox = null;
        boolean haveIcon = false;
        float boundingBoxWidth = 0;
        float boundingBoxHeight = 0;
        PdfArray matrix = null;
        if (iconBox != null) {
            if (image != null) {
                tp = new PdfTemplate(writer);
                tp.setBoundingBox(new Rectangle(image));
                writer.addDirectTemplateSimple(tp, PdfName.FRM);
                tp.addImage(image, image.getWidth(), 0, 0, image.getHeight(), 0, 0);
                haveIcon = true;
                boundingBoxWidth = tp.getBoundingBox().getWidth();
                boundingBoxHeight = tp.getBoundingBox().getHeight();
            }
            else if (template != null) {
                tp = new PdfTemplate(writer);
                tp.setBoundingBox(new Rectangle(template.getWidth(), template.getHeight()));
                writer.addDirectTemplateSimple(tp, PdfName.FRM);
                tp.addTemplate(template, template.getBoundingBox().getLeft(), template.getBoundingBox().getBottom());
                haveIcon = true;
                boundingBoxWidth = tp.getBoundingBox().getWidth();
                boundingBoxHeight = tp.getBoundingBox().getHeight();
            }
            else if (iconReference != null) {
                PdfDictionary dic = (PdfDictionary)PdfReader.getPdfObject(iconReference);
                if (dic != null) {
                    Rectangle r2 = PdfReader.getNormalizedRectangle((PdfArray)PdfReader.getPdfObject(dic.get(PdfName.BBOX)));
                    matrix = (PdfArray)PdfReader.getPdfObject(dic.get(PdfName.MATRIX));
                    haveIcon = true;
                    boundingBoxWidth = r2.getWidth();
                    boundingBoxHeight = r2.getHeight();
                }
            }
        }
        if (haveIcon) {
            float icx = iconBox.getWidth() / boundingBoxWidth;
            float icy = iconBox.getHeight() / boundingBoxHeight;
            if (proportionalIcon) {
                switch (scaleIcon) {
                    case SCALE_ICON_IS_TOO_BIG:
                        icx = Math.min(icx, icy);
                        icx = Math.min(icx, 1);
                        break;
                    case SCALE_ICON_IS_TOO_SMALL:
                        icx = Math.min(icx, icy);
                        icx = Math.max(icx, 1);
                        break;
                    case SCALE_ICON_NEVER:
                        icx = 1;
                        break;
                    default:
                        icx = Math.min(icx, icy);
                        break;
                }
                icy = icx;
            }
            else {
                switch (scaleIcon) {
                    case SCALE_ICON_IS_TOO_BIG:
                        icx = Math.min(icx, 1);
                        icy = Math.min(icy, 1);
                        break;
                    case SCALE_ICON_IS_TOO_SMALL:
                        icx = Math.max(icx, 1);
                        icy = Math.max(icy, 1);
                        break;
                    case SCALE_ICON_NEVER:
                        icx = icy = 1;
                        break;
                    default:
                        break;
                }
            }
            float xpos = iconBox.getLeft() + (iconBox.getWidth() - (boundingBoxWidth * icx)) * iconHorizontalAdjustment;
            float ypos = iconBox.getBottom() + (iconBox.getHeight() - (boundingBoxHeight * icy)) * iconVerticalAdjustment;
            app.saveState();
            app.rectangle(iconBox.getLeft(), iconBox.getBottom(), iconBox.getWidth(), iconBox.getHeight());
            app.clip();
            app.newPath();
            if (tp != null)
                app.addTemplate(tp, icx, 0, 0, icy, xpos, ypos);
            else {
                float cox = 0;
                float coy = 0;
                if (matrix != null && matrix.size() == 6) {
                    PdfNumber nm = (PdfNumber)PdfReader.getPdfObject(matrix.getArrayList().get(4));
                    if (nm != null)
                        cox = nm.floatValue();
                    nm = (PdfNumber)PdfReader.getPdfObject(matrix.getArrayList().get(5));
                    if (nm != null)
                        coy = nm.floatValue();
                }
                app.addTemplateReference(iconReference, PdfName.FRM, icx, 0, 0, icy, xpos - cox * icx, ypos - coy * icy);
            }
            app.restoreState();
        }
        if (!Float.isNaN(textX)) {
            app.saveState();
            app.rectangle(offX, offX, box.getWidth() - 2 * offX, box.getHeight() - 2 * offX);
            app.clip();
            app.newPath();
            if (textColor == null)
                app.resetGrayFill();
            else
                app.setColorFill(textColor);
            app.beginText();
            app.setFontAndSize(ufont, fsize);
            app.setTextMatrix(textX, textY);
            app.showText(text);
            app.endText();
            app.restoreState();
        }
        return app;
    }

        
    public PdfFormField getField() throws IOException, DocumentException {
        PdfFormField field = PdfFormField.createPushButton(writer);
        field.setWidget(box, PdfAnnotation.HIGHLIGHT_INVERT);
        if (fieldName != null) {
            field.setFieldName(fieldName);
            if ((options & READ_ONLY) != 0)
                field.setFieldFlags(PdfFormField.FF_READ_ONLY);
            if ((options & REQUIRED) != 0)
                field.setFieldFlags(PdfFormField.FF_REQUIRED);
        }
        if (text != null)
            field.setMKNormalCaption(text);
        if (rotation != 0)
            field.setMKRotation(rotation);
        field.setBorderStyle(new PdfBorderDictionary(borderWidth, borderStyle, new PdfDashPattern(3)));
        PdfAppearance tpa = getAppearance();
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, tpa);
        PdfAppearance da = (PdfAppearance)tpa.getDuplicate();
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
        if (tp != null)
            field.setMKNormalIcon(tp);
        field.setMKTextPosition(layout - 1);
        PdfName scale = PdfName.A;
        if (scaleIcon == SCALE_ICON_IS_TOO_BIG)
            scale = PdfName.B;
        else if (scaleIcon == SCALE_ICON_IS_TOO_SMALL)
            scale = PdfName.S;
        else if (scaleIcon == SCALE_ICON_NEVER)
            scale = PdfName.N;
        field.setMKIconFit(scale, proportionalIcon ? PdfName.P : PdfName.A, iconHorizontalAdjustment,
            iconVerticalAdjustment, iconFitToBounds);
        return field;
    }
    
    
    public boolean isIconFitToBounds() {
        return this.iconFitToBounds;
    }
    
    
    public void setIconFitToBounds(boolean iconFitToBounds) {
        this.iconFitToBounds = iconFitToBounds;
    }

    
    private PRIndirectReference iconReference;

    
    public PRIndirectReference getIconReference() {
        return this.iconReference;
    }

    
    public void setIconReference(PRIndirectReference iconReference) {
        this.iconReference = iconReference;
    }
    
}