

package com.lowagie.text.pdf;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;


public class TextField extends BaseField {
    
    
    private String defaultText;
    
    
    private String[] choices;
    
    
    private String[] choiceExports;
    
    
    private int choiceSelection;
    
    private int topFirst;
    
    private float extraMarginLeft;
    private float extraMarginTop;
    
    
    public TextField(PdfWriter writer, Rectangle box, String fieldName) {
        super(writer, box, fieldName);
    }
    
    private static boolean checkRTL(String text) {
        if (text == null || text.length() == 0)
            return false;
        char[] cc = text.toCharArray();
        for (int k = 0; k < cc.length; ++k) {
            int c = cc[k];
            if (c >= 0x590 && c < 0x0780)
                return true;
        }
        return false;
    }
    
    private static void changeFontSize(Phrase p, float size) {
        for (int k = 0; k < p.size(); ++k) {
            ((Chunk)p.get(k)).getFont().setSize(size);
        }
    }
    
    private Phrase composePhrase(String text, BaseFont ufont, Color color, float fontSize) {
        Phrase phrase = null;
        if (extensionFont == null && (substitutionFonts == null || substitutionFonts.isEmpty()))
            phrase = new Phrase(new Chunk(text, new Font(ufont, fontSize, 0, color)));
        else {
            FontSelector fs = new FontSelector();
            fs.addFont(new Font(ufont, fontSize, 0, color));
            if (extensionFont != null)
                fs.addFont(new Font(extensionFont, fontSize, 0, color));
            if (substitutionFonts != null) {
                for (int k = 0; k < substitutionFonts.size(); ++k) {
                    fs.addFont(new Font(substitutionFonts.get(k), fontSize, 0, color));
                }
            }
            phrase = fs.process(text);
        }
        return phrase;
    }
    
    private static String removeCRLF(String text) {
        if (text.indexOf('\n') >= 0 || text.indexOf('\r') >= 0) {
            char[] p = text.toCharArray();
            StringBuffer sb = new StringBuffer(p.length);
            for (int k = 0; k < p.length; ++k) {
                char c = p[k];
                if (c == '\n')
                    sb.append(' ');
                else if (c == '\r') {
                    sb.append(' ');
                    if (k < p.length - 1 && p[k + 1] == '\n')
                        ++k;
                }
                else
                    sb.append(c);
            }
            return sb.toString();
        }
        return text;
    }
    
    public PdfAppearance getAppearance() throws IOException, DocumentException {
        PdfAppearance app = getBorderAppearance();
        app.beginVariableText();
        if (text == null || text.length() == 0) {
            app.endVariableText();
            return app;
        }
        BaseFont ufont = getRealFont();
        boolean borderExtra = borderStyle == PdfBorderDictionary.STYLE_BEVELED || borderStyle == PdfBorderDictionary.STYLE_INSET;
        float h = box.getHeight() - borderWidth * 2;
        float bw2 = borderWidth;
        if (borderExtra) {
            h -= borderWidth * 2;
            bw2 *= 2;
        }
        h -= extraMarginTop;
        float offsetX = (borderExtra ? 2 * borderWidth : borderWidth);
        offsetX = Math.max(offsetX, 1);
        float offX = Math.min(bw2, offsetX);
        app.saveState();
        app.rectangle(offX, offX, box.getWidth() - 2 * offX, box.getHeight() - 2 * offX);
        app.clip();
        app.newPath();
        Color fcolor = (textColor == null) ? GrayColor.GRAYBLACK : textColor;
        String ptext = text; 
        if ((options & PASSWORD) != 0) { 
            char[] pchar = new char[text.length()];
            for (int i = 0; i < text.length(); i++)
                pchar[i] = '*';
            ptext = new String(pchar);
        }
        int rtl = checkRTL(ptext) ? PdfWriter.RUN_DIRECTION_LTR : PdfWriter.RUN_DIRECTION_NO_BIDI;
        if ((options & MULTILINE) == 0) {
            ptext = removeCRLF(text);
        }
        Phrase phrase = composePhrase(ptext, ufont, fcolor, fontSize);
        if ((options & MULTILINE) != 0) {
            float usize = fontSize;
            float width = box.getWidth() - 4 * offsetX - extraMarginLeft;
            float factor = ufont.getFontDescriptor(BaseFont.BBOXURY, 1) - ufont.getFontDescriptor(BaseFont.BBOXLLY, 1);
            ColumnText ct = new ColumnText(null);
            if (usize == 0) {
                usize = h / factor;
                if (usize > 4) {
                    if (usize > 12)
                        usize = 12;
                    float step = Math.max((usize - 4) / 10, 0.2f);
                    ct.setSimpleColumn(0, -h, width, 0);
                    ct.setAlignment(alignment);
                    ct.setRunDirection(rtl);
                    for (; usize > 4; usize -= step) {
                        ct.setYLine(0);
                        changeFontSize(phrase, usize);
                        ct.setText(phrase);
                        ct.setLeading(factor * usize);
                        int status = ct.go(true);
                        if ((status & ColumnText.NO_MORE_COLUMN) == 0)
                            break;
                    }
                }
                if (usize < 4) {
                    usize = 4;
                }
            }
            changeFontSize(phrase, usize);
            ct.setCanvas(app);
            float leading = usize * factor;
            float offsetY = offsetX + h - ufont.getFontDescriptor(BaseFont.BBOXURY, usize);
            ct.setSimpleColumn(extraMarginLeft + 2 * offsetX, -20000, box.getWidth() - 2 * offsetX, offsetY + leading);
            ct.setLeading(leading);
            ct.setAlignment(alignment);
            ct.setRunDirection(rtl);
            ct.setText(phrase);
            ct.go();
        }
        else {
            float usize = fontSize;
            if (usize == 0) {
                float maxCalculatedSize = h / (ufont.getFontDescriptor(BaseFont.BBOXURX, 1) - ufont.getFontDescriptor(BaseFont.BBOXLLY, 1));
                changeFontSize(phrase, 1);
                float wd = ColumnText.getWidth(phrase, rtl, 0);
                if (wd == 0)
                    usize = maxCalculatedSize;
                else
                    usize = (box.getWidth() - extraMarginLeft - 4 * offsetX) / wd;
                if (usize > maxCalculatedSize)
                    usize = maxCalculatedSize;
                if (usize < 4)
                    usize = 4;
            }
            changeFontSize(phrase, usize);
            float offsetY = offX + ((box.getHeight() - 2*offX) - ufont.getFontDescriptor(BaseFont.ASCENT, usize)) / 2;
            if (offsetY < offX)
                offsetY = offX;
            if (offsetY - offX < -ufont.getFontDescriptor(BaseFont.DESCENT, usize)) {
                float ny = -ufont.getFontDescriptor(BaseFont.DESCENT, usize) + offX;
                float dy = box.getHeight() - offX - ufont.getFontDescriptor(BaseFont.ASCENT, usize);
                offsetY = Math.min(ny, Math.max(offsetY, dy));
            }
            if ((options & COMB) != 0 && maxCharacterLength > 0) {
                int textLen = Math.min(maxCharacterLength, ptext.length());
                int position = 0;
                if (alignment == Element.ALIGN_RIGHT) {
                    position = maxCharacterLength - textLen;
                }
                else if (alignment == Element.ALIGN_CENTER) {
                    position = (maxCharacterLength - textLen) / 2;
                }
                float step = (box.getWidth() - extraMarginLeft) / maxCharacterLength;
                float start = step / 2 + position * step;
                if (textColor == null)
                    app.setGrayFill(0);
                else
                    app.setColorFill(textColor);
                app.beginText();
                for (int k = 0; k < phrase.size(); ++k) {
                    Chunk ck = (Chunk)phrase.get(k);
                    BaseFont bf = ck.getFont().getBaseFont();
                    app.setFontAndSize(bf, usize);
                    StringBuffer sb = ck.append("");
                    for (int j = 0; j < sb.length(); ++j) {
                        String c = sb.substring(j, j + 1);
                        float wd = bf.getWidthPoint(c, usize);
                        app.setTextMatrix(extraMarginLeft + start - wd / 2, offsetY - extraMarginTop);
                        app.showText(c);
                        start += step;
                    }
                }
                app.endText();
            }
            else {
                if (alignment == Element.ALIGN_RIGHT) {
                    ColumnText.showTextAligned(app, Element.ALIGN_RIGHT, phrase, extraMarginLeft + box.getWidth() - 2 * offsetX, offsetY - extraMarginTop, 0, rtl, 0);
                }
                else if (alignment == Element.ALIGN_CENTER) {
                    ColumnText.showTextAligned(app, Element.ALIGN_CENTER, phrase, extraMarginLeft + box.getWidth() / 2, offsetY - extraMarginTop, 0, rtl, 0);
                }
                else
                    ColumnText.showTextAligned(app, Element.ALIGN_LEFT, phrase, extraMarginLeft + 2 * offsetX, offsetY - extraMarginTop, 0, rtl, 0);
            }
        }
        app.restoreState();
        app.endVariableText();
        return app;
    }

    PdfAppearance getListAppearance() throws IOException, DocumentException {
        PdfAppearance app = getBorderAppearance();
        app.beginVariableText();
        if (choices == null || choices.length == 0) {
            app.endVariableText();
            return app;
        }
        int topChoice = choiceSelection;
        if (topChoice >= choices.length) {
            topChoice = choices.length - 1;
        }
        if (topChoice < 0)
            topChoice = 0;
        BaseFont ufont = getRealFont();
        float usize = fontSize;
        if (usize == 0)
            usize = 12;
        boolean borderExtra = borderStyle == PdfBorderDictionary.STYLE_BEVELED || borderStyle == PdfBorderDictionary.STYLE_INSET;
        float h = box.getHeight() - borderWidth * 2;
        if (borderExtra)
            h -= borderWidth * 2;
        float offsetX = (borderExtra ? 2 * borderWidth : borderWidth);
        float leading = ufont.getFontDescriptor(BaseFont.BBOXURY, usize) - ufont.getFontDescriptor(BaseFont.BBOXLLY, usize);
        int maxFit = (int)(h / leading) + 1;
        int first = 0;
        int last = 0;
        last = topChoice + maxFit / 2 + 1;
        first = last - maxFit;
        if (first < 0) {
            last += first;
            first = 0;
        }

        last = first + maxFit;
        if (last > choices.length)
            last = choices.length;
        topFirst = first;
        app.saveState();
        app.rectangle(offsetX, offsetX, box.getWidth() - 2 * offsetX, box.getHeight() - 2 * offsetX);
        app.clip();
        app.newPath();
        Color fcolor = (textColor == null) ? GrayColor.GRAYBLACK : textColor;
        app.setColorFill(new Color(10, 36, 106));
        app.rectangle(offsetX, offsetX + h - (topChoice - first + 1) * leading, box.getWidth() - 2 * offsetX, leading);
        app.fill();
        float xp = offsetX * 2;
        float yp = offsetX + h - ufont.getFontDescriptor(BaseFont.BBOXURY, usize);
        for (int idx = first; idx < last; ++idx, yp -= leading) {
            String ptext = choices[idx];
            int rtl = checkRTL(ptext) ? PdfWriter.RUN_DIRECTION_LTR : PdfWriter.RUN_DIRECTION_NO_BIDI;
            ptext = removeCRLF(ptext);
            Phrase phrase = composePhrase(ptext, ufont, (idx == topChoice) ? GrayColor.GRAYWHITE : fcolor, usize);
            ColumnText.showTextAligned(app, Element.ALIGN_LEFT, phrase, xp, yp, 0, rtl, 0);
        }
        app.restoreState();
        app.endVariableText();
        return app;
    }

        
    public PdfFormField getTextField() throws IOException, DocumentException {
        if (maxCharacterLength <= 0)
            options &= ~COMB;
        if ((options & COMB) != 0)
            options &= ~MULTILINE;
        PdfFormField field = PdfFormField.createTextField(writer, false, false, maxCharacterLength);
        field.setWidget(box, PdfAnnotation.HIGHLIGHT_INVERT);
        switch (alignment) {
            case Element.ALIGN_CENTER:
                field.setQuadding(PdfFormField.Q_CENTER);
                break;
            case Element.ALIGN_RIGHT:
                field.setQuadding(PdfFormField.Q_RIGHT);
                break;
        }
        if (rotation != 0)
            field.setMKRotation(rotation);
        if (fieldName != null) {
            field.setFieldName(fieldName);
            if (!"".equals(text))
                field.setValueAsString(text);
            if (defaultText != null)
                field.setDefaultValueAsString(defaultText);
            if ((options & READ_ONLY) != 0)
                field.setFieldFlags(PdfFormField.FF_READ_ONLY);
            if ((options & REQUIRED) != 0)
                field.setFieldFlags(PdfFormField.FF_REQUIRED);
            if ((options & MULTILINE) != 0)
                field.setFieldFlags(PdfFormField.FF_MULTILINE);
            if ((options & DO_NOT_SCROLL) != 0)
                field.setFieldFlags(PdfFormField.FF_DONOTSCROLL);
            if ((options & PASSWORD) != 0)
                field.setFieldFlags(PdfFormField.FF_PASSWORD);
            if ((options & FILE_SELECTION) != 0)
                field.setFieldFlags(PdfFormField.FF_FILESELECT);
            if ((options & DO_NOT_SPELL_CHECK) != 0)
                field.setFieldFlags(PdfFormField.FF_DONOTSPELLCHECK);
            if ((options & COMB) != 0)
                field.setFieldFlags(PdfFormField.FF_COMB);
        }
        field.setBorderStyle(new PdfBorderDictionary(borderWidth, borderStyle, new PdfDashPattern(3)));
        PdfAppearance tp = getAppearance();
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, tp);
        PdfAppearance da = (PdfAppearance)tp.getDuplicate();
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
    
        
    public PdfFormField getComboField() throws IOException, DocumentException {
        return getChoiceField(false);
    }
    
        
    public PdfFormField getListField() throws IOException, DocumentException {
        return getChoiceField(true);
    }

    protected PdfFormField getChoiceField(boolean isList) throws IOException, DocumentException {
        options &= (~MULTILINE) & (~COMB);
        String uchoices[] = choices;
        if (uchoices == null)
            uchoices = new String[0];
        int topChoice = choiceSelection;
        if (topChoice >= uchoices.length)
            topChoice = uchoices.length - 1;
        if (text == null) text = ""; 
        if (topChoice >= 0)
            text = uchoices[topChoice];
        if (topChoice < 0)
            topChoice = 0;
        PdfFormField field = null;
        String mix[][] = null;
        if (choiceExports == null) {
            if (isList)
                field = PdfFormField.createList(writer, uchoices, topChoice);
            else
                field = PdfFormField.createCombo(writer, (options & EDIT) != 0, uchoices, topChoice);
        }
        else {
            mix = new String[uchoices.length][2];
            for (int k = 0; k < mix.length; ++k)
                mix[k][0] = mix[k][1] = uchoices[k];
            int top = Math.min(uchoices.length, choiceExports.length);
            for (int k = 0; k < top; ++k) {
                if (choiceExports[k] != null)
                    mix[k][0] = choiceExports[k];
            }
            if (isList)
                field = PdfFormField.createList(writer, mix, topChoice);
            else
                field = PdfFormField.createCombo(writer, (options & EDIT) != 0, mix, topChoice);
        }
        field.setWidget(box, PdfAnnotation.HIGHLIGHT_INVERT);
        if (rotation != 0)
            field.setMKRotation(rotation);
        if (fieldName != null) {
            field.setFieldName(fieldName);
            if (uchoices.length > 0) {
                if (mix != null) {
                    field.setValueAsString(mix[topChoice][0]);
                    field.setDefaultValueAsString(mix[topChoice][0]);
                }
                else {
                    field.setValueAsString(text);
                    field.setDefaultValueAsString(text);
                }
            }
            if ((options & READ_ONLY) != 0)
                field.setFieldFlags(PdfFormField.FF_READ_ONLY);
            if ((options & REQUIRED) != 0)
                field.setFieldFlags(PdfFormField.FF_REQUIRED);
            if ((options & DO_NOT_SPELL_CHECK) != 0)
                field.setFieldFlags(PdfFormField.FF_DONOTSPELLCHECK);
        }
        field.setBorderStyle(new PdfBorderDictionary(borderWidth, borderStyle, new PdfDashPattern(3)));
        PdfAppearance tp;
        if (isList) {
            tp = getListAppearance();
            if (topFirst > 0)
                field.put(PdfName.TI, new PdfNumber(topFirst));
        }
        else
            tp = getAppearance();
        field.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, tp);
        PdfAppearance da = (PdfAppearance)tp.getDuplicate();
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
    
    
    public String getDefaultText() {
        return this.defaultText;
    }
    
    
    public void setDefaultText(String defaultText) {
        this.defaultText = defaultText;
    }
    
    
    public String[] getChoices() {
        return this.choices;
    }
    
    
    public void setChoices(String[] choices) {
        this.choices = choices;
    }
    
    
    public String[] getChoiceExports() {
        return this.choiceExports;
    }
    
    
    public void setChoiceExports(String[] choiceExports) {
        this.choiceExports = choiceExports;
    }
    
    
    public int getChoiceSelection() {
        return this.choiceSelection;
    }
    
    
    public void setChoiceSelection(int choiceSelection) {
        this.choiceSelection = choiceSelection;
    }
    
    int getTopFirst() {
        return topFirst;
    }
    
        
    public void setExtraMargin(float extraMarginLeft, float extraMarginTop) {
        this.extraMarginLeft = extraMarginLeft;
        this.extraMarginTop = extraMarginTop;
    }

    
    private ArrayList<BaseFont> substitutionFonts;

    
    public ArrayList<BaseFont> getSubstitutionFonts() {
        return this.substitutionFonts;
    }

    
    public void setSubstitutionFonts(ArrayList<BaseFont> substitutionFonts) {
        this.substitutionFonts = substitutionFonts;
    }

    
    private BaseFont extensionFont;

    
    public BaseFont getExtensionFont() {
        return this.extensionFont;
    }

    
    public void setExtensionFont(BaseFont extensionFont) {
        this.extensionFont = extensionFont;
    }
}