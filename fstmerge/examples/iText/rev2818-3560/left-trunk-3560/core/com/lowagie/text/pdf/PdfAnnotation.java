

package com.lowagie.text.pdf;

import java.awt.Color;
import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.Rectangle;


public class PdfAnnotation extends PdfDictionary {
    
    public static final PdfName HIGHLIGHT_NONE = PdfName.N;
    
    public static final PdfName HIGHLIGHT_INVERT = PdfName.I;
    
    public static final PdfName HIGHLIGHT_OUTLINE = PdfName.O;
    
    public static final PdfName HIGHLIGHT_PUSH = PdfName.P;
    
    public static final PdfName HIGHLIGHT_TOGGLE = PdfName.T;
    
    public static final int FLAGS_INVISIBLE = 1;
    
    public static final int FLAGS_HIDDEN = 2;
    
    public static final int FLAGS_PRINT = 4;
    
    public static final int FLAGS_NOZOOM = 8;
    
    public static final int FLAGS_NOROTATE = 16;
    
    public static final int FLAGS_NOVIEW = 32;
    
    public static final int FLAGS_READONLY = 64;
    
    public static final int FLAGS_LOCKED = 128;
    
    public static final int FLAGS_TOGGLENOVIEW = 256;
    
    public static final PdfName APPEARANCE_NORMAL = PdfName.N;
    
    public static final PdfName APPEARANCE_ROLLOVER = PdfName.R;
    
    public static final PdfName APPEARANCE_DOWN = PdfName.D;
    
    public static final PdfName AA_ENTER = PdfName.E;
    
    public static final PdfName AA_EXIT = PdfName.X;
    
    public static final PdfName AA_DOWN = PdfName.D;
    
    public static final PdfName AA_UP = PdfName.U;
    
    public static final PdfName AA_FOCUS = PdfName.FO;
    
    public static final PdfName AA_BLUR = PdfName.BL;
    
    public static final PdfName AA_JS_KEY = PdfName.K;
    
    public static final PdfName AA_JS_FORMAT = PdfName.F;
    
    public static final PdfName AA_JS_CHANGE = PdfName.V;
    
    public static final PdfName AA_JS_OTHER_CHANGE = PdfName.C;
    
    public static final int MARKUP_HIGHLIGHT = 0;
    
    public static final int MARKUP_UNDERLINE = 1;
    
    public static final int MARKUP_STRIKEOUT = 2;
    
    public static final int MARKUP_SQUIGGLY = 3;
    
    protected PdfWriter writer;
    protected PdfIndirectReference reference;
    protected HashMap templates;
    protected boolean form = false;
    protected boolean annotation = true;
    
    
    protected boolean used = false;
    
    
    private int placeInPage = -1;
    
    
    public PdfAnnotation(PdfWriter writer, Rectangle rect) {
        this.writer = writer;
        if (rect != null)
            put(PdfName.RECT, new PdfRectangle(rect));
    }
    

    
    public PdfAnnotation(PdfWriter writer, float llx, float lly, float urx, float ury, PdfString title, PdfString content) {
        this.writer = writer;
        put(PdfName.SUBTYPE, PdfName.TEXT);
        put(PdfName.T, title);
        put(PdfName.RECT, new PdfRectangle(llx, lly, urx, ury));
        put(PdfName.CONTENTS, content);
    }
    

    
    public PdfAnnotation(PdfWriter writer, float llx, float lly, float urx, float ury, PdfAction action) {
        this.writer = writer;
        put(PdfName.SUBTYPE, PdfName.LINK);
        put(PdfName.RECT, new PdfRectangle(llx, lly, urx, ury));
        put(PdfName.A, action);
        put(PdfName.BORDER, new PdfBorderArray(0, 0, 0));
        put(PdfName.C, new PdfColor(0x00, 0x00, 0xFF));
    }

    
    public static PdfAnnotation createScreen(PdfWriter writer, Rectangle rect, String clipTitle, PdfFileSpecification fs,
                                             String mimeType, boolean playOnDisplay) throws IOException {
        PdfAnnotation ann = new PdfAnnotation(writer, rect);
        ann.put(PdfName.SUBTYPE, PdfName.SCREEN);
        ann.put (PdfName.F, new PdfNumber(FLAGS_PRINT));
        ann.put(PdfName.TYPE, PdfName.ANNOT);
        ann.setPage();
        PdfIndirectReference ref = ann.getIndirectReference();
        PdfAction action = PdfAction.rendition(clipTitle,fs,mimeType, ref);
        PdfIndirectReference actionRef = writer.addToBody(action).getIndirectReference();
        
        if (playOnDisplay)
        {
            PdfDictionary aa = new PdfDictionary();
            aa.put(new PdfName("PV"), actionRef);
            ann.put(PdfName.AA, aa);
        }
        ann.put(PdfName.A, actionRef);
        return ann;
    }

    public PdfIndirectReference getIndirectReference() {
        if (reference == null) {
            reference = writer.getPdfIndirectReference();
        }
        return reference;
    }
    
    
    public static PdfAnnotation createText(PdfWriter writer, Rectangle rect, String title, String contents, boolean open, String icon) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.TEXT);
        if (title != null)
            annot.put(PdfName.T, new PdfString(title, PdfObject.TEXT_UNICODE));
        if (contents != null)
            annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        if (open)
            annot.put(PdfName.OPEN, PdfBoolean.PDFTRUE);
        if (icon != null) {
            annot.put(PdfName.NAME, new PdfName(icon));
        }
        return annot;
    }
    
    
    protected static PdfAnnotation createLink(PdfWriter writer, Rectangle rect, PdfName highlight) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.LINK);
        if (!highlight.equals(HIGHLIGHT_INVERT))
            annot.put(PdfName.H, highlight);
        return annot;
    }
    
    
    public static PdfAnnotation createLink(PdfWriter writer, Rectangle rect, PdfName highlight, PdfAction action) {
        PdfAnnotation annot = createLink(writer, rect, highlight);
        annot.putEx(PdfName.A, action);
        return annot;
    }

    
    public static PdfAnnotation createLink(PdfWriter writer, Rectangle rect, PdfName highlight, String namedDestination) {
        PdfAnnotation annot = createLink(writer, rect, highlight);
        annot.put(PdfName.DEST, new PdfString(namedDestination));
        return annot;
    }

    
    public static PdfAnnotation createLink(PdfWriter writer, Rectangle rect, PdfName highlight, int page, PdfDestination dest) {
        PdfAnnotation annot = createLink(writer, rect, highlight);
        PdfIndirectReference ref = writer.getPageReference(page);
        dest.addPage(ref);
        annot.put(PdfName.DEST, dest);
        return annot;
    }
    
    
    public static PdfAnnotation createFreeText(PdfWriter writer, Rectangle rect, String contents, PdfContentByte defaultAppearance) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.FREETEXT);
        annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        annot.setDefaultAppearanceString(defaultAppearance);
        return annot;
    }

    
    public static PdfAnnotation createLine(PdfWriter writer, Rectangle rect, String contents, float x1, float y1, float x2, float y2) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.LINE);
        annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        PdfArray array = new PdfArray(new PdfNumber(x1));
        array.add(new PdfNumber(y1));
        array.add(new PdfNumber(x2));
        array.add(new PdfNumber(y2));
        annot.put(PdfName.L, array);
        return annot;
    }

    
    public static PdfAnnotation createSquareCircle(PdfWriter writer, Rectangle rect, String contents, boolean square) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        if (square)
            annot.put(PdfName.SUBTYPE, PdfName.SQUARE);
        else
            annot.put(PdfName.SUBTYPE, PdfName.CIRCLE);
        annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        return annot;
    }

    public static PdfAnnotation createMarkup(PdfWriter writer, Rectangle rect, String contents, int type, float quadPoints[]) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        PdfName name = PdfName.HIGHLIGHT;
        switch (type) {
            case MARKUP_UNDERLINE:
                name = PdfName.UNDERLINE;
                break;
            case MARKUP_STRIKEOUT:
                name = PdfName.STRIKEOUT;
                break;
            case MARKUP_SQUIGGLY:
                name = PdfName.SQUIGGLY;
                break;
        }
        annot.put(PdfName.SUBTYPE, name);
        annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        PdfArray array = new PdfArray();
        for (int k = 0; k < quadPoints.length; ++k)
            array.add(new PdfNumber(quadPoints[k]));
        annot.put(PdfName.QUADPOINTS, array);
        return annot;
    }

    
    public static PdfAnnotation createStamp(PdfWriter writer, Rectangle rect, String contents, String name) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.STAMP);
        annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        annot.put(PdfName.NAME, new PdfName(name));
        return annot;
    }

    public static PdfAnnotation createInk(PdfWriter writer, Rectangle rect, String contents, float inkList[][]) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.INK);
        annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        PdfArray outer = new PdfArray();
        for (int k = 0; k < inkList.length; ++k) {
            PdfArray inner = new PdfArray();
            float deep[] = inkList[k];
            for (int j = 0; j < deep.length; ++j)
                inner.add(new PdfNumber(deep[j]));
            outer.add(inner);
        }
        annot.put(PdfName.INKLIST, outer);
        return annot;
    }

        
    public static PdfAnnotation createFileAttachment(PdfWriter writer, Rectangle rect, String contents, byte fileStore[], String file, String fileDisplay) throws IOException {
        return createFileAttachment(writer, rect, contents, PdfFileSpecification.fileEmbedded(writer, file, fileDisplay, fileStore));
    }

    
    public static PdfAnnotation createFileAttachment(PdfWriter writer, Rectangle rect, String contents, PdfFileSpecification fs) throws IOException {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.FILEATTACHMENT);
        if (contents != null)
            annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        annot.put(PdfName.FS, fs.getReference());
        return annot;
    }
    
    
    public static PdfAnnotation createPopup(PdfWriter writer, Rectangle rect, String contents, boolean open) {
        PdfAnnotation annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.POPUP);
        if (contents != null)
            annot.put(PdfName.CONTENTS, new PdfString(contents, PdfObject.TEXT_UNICODE));
        if (open)
            annot.put(PdfName.OPEN, PdfBoolean.PDFTRUE);
        return annot;
    }

    public void setDefaultAppearanceString(PdfContentByte cb) {
        byte b[] = cb.getInternalBuffer().toByteArray();
        int len = b.length;
        for (int k = 0; k < len; ++k) {
            if (b[k] == '\n')
                b[k] = 32;
        }
        put(PdfName.DA, new PdfString(b));
    }
    
    public void setFlags(int flags) {
        if (flags == 0)
            remove(PdfName.F);
        else
            put(PdfName.F, new PdfNumber(flags));
    }
    
    public void setBorder(PdfBorderArray border) {
        put(PdfName.BORDER, border);
    }

    public void setBorderStyle(PdfBorderDictionary border) {
        put(PdfName.BS, border);
    }
    
        
    public void setHighlighting(PdfName highlight) {
        if (highlight.equals(HIGHLIGHT_INVERT))
            remove(PdfName.H);
        else
            put(PdfName.H, highlight);
    }
    
    public void setAppearance(PdfName ap, PdfTemplate template) {
        PdfDictionary dic = (PdfDictionary)get(PdfName.AP);
        if (dic == null)
            dic = new PdfDictionary();
        dic.put(ap, template.getIndirectReference());
        put(PdfName.AP, dic);
        if (!form)
            return;
        if (templates == null)
            templates = new HashMap();
        templates.put(template, null);
    }

    public void setAppearance(PdfName ap, String state, PdfTemplate template) {
        PdfDictionary dicAp = (PdfDictionary)get(PdfName.AP);
        if (dicAp == null)
            dicAp = new PdfDictionary();

        PdfDictionary dic;
        PdfObject obj = dicAp.get(ap);
        if (obj != null && obj.isDictionary())
            dic = (PdfDictionary)obj;
        else
            dic = new PdfDictionary();
        dic.put(new PdfName(state), template.getIndirectReference());
        dicAp.put(ap, dic);
        put(PdfName.AP, dicAp);
        if (!form)
            return;
        if (templates == null)
            templates = new HashMap();
        templates.put(template, null);
    }
    
    public void setAppearanceState(String state) {
        if (state == null) {
            remove(PdfName.AS);
            return;
        }
        put(PdfName.AS, new PdfName(state));
    }
    
    public void setColor(Color color) {
        put(PdfName.C, new PdfColor(color));
    }
    
    public void setTitle(String title) {
        if (title == null) {
            remove(PdfName.T);
            return;
        }
        put(PdfName.T, new PdfString(title, PdfObject.TEXT_UNICODE));
    }
    
    public void setPopup(PdfAnnotation popup) {
        put(PdfName.POPUP, popup.getIndirectReference());
        popup.put(PdfName.PARENT, getIndirectReference());
    }
    
    public void setAction(PdfAction action) {
        put(PdfName.A, action);
    }
    
    public void setAdditionalActions(PdfName key, PdfAction action) {
        PdfDictionary dic;
        PdfObject obj = get(PdfName.AA);
        if (obj != null && obj.isDictionary())
            dic = (PdfDictionary)obj;
        else
            dic = new PdfDictionary();
        dic.put(key, action);
        put(PdfName.AA, dic);
    }
    
    
    public boolean isUsed() {
        return used;
    }
    
    
    public void setUsed() {
        used = true;
    }
    
    public HashMap getTemplates() {
        return templates;
    }
    
    
    public boolean isForm() {
        return form;
    }
    
    
    public boolean isAnnotation() {
        return annotation;
    }
    
    public void setPage(int page) {
        put(PdfName.P, writer.getPageReference(page));
    }
    
    public void setPage() {
        put(PdfName.P, writer.getCurrentPage());
    }
    
    
    public int getPlaceInPage() {
        return placeInPage;
    }
    
    
    public void setPlaceInPage(int placeInPage) {
        this.placeInPage = placeInPage;
    }
    
    public void setRotate(int v) {
        put(PdfName.ROTATE, new PdfNumber(v));
    }
    
    PdfDictionary getMK() {
        PdfDictionary mk = (PdfDictionary)get(PdfName.MK);
        if (mk == null) {
            mk = new PdfDictionary();
            put(PdfName.MK, mk);
        }
        return mk;
    }
    
    public void setMKRotation(int rotation) {
        getMK().put(PdfName.R, new PdfNumber(rotation));
    }
    
    public static PdfArray getMKColor(Color color) {
        PdfArray array = new PdfArray();
        int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_GRAY: {
                array.add(new PdfNumber(((GrayColor)color).getGray()));
                break;
            }
            case ExtendedColor.TYPE_CMYK: {
                CMYKColor cmyk = (CMYKColor)color;
                array.add(new PdfNumber(cmyk.getCyan()));
                array.add(new PdfNumber(cmyk.getMagenta()));
                array.add(new PdfNumber(cmyk.getYellow()));
                array.add(new PdfNumber(cmyk.getBlack()));
                break;
            }
            case ExtendedColor.TYPE_SEPARATION:
            case ExtendedColor.TYPE_PATTERN:
            case ExtendedColor.TYPE_SHADING:
                throw new RuntimeException("Separations, patterns and shadings are not allowed in MK dictionary.");
            default:
                array.add(new PdfNumber(color.getRed() / 255f));
                array.add(new PdfNumber(color.getGreen() / 255f));
                array.add(new PdfNumber(color.getBlue() / 255f));
        }
        return array;
    }
    
    public void setMKBorderColor(Color color) {
        if (color == null)
            getMK().remove(PdfName.BC);
        else
            getMK().put(PdfName.BC, getMKColor(color));
    }
    
    public void setMKBackgroundColor(Color color) {
        if (color == null)
            getMK().remove(PdfName.BG);
        else
            getMK().put(PdfName.BG, getMKColor(color));
    }
    
    public void setMKNormalCaption(String caption) {
        getMK().put(PdfName.CA, new PdfString(caption, PdfObject.TEXT_UNICODE));
    }
    
    public void setMKRolloverCaption(String caption) {
        getMK().put(PdfName.RC, new PdfString(caption, PdfObject.TEXT_UNICODE));
    }
    
    public void setMKAlternateCaption(String caption) {
        getMK().put(PdfName.AC, new PdfString(caption, PdfObject.TEXT_UNICODE));
    }
    
    public void setMKNormalIcon(PdfTemplate template) {
        getMK().put(PdfName.I, template.getIndirectReference());
    }
    
    public void setMKRolloverIcon(PdfTemplate template) {
        getMK().put(PdfName.RI, template.getIndirectReference());
    }
    
    public void setMKAlternateIcon(PdfTemplate template) {
        getMK().put(PdfName.IX, template.getIndirectReference());
    }
    
    public void setMKIconFit(PdfName scale, PdfName scalingType, float leftoverLeft, float leftoverBottom, boolean fitInBounds) {
        PdfDictionary dic = new PdfDictionary();
        if (!scale.equals(PdfName.A))
            dic.put(PdfName.SW, scale);
        if (!scalingType.equals(PdfName.P))
            dic.put(PdfName.S, scalingType);
        if (leftoverLeft != 0.5f || leftoverBottom != 0.5f) {
            PdfArray array = new PdfArray(new PdfNumber(leftoverLeft));
            array.add(new PdfNumber(leftoverBottom));
            dic.put(PdfName.A, array);
        }
        if (fitInBounds)
            dic.put(PdfName.FB, PdfBoolean.PDFTRUE);
        getMK().put(PdfName.IF, dic);
    }
    
    public void setMKTextPosition(int tp) {
        getMK().put(PdfName.TP, new PdfNumber(tp));
    }
    
        
    public void setLayer(PdfOCG layer) {
        put(PdfName.OC, layer.getRef());
    }
    
    
    public void setName(String name) {
        put(PdfName.NM, new PdfString(name));
    }
    
    
    public static class PdfImportedLink {
        float llx, lly, urx, ury;
        HashMap parameters = new HashMap();
        PdfArray destination = null;
        int newPage=0;
        
        PdfImportedLink(PdfDictionary annotation) {
            parameters.putAll(annotation.hashMap);
            try {
                destination = (PdfArray) parameters.remove(PdfName.DEST);
            } catch (ClassCastException ex) {
                throw new IllegalArgumentException("You have to consolidate the named destinations of your reader.");
            }
            if (destination != null) {
                destination = new PdfArray(destination);
            }
            PdfArray rc = (PdfArray) parameters.remove(PdfName.RECT);
            llx = rc.getAsNumber(0).floatValue();
            lly = rc.getAsNumber(1).floatValue();
            urx = rc.getAsNumber(2).floatValue();
            ury = rc.getAsNumber(3).floatValue();
        }
        
        public boolean isInternal() {
            return destination != null;
        }
        
        public int getDestinationPage() {
            if (!isInternal()) return 0;
            
            
            
            PdfIndirectReference ref = destination.getAsIndirectObject(0);
            
            PRIndirectReference pr = (PRIndirectReference) ref;
            PdfReader r = pr.getReader();
            for (int i = 1; i <= r.getNumberOfPages(); i++) {
                PRIndirectReference pp = r.getPageOrigRef(i);
                if (pp.getGeneration() == pr.getGeneration() && pp.getNumber() == pr.getNumber()) return i;
            }
            throw new IllegalArgumentException("Page not found.");
        }
        
        public void setDestinationPage(int newPage) {
            if (!isInternal()) throw new IllegalArgumentException("Cannot change destination of external link");
            this.newPage=newPage;
        }
        
        public void transformDestination(float a, float b, float c, float d, float e, float f) {
            if (!isInternal()) throw new IllegalArgumentException("Cannot change destination of external link");
            if (destination.getAsName(1).equals(PdfName.XYZ)) {
                float x = destination.getAsNumber(2).floatValue();
                float y = destination.getAsNumber(3).floatValue();
                float xx = x * a + y * c + e;
                float yy = x * b + y * d + f;
                destination.getArrayList().set(2, new PdfNumber(xx));
                destination.getArrayList().set(3, new PdfNumber(yy));
            }
        }
        
        public void transformRect(float a, float b, float c, float d, float e, float f) {
            float x = llx * a + lly * c + e;
            float y = llx * b + lly * d + f;
            llx = x;
            lly = y;
            x = urx * a + ury * c + e;
            y = urx * b + ury * d + f;
            urx = x;
            ury = y;
        }
        
        public PdfAnnotation createAnnotation(PdfWriter writer) {
            PdfAnnotation annotation = new PdfAnnotation(writer, new Rectangle(llx, lly, urx, ury));
            if (newPage != 0) {
                PdfIndirectReference ref = writer.getPageReference(newPage);
                destination.getArrayList().set(0, ref);
            }
            if (destination != null) annotation.put(PdfName.DEST, destination);
            annotation.hashMap.putAll(parameters);
            return annotation;
        }
    }
}
