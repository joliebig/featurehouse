

package com.lowagie.text.pdf;
import java.awt.Color;
import java.awt.geom.AffineTransform;
import java.awt.print.PrinterJob;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import com.lowagie.text.Annotation;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.ImgJBIG2;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.internal.PdfAnnotationsImp;
import com.lowagie.text.pdf.internal.PdfXConformanceImp;



public class PdfContentByte {

    

    static class GraphicState {

        
        FontDetails fontDetails;

        
        ColorDetails colorDetails;

        
        float size;

        
        protected float xTLM = 0;
        
        protected float yTLM = 0;

        
        protected float leading = 0;

        
        protected float scale = 100;

        
        protected float charSpace = 0;

        
        protected float wordSpace = 0;

        GraphicState() {
        }

        GraphicState(GraphicState cp) {
            fontDetails = cp.fontDetails;
            colorDetails = cp.colorDetails;
            size = cp.size;
            xTLM = cp.xTLM;
            yTLM = cp.yTLM;
            leading = cp.leading;
            scale = cp.scale;
            charSpace = cp.charSpace;
            wordSpace = cp.wordSpace;
        }
    }

    
    public static final int ALIGN_CENTER = Element.ALIGN_CENTER;

    
    public static final int ALIGN_LEFT = Element.ALIGN_LEFT;

    
    public static final int ALIGN_RIGHT = Element.ALIGN_RIGHT;

    
    public static final int LINE_CAP_BUTT = 0;
    
    public static final int LINE_CAP_ROUND = 1;
    
    public static final int LINE_CAP_PROJECTING_SQUARE = 2;

    
    public static final int LINE_JOIN_MITER = 0;
    
    public static final int LINE_JOIN_ROUND = 1;
    
    public static final int LINE_JOIN_BEVEL = 2;

    
    public static final int TEXT_RENDER_MODE_FILL = 0;
    
    public static final int TEXT_RENDER_MODE_STROKE = 1;
    
    public static final int TEXT_RENDER_MODE_FILL_STROKE = 2;
    
    public static final int TEXT_RENDER_MODE_INVISIBLE = 3;
    
    public static final int TEXT_RENDER_MODE_FILL_CLIP = 4;
    
    public static final int TEXT_RENDER_MODE_STROKE_CLIP = 5;
    
    public static final int TEXT_RENDER_MODE_FILL_STROKE_CLIP = 6;
    
    public static final int TEXT_RENDER_MODE_CLIP = 7;

    private static final float[] unitRect = {0, 0, 0, 1, 1, 0, 1, 1};
    

    
    protected ByteBuffer content = new ByteBuffer();

    
    protected PdfWriter writer;

    
    protected PdfDocument pdf;

    
    protected GraphicState state = new GraphicState();

    
    protected ArrayList<GraphicState> stateList = new ArrayList<GraphicState>();

    
    protected ArrayList<Integer> layerDepth;

    
    protected int separator = '\n';

    private static HashMap<PdfName, String> abrev = new HashMap<PdfName, String>();

    static {
        abrev.put(PdfName.BITSPERCOMPONENT, "/BPC ");
        abrev.put(PdfName.COLORSPACE, "/CS ");
        abrev.put(PdfName.DECODE, "/D ");
        abrev.put(PdfName.DECODEPARMS, "/DP ");
        abrev.put(PdfName.FILTER, "/F ");
        abrev.put(PdfName.HEIGHT, "/H ");
        abrev.put(PdfName.IMAGEMASK, "/IM ");
        abrev.put(PdfName.INTENT, "/Intent ");
        abrev.put(PdfName.INTERPOLATE, "/I ");
        abrev.put(PdfName.WIDTH, "/W ");
    }

    

    

    public PdfContentByte(PdfWriter wr) {
        if (wr != null) {
            writer = wr;
            pdf = writer.getPdfDocument();
        }
    }

    

    

    public String toString() {
        return content.toString();
    }

    
    public ByteBuffer getInternalBuffer() {
        return content;
    }

    

    public byte[] toPdf(PdfWriter writer) {
        return content.toByteArray();
    }

    

    

    public void add(PdfContentByte other) {
        if (other.writer != null && writer != other.writer)
            throw new RuntimeException("Inconsistent writers. Are you mixing two documents?");
        content.append(other.content);
    }

    
    public float getXTLM() {
        return state.xTLM;
    }

    
    public float getYTLM() {
        return state.yTLM;
    }

    
    public float getLeading() {
        return state.leading;
    }

    
    public float getCharacterSpacing() {
        return state.charSpace;
    }

    
    public float getWordSpacing() {
        return state.wordSpace;
    }

    
    public float getHorizontalScaling() {
        return state.scale;
    }

    

    public void setFlatness(float flatness) {
        if (flatness >= 0 && flatness <= 100) {
            content.append(flatness).append(" i").append_i(separator);
        }
    }

    

    public void setLineCap(int style) {
        if (style >= 0 && style <= 2) {
            content.append(style).append(" J").append_i(separator);
        }
    }

    

    public void setLineDash(float phase) {
        content.append("[] ").append(phase).append(" d").append_i(separator);
    }

    

    public void setLineDash(float unitsOn, float phase) {
        content.append("[").append(unitsOn).append("] ").append(phase).append(" d").append_i(separator);
    }

    

    public void setLineDash(float unitsOn, float unitsOff, float phase) {
        content.append("[").append(unitsOn).append(' ').append(unitsOff).append("] ").append(phase).append(" d").append_i(separator);
    }

    

    public final void setLineDash(float[] array, float phase) {
        content.append("[");
        for (int i = 0; i < array.length; i++) {
            content.append(array[i]);
            if (i < array.length - 1) content.append(' ');
        }
        content.append("] ").append(phase).append(" d").append_i(separator);
    }

    

    public void setLineJoin(int style) {
        if (style >= 0 && style <= 2) {
            content.append(style).append(" j").append_i(separator);
        }
    }

    

    public void setLineWidth(float w) {
        content.append(w).append(" w").append_i(separator);
    }

    

    public void setMiterLimit(float miterLimit) {
        if (miterLimit > 1) {
            content.append(miterLimit).append(" M").append_i(separator);
        }
    }

    

    public void clip() {
        content.append("W").append_i(separator);
    }

    

    public void eoClip() {
        content.append("W*").append_i(separator);
    }

    

    public void setGrayFill(float gray) {
        content.append(gray).append(" g").append_i(separator);
    }

    

    public void resetGrayFill() {
        content.append("0 g").append_i(separator);
    }

    

    public void setGrayStroke(float gray) {
        content.append(gray).append(" G").append_i(separator);
    }

    

    public void resetGrayStroke() {
        content.append("0 G").append_i(separator);
    }

    
    private void HelperRGB(float red, float green, float blue) {
        PdfXConformanceImp.checkPDFXConformance(writer, PdfXConformanceImp.PDFXKEY_RGB, null);
        if (red < 0)
            red = 0.0f;
        else if (red > 1.0f)
            red = 1.0f;
        if (green < 0)
            green = 0.0f;
        else if (green > 1.0f)
            green = 1.0f;
        if (blue < 0)
            blue = 0.0f;
        else if (blue > 1.0f)
            blue = 1.0f;
        content.append(red).append(' ').append(green).append(' ').append(blue);
    }

    

    public void setRGBColorFillF(float red, float green, float blue) {
        HelperRGB(red, green, blue);
        content.append(" rg").append_i(separator);
    }

    

    public void resetRGBColorFill() {
        content.append("0 g").append_i(separator);
    }

    

    public void setRGBColorStrokeF(float red, float green, float blue) {
        HelperRGB(red, green, blue);
        content.append(" RG").append_i(separator);
    }

    

    public void resetRGBColorStroke() {
        content.append("0 G").append_i(separator);
    }

    
    private void HelperCMYK(float cyan, float magenta, float yellow, float black) {
        if (cyan < 0)
            cyan = 0.0f;
        else if (cyan > 1.0f)
            cyan = 1.0f;
        if (magenta < 0)
            magenta = 0.0f;
        else if (magenta > 1.0f)
            magenta = 1.0f;
        if (yellow < 0)
            yellow = 0.0f;
        else if (yellow > 1.0f)
            yellow = 1.0f;
        if (black < 0)
            black = 0.0f;
        else if (black > 1.0f)
            black = 1.0f;
        content.append(cyan).append(' ').append(magenta).append(' ').append(yellow).append(' ').append(black);
    }

    

    public void setCMYKColorFillF(float cyan, float magenta, float yellow, float black) {
        HelperCMYK(cyan, magenta, yellow, black);
        content.append(" k").append_i(separator);
    }

    

    public void resetCMYKColorFill() {
        content.append("0 0 0 1 k").append_i(separator);
    }

    

    public void setCMYKColorStrokeF(float cyan, float magenta, float yellow, float black) {
        HelperCMYK(cyan, magenta, yellow, black);
        content.append(" K").append_i(separator);
    }

    

    public void resetCMYKColorStroke() {
        content.append("0 0 0 1 K").append_i(separator);
    }

    

    public void moveTo(float x, float y) {
        content.append(x).append(' ').append(y).append(" m").append_i(separator);
    }

    

    public void lineTo(float x, float y) {
        content.append(x).append(' ').append(y).append(" l").append_i(separator);
    }

    

    public void curveTo(float x1, float y1, float x2, float y2, float x3, float y3) {
        content.append(x1).append(' ').append(y1).append(' ').append(x2).append(' ').append(y2).append(' ').append(x3).append(' ').append(y3).append(" c").append_i(separator);
    }

    

    public void curveTo(float x2, float y2, float x3, float y3) {
        content.append(x2).append(' ').append(y2).append(' ').append(x3).append(' ').append(y3).append(" v").append_i(separator);
    }

    

    public void curveFromTo(float x1, float y1, float x3, float y3) {
        content.append(x1).append(' ').append(y1).append(' ').append(x3).append(' ').append(y3).append(" y").append_i(separator);
    }

    
    public void circle(float x, float y, float r) {
        float b = 0.5523f;
        moveTo(x + r, y);
        curveTo(x + r, y + r * b, x + r * b, y + r, x, y + r);
        curveTo(x - r * b, y + r, x - r, y + r * b, x - r, y);
        curveTo(x - r, y - r * b, x - r * b, y - r, x, y - r);
        curveTo(x + r * b, y - r, x + r, y - r * b, x + r, y);
    }



    

    public void rectangle(float x, float y, float w, float h) {
        content.append(x).append(' ').append(y).append(' ').append(w).append(' ').append(h).append(" re").append_i(separator);
    }

    private boolean compareColors(Color c1, Color c2) {
        if (c1 == null && c2 == null)
            return true;
        if (c1 == null || c2 == null)
            return false;
        if (c1 instanceof ExtendedColor)
            return c1.equals(c2);
        return c2.equals(c1);
    }

    
    public void variableRectangle(Rectangle rect) {
        float t = rect.getTop();
        float b = rect.getBottom();
        float r = rect.getRight();
        float l = rect.getLeft();
        float wt = rect.getBorderWidthTop();
        float wb = rect.getBorderWidthBottom();
        float wr = rect.getBorderWidthRight();
        float wl = rect.getBorderWidthLeft();
        Color ct = rect.getBorderColorTop();
        Color cb = rect.getBorderColorBottom();
        Color cr = rect.getBorderColorRight();
        Color cl = rect.getBorderColorLeft();
        saveState();
        setLineCap(PdfContentByte.LINE_CAP_BUTT);
        setLineJoin(PdfContentByte.LINE_JOIN_MITER);
        float clw = 0;
        boolean cdef = false;
        Color ccol = null;
        boolean cdefi = false;
        Color cfil = null;
        
        if (wt > 0) {
            setLineWidth(clw = wt);
            cdef = true;
            if (ct == null)
                resetRGBColorStroke();
            else
                setColorStroke(ct);
            ccol = ct;
            moveTo(l, t - wt / 2f);
            lineTo(r, t - wt / 2f);
            stroke();
        }

        
        if (wb > 0) {
            if (wb != clw)
                setLineWidth(clw = wb);
            if (!cdef || !compareColors(ccol, cb)) {
                cdef = true;
                if (cb == null)
                    resetRGBColorStroke();
                else
                    setColorStroke(cb);
                ccol = cb;
            }
            moveTo(r, b + wb / 2f);
            lineTo(l, b + wb / 2f);
            stroke();
        }

        
        if (wr > 0) {
            if (wr != clw)
                setLineWidth(clw = wr);
            if (!cdef || !compareColors(ccol, cr)) {
                cdef = true;
                if (cr == null)
                    resetRGBColorStroke();
                else
                    setColorStroke(cr);
                ccol = cr;
            }
            boolean bt = compareColors(ct, cr);
            boolean bb = compareColors(cb, cr);
            moveTo(r - wr / 2f, bt ? t : t - wt);
            lineTo(r - wr / 2f, bb ? b : b + wb);
            stroke();
            if (!bt || !bb) {
                cdefi = true;
                if (cr == null)
                    resetRGBColorFill();
                else
                    setColorFill(cr);
                cfil = cr;
                if (!bt) {
                    moveTo(r, t);
                    lineTo(r, t - wt);
                    lineTo(r - wr, t - wt);
                    fill();
                }
                if (!bb) {
                    moveTo(r, b);
                    lineTo(r, b + wb);
                    lineTo(r - wr, b + wb);
                    fill();
                }
            }
        }

        
        if (wl > 0) {
            if (wl != clw)
                setLineWidth(wl);
            if (!cdef || !compareColors(ccol, cl)) {
                if (cl == null)
                    resetRGBColorStroke();
                else
                    setColorStroke(cl);
            }
            boolean bt = compareColors(ct, cl);
            boolean bb = compareColors(cb, cl);
            moveTo(l + wl / 2f, bt ? t : t - wt);
            lineTo(l + wl / 2f, bb ? b : b + wb);
            stroke();
            if (!bt || !bb) {
                if (!cdefi || !compareColors(cfil, cl)) {
                    if (cl == null)
                        resetRGBColorFill();
                    else
                        setColorFill(cl);
                }
                if (!bt) {
                    moveTo(l, t);
                    lineTo(l, t - wt);
                    lineTo(l + wl, t - wt);
                    fill();
                }
                if (!bb) {
                    moveTo(l, b);
                    lineTo(l, b + wb);
                    lineTo(l + wl, b + wb);
                    fill();
                }
            }
        }
        restoreState();
    }

    

    public void rectangle(Rectangle rectangle) {
        
        float x1 = rectangle.getLeft();
        float y1 = rectangle.getBottom();
        float x2 = rectangle.getRight();
        float y2 = rectangle.getTop();

        
        Color background = rectangle.getBackgroundColor();
        if (background != null) {
            setColorFill(background);
            rectangle(x1, y1, x2 - x1, y2 - y1);
            fill();
            resetRGBColorFill();
        }

        
        if (! rectangle.hasBorders()) {
            return;
        }

        
        
        
        if (rectangle.isUseVariableBorders()) {
            variableRectangle(rectangle);
        }
        else {
            
            if (rectangle.getBorderWidth() != Rectangle.UNDEFINED) {
                setLineWidth(rectangle.getBorderWidth());
            }

            
            Color color = rectangle.getBorderColor();
            if (color != null) {
                setColorStroke(color);
            }

            
            if (rectangle.hasBorder(Rectangle.BOX)) {
               rectangle(x1, y1, x2 - x1, y2 - y1);
            }
            
            else {
                if (rectangle.hasBorder(Rectangle.RIGHT)) {
                    moveTo(x2, y1);
                    lineTo(x2, y2);
                }
                if (rectangle.hasBorder(Rectangle.LEFT)) {
                    moveTo(x1, y1);
                    lineTo(x1, y2);
                }
                if (rectangle.hasBorder(Rectangle.BOTTOM)) {
                    moveTo(x1, y1);
                    lineTo(x2, y1);
                }
                if (rectangle.hasBorder(Rectangle.TOP)) {
                    moveTo(x1, y2);
                    lineTo(x2, y2);
                }
            }

            stroke();

            if (color != null) {
                resetRGBColorStroke();
            }
        }
    }

    

    public void closePath() {
        content.append("h").append_i(separator);
    }

    

    public void newPath() {
        content.append("n").append_i(separator);
    }

    

    public void stroke() {
        content.append("S").append_i(separator);
    }

    

    public void closePathStroke() {
        content.append("s").append_i(separator);
    }

    

    public void fill() {
        content.append("f").append_i(separator);
    }

    

    public void eoFill() {
        content.append("f*").append_i(separator);
    }

    

    public void fillStroke() {
        content.append("B").append_i(separator);
    }

    

    public void closePathFillStroke() {
        content.append("b").append_i(separator);
    }

    

    public void eoFillStroke() {
        content.append("B*").append_i(separator);
    }

    

    public void closePathEoFillStroke() {
        content.append("b*").append_i(separator);
    }

    
    public void addImage(Image image) throws DocumentException {
        addImage(image, false);
    }

    
    public void addImage(Image image, boolean inlineImage) throws DocumentException {
        if (!image.hasAbsoluteY())
            throw new DocumentException("The image must have absolute positioning.");
        float matrix[] = image.matrix();
        matrix[Image.CX] = image.getAbsoluteX() - matrix[Image.CX];
        matrix[Image.CY] = image.getAbsoluteY() - matrix[Image.CY];
        addImage(image, matrix[0], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], inlineImage);
    }

    
    public void addImage(Image image, float a, float b, float c, float d, float e, float f) throws DocumentException {
        addImage(image, a, b, c, d, e, f, false);
    }

    
    public void addImage(Image image, float a, float b, float c, float d, float e, float f, boolean inlineImage) throws DocumentException {
        try {
            if (image.getLayer() != null)
                beginLayer(image.getLayer());
            if (image.isImgTemplate()) {
                writer.addDirectImageSimple(image);
                PdfTemplate template = image.getTemplateData();
                float w = template.getWidth();
                float h = template.getHeight();
                addTemplate(template, a / w, b / w, c / h, d / h, e, f);
            }
            else {
                content.append("q ");
                content.append(a).append(' ');
                content.append(b).append(' ');
                content.append(c).append(' ');
                content.append(d).append(' ');
                content.append(e).append(' ');
                content.append(f).append(" cm");
                if (inlineImage) {
                    content.append("\nBI\n");
                    PdfImage pimage = new PdfImage(image, "", null);
                    if (image instanceof ImgJBIG2) {
                        byte[] globals = ((ImgJBIG2)image).getGlobalBytes();
                        if (globals != null) {
                            PdfDictionary decodeparms = new PdfDictionary();
                            decodeparms.put(PdfName.JBIG2GLOBALS, writer.getReferenceJBIG2Globals(globals));
                            pimage.put(PdfName.DECODEPARMS, decodeparms);
                        }
                    }
                    for (Iterator<PdfName> it = pimage.getKeys().iterator(); it.hasNext();) {
                        PdfName key = it.next();
                        PdfObject value = pimage.get(key);
                        String s = abrev.get(key);
                        if (s == null)
                            continue;
                        content.append(s);
                        boolean check = true;
                        if (key.equals(PdfName.COLORSPACE) && value.isArray()) {
                            PdfArray ar = (PdfArray)value;
                            if (ar.size() == 4 
                                && PdfName.INDEXED.equals(ar.getAsName(0)) 
                                && ar.getPdfObject(1).isName()
                                && ar.getPdfObject(2).isNumber()
                                && ar.getPdfObject(3).isString()
                            ) {
                                check = false;
                            }

                        }
                        if (check && key.equals(PdfName.COLORSPACE) && !value.isName()) {
                            PdfName cs = writer.getColorspaceName();
                            PageResources prs = getPageResources();
                            prs.addColor(cs, writer.addToBody(value).getIndirectReference());
                            value = cs;
                        }
                        value.toPdf(null, content);
                        content.append('\n');
                    }
                    content.append("ID\n");
                    pimage.writeContent(content);
                    content.append("\nEI\nQ").append_i(separator);
                }
                else {
                    PdfName name;
                    PageResources prs = getPageResources();
                    Image maskImage = image.getImageMask();
                    if (maskImage != null) {
                        name = writer.addDirectImageSimple(maskImage);
                        prs.addXObject(name, writer.getImageReference(name));
                    }
                    name = writer.addDirectImageSimple(image);
                    name = prs.addXObject(name, writer.getImageReference(name));
                    content.append(' ').append(name.getBytes()).append(" Do Q").append_i(separator);
                }
            }
            if (image.hasBorders()) {
                saveState();
                float w = image.getWidth();
                float h = image.getHeight();
                concatCTM(a / w, b / w, c / h, d / h, e, f);
                rectangle(image);
                restoreState();
            }
            if (image.getLayer() != null)
                endLayer();
            Annotation annot = image.getAnnotation();
            if (annot == null)
                return;
            float[] r = new float[unitRect.length];
            for (int k = 0; k < unitRect.length; k += 2) {
                r[k] = a * unitRect[k] + c * unitRect[k + 1] + e;
                r[k + 1] = b * unitRect[k] + d * unitRect[k + 1] + f;
            }
            float llx = r[0];
            float lly = r[1];
            float urx = llx;
            float ury = lly;
            for (int k = 2; k < r.length; k += 2) {
                llx = Math.min(llx, r[k]);
                lly = Math.min(lly, r[k + 1]);
                urx = Math.max(urx, r[k]);
                ury = Math.max(ury, r[k + 1]);
            }
            annot = new Annotation(annot);
            annot.setDimensions(llx, lly, urx, ury);
            PdfAnnotation an = PdfAnnotationsImp.convertAnnotation(writer, annot, new Rectangle(llx, lly, urx, ury));
            if (an == null)
                return;
            addAnnotation(an);
        }
        catch (Exception ee) {
            throw new DocumentException(ee);
        }
    }

    
    public void reset() {
        content.reset();
        stateList.clear();
        state = new GraphicState();
    }

    
    public void beginText() {
        state.xTLM = 0;
        state.yTLM = 0;
        content.append("BT").append_i(separator);
    }

    
    public void endText() {
        content.append("ET").append_i(separator);
    }

    
    public void saveState() {
        content.append("q").append_i(separator);
        stateList.add(new GraphicState(state));
    }

    
    public void restoreState() {
        content.append("Q").append_i(separator);
        int idx = stateList.size() - 1;
        if (idx < 0)
            throw new RuntimeException("Unbalanced save/restore state operators.");
        state = stateList.get(idx);
        stateList.remove(idx);
    }

    
    public void setCharacterSpacing(float charSpace) {
        state.charSpace = charSpace;
        content.append(charSpace).append(" Tc").append_i(separator);
    }

    
    public void setWordSpacing(float wordSpace) {
        state.wordSpace = wordSpace;
        content.append(wordSpace).append(" Tw").append_i(separator);
    }

    
    public void setHorizontalScaling(float scale) {
        state.scale = scale;
        content.append(scale).append(" Tz").append_i(separator);
    }

    
    public void setLeading(float leading) {
        state.leading = leading;
        content.append(leading).append(" TL").append_i(separator);
    }

    
    public void setFontAndSize(BaseFont bf, float size) {
        checkWriter();
        if (size < 0.0001f && size > -0.0001f)
            throw new IllegalArgumentException("Font size too small: " + size);
        state.size = size;
        state.fontDetails = writer.addSimple(bf);
        PageResources prs = getPageResources();
        PdfName name = state.fontDetails.getFontName();
        name = prs.addFont(name, state.fontDetails.getIndirectReference());
        content.append(name.getBytes()).append(' ').append(size).append(" Tf").append_i(separator);
    }

    
    public void setTextRenderingMode(int rendering) {
        content.append(rendering).append(" Tr").append_i(separator);
    }

    
    public void setTextRise(float rise) {
        content.append(rise).append(" Ts").append_i(separator);
    }

    
    private void showText2(String text) {
        if (state.fontDetails == null)
            throw new NullPointerException("Font and size must be set before writing any text");
        byte b[] = state.fontDetails.convertToBytes(text);
        escapeString(b, content);
    }

    
    public void showText(String text) {
        showText2(text);
        content.append("Tj").append_i(separator);
    }

    
    public static PdfTextArray getKernArray(String text, BaseFont font) {
        PdfTextArray pa = new PdfTextArray();
        StringBuffer acc = new StringBuffer();
        int len = text.length() - 1;
        char c[] = text.toCharArray();
        if (len >= 0)
            acc.append(c, 0, 1);
        for (int k = 0; k < len; ++k) {
            char c2 = c[k + 1];
            int kern = font.getKerning(c[k], c2);
            if (kern == 0) {
                acc.append(c2);
            }
            else {
                pa.add(acc.toString());
                acc.setLength(0);
                acc.append(c, k + 1, 1);
                pa.add(-kern);
            }
        }
        pa.add(acc.toString());
        return pa;
    }

    
    public void showTextKerned(String text) {
        if (state.fontDetails == null)
            throw new NullPointerException("Font and size must be set before writing any text");
        BaseFont bf = state.fontDetails.getBaseFont();
        if (bf.hasKernPairs())
            showText(getKernArray(text, bf));
        else
            showText(text);
    }

    
    public void newlineShowText(String text) {
        state.yTLM -= state.leading;
        showText2(text);
        content.append("'").append_i(separator);
    }

    
    public void newlineShowText(float wordSpacing, float charSpacing, String text) {
        state.yTLM -= state.leading;
        content.append(wordSpacing).append(' ').append(charSpacing);
        showText2(text);
        content.append("\"").append_i(separator);

        
        
        state.charSpace = charSpacing;
        state.wordSpace = wordSpacing;
    }

    
    public void setTextMatrix(float a, float b, float c, float d, float x, float y) {
        state.xTLM = x;
        state.yTLM = y;
        content.append(a).append(' ').append(b).append_i(' ')
        .append(c).append_i(' ').append(d).append_i(' ')
        .append(x).append_i(' ').append(y).append(" Tm").append_i(separator);
    }

    
    public void setTextMatrix(float x, float y) {
        setTextMatrix(1, 0, 0, 1, x, y);
    }

    
    public void moveText(float x, float y) {
        state.xTLM += x;
        state.yTLM += y;
        content.append(x).append(' ').append(y).append(" Td").append_i(separator);
    }

    
    public void moveTextWithLeading(float x, float y) {
        state.xTLM += x;
        state.yTLM += y;
        state.leading = -y;
        content.append(x).append(' ').append(y).append(" TD").append_i(separator);
    }

    
    public void newlineText() {
        state.yTLM -= state.leading;
        content.append("T*").append_i(separator);
    }

    
    int size() {
        return content.size();
    }

    
    static byte[] escapeString(byte b[]) {
        ByteBuffer content = new ByteBuffer();
        escapeString(b, content);
        return content.toByteArray();
    }

    
    static void escapeString(byte b[], ByteBuffer content) {
        content.append_i('(');
        for (int k = 0; k < b.length; ++k) {
            byte c = b[k];
            switch (c) {
                case '\r':
                    content.append("\\r");
                    break;
                case '\n':
                    content.append("\\n");
                    break;
                case '\t':
                    content.append("\\t");
                    break;
                case '\b':
                    content.append("\\b");
                    break;
                case '\f':
                    content.append("\\f");
                    break;
                case '(':
                case ')':
                case '\\':
                    content.append_i('\\').append_i(c);
                    break;
                default:
                    content.append_i(c);
            }
        }
        content.append(")");
    }

    
    public void addOutline(PdfOutline outline, String name) {
        checkWriter();
        pdf.addOutline(outline, name);
    }
    
    public PdfOutline getRootOutline() {
        checkWriter();
        return pdf.getRootOutline();
    }

    

    public float getEffectiveStringWidth(String text, boolean kerned) {
        BaseFont bf = state.fontDetails.getBaseFont();

        float w;
        if (kerned)
            w = bf.getWidthPointKerned(text, state.size);
        else
            w = bf.getWidthPoint(text, state.size);

        if (state.charSpace != 0.0f && text.length() > 1) {
            w += state.charSpace * (text.length() -1);
        }

        int ft = bf.getFontType();
        if (state.wordSpace != 0.0f && (ft == BaseFont.FONT_TYPE_T1 || ft == BaseFont.FONT_TYPE_TT || ft == BaseFont.FONT_TYPE_T3)) {
            for (int i = 0; i < (text.length() -1); i++) {
                if (text.charAt(i) == ' ')
                    w += state.wordSpace;
            }
        }
        if (state.scale != 100.0)
            w = (w * state.scale) / 100.0f;

        
        return w;
    }

    
    public void showTextAligned(int alignment, String text, float x, float y, float rotation) {
        showTextAligned(alignment, text, x, y, rotation, false);
    }

    private void showTextAligned(int alignment, String text, float x, float y, float rotation, boolean kerned) {
        if (state.fontDetails == null)
            throw new NullPointerException("Font and size must be set before writing any text");
        if (rotation == 0) {
            switch (alignment) {
                case ALIGN_CENTER:
                    x -= getEffectiveStringWidth(text, kerned) / 2;
                    break;
                case ALIGN_RIGHT:
                    x -= getEffectiveStringWidth(text, kerned);
                    break;
            }
            setTextMatrix(x, y);
            if (kerned)
                showTextKerned(text);
            else
                showText(text);
        }
        else {
            double alpha = rotation * Math.PI / 180.0;
            float cos = (float)Math.cos(alpha);
            float sin = (float)Math.sin(alpha);
            float len;
            switch (alignment) {
                case ALIGN_CENTER:
                    len = getEffectiveStringWidth(text, kerned) / 2;
                    x -=  len * cos;
                    y -=  len * sin;
                    break;
                case ALIGN_RIGHT:
                    len = getEffectiveStringWidth(text, kerned);
                    x -=  len * cos;
                    y -=  len * sin;
                    break;
            }
            setTextMatrix(cos, sin, -sin, cos, x, y);
            if (kerned)
                showTextKerned(text);
            else
                showText(text);
            setTextMatrix(0f, 0f);
        }
    }

    
    public void showTextAlignedKerned(int alignment, String text, float x, float y, float rotation) {
        showTextAligned(alignment, text, x, y, rotation, true);
    }

    
    public void concatCTM(float a, float b, float c, float d, float e, float f) {
        content.append(a).append(' ').append(b).append(' ').append(c).append(' ');
        content.append(d).append(' ').append(e).append(' ').append(f).append(" cm").append_i(separator);
    }

    
    public static ArrayList<float[]> bezierArc(float x1, float y1, float x2, float y2, float startAng, float extent) {
        float tmp;
        if (x1 > x2) {
            tmp = x1;
            x1 = x2;
            x2 = tmp;
        }
        if (y2 > y1) {
            tmp = y1;
            y1 = y2;
            y2 = tmp;
        }

        float fragAngle;
        int Nfrag;
        if (Math.abs(extent) <= 90f) {
            fragAngle = extent;
            Nfrag = 1;
        }
        else {
            Nfrag = (int)(Math.ceil(Math.abs(extent)/90f));
            fragAngle = extent / Nfrag;
        }
        float x_cen = (x1+x2)/2f;
        float y_cen = (y1+y2)/2f;
        float rx = (x2-x1)/2f;
        float ry = (y2-y1)/2f;
        float halfAng = (float)(fragAngle * Math.PI / 360.);
        float kappa = (float)(Math.abs(4. / 3. * (1. - Math.cos(halfAng)) / Math.sin(halfAng)));
        ArrayList<float[]> pointList = new ArrayList<float[]>();
        for (int i = 0; i < Nfrag; ++i) {
            float theta0 = (float)((startAng + i*fragAngle) * Math.PI / 180.);
            float theta1 = (float)((startAng + (i+1)*fragAngle) * Math.PI / 180.);
            float cos0 = (float)Math.cos(theta0);
            float cos1 = (float)Math.cos(theta1);
            float sin0 = (float)Math.sin(theta0);
            float sin1 = (float)Math.sin(theta1);
            if (fragAngle > 0f) {
                pointList.add(new float[]{x_cen + rx * cos0,
                y_cen - ry * sin0,
                x_cen + rx * (cos0 - kappa * sin0),
                y_cen - ry * (sin0 + kappa * cos0),
                x_cen + rx * (cos1 + kappa * sin1),
                y_cen - ry * (sin1 - kappa * cos1),
                x_cen + rx * cos1,
                y_cen - ry * sin1});
            }
            else {
                pointList.add(new float[]{x_cen + rx * cos0,
                y_cen - ry * sin0,
                x_cen + rx * (cos0 + kappa * sin0),
                y_cen - ry * (sin0 - kappa * cos0),
                x_cen + rx * (cos1 - kappa * sin1),
                y_cen - ry * (sin1 + kappa * cos1),
                x_cen + rx * cos1,
                y_cen - ry * sin1});
            }
        }
        return pointList;
    }

    
    public void arc(float x1, float y1, float x2, float y2, float startAng, float extent) {
        ArrayList<float[]> ar = bezierArc(x1, y1, x2, y2, startAng, extent);
        if (ar.isEmpty())
            return;
        float pt[] = ar.get(0);
        moveTo(pt[0], pt[1]);
        for (int k = 0; k < ar.size(); ++k) {
            pt = ar.get(k);
            curveTo(pt[2], pt[3], pt[4], pt[5], pt[6], pt[7]);
        }
    }

    
    public void ellipse(float x1, float y1, float x2, float y2) {
        arc(x1, y1, x2, y2, 0f, 360f);
    }

    
    public PdfPatternPainter createPattern(float width, float height, float xstep, float ystep) {
        checkWriter();
        if ( xstep == 0.0f || ystep == 0.0f )
            throw new RuntimeException("XStep or YStep can not be ZERO.");
        PdfPatternPainter painter = new PdfPatternPainter(writer);
        painter.setWidth(width);
        painter.setHeight(height);
        painter.setXStep(xstep);
        painter.setYStep(ystep);
        writer.addSimplePattern(painter);
        return painter;
    }

    
    public PdfPatternPainter createPattern(float width, float height) {
        return createPattern(width, height, width, height);
    }

    
    public PdfPatternPainter createPattern(float width, float height, float xstep, float ystep, Color color) {
        checkWriter();
        if ( xstep == 0.0f || ystep == 0.0f )
            throw new RuntimeException("XStep or YStep can not be ZERO.");
        PdfPatternPainter painter = new PdfPatternPainter(writer, color);
        painter.setWidth(width);
        painter.setHeight(height);
        painter.setXStep(xstep);
        painter.setYStep(ystep);
        writer.addSimplePattern(painter);
        return painter;
    }

    
    public PdfPatternPainter createPattern(float width, float height, Color color) {
        return createPattern(width, height, width, height, color);
    }

    
    public PdfTemplate createTemplate(float width, float height) {
        return createTemplate(width, height, null);
    }

    PdfTemplate createTemplate(float width, float height, PdfName forcedName) {
        checkWriter();
        PdfTemplate template = new PdfTemplate(writer);
        template.setWidth(width);
        template.setHeight(height);
        writer.addDirectTemplateSimple(template, forcedName);
        return template;
    }

    
    public PdfAppearance createAppearance(float width, float height) {
        return createAppearance(width, height, null);
    }

    PdfAppearance createAppearance(float width, float height, PdfName forcedName) {
        checkWriter();
        PdfAppearance template = new PdfAppearance(writer);
        template.setWidth(width);
        template.setHeight(height);
        writer.addDirectTemplateSimple(template, forcedName);
        return template;
    }

    
    public void addPSXObject(PdfPSXObject psobject) {
        checkWriter();
        PdfName name = writer.addDirectTemplateSimple(psobject, null);
        PageResources prs = getPageResources();
        name = prs.addXObject(name, psobject.getIndirectReference());
        content.append(name.getBytes()).append(" Do").append_i(separator);
    }

    
    public void addTemplate(PdfTemplate template, float a, float b, float c, float d, float e, float f) {
        checkWriter();
        checkNoPattern(template);
        PdfName name = writer.addDirectTemplateSimple(template, null);
        PageResources prs = getPageResources();
        name = prs.addXObject(name, template.getIndirectReference());
        content.append("q ");
        content.append(a).append(' ');
        content.append(b).append(' ');
        content.append(c).append(' ');
        content.append(d).append(' ');
        content.append(e).append(' ');
        content.append(f).append(" cm ");
        content.append(name.getBytes()).append(" Do Q").append_i(separator);
    }

    void addTemplateReference(PdfIndirectReference template, PdfName name, float a, float b, float c, float d, float e, float f) {
        checkWriter();
        PageResources prs = getPageResources();
        name = prs.addXObject(name, template);
        content.append("q ");
        content.append(a).append(' ');
        content.append(b).append(' ');
        content.append(c).append(' ');
        content.append(d).append(' ');
        content.append(e).append(' ');
        content.append(f).append(" cm ");
        content.append(name.getBytes()).append(" Do Q").append_i(separator);
    }

    
    public void addTemplate(PdfTemplate template, float x, float y) {
        addTemplate(template, 1, 0, 0, 1, x, y);
    }

    

    public void setCMYKColorFill(int cyan, int magenta, int yellow, int black) {
        content.append((float)(cyan & 0xFF) / 0xFF);
        content.append(' ');
        content.append((float)(magenta & 0xFF) / 0xFF);
        content.append(' ');
        content.append((float)(yellow & 0xFF) / 0xFF);
        content.append(' ');
        content.append((float)(black & 0xFF) / 0xFF);
        content.append(" k").append_i(separator);
    }
    

    public void setCMYKColorStroke(int cyan, int magenta, int yellow, int black) {
        content.append((float)(cyan & 0xFF) / 0xFF);
        content.append(' ');
        content.append((float)(magenta & 0xFF) / 0xFF);
        content.append(' ');
        content.append((float)(yellow & 0xFF) / 0xFF);
        content.append(' ');
        content.append((float)(black & 0xFF) / 0xFF);
        content.append(" K").append_i(separator);
    }

    

    public void setRGBColorFill(int red, int green, int blue) {
        HelperRGB((float)(red & 0xFF) / 0xFF, (float)(green & 0xFF) / 0xFF, (float)(blue & 0xFF) / 0xFF);
        content.append(" rg").append_i(separator);
    }

    

    public void setRGBColorStroke(int red, int green, int blue) {
        HelperRGB((float)(red & 0xFF) / 0xFF, (float)(green & 0xFF) / 0xFF, (float)(blue & 0xFF) / 0xFF);
        content.append(" RG").append_i(separator);
    }

    
    public void setColorStroke(Color color) {
        PdfXConformanceImp.checkPDFXConformance(writer, PdfXConformanceImp.PDFXKEY_COLOR, color);
        int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_GRAY: {
                setGrayStroke(((GrayColor)color).getGray());
                break;
            }
            case ExtendedColor.TYPE_CMYK: {
                CMYKColor cmyk = (CMYKColor)color;
                setCMYKColorStrokeF(cmyk.getCyan(), cmyk.getMagenta(), cmyk.getYellow(), cmyk.getBlack());
                break;
            }
            case ExtendedColor.TYPE_SEPARATION: {
                SpotColor spot = (SpotColor)color;
                setColorStroke(spot.getPdfSpotColor(), spot.getTint());
                break;
            }
            case ExtendedColor.TYPE_PATTERN: {
                PatternColor pat = (PatternColor) color;
                setPatternStroke(pat.getPainter());
                break;
            }
            case ExtendedColor.TYPE_SHADING: {
                ShadingColor shading = (ShadingColor) color;
                setShadingStroke(shading.getPdfShadingPattern());
                break;
            }
            default:
                setRGBColorStroke(color.getRed(), color.getGreen(), color.getBlue());
        }
    }

    
    public void setColorFill(Color color) {
        PdfXConformanceImp.checkPDFXConformance(writer, PdfXConformanceImp.PDFXKEY_COLOR, color);
        int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_GRAY: {
                setGrayFill(((GrayColor)color).getGray());
                break;
            }
            case ExtendedColor.TYPE_CMYK: {
                CMYKColor cmyk = (CMYKColor)color;
                setCMYKColorFillF(cmyk.getCyan(), cmyk.getMagenta(), cmyk.getYellow(), cmyk.getBlack());
                break;
            }
            case ExtendedColor.TYPE_SEPARATION: {
                SpotColor spot = (SpotColor)color;
                setColorFill(spot.getPdfSpotColor(), spot.getTint());
                break;
            }
            case ExtendedColor.TYPE_PATTERN: {
                PatternColor pat = (PatternColor) color;
                setPatternFill(pat.getPainter());
                break;
            }
            case ExtendedColor.TYPE_SHADING: {
                ShadingColor shading = (ShadingColor) color;
                setShadingFill(shading.getPdfShadingPattern());
                break;
            }
            default:
                setRGBColorFill(color.getRed(), color.getGreen(), color.getBlue());
        }
    }

    
    public void setColorFill(PdfSpotColor sp, float tint) {
        checkWriter();
        state.colorDetails = writer.addSimple(sp);
        PageResources prs = getPageResources();
        PdfName name = state.colorDetails.getColorName();
        name = prs.addColor(name, state.colorDetails.getIndirectReference());
        content.append(name.getBytes()).append(" cs ").append(tint).append(" scn").append_i(separator);
    }

    
    public void setColorStroke(PdfSpotColor sp, float tint) {
        checkWriter();
        state.colorDetails = writer.addSimple(sp);
        PageResources prs = getPageResources();
        PdfName name = state.colorDetails.getColorName();
        name = prs.addColor(name, state.colorDetails.getIndirectReference());
        content.append(name.getBytes()).append(" CS ").append(tint).append(" SCN").append_i(separator);
    }

    
    public void setPatternFill(PdfPatternPainter p) {
        if (p.isStencil()) {
            setPatternFill(p, p.getDefaultColor());
            return;
        }
        checkWriter();
        PageResources prs = getPageResources();
        PdfName name = writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        content.append(PdfName.PATTERN.getBytes()).append(" cs ").append(name.getBytes()).append(" scn").append_i(separator);
    }

    
    void outputColorNumbers(Color color, float tint) {
        PdfXConformanceImp.checkPDFXConformance(writer, PdfXConformanceImp.PDFXKEY_COLOR, color);
        int type = ExtendedColor.getType(color);
        switch (type) {
            case ExtendedColor.TYPE_RGB:
                content.append((float)(color.getRed()) / 0xFF);
                content.append(' ');
                content.append((float)(color.getGreen()) / 0xFF);
                content.append(' ');
                content.append((float)(color.getBlue()) / 0xFF);
                break;
            case ExtendedColor.TYPE_GRAY:
                content.append(((GrayColor)color).getGray());
                break;
            case ExtendedColor.TYPE_CMYK: {
                CMYKColor cmyk = (CMYKColor)color;
                content.append(cmyk.getCyan()).append(' ').append(cmyk.getMagenta());
                content.append(' ').append(cmyk.getYellow()).append(' ').append(cmyk.getBlack());
                break;
            }
            case ExtendedColor.TYPE_SEPARATION:
                content.append(tint);
                break;
            default:
                throw new RuntimeException("Invalid color type.");
        }
    }

    
    public void setPatternFill(PdfPatternPainter p, Color color) {
        if (ExtendedColor.getType(color) == ExtendedColor.TYPE_SEPARATION)
            setPatternFill(p, color, ((SpotColor)color).getTint());
        else
            setPatternFill(p, color, 0);
    }

    
    public void setPatternFill(PdfPatternPainter p, Color color, float tint) {
        checkWriter();
        if (!p.isStencil())
            throw new RuntimeException("An uncolored pattern was expected.");
        PageResources prs = getPageResources();
        PdfName name = writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        ColorDetails csDetail = writer.addSimplePatternColorspace(color);
        PdfName cName = prs.addColor(csDetail.getColorName(), csDetail.getIndirectReference());
        content.append(cName.getBytes()).append(" cs").append_i(separator);
        outputColorNumbers(color, tint);
        content.append(' ').append(name.getBytes()).append(" scn").append_i(separator);
    }

    
    public void setPatternStroke(PdfPatternPainter p, Color color) {
        if (ExtendedColor.getType(color) == ExtendedColor.TYPE_SEPARATION)
            setPatternStroke(p, color, ((SpotColor)color).getTint());
        else
            setPatternStroke(p, color, 0);
    }

    
    public void setPatternStroke(PdfPatternPainter p, Color color, float tint) {
        checkWriter();
        if (!p.isStencil())
            throw new RuntimeException("An uncolored pattern was expected.");
        PageResources prs = getPageResources();
        PdfName name = writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        ColorDetails csDetail = writer.addSimplePatternColorspace(color);
        PdfName cName = prs.addColor(csDetail.getColorName(), csDetail.getIndirectReference());
        content.append(cName.getBytes()).append(" CS").append_i(separator);
        outputColorNumbers(color, tint);
        content.append(' ').append(name.getBytes()).append(" SCN").append_i(separator);
    }

    
    public void setPatternStroke(PdfPatternPainter p) {
        if (p.isStencil()) {
            setPatternStroke(p, p.getDefaultColor());
            return;
        }
        checkWriter();
        PageResources prs = getPageResources();
        PdfName name = writer.addSimplePattern(p);
        name = prs.addPattern(name, p.getIndirectReference());
        content.append(PdfName.PATTERN.getBytes()).append(" CS ").append(name.getBytes()).append(" SCN").append_i(separator);
    }

    
    public void paintShading(PdfShading shading) {
        writer.addSimpleShading(shading);
        PageResources prs = getPageResources();
        PdfName name = prs.addShading(shading.getShadingName(), shading.getShadingReference());
        content.append(name.getBytes()).append(" sh").append_i(separator);
        ColorDetails details = shading.getColorDetails();
        if (details != null)
            prs.addColor(details.getColorName(), details.getIndirectReference());
    }

    
    public void paintShading(PdfShadingPattern shading) {
        paintShading(shading.getShading());
    }

    
    public void setShadingFill(PdfShadingPattern shading) {
        writer.addSimpleShadingPattern(shading);
        PageResources prs = getPageResources();
        PdfName name = prs.addPattern(shading.getPatternName(), shading.getPatternReference());
        content.append(PdfName.PATTERN.getBytes()).append(" cs ").append(name.getBytes()).append(" scn").append_i(separator);
        ColorDetails details = shading.getColorDetails();
        if (details != null)
            prs.addColor(details.getColorName(), details.getIndirectReference());
    }

    
    public void setShadingStroke(PdfShadingPattern shading) {
        writer.addSimpleShadingPattern(shading);
        PageResources prs = getPageResources();
        PdfName name = prs.addPattern(shading.getPatternName(), shading.getPatternReference());
        content.append(PdfName.PATTERN.getBytes()).append(" CS ").append(name.getBytes()).append(" SCN").append_i(separator);
        ColorDetails details = shading.getColorDetails();
        if (details != null)
            prs.addColor(details.getColorName(), details.getIndirectReference());
    }

    
    protected void checkWriter() {
        if (writer == null)
            throw new NullPointerException("The writer in PdfContentByte is null.");
    }

    
    public void showText(PdfTextArray text) {
        if (state.fontDetails == null)
            throw new NullPointerException("Font and size must be set before writing any text");
        content.append("[");
        ArrayList<Object> arrayList = text.getArrayList();
        boolean lastWasNumber = false;
        for (int k = 0; k < arrayList.size(); ++k) {
            Object obj = arrayList.get(k);
            if (obj instanceof String) {
                showText2((String)obj);
                lastWasNumber = false;
            }
            else {
                if (lastWasNumber)
                    content.append(' ');
                else
                    lastWasNumber = true;
                content.append(((Float)obj).floatValue());
            }
        }
        content.append("]TJ").append_i(separator);
    }

    
    public PdfWriter getPdfWriter() {
        return writer;
    }

    
    public PdfDocument getPdfDocument() {
        return pdf;
    }

    
    public void localGoto(String name, float llx, float lly, float urx, float ury) {
        pdf.localGoto(name, llx, lly, urx, ury);
    }

    
    public boolean localDestination(String name, PdfDestination destination) {
        return pdf.localDestination(name, destination);
    }

    
    public PdfContentByte getDuplicate() {
        return new PdfContentByte(writer);
    }

    
    public void remoteGoto(String filename, String name, float llx, float lly, float urx, float ury) {
        pdf.remoteGoto(filename, name, llx, lly, urx, ury);
    }

    
    public void remoteGoto(String filename, int page, float llx, float lly, float urx, float ury) {
        pdf.remoteGoto(filename, page, llx, lly, urx, ury);
    }
    
    public void roundRectangle(float x, float y, float w, float h, float r) {
        if (w < 0) {
            x += w;
            w = -w;
        }
        if (h < 0) {
            y += h;
            h = -h;
        }
        if (r < 0)
            r = -r;
        float b = 0.4477f;
        moveTo(x + r, y);
        lineTo(x + w - r, y);
        curveTo(x + w - r * b, y, x + w, y + r * b, x + w, y + r);
        lineTo(x + w, y + h - r);
        curveTo(x + w, y + h - r * b, x + w - r * b, y + h, x + w - r, y + h);
        lineTo(x + r, y + h);
        curveTo(x + r * b, y + h, x, y + h - r * b, x, y + h - r);
        lineTo(x, y + r);
        curveTo(x, y + r * b, x + r * b, y, x + r, y);
    }

    
    public void setAction(PdfAction action, float llx, float lly, float urx, float ury) {
        pdf.setAction(action, llx, lly, urx, ury);
    }

    
    public void setLiteral(String s) {
        content.append(s);
    }

    
    public void setLiteral(char c) {
        content.append(c);
    }

    
    public void setLiteral(float n) {
        content.append(n);
    }

    
    void checkNoPattern(PdfTemplate t) {
        if (t.getType() == PdfTemplate.TYPE_PATTERN)
            throw new RuntimeException("Invalid use of a pattern. A template was expected.");
    }

    
    public void drawRadioField(float llx, float lly, float urx, float ury, boolean on) {
        if (llx > urx) { float x = llx; llx = urx; urx = x; }
        if (lly > ury) { float y = lly; lly = ury; ury = y; }
        
        setLineWidth(1);
        setLineCap(1);
        setColorStroke(new Color(0xC0, 0xC0, 0xC0));
        arc(llx + 1f, lly + 1f, urx - 1f, ury - 1f, 0f, 360f);
        stroke();
        
        setLineWidth(1);
        setLineCap(1);
        setColorStroke(new Color(0xA0, 0xA0, 0xA0));
        arc(llx + 0.5f, lly + 0.5f, urx - 0.5f, ury - 0.5f, 45, 180);
        stroke();
        
        setLineWidth(1);
        setLineCap(1);
        setColorStroke(new Color(0x00, 0x00, 0x00));
        arc(llx + 1.5f, lly + 1.5f, urx - 1.5f, ury - 1.5f, 45, 180);
        stroke();
        if (on) {
            
            setLineWidth(1);
            setLineCap(1);
            setColorFill(new Color(0x00, 0x00, 0x00));
            arc(llx + 4f, lly + 4f, urx - 4f, ury - 4f, 0, 360);
            fill();
        }
    }

    
    public void drawTextField(float llx, float lly, float urx, float ury) {
        if (llx > urx) { float x = llx; llx = urx; urx = x; }
        if (lly > ury) { float y = lly; lly = ury; ury = y; }
        
        setColorStroke(new Color(0xC0, 0xC0, 0xC0));
        setLineWidth(1);
        setLineCap(0);
        rectangle(llx, lly, urx - llx, ury - lly);
        stroke();
        
        setLineWidth(1);
        setLineCap(0);
        setColorFill(new Color(0xFF, 0xFF, 0xFF));
        rectangle(llx + 0.5f, lly + 0.5f, urx - llx - 1f, ury -lly - 1f);
        fill();
        
        setColorStroke(new Color(0xC0, 0xC0, 0xC0));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1.5f);
        lineTo(urx - 1.5f, lly + 1.5f);
        lineTo(urx - 1.5f, ury - 1f);
        stroke();
        
        setColorStroke(new Color(0xA0, 0xA0, 0xA0));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1);
        lineTo(llx + 1f, ury - 1f);
        lineTo(urx - 1f, ury - 1f);
        stroke();
        
        setColorStroke(new Color(0x00, 0x00, 0x00));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 2f, lly + 2f);
        lineTo(llx + 2f, ury - 2f);
        lineTo(urx - 2f, ury - 2f);
        stroke();
    }

    
    public void drawButton(float llx, float lly, float urx, float ury, String text, BaseFont bf, float size) {
        if (llx > urx) { float x = llx; llx = urx; urx = x; }
        if (lly > ury) { float y = lly; lly = ury; ury = y; }
        
        setColorStroke(new Color(0x00, 0x00, 0x00));
        setLineWidth(1);
        setLineCap(0);
        rectangle(llx, lly, urx - llx, ury - lly);
        stroke();
        
        setLineWidth(1);
        setLineCap(0);
        setColorFill(new Color(0xC0, 0xC0, 0xC0));
        rectangle(llx + 0.5f, lly + 0.5f, urx - llx - 1f, ury -lly - 1f);
        fill();
        
        setColorStroke(new Color(0xFF, 0xFF, 0xFF));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1f);
        lineTo(llx + 1f, ury - 1f);
        lineTo(urx - 1f, ury - 1f);
        stroke();
        
        setColorStroke(new Color(0xA0, 0xA0, 0xA0));
        setLineWidth(1);
        setLineCap(0);
        moveTo(llx + 1f, lly + 1f);
        lineTo(urx - 1f, lly + 1f);
        lineTo(urx - 1f, ury - 1f);
        stroke();
        
        resetRGBColorFill();
        beginText();
        setFontAndSize(bf, size);
        showTextAligned(PdfContentByte.ALIGN_CENTER, text, llx + (urx - llx) / 2, lly + (ury - lly - size) / 2, 0);
        endText();
    }

    
    public java.awt.Graphics2D createGraphicsShapes(float width, float height) {
        return new PdfGraphics2D(this, width, height, null, true, false, 0);
    }

    
    public java.awt.Graphics2D createPrinterGraphicsShapes(float width, float height, PrinterJob printerJob) {
        return new PdfPrinterGraphics2D(this, width, height, null, true, false, 0, printerJob);
    }

    
    public java.awt.Graphics2D createGraphics(float width, float height) {
        return new PdfGraphics2D(this, width, height, null, false, false, 0);
    }

    
    public java.awt.Graphics2D createPrinterGraphics(float width, float height, PrinterJob printerJob) {
        return new PdfPrinterGraphics2D(this, width, height, null, false, false, 0, printerJob);
    }

    
    public java.awt.Graphics2D createGraphics(float width, float height, boolean convertImagesToJPEG, float quality) {
        return new PdfGraphics2D(this, width, height, null, false, convertImagesToJPEG, quality);
    }

    
    public java.awt.Graphics2D createPrinterGraphics(float width, float height, boolean convertImagesToJPEG, float quality, PrinterJob printerJob) {
        return new PdfPrinterGraphics2D(this, width, height, null, false, convertImagesToJPEG, quality, printerJob);
    }

    
    public java.awt.Graphics2D createGraphicsShapes(float width, float height, boolean convertImagesToJPEG, float quality) {
        return new PdfGraphics2D(this, width, height, null, true, convertImagesToJPEG, quality);
    }

    
    public java.awt.Graphics2D createPrinterGraphicsShapes(float width, float height, boolean convertImagesToJPEG, float quality, PrinterJob printerJob) {
        return new PdfPrinterGraphics2D(this, width, height, null, true, convertImagesToJPEG, quality, printerJob);
    }

    
    public java.awt.Graphics2D createGraphics(float width, float height, FontMapper fontMapper) {
        return new PdfGraphics2D(this, width, height, fontMapper, false, false, 0);
    }

    
    public java.awt.Graphics2D createPrinterGraphics(float width, float height, FontMapper fontMapper, PrinterJob printerJob) {
        return new PdfPrinterGraphics2D(this, width, height, fontMapper, false, false, 0, printerJob);
    }

    
    public java.awt.Graphics2D createGraphics(float width, float height, FontMapper fontMapper, boolean convertImagesToJPEG, float quality) {
        return new PdfGraphics2D(this, width, height, fontMapper, false, convertImagesToJPEG, quality);
    }

    
    public java.awt.Graphics2D createPrinterGraphics(float width, float height, FontMapper fontMapper, boolean convertImagesToJPEG, float quality, PrinterJob printerJob) {
        return new PdfPrinterGraphics2D(this, width, height, fontMapper, false, convertImagesToJPEG, quality, printerJob);
    }

    PageResources getPageResources() {
        return pdf.getPageResources();
    }

    
    public void setGState(PdfGState gstate) {
        PdfObject obj[] = writer.addSimpleExtGState(gstate);
        PageResources prs = getPageResources();
        PdfName name = prs.addExtGState((PdfName)obj[0], (PdfIndirectReference)obj[1]);
        content.append(name.getBytes()).append(" gs").append_i(separator);
    }

    
    public void beginLayer(PdfOCG layer) {
        if ((layer instanceof PdfLayer) && ((PdfLayer)layer).getTitle() != null)
            throw new IllegalArgumentException("A title is not a layer");
        if (layerDepth == null)
            layerDepth = new ArrayList<Integer>();
        if (layer instanceof PdfLayerMembership) {
            layerDepth.add(new Integer(1));
            beginLayer2(layer);
            return;
        }
        int n = 0;
        PdfLayer la = (PdfLayer)layer;
        while (la != null) {
            if (la.getTitle() == null) {
                beginLayer2(la);
                ++n;
            }
            la = la.getParent();
        }
        layerDepth.add(new Integer(n));
    }

    private void beginLayer2(PdfOCG layer) {
        PdfName name = (PdfName)writer.addSimpleProperty(layer, layer.getRef())[0];
        PageResources prs = getPageResources();
        name = prs.addProperty(name, layer.getRef());
        content.append("/OC ").append(name.getBytes()).append(" BDC").append_i(separator);
    }

    
    public void endLayer() {
        int n = 1;
        if (layerDepth != null && !layerDepth.isEmpty()) {
            n = layerDepth.get(layerDepth.size() - 1).intValue();
            layerDepth.remove(layerDepth.size() - 1);
        }
        while (n-- > 0)
            content.append("EMC").append_i(separator);
    }

    
    public void transform(AffineTransform af) {
        double arr[] = new double[6];
        af.getMatrix(arr);
        content.append(arr[0]).append(' ').append(arr[1]).append(' ').append(arr[2]).append(' ');
        content.append(arr[3]).append(' ').append(arr[4]).append(' ').append(arr[5]).append(" cm").append_i(separator);
    }

    void addAnnotation(PdfAnnotation annot) {
        writer.addAnnotation(annot);
    }

    
    public void setDefaultColorspace(PdfName name, PdfObject obj) {
        PageResources prs = getPageResources();
        prs.addDefaultColor(name, obj);
    }

    
    public void beginMarkedContentSequence(PdfStructureElement struc) {
        struc.setPageMark(writer.getPageNumber() - 1);

        struc.setMarkedContent(writer.getCurrentPage());

        int mark = struc.getMCID();
        content.append(struc.get(PdfName.S).getBytes()).append(" <</MCID ").append(mark).append(">> BDC").append_i(separator);
    }

    
    public void endMarkedContentSequence() {
        content.append("EMC").append_i(separator);
    }

    
    public void beginMarkedContentSequence(PdfName tag, PdfDictionary property, boolean inline) {
        if (property == null) {
            content.append(tag.getBytes()).append(" BMC").append_i(separator);
            return;
        }
        content.append(tag.getBytes()).append(' ');
        if (inline)
            try {
                property.toPdf(writer, content);
            }
            catch (Exception e) {
                throw new ExceptionConverter(e);
            }
        else {
            PdfObject[] objs;
            if (writer.propertyExists(property))
                objs = writer.addSimpleProperty(property, null);
            else
                objs = writer.addSimpleProperty(property, writer.getPdfIndirectReference());
            PdfName name = (PdfName)objs[0];
            PageResources prs = getPageResources();
            name = prs.addProperty(name, (PdfIndirectReference)objs[1]);
            content.append(name.getBytes());
        }
        content.append(" BDC").append_i(separator);
    }

    
    public void beginMarkedContentSequence(PdfName tag) {
        beginMarkedContentSequence(tag, null, false);
    }
}
