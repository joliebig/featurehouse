

package com.lowagie.text;

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import com.lowagie.text.pdf.HyphenationEvent;
import com.lowagie.text.pdf.PdfAction;
import com.lowagie.text.pdf.PdfAnnotation;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.draw.DrawInterface;



public class Chunk implements Element {

    

    
    public static final String OBJECT_REPLACEMENT_CHARACTER = "\u";

    
    public static final Chunk NEWLINE = new Chunk("\n");

    
    public static final Chunk NEXTPAGE = new Chunk("");
    static {
        NEXTPAGE.setNewPage();
    }

    

    
    protected StringBuffer content = null;

    
    protected Font font = null;

    
    protected HashMap<String, Object> attributes = null;

    

    
    public Chunk() {
        this.content = new StringBuffer();
        this.font = new Font();
    }

        
    public Chunk(Chunk ck) {
        if (ck.content != null) {
            content = new StringBuffer(ck.content.toString());
        }
        if (ck.font != null) {
            font = new Font(ck.font);
        }
        if (ck.attributes != null) {
            attributes = new HashMap<String, Object>(ck.attributes);
        }
    }
    
    
    public Chunk(String content, Font font) {
        this.content = new StringBuffer(content);
        this.font = font;
    }

    
    public Chunk(String content) {
        this(content, new Font());
    }

    
    public Chunk(char c, Font font) {
        this.content = new StringBuffer();
        this.content.append(c);
        this.font = font;
    }

    
    public Chunk(char c) {
        this(c, new Font());
    }

    
    public Chunk(Image image, float offsetX, float offsetY) {
        this(OBJECT_REPLACEMENT_CHARACTER, new Font());
        Image copyImage = Image.getInstance(image);
        copyImage.setAbsolutePosition(Float.NaN, Float.NaN);
        setAttribute(IMAGE, new Object[] { copyImage, new Float(offsetX),
                new Float(offsetY), Boolean.FALSE });
    }

    
    public static final String SEPARATOR = "SEPARATOR";
    
    
    public Chunk(DrawInterface separator) {
        this(separator, false);
    }    
    
    
    public Chunk(DrawInterface separator, boolean vertical) {
        this(OBJECT_REPLACEMENT_CHARACTER, new Font());
        setAttribute(SEPARATOR, new Object[] {separator, Boolean.valueOf(vertical)});
    }

    
    public static final String TAB = "TAB";
    
    
    public Chunk(DrawInterface separator, float tabPosition) {
        this(separator, tabPosition, false);
    }
    
    
    public Chunk(DrawInterface separator, float tabPosition, boolean newline) {
        this(OBJECT_REPLACEMENT_CHARACTER, new Font());
        if (tabPosition < 0) {
            throw new IllegalArgumentException("A tab position may not be lower than 0; yours is " + tabPosition);
        }
        setAttribute(TAB, new Object[] {separator, new Float(tabPosition), Boolean.valueOf(newline), new Float(0)});
    }

    
    public Chunk(Image image, float offsetX, float offsetY,
            boolean changeLeading) {
        this(OBJECT_REPLACEMENT_CHARACTER, new Font());
        setAttribute(IMAGE, new Object[] { image, new Float(offsetX),
                new Float(offsetY), Boolean.valueOf(changeLeading) });
    }

    

    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        } catch (DocumentException de) {
            return false;
        }
    }

    
    public int type() {
        return Element.CHUNK;
    }

    
    public ArrayList<Chunk> getChunks() {
        ArrayList<Chunk> tmp = new ArrayList<Chunk>();
        tmp.add(this);
        return tmp;
    }

    

    
    public StringBuffer append(String string) {
        return content.append(string);
    }

    
    public void setFont(Font font) {
        this.font = font;
    }

    

    
    public Font getFont() {
        return font;
    }

    
    public String getContent() {
        return content.toString();
    }

    
    public String toString() {
        return getContent();
    }

    
    public boolean isEmpty() {
        return (content.toString().trim().length() == 0)
                && (content.toString().indexOf("\n") == -1)
                && (attributes == null);
    }

    
    public float getWidthPoint() {
        if (getImage() != null) {
            return getImage().getScaledWidth();
        }
        return font.getCalculatedBaseFont(true).getWidthPoint(getContent(),
                font.getCalculatedSize())
                * getHorizontalScaling();
    }

    

    

    public boolean hasAttributes() {
        return attributes != null;
    }

    

    public HashMap<String, Object> getAttributes() {
        return attributes;
    }

    
    public void setAttributes(HashMap<String, Object> attributes) {
        this.attributes = attributes;
    }

    

    private Chunk setAttribute(String name, Object obj) {
        if (attributes == null)
            attributes = new HashMap<String, Object>();
        attributes.put(name, obj);
        return this;
    }

    

    
    public static final String HSCALE = "HSCALE";

    
    public Chunk setHorizontalScaling(float scale) {
        return setAttribute(HSCALE, new Float(scale));
    }

    
    public float getHorizontalScaling() {
        if (attributes == null)
            return 1f;
        Float f = (Float) attributes.get(HSCALE);
        if (f == null)
            return 1f;
        return f.floatValue();
    }

    
    public static final String UNDERLINE = "UNDERLINE";

    
    public Chunk setUnderline(float thickness, float yPosition) {
        return setUnderline(null, thickness, 0f, yPosition, 0f,
                PdfContentByte.LINE_CAP_BUTT);
    }

    
    public Chunk setUnderline(Color color, float thickness, float thicknessMul,
            float yPosition, float yPositionMul, int cap) {
        if (attributes == null)
            attributes = new HashMap<String, Object>();
        Object obj[] = {
                color,
                new float[] { thickness, thicknessMul, yPosition, yPositionMul, cap } };
        Object unders[][] = Utilities.addToArray((Object[][]) attributes.get(UNDERLINE),
                obj);
        return setAttribute(UNDERLINE, unders);
    }
    
    
    public static final String SUBSUPSCRIPT = "SUBSUPSCRIPT";
    
    

    public Chunk setTextRise(float rise) {
        return setAttribute(SUBSUPSCRIPT, new Float(rise));
    }

    
    public float getTextRise() {
        if (attributes != null && attributes.containsKey(SUBSUPSCRIPT)) {
            Float f = (Float) attributes.get(SUBSUPSCRIPT);
            return f.floatValue();
        }
        return 0.0f;
    }

    
    public static final String SKEW = "SKEW";

    
    public Chunk setSkew(float alpha, float beta) {
        alpha = (float) Math.tan(alpha * Math.PI / 180);
        beta = (float) Math.tan(beta * Math.PI / 180);
        return setAttribute(SKEW, new float[] { alpha, beta });
    }

    
    public static final String BACKGROUND = "BACKGROUND";

    
    public Chunk setBackground(Color color) {
        return setBackground(color, 0, 0, 0, 0);
    }

    
    public Chunk setBackground(Color color, float extraLeft, float extraBottom,
            float extraRight, float extraTop) {
        return setAttribute(BACKGROUND, new Object[] { color,
                new float[] { extraLeft, extraBottom, extraRight, extraTop } });
    }

    
    public static final String TEXTRENDERMODE = "TEXTRENDERMODE";

    
    public Chunk setTextRenderMode(int mode, float strokeWidth,
            Color strokeColor) {
        return setAttribute(TEXTRENDERMODE, new Object[] { new Integer(mode),
                new Float(strokeWidth), strokeColor });
    }

    
    public static final String SPLITCHARACTER = "SPLITCHARACTER";

    

    public Chunk setSplitCharacter(SplitCharacter splitCharacter) {
        return setAttribute(SPLITCHARACTER, splitCharacter);
    }

    
    public static final String HYPHENATION = "HYPHENATION";
    
    
    public Chunk setHyphenation(HyphenationEvent hyphenation) {
        return setAttribute(HYPHENATION, hyphenation);
    }

    
    public static final String REMOTEGOTO = "REMOTEGOTO";

    

    public Chunk setRemoteGoto(String filename, String name) {
        return setAttribute(REMOTEGOTO, new Object[] { filename, name });
    }

    

    public Chunk setRemoteGoto(String filename, int page) {
        return setAttribute(REMOTEGOTO, new Object[] { filename,
                new Integer(page) });
    }

    
    public static final String LOCALGOTO = "LOCALGOTO";
    
    

    public Chunk setLocalGoto(String name) {
        return setAttribute(LOCALGOTO, name);
    }

    
    public static final String LOCALDESTINATION = "LOCALDESTINATION";

    
    public Chunk setLocalDestination(String name) {
        return setAttribute(LOCALDESTINATION, name);
    }

    
    public static final String GENERICTAG = "GENERICTAG";

    

    public Chunk setGenericTag(String text) {
        return setAttribute(GENERICTAG, text);
    }
    
    
    public static final String IMAGE = "IMAGE";

    

    public Image getImage() {
        if (attributes == null)
            return null;
        Object obj[] = (Object[]) attributes.get(Chunk.IMAGE);
        if (obj == null)
            return null;
        else {
            return (Image) obj[0];
        }
    }
    
    
    public static final String ACTION = "ACTION";

    

    public Chunk setAction(PdfAction action) {
        return setAttribute(ACTION, action);
    }

    

    public Chunk setAnchor(URL url) {
        return setAttribute(ACTION, new PdfAction(url.toExternalForm()));
    }

    

    public Chunk setAnchor(String url) {
        return setAttribute(ACTION, new PdfAction(url));
    }
    
    
    public static final String NEWPAGE = "NEWPAGE";

    

    public Chunk setNewPage() {
        return setAttribute(NEWPAGE, null);
    }

    
    public static final String PDFANNOTATION = "PDFANNOTATION";

    
    public Chunk setAnnotation(PdfAnnotation annotation) {
        return setAttribute(PDFANNOTATION, annotation);
    }
    
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return true;
    }

    
    public HyphenationEvent getHyphenation() {
        if (attributes == null) return null;
        return (HyphenationEvent) attributes.get(Chunk.HYPHENATION);
    }
    
    
    
    
    public static final String COLOR = "COLOR";

    
    public static final String ENCODING = "ENCODING";

}