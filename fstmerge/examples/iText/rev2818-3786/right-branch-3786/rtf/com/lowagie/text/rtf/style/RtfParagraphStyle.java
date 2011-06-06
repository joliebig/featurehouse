
package com.lowagie.text.rtf.style;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.text.RtfParagraph;


public class RtfParagraphStyle extends RtfFont {

    
    public static final byte[] ALIGN_LEFT = DocWriter.getISOBytes("\\ql");
    
    public static final byte[] ALIGN_RIGHT = DocWriter.getISOBytes("\\qr");
    
    public static final byte[] ALIGN_CENTER = DocWriter.getISOBytes("\\qc");
    
    public static final byte[] ALIGN_JUSTIFY = DocWriter.getISOBytes("\\qj");
    
    public static final byte[] FIRST_LINE_INDENT = DocWriter.getISOBytes("\\fi");
    
    public static final byte[] INDENT_LEFT = DocWriter.getISOBytes("\\li");
    
    public static final byte[] INDENT_RIGHT = DocWriter.getISOBytes("\\ri");
    
    public static final byte[] KEEP_TOGETHER = DocWriter.getISOBytes("\\keep");
    
    public static final byte[] KEEP_TOGETHER_WITH_NEXT = DocWriter.getISOBytes("\\keepn");
    
    public static final byte[] SPACING_AFTER = DocWriter.getISOBytes("\\sa");
    
    public static final byte[] SPACING_BEFORE = DocWriter.getISOBytes("\\sb");

    
    public static final RtfParagraphStyle STYLE_NORMAL = new RtfParagraphStyle("Normal", "Arial", 12, Font.NORMAL, Color.black);
    
    public static final RtfParagraphStyle STYLE_HEADING_1 = new RtfParagraphStyle("heading 1", "Normal");
    
    public static final RtfParagraphStyle STYLE_HEADING_2 = new RtfParagraphStyle("heading 2", "Normal");
    
    public static final RtfParagraphStyle STYLE_HEADING_3 = new RtfParagraphStyle("heading 3", "Normal");

    
    static {
        STYLE_HEADING_1.setSize(16);
        STYLE_HEADING_1.setStyle(Font.BOLD);
        STYLE_HEADING_2.setSize(14);
        STYLE_HEADING_2.setStyle(Font.BOLDITALIC);
        STYLE_HEADING_3.setSize(13);
        STYLE_HEADING_3.setStyle(Font.BOLD);
    }
    
    
    private static final int MODIFIED_NONE = 0;
    
    private static final int MODIFIED_ALIGNMENT = 1;
    
    private static final int MODIFIED_INDENT_LEFT = 2;
    
    private static final int MODIFIED_INDENT_RIGHT = 4;
    
    private static final int MODIFIED_SPACING_BEFORE = 8;
    
    private static final int MODIFIED_SPACING_AFTER = 16;
    
    private static final int MODIFIED_FONT_NAME = 32;
    
    private static final int MODIFIED_FONT_SIZE = 64;
    
    private static final int MODIFIED_FONT_STYLE = 128;
    
    private static final int MODIFIED_FONT_COLOR = 256;
    
    private static final int MODIFIED_LINE_LEADING = 512;
    
    private static final int MODIFIED_KEEP_TOGETHER = 1024;
    
    private static final int MODIFIED_KEEP_TOGETHER_WITH_NEXT = 2048;
    
    
    private int alignment = Element.ALIGN_LEFT;
    
    private int firstLineIndent = 0;
    
    private int indentLeft = 0;
    
    private int indentRight = 0;
    
    private int spacingBefore = 0;
    
    private int spacingAfter = 0;
    
    private int lineLeading = 0;
    
    private boolean keepTogether = false;
    
    private boolean keepTogetherWithNext = false;
    
    private String styleName = "";
    
    private String basedOnName = null;
    
    private RtfParagraphStyle baseStyle = null;
    
    private int modified = MODIFIED_NONE;
    
    private int styleNumber = -1;
    
    
    public RtfParagraphStyle(String styleName, String fontName, int fontSize, int fontStyle, Color fontColor) {
        super(null, new RtfFont(fontName, fontSize, fontStyle, fontColor));
        this.styleName = styleName;
    }
    
    
    public RtfParagraphStyle(String styleName, String basedOnName) {
        super(null, new Font());
        this.styleName = styleName;
        this.basedOnName = basedOnName;
    }
    
    
    public RtfParagraphStyle(RtfDocument doc, RtfParagraphStyle style) {
        super(doc, style);
        this.document = doc;
        this.styleName = style.getStyleName();
        this.alignment = style.getAlignment();
        this.firstLineIndent = (int)(style.getFirstLineIndent() * RtfBasicElement.TWIPS_FACTOR);
        this.indentLeft = (int) (style.getIndentLeft() * RtfBasicElement.TWIPS_FACTOR);
        this.indentRight = (int) (style.getIndentRight() * RtfBasicElement.TWIPS_FACTOR);
        this.spacingBefore = (int) (style.getSpacingBefore() * RtfBasicElement.TWIPS_FACTOR);
        this.spacingAfter = (int) (style.getSpacingAfter() * RtfBasicElement.TWIPS_FACTOR);
        this.lineLeading = (int) (style.getLineLeading() * RtfBasicElement.TWIPS_FACTOR);
        this.keepTogether = style.getKeepTogether();
        this.keepTogetherWithNext = style.getKeepTogetherWithNext();
        this.basedOnName = style.basedOnName;
        this.modified = style.modified;
        this.styleNumber = style.getStyleNumber();

        if(this.document != null) {
            setRtfDocument(this.document);
        }
    }

    
    public String getStyleName() {
        return this.styleName;
    }
    
    
    public String getBasedOnName() {
        return this.basedOnName;
    }
    
    
    public int getAlignment() {
        return this.alignment;
    }

    
    public void setAlignment(int alignment) {
        this.modified = this.modified | MODIFIED_ALIGNMENT;
        this.alignment = alignment;
    }
    
    
    public int getFirstLineIndent() {
        return this.firstLineIndent;
    }
    
    
    public void setFirstLineIndent(int firstLineIndent) {
        this.firstLineIndent = firstLineIndent;
    }
    
    
    public int getIndentLeft() {
        return this.indentLeft;
    }

    
    public void setIndentLeft(int indentLeft) {
        this.modified = this.modified | MODIFIED_INDENT_LEFT;
        this.indentLeft = indentLeft;
    }
    
    
    public int getIndentRight() {
        return this.indentRight;
    }

    
    public void setIndentRight(int indentRight) {
        this.modified = this.modified | MODIFIED_INDENT_RIGHT;
        this.indentRight = indentRight;
    }
    
    
    public int getSpacingBefore() {
        return this.spacingBefore;
    }

    
    public void setSpacingBefore(int spacingBefore) {
        this.modified = this.modified | MODIFIED_SPACING_BEFORE;
        this.spacingBefore = spacingBefore;
    }
    
    
    public int getSpacingAfter() {
        return this.spacingAfter;
    }
    
    
    public void setSpacingAfter(int spacingAfter) {
        this.modified = this.modified | MODIFIED_SPACING_AFTER;
        this.spacingAfter = spacingAfter;
    }
    
    
    public void setFontName(String fontName) {
        this.modified = this.modified | MODIFIED_FONT_NAME;
        super.setFontName(fontName);
    }
    
    
    public void setSize(float fontSize) {
        this.modified = this.modified | MODIFIED_FONT_SIZE;
        super.setSize(fontSize);
    }
    
    
    public void setStyle(int fontStyle) {
        this.modified = this.modified | MODIFIED_FONT_STYLE;
        super.setStyle(fontStyle);
    }
    
    
    public void setColor(Color color) {
        this.modified = this.modified | MODIFIED_FONT_COLOR;
        super.setColor(color);
    }
    
    
    public int getLineLeading() {
        return this.lineLeading;
    }
    
    
    public void setLineLeading(int lineLeading) {
        this.lineLeading = lineLeading;
        this.modified = this.modified | MODIFIED_LINE_LEADING;
    }
    
    
    public boolean getKeepTogether() {
        return this.keepTogether;
    }
    
    
    public void setKeepTogether(boolean keepTogether) {
        this.keepTogether = keepTogether;
        this.modified = this.modified | MODIFIED_KEEP_TOGETHER;
    }
    
    
    public boolean getKeepTogetherWithNext() {
        return this.keepTogetherWithNext;
    }
    
    
    public void setKeepTogetherWithNext(boolean keepTogetherWithNext) {
        this.keepTogetherWithNext = keepTogetherWithNext;
        this.modified = this.modified | MODIFIED_KEEP_TOGETHER_WITH_NEXT;
    }
    
    
    public void handleInheritance() {
        if(this.basedOnName != null && this.document.getDocumentHeader().getRtfParagraphStyle(this.basedOnName) != null) {
            this.baseStyle = this.document.getDocumentHeader().getRtfParagraphStyle(this.basedOnName);
            this.baseStyle.handleInheritance();
            if(!((this.modified & MODIFIED_ALIGNMENT) == MODIFIED_ALIGNMENT)) {
                this.alignment = this.baseStyle.getAlignment();
            }
            if(!((this.modified & MODIFIED_INDENT_LEFT) == MODIFIED_INDENT_LEFT)) {
                this.indentLeft = this.baseStyle.getIndentLeft();
            }
            if(!((this.modified & MODIFIED_INDENT_RIGHT) == MODIFIED_INDENT_RIGHT)) {
                this.indentRight = this.baseStyle.getIndentRight();
            }
            if(!((this.modified & MODIFIED_SPACING_BEFORE) == MODIFIED_SPACING_BEFORE)) {
                this.spacingBefore = this.baseStyle.getSpacingBefore();
            }
            if(!((this.modified & MODIFIED_SPACING_AFTER) == MODIFIED_SPACING_AFTER)) {
                this.spacingAfter = this.baseStyle.getSpacingAfter();
            }
            if(!((this.modified & MODIFIED_FONT_NAME) == MODIFIED_FONT_NAME)) {
                setFontName(this.baseStyle.getFontName());
            }
            if(!((this.modified & MODIFIED_FONT_SIZE) == MODIFIED_FONT_SIZE)) {
                setSize(this.baseStyle.getFontSize());
            }
            if(!((this.modified & MODIFIED_FONT_STYLE) == MODIFIED_FONT_STYLE)) {
                setStyle(this.baseStyle.getFontStyle());
            }
            if(!((this.modified & MODIFIED_FONT_COLOR) == MODIFIED_FONT_COLOR)) {
                setColor(this.baseStyle.getColor());
            }
            if(!((this.modified & MODIFIED_LINE_LEADING) == MODIFIED_LINE_LEADING)) {
                setLineLeading(this.baseStyle.getLineLeading());
            }
            if(!((this.modified & MODIFIED_KEEP_TOGETHER) == MODIFIED_KEEP_TOGETHER)) {
                setKeepTogether(this.baseStyle.getKeepTogether());
            }
            if(!((this.modified & MODIFIED_KEEP_TOGETHER_WITH_NEXT) == MODIFIED_KEEP_TOGETHER_WITH_NEXT)) {
                setKeepTogetherWithNext(this.baseStyle.getKeepTogetherWithNext());
            }
        }
    }
    
    
    private void writeParagraphSettings(final OutputStream result) throws IOException {
        if(this.keepTogether) {
            result.write(RtfParagraphStyle.KEEP_TOGETHER);
        }
        if(this.keepTogetherWithNext) {
            result.write(RtfParagraphStyle.KEEP_TOGETHER_WITH_NEXT);
        }
        switch (alignment) {
            case Element.ALIGN_LEFT:
                result.write(RtfParagraphStyle.ALIGN_LEFT);
                break;
            case Element.ALIGN_RIGHT:
                result.write(RtfParagraphStyle.ALIGN_RIGHT);
                break;
            case Element.ALIGN_CENTER:
                result.write(RtfParagraphStyle.ALIGN_CENTER);
                break;
            case Element.ALIGN_JUSTIFIED:
            case Element.ALIGN_JUSTIFIED_ALL:
                result.write(RtfParagraphStyle.ALIGN_JUSTIFY);
                break;
        }
        result.write(FIRST_LINE_INDENT);
        result.write(intToByteArray(this.firstLineIndent));
        result.write(RtfParagraphStyle.INDENT_LEFT);
        result.write(intToByteArray(indentLeft));
        result.write(RtfParagraphStyle.INDENT_RIGHT);
        result.write(intToByteArray(indentRight));
        if(this.spacingBefore > 0) {
            result.write(RtfParagraphStyle.SPACING_BEFORE);
            result.write(intToByteArray(this.spacingBefore));
        }
        if(this.spacingAfter > 0) {
            result.write(RtfParagraphStyle.SPACING_AFTER);
            result.write(intToByteArray(this.spacingAfter));
        }
        if(this.lineLeading > 0) {
            result.write(RtfParagraph.LINE_SPACING);
            result.write(intToByteArray(this.lineLeading));
        }            
    }

    
    public void writeDefinition(final OutputStream result) throws IOException 
    {
        result.write(DocWriter.getISOBytes("{"));
        result.write(DocWriter.getISOBytes("\\style"));
        result.write(DocWriter.getISOBytes("\\s"));
        result.write(intToByteArray(this.styleNumber));
        result.write(RtfBasicElement.DELIMITER);
        writeParagraphSettings(result);
        super.writeBegin(result);
        result.write(RtfBasicElement.DELIMITER);
        result.write(DocWriter.getISOBytes(this.styleName));
        result.write(DocWriter.getISOBytes(";"));
        result.write(DocWriter.getISOBytes("}"));
        this.document.outputDebugLinebreak(result);       
    }
    
    
    public void writeBegin(final OutputStream result) throws IOException {
        result.write(DocWriter.getISOBytes("\\s"));
        result.write(intToByteArray(this.styleNumber));
        writeParagraphSettings(result);
    }
    
    
    public void writeEnd(final OutputStream result) throws IOException {
    }
    
    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public boolean equals(Object o) {
        if(!(o instanceof RtfParagraphStyle)) {
            return false;
        }
        RtfParagraphStyle paragraphStyle = (RtfParagraphStyle) o;
        boolean result = this.getStyleName().equals(paragraphStyle.getStyleName());
        return result;
    }
    
    
    public int hashCode() {
        return this.styleName.hashCode();
    }
    
    
    private int getStyleNumber() {
        return this.styleNumber;
    }
    
    
    protected void setStyleNumber(int styleNumber) {
        this.styleNumber = styleNumber;
    }
}
