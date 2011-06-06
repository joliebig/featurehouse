

package com.lowagie.text.pdf;

import java.awt.Color;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.SplitCharacter;
import com.lowagie.text.Utilities;



public class PdfChunk implements SplitCharacter{

    private static final char singleSpace[] = {' '};
    private static final PdfChunk thisChunk[] = new PdfChunk[1];
    private static final float ITALIC_ANGLE = 0.21256f;

    private static final HashMap keysAttributes = new HashMap();
    

    private static final HashMap keysNoStroke = new HashMap();
    
    static {
        keysAttributes.put(Chunk.ACTION, null);
        keysAttributes.put(Chunk.UNDERLINE, null);
        keysAttributes.put(Chunk.REMOTEGOTO, null);
        keysAttributes.put(Chunk.LOCALGOTO, null);
        keysAttributes.put(Chunk.LOCALDESTINATION, null);
        keysAttributes.put(Chunk.GENERICTAG, null);
        keysAttributes.put(Chunk.NEWPAGE, null);
        keysAttributes.put(Chunk.IMAGE, null);
        keysAttributes.put(Chunk.BACKGROUND, null);
        keysAttributes.put(Chunk.PDFANNOTATION, null);
        keysAttributes.put(Chunk.SKEW, null);
        keysAttributes.put(Chunk.HSCALE, null);
        keysNoStroke.put(Chunk.SUBSUPSCRIPT, null);
        keysNoStroke.put(Chunk.SPLITCHARACTER, null);
        keysNoStroke.put(Chunk.HYPHENATION, null);
        keysNoStroke.put(Chunk.TEXTRENDERMODE, null);
    }
    
    

    
    protected String value = PdfObject.NOTHING;
    
    
    protected String encoding = BaseFont.WINANSI;
    
    

    protected PdfFont font;
    
    protected BaseFont baseFont;
    
    protected SplitCharacter splitCharacter;

    protected HashMap attributes = new HashMap();
    

    protected HashMap noStroke = new HashMap();
    

    protected boolean newlineSplit;
    

    protected Image image;
    

    protected float offsetX;
    

    protected float offsetY;


    protected boolean changeLeading = false;

    
    

    
    PdfChunk(String string, PdfChunk other) {
        thisChunk[0] = this;
        value = string;
        this.font = other.font;
        this.attributes = other.attributes;
        this.noStroke = other.noStroke;
        this.baseFont = other.baseFont;
        Object obj[] = (Object[])attributes.get(Chunk.IMAGE);
        if (obj == null)
            image = null;
        else {
            image = (Image)obj[0];
            offsetX = ((Float)obj[1]).floatValue();
            offsetY = ((Float)obj[2]).floatValue();
            changeLeading = ((Boolean)obj[3]).booleanValue();
        }
        encoding = font.getFont().getEncoding();
        splitCharacter = (SplitCharacter)noStroke.get(Chunk.SPLITCHARACTER);
        if (splitCharacter == null)
            splitCharacter = this;
    }
    

    
    PdfChunk(Chunk chunk, PdfAction action) {
        thisChunk[0] = this;
        value = chunk.getContent();
        
        Font f = chunk.getFont();
        float size = f.getSize();
        if (size == Font.UNDEFINED)
            size = 12;
        baseFont = f.getBaseFont();
        int style = f.getStyle();
        if (style == Font.UNDEFINED) {
            style = Font.NORMAL;
        }
        if (baseFont == null) {
            
            baseFont = f.getCalculatedBaseFont(false);
        }
        else {
            
            if ((style & Font.BOLD) != 0)
                attributes.put(Chunk.TEXTRENDERMODE, new Object[]{new Integer(PdfContentByte.TEXT_RENDER_MODE_FILL_STROKE), new Float(size / 30f), null});
            
            if ((style & Font.ITALIC) != 0)
                attributes.put(Chunk.SKEW, new float[]{0, ITALIC_ANGLE});
        }
        font = new PdfFont(baseFont, size);
        
        HashMap attr = chunk.getAttributes();
        if (attr != null) {
            for (Iterator i = attr.entrySet().iterator(); i.hasNext();) {
                Map.Entry entry = (Map.Entry) i.next();
                Object name = entry.getKey();
                if (keysAttributes.containsKey(name)) {
                    attributes.put(name, entry.getValue());
                }
                else if (keysNoStroke.containsKey(name)) {
                    noStroke.put(name, entry.getValue());
                }
            }
            if ("".equals(attr.get(Chunk.GENERICTAG))) {
                attributes.put(Chunk.GENERICTAG, chunk.getContent());
            }
        }
        if (f.isUnderlined()) {
            Object obj[] = {null, new float[]{0, 1f / 15, 0, -1f / 3, 0}};
            Object unders[][] = Utilities.addToArray((Object[][])attributes.get(Chunk.UNDERLINE), obj);
            attributes.put(Chunk.UNDERLINE, unders);
        }
        if (f.isStrikethru()) {
            Object obj[] = {null, new float[]{0, 1f / 15, 0, 1f / 3, 0}};
            Object unders[][] = Utilities.addToArray((Object[][])attributes.get(Chunk.UNDERLINE), obj);
            attributes.put(Chunk.UNDERLINE, unders);
        }
        if (action != null)
            attributes.put(Chunk.ACTION, action);
        
        noStroke.put(Chunk.COLOR, f.getColor());
        noStroke.put(Chunk.ENCODING, font.getFont().getEncoding());
        Object obj[] = (Object[])attributes.get(Chunk.IMAGE);
        if (obj == null) {
            image = null;
        }
        else {
            attributes.remove(Chunk.HSCALE); 
            image = (Image)obj[0];
            offsetX = ((Float)obj[1]).floatValue();
            offsetY = ((Float)obj[2]).floatValue();
            changeLeading = ((Boolean)obj[3]).booleanValue();
        }
        font.setImage(image);
        Float hs = (Float)attributes.get(Chunk.HSCALE);
        if (hs != null)
            font.setHorizontalScaling(hs.floatValue());
        encoding = font.getFont().getEncoding();
        splitCharacter = (SplitCharacter)noStroke.get(Chunk.SPLITCHARACTER);
        if (splitCharacter == null)
            splitCharacter = this;
    }
    
    
    
        
    public char getUnicodeEquivalent(char c) {
        return baseFont.getUnicodeEquivalent(c);
    }

    protected int getWord(String text, int start) {
        int len = text.length();
        while (start < len) {
            if (!Character.isLetter(text.charAt(start)))
                break;
            ++start;
        }
        return start;
    }
    

    
    PdfChunk split(float width) {
        newlineSplit = false;
        if (image != null) {
            if (image.getScaledWidth() > width) {
                PdfChunk pc = new PdfChunk(Chunk.OBJECT_REPLACEMENT_CHARACTER, this);
                value = "";
                attributes = new HashMap();
                image = null;
                font = PdfFont.getDefaultFont();
                return pc;
            }
            else
                return null;
        }
        HyphenationEvent hyphenationEvent = (HyphenationEvent)noStroke.get(Chunk.HYPHENATION);
        int currentPosition = 0;
        int splitPosition = -1;
        float currentWidth = 0;
        
        
        
        int lastSpace = -1;
        float lastSpaceWidth = 0;
        int length = value.length();
        char valueArray[] = value.toCharArray();
        char character = 0;
        BaseFont ft = font.getFont();
        if (ft.getFontType() == BaseFont.FONT_TYPE_CJK && ft.getUnicodeEquivalent(' ') != ' ') {
            while (currentPosition < length) {
                
                char cidChar = valueArray[currentPosition];
                character = ft.getUnicodeEquivalent(cidChar);
                
                if (character == '\n') {
                    newlineSplit = true;
                    String returnValue = value.substring(currentPosition + 1);
                    value = value.substring(0, currentPosition);
                    if (value.length() < 1) {
                        value = "\u";
                    }
                    PdfChunk pc = new PdfChunk(returnValue, this);
                    return pc;
                }
                currentWidth += font.width(cidChar);
                if (character == ' ') {
                    lastSpace = currentPosition + 1;
                    lastSpaceWidth = currentWidth;
                }
                if (currentWidth > width)
                    break;
                
                if (splitCharacter.isSplitCharacter(0, currentPosition, length, valueArray, thisChunk))
                    splitPosition = currentPosition + 1;
                currentPosition++;
            }
        }
        else {
            while (currentPosition < length) {
                
                character = valueArray[currentPosition];
                
                if (character == '\r' || character == '\n') {
                    newlineSplit = true;
                    int inc = 1;
                    if (character == '\r' && currentPosition + 1 < length && valueArray[currentPosition + 1] == '\n')
                        inc = 2;
                    String returnValue = value.substring(currentPosition + inc);
                    value = value.substring(0, currentPosition);
                    if (value.length() < 1) {
                        value = " ";
                    }
                    PdfChunk pc = new PdfChunk(returnValue, this);
                    return pc;
                }
                currentWidth += font.width(character);
                if (character == ' ') {
                    lastSpace = currentPosition + 1;
                    lastSpaceWidth = currentWidth;
                }
                if (currentWidth > width)
                    break;
                
                if (splitCharacter.isSplitCharacter(0, currentPosition, length, valueArray, null))
                    splitPosition = currentPosition + 1;
                currentPosition++;
            }
        }
        
        
        if (currentPosition == length) {
            return null;
        }
        
        if (splitPosition < 0) {
            String returnValue = value;
            value = "";
            PdfChunk pc = new PdfChunk(returnValue, this);
            return pc;
        }
        if (lastSpace > splitPosition && splitCharacter.isSplitCharacter(0, 0, 1, singleSpace, null))
            splitPosition = lastSpace;
        if (hyphenationEvent != null && lastSpace >= 0 && lastSpace < currentPosition) {
            int wordIdx = getWord(value, lastSpace);
            if (wordIdx > lastSpace) {
                String pre = hyphenationEvent.getHyphenatedWordPre(value.substring(lastSpace, wordIdx), font.getFont(), font.size(), width - lastSpaceWidth);
                String post = hyphenationEvent.getHyphenatedWordPost();
                if (pre.length() > 0) {
                    String returnValue = post + value.substring(wordIdx);
                    value = trim(value.substring(0, lastSpace) + pre);
                    PdfChunk pc = new PdfChunk(returnValue, this);
                    return pc;
                }
            }
        }
        String returnValue = value.substring(splitPosition);
        value = trim(value.substring(0, splitPosition));
        PdfChunk pc = new PdfChunk(returnValue, this);
        return pc;
    }
    

    
    PdfChunk truncate(float width) {
        if (image != null) {
            if (image.getScaledWidth() > width) {
                PdfChunk pc = new PdfChunk("", this);
                value = "";
                attributes.remove(Chunk.IMAGE);
                image = null;
                font = PdfFont.getDefaultFont();
                return pc;
            }
            else
                return null;
        }
        
        int currentPosition = 0;
        float currentWidth = 0;
        
        
        if (width < font.width()) {
            String returnValue = value.substring(1);
            value = value.substring(0, 1);
            PdfChunk pc = new PdfChunk(returnValue, this);
            return pc;
        }
        
        
        
        int length = value.length();
        char character;
        while (currentPosition < length) {
            
            character = value.charAt(currentPosition);
            currentWidth += font.width(character);
            if (currentWidth > width)
                break;
            currentPosition++;
        }
        
        
        if (currentPosition == length) {
            return null;
        }
        
        
        
        
        if (currentPosition == 0) {
            currentPosition = 1;
        }
        String returnValue = value.substring(currentPosition);
        value = value.substring(0, currentPosition);
        PdfChunk pc = new PdfChunk(returnValue, this);
        return pc;
    }
    
    
    

    
    PdfFont font() {
        return font;
    }
    

    
    Color color() {
        return (Color)noStroke.get(Chunk.COLOR);
    }
    

    
    float width() {
        return font.width(value);
    }
    

    
    public boolean isNewlineSplit()
    {
        return newlineSplit;
    }
    

    
    public float getWidthCorrected(float charSpacing, float wordSpacing)
    {
        if (image != null) {
            return image.getScaledWidth() + charSpacing;
        }
        int numberOfSpaces = 0;
        int idx = -1;
        while ((idx = value.indexOf(' ', idx + 1)) >= 0)
            ++numberOfSpaces;
        return width() + (value.length() * charSpacing + numberOfSpaces * wordSpacing);
    }
    
    
    public float getTextRise() {
        Float f = (Float) getAttribute(Chunk.SUBSUPSCRIPT);
        if (f != null) {
            return f.floatValue();
        }
        return 0.0f;
    }
    

    
    public float trimLastSpace()
    {
        BaseFont ft = font.getFont();
        if (ft.getFontType() == BaseFont.FONT_TYPE_CJK && ft.getUnicodeEquivalent(' ') != ' ') {
            if (value.length() > 1 && value.endsWith("\u")) {
                value = value.substring(0, value.length() - 1);
                return font.width('\u');
            }
        }
        else {
            if (value.length() > 1 && value.endsWith(" ")) {
                value = value.substring(0, value.length() - 1);
                return font.width(' ');
            }
        }
        return 0;
    }    
    public float trimFirstSpace()
    {
        BaseFont ft = font.getFont();
        if (ft.getFontType() == BaseFont.FONT_TYPE_CJK && ft.getUnicodeEquivalent(' ') != ' ') {
            if (value.length() > 1 && value.startsWith("\u")) {
                value = value.substring(1);
                return font.width('\u');
            }
        }
        else {
            if (value.length() > 1 && value.startsWith(" ")) {
                value = value.substring(1);
                return font.width(' ');
            }
        }
        return 0;
    }
    

    
    Object getAttribute(String name)
    {
        if (attributes.containsKey(name))
            return attributes.get(name);
        return noStroke.get(name);
    }
    

    
    boolean isAttribute(String name)
    {
        if (attributes.containsKey(name))
            return true;
        return noStroke.containsKey(name);
    }
    

    
    boolean isStroked()
    {
        return (!attributes.isEmpty());
    }
    

    
    boolean isImage()
    {
        return image != null;
    }
    

    
    Image getImage()
    {
        return image;
    }
    

    
    void setImageOffsetX(float offsetX)
    {
        this.offsetX = offsetX;
    }
    

    
    float getImageOffsetX()
    {
        return offsetX;
    }
    

    
    void setImageOffsetY(float offsetY)
    {
        this.offsetY = offsetY;
    }
    

    
    float getImageOffsetY()
    {
        return offsetY;
    }
    

    
    void setValue(String value)
    {
        this.value = value;
    }

    
    public String toString() {
        return value;
    }

    
    
    boolean isSpecialEncoding() {
        return encoding.equals(CJKFont.CJK_ENCODING) || encoding.equals(BaseFont.IDENTITY_H);
    }
    
    
    
    String getEncoding() {
        return encoding;
    }

    int length() {
        return value.length();
    }

    public boolean isSplitCharacter(int start, int current, int end, char[] cc, PdfChunk[] ck) {
        char c;
        if (ck == null)
            c = cc[current];
        else
            c = ck[Math.min(current, ck.length - 1)].getUnicodeEquivalent(cc[current]);
        if (c <= ' ' || c == '-' || c == '\u') {
            return true;
        }
        if (c < 0x2002)
            return false;
        return ((c >= 0x2002 && c <= 0x200b)
        || (c >= 0x2e80 && c < 0xd7a0)
        || (c >= 0xf900 && c < 0xfb00)
        || (c >= 0xfe30 && c < 0xfe50)
        || (c >= 0xff61 && c < 0xffa0));
    }
    
    boolean isExtSplitCharacter(int start, int current, int end, char[] cc, PdfChunk[] ck) {
        return splitCharacter.isSplitCharacter(start, current, end, cc, ck);
    }
    
    
    String trim(String string) {
        BaseFont ft = font.getFont();
        if (ft.getFontType() == BaseFont.FONT_TYPE_CJK && ft.getUnicodeEquivalent(' ') != ' ') {
            while (string.endsWith("\u")) {
                string = string.substring(0, string.length() - 1);
            }
        }
        else {
            while (string.endsWith(" ") || string.endsWith("\t")) {
                string = string.substring(0, string.length() - 1);
            }
        }
        return string;
    }

    public boolean changeLeading() {
        return changeLeading;
    }
    
    float getCharWidth(char c) {
        if (noPrint(c))
            return 0;
        return font.width(c);
    }
    
    public static boolean noPrint(char c) {
        return ((c >= 0x200b && c <= 0x200f) || (c >= 0x202a && c <= 0x202e));
    }
    
}
