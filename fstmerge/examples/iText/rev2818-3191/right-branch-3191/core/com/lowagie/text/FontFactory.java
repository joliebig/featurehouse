

package com.lowagie.text;

import java.awt.Color;
import java.util.Properties;
import java.util.Set;

import com.lowagie.text.pdf.BaseFont;



public class FontFactory {
    

    public static final String COURIER = BaseFont.COURIER;
    

    public static final String COURIER_BOLD = BaseFont.COURIER_BOLD;
    

    public static final String COURIER_OBLIQUE = BaseFont.COURIER_OBLIQUE;
    

    public static final String COURIER_BOLDOBLIQUE = BaseFont.COURIER_BOLDOBLIQUE;
    

    public static final String HELVETICA = BaseFont.HELVETICA;
    

    public static final String HELVETICA_BOLD = BaseFont.HELVETICA_BOLD;
    

    public static final String HELVETICA_OBLIQUE = BaseFont.HELVETICA_OBLIQUE;
    

    public static final String HELVETICA_BOLDOBLIQUE = BaseFont.HELVETICA_BOLDOBLIQUE;
    

    public static final String SYMBOL = BaseFont.SYMBOL;
    

    public static final String TIMES = "Times";
    

    public static final String TIMES_ROMAN = BaseFont.TIMES_ROMAN;
    

    public static final String TIMES_BOLD = BaseFont.TIMES_BOLD;
    

    public static final String TIMES_ITALIC = BaseFont.TIMES_ITALIC;
    

    public static final String TIMES_BOLDITALIC = BaseFont.TIMES_BOLDITALIC;
    

    public static final String ZAPFDINGBATS = BaseFont.ZAPFDINGBATS;
    
    private static FontFactoryImp fontImp = new FontFactoryImp();
    

    public static String defaultEncoding = BaseFont.WINANSI;
    

    public static boolean defaultEmbedding = BaseFont.NOT_EMBEDDED;
    

    private FontFactory() {
    }
    

    
    public static Font getFont(String fontname, String encoding, boolean embedded, float size, int style, Color color) {
        return fontImp.getFont(fontname, encoding, embedded, size, style, color);
    }
    

    
    public static Font getFont(String fontname, String encoding, boolean embedded, float size, int style, Color color, boolean cached) {
        return fontImp.getFont(fontname, encoding, embedded, size, style, color, cached);
    }
    

    
    public static Font getFont(Properties attributes) {
        fontImp.defaultEmbedding = defaultEmbedding;
        fontImp.defaultEncoding = defaultEncoding;
        return fontImp.getFont(attributes);
    }
    

    
    public static Font getFont(String fontname, String encoding, boolean embedded, float size, int style) {
        return getFont(fontname, encoding, embedded, size, style, null);
    }
    

    
    public static Font getFont(String fontname, String encoding, boolean embedded, float size) {
        return getFont(fontname, encoding, embedded, size, Font.UNDEFINED, null);
    }
    

    
    public static Font getFont(String fontname, String encoding, boolean embedded) {
        return getFont(fontname, encoding, embedded, Font.UNDEFINED, Font.UNDEFINED, null);
    }
    

    
    public static Font getFont(String fontname, String encoding, float size, int style, Color color) {
        return getFont(fontname, encoding, defaultEmbedding, size, style, color);
    }
    

    
    public static Font getFont(String fontname, String encoding, float size, int style) {
        return getFont(fontname, encoding, defaultEmbedding, size, style, null);
    }
    

    
    public static Font getFont(String fontname, String encoding, float size) {
        return getFont(fontname, encoding, defaultEmbedding, size, Font.UNDEFINED, null);
    }
    

    
    public static Font getFont(String fontname, String encoding) {
        return getFont(fontname, encoding, defaultEmbedding, Font.UNDEFINED, Font.UNDEFINED, null);
    }
    

    
    public static Font getFont(String fontname, float size, int style, Color color) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, style, color);
    }
    

    
    public static Font getFont(String fontname, float size, int style) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, style, null);
    }
    

    
    public static Font getFont(String fontname, float size) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, Font.UNDEFINED, null);
    }
    

    
    public static Font getFont(String fontname) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, Font.UNDEFINED, Font.UNDEFINED, null);
    }
    
    
    public void registerFamily(String familyName, String fullName, String path) {
        fontImp.registerFamily(familyName, fullName, path);
    }
    

    
    public static void register(String path) {
        register(path, null);
    }
    

    
    public static void register(String path, String alias) {
        fontImp.register(path, alias);
    }

        
    public static int registerDirectory(String dir) {
        return fontImp.registerDirectory(dir);
    }

        
    public static int registerDirectories() {
        return fontImp.registerDirectories();
    }


    
    public static Set<String> getRegisteredFonts() {
        return fontImp.getRegisteredFonts();
    }
    

    
    public static Set<String> getRegisteredFamilies() {
        return fontImp.getRegisteredFamilies();
    }
    

    
    public static boolean contains(String fontname) {
        return fontImp.isRegistered(fontname);
    }
    

    
    public static boolean isRegistered(String fontname) {
        return fontImp.isRegistered(fontname);
    }
    
        
    public static FontFactoryImp getFontImp() {
        return fontImp;
    }
    
        
    public static void setFontImp(FontFactoryImp fontImp) {
        if (fontImp == null)
            throw new NullPointerException("FontFactoryImp cannot be null.");
        FontFactory.fontImp = fontImp;
    }
}