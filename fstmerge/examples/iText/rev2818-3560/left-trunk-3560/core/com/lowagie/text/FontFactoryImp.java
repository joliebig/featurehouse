

package com.lowagie.text;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import com.lowagie.text.html.Markup;
import com.lowagie.text.pdf.BaseFont;



public class FontFactoryImp {
        

    private Properties trueTypeFonts = new Properties();
    
    private static String[] TTFamilyOrder = {
        "3", "1", "1033",
        "3", "0", "1033",
        "1", "0", "0",
        "0", "3", "0"
    };


    private Hashtable fontFamilies = new Hashtable();
    

    public String defaultEncoding = BaseFont.WINANSI;
    

    public boolean defaultEmbedding = BaseFont.NOT_EMBEDDED;
    

    public FontFactoryImp() {
        trueTypeFonts.setProperty(FontFactory.COURIER.toLowerCase(), FontFactory.COURIER);
        trueTypeFonts.setProperty(FontFactory.COURIER_BOLD.toLowerCase(), FontFactory.COURIER_BOLD);
        trueTypeFonts.setProperty(FontFactory.COURIER_OBLIQUE.toLowerCase(), FontFactory.COURIER_OBLIQUE);
        trueTypeFonts.setProperty(FontFactory.COURIER_BOLDOBLIQUE.toLowerCase(), FontFactory.COURIER_BOLDOBLIQUE);
        trueTypeFonts.setProperty(FontFactory.HELVETICA.toLowerCase(), FontFactory.HELVETICA);
        trueTypeFonts.setProperty(FontFactory.HELVETICA_BOLD.toLowerCase(), FontFactory.HELVETICA_BOLD);
        trueTypeFonts.setProperty(FontFactory.HELVETICA_OBLIQUE.toLowerCase(), FontFactory.HELVETICA_OBLIQUE);
        trueTypeFonts.setProperty(FontFactory.HELVETICA_BOLDOBLIQUE.toLowerCase(), FontFactory.HELVETICA_BOLDOBLIQUE);
        trueTypeFonts.setProperty(FontFactory.SYMBOL.toLowerCase(), FontFactory.SYMBOL);
        trueTypeFonts.setProperty(FontFactory.TIMES_ROMAN.toLowerCase(), FontFactory.TIMES_ROMAN);
        trueTypeFonts.setProperty(FontFactory.TIMES_BOLD.toLowerCase(), FontFactory.TIMES_BOLD);
        trueTypeFonts.setProperty(FontFactory.TIMES_ITALIC.toLowerCase(), FontFactory.TIMES_ITALIC);
        trueTypeFonts.setProperty(FontFactory.TIMES_BOLDITALIC.toLowerCase(), FontFactory.TIMES_BOLDITALIC);
        trueTypeFonts.setProperty(FontFactory.ZAPFDINGBATS.toLowerCase(), FontFactory.ZAPFDINGBATS);

        ArrayList tmp;
        tmp = new ArrayList();
        tmp.add(FontFactory.COURIER);
        tmp.add(FontFactory.COURIER_BOLD);
        tmp.add(FontFactory.COURIER_OBLIQUE);
        tmp.add(FontFactory.COURIER_BOLDOBLIQUE);
        fontFamilies.put(FontFactory.COURIER.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.HELVETICA);
        tmp.add(FontFactory.HELVETICA_BOLD);
        tmp.add(FontFactory.HELVETICA_OBLIQUE);
        tmp.add(FontFactory.HELVETICA_BOLDOBLIQUE);
        fontFamilies.put(FontFactory.HELVETICA.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.SYMBOL);
        fontFamilies.put(FontFactory.SYMBOL.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.TIMES_ROMAN);
        tmp.add(FontFactory.TIMES_BOLD);
        tmp.add(FontFactory.TIMES_ITALIC);
        tmp.add(FontFactory.TIMES_BOLDITALIC);
        fontFamilies.put(FontFactory.TIMES.toLowerCase(), tmp);
        fontFamilies.put(FontFactory.TIMES_ROMAN.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.ZAPFDINGBATS);
        fontFamilies.put(FontFactory.ZAPFDINGBATS.toLowerCase(), tmp);
    }
    
    
    public Font getFont(String fontname, String encoding, boolean embedded, float size, int style, Color color) {
        return getFont(fontname, encoding, embedded, size, style, color, true);
    }
    
    
    
    
    public Font getFont(String fontname, String encoding, boolean embedded, float size, int style, Color color, boolean cached) {
        if (fontname == null) return new Font(Font.UNDEFINED, size, style, color);
        String lowercasefontname = fontname.toLowerCase();
        ArrayList tmp = (ArrayList) fontFamilies.get(lowercasefontname);
        if (tmp != null) {
            
            int s = style == Font.UNDEFINED ? Font.NORMAL : style;
            int fs = Font.NORMAL;
            boolean found = false;
            for (Iterator i = tmp.iterator(); i.hasNext(); ) {
                String f = (String) i.next();
                String lcf = f.toLowerCase();
                fs = Font.NORMAL;
                if (lcf.toLowerCase().indexOf("bold") != -1) fs |= Font.BOLD;
                if (lcf.toLowerCase().indexOf("italic") != -1 || lcf.toLowerCase().indexOf("oblique") != -1) fs |= Font.ITALIC;
                if ((s & Font.BOLDITALIC) == fs) {
                    fontname = f;
                    found = true;
                    break;
                }
            }
            if (style != Font.UNDEFINED && found) {
                style &= ~fs;
            }
        }
        BaseFont basefont = null;
        try {
            try {
                
                basefont = BaseFont.createFont(fontname, encoding, embedded, cached, null, null, true);
            }
            catch(DocumentException de) {
            }
            if (basefont == null) {
                
                fontname = trueTypeFonts.getProperty(fontname.toLowerCase());
                
                if (fontname == null) return new Font(Font.UNDEFINED, size, style, color);
                
                basefont = BaseFont.createFont(fontname, encoding, embedded, cached, null, null);
            }
        }
        catch(DocumentException de) {
            
            throw new ExceptionConverter(de);
        }
        catch(IOException ioe) {
            
            return new Font(Font.UNDEFINED, size, style, color);
        }
        catch(NullPointerException npe) {
            
            return new Font(Font.UNDEFINED, size, style, color);
        }
        return new Font(basefont, size, style, color);
    }
    
    

    
    public Font getFont(Properties attributes) {
        String fontname = null;
        String encoding = defaultEncoding;
        boolean embedded = defaultEmbedding;
        float size = Font.UNDEFINED;
        int style = Font.NORMAL;
        Color color = null;
        String value = attributes.getProperty(Markup.HTML_ATTR_STYLE);
        if (value != null && value.length() > 0) {
            Properties styleAttributes = Markup.parseAttributes(value);
            if (styleAttributes.isEmpty()) {
                attributes.put(Markup.HTML_ATTR_STYLE, value);
            }
            else {
                fontname = styleAttributes.getProperty(Markup.CSS_KEY_FONTFAMILY);
                if (fontname != null) {
                    String tmp;
                    while (fontname.indexOf(',') != -1) {
                        tmp = fontname.substring(0, fontname.indexOf(','));
                        if (isRegistered(tmp)) {
                            fontname = tmp;
                        }
                        else {
                            fontname = fontname.substring(fontname.indexOf(',') + 1);
                        }
                    }
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_FONTSIZE)) != null) {
                    size = Markup.parseLength(value);
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_FONTWEIGHT)) != null) {
                    style |= Font.getStyleValue(value);
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_FONTSTYLE)) != null) {
                    style |= Font.getStyleValue(value);
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_COLOR)) != null) {
                    color = Markup.decodeColor(value);
                }
                attributes.putAll(styleAttributes);
                for (Enumeration e = styleAttributes.keys(); e.hasMoreElements();) {
                    Object o = e.nextElement();
                    attributes.put(o, styleAttributes.get(o));
                }
            }
        }
        if ((value = attributes.getProperty(ElementTags.ENCODING)) != null) {
            encoding = value;
        }
        if ("true".equals(attributes.getProperty(ElementTags.EMBEDDED))) {
            embedded = true;
        }
        if ((value = attributes.getProperty(ElementTags.FONT)) != null) {
            fontname = value;
        }
        if ((value = attributes.getProperty(ElementTags.SIZE)) != null) {
            size = Markup.parseLength(value);
        }
        if ((value = attributes.getProperty(Markup.HTML_ATTR_STYLE)) != null) {
            style |= Font.getStyleValue(value);
        }
        if ((value = attributes.getProperty(ElementTags.STYLE)) != null) {
            style |= Font.getStyleValue(value);
        }
        String r = attributes.getProperty(ElementTags.RED);
        String g = attributes.getProperty(ElementTags.GREEN);
        String b = attributes.getProperty(ElementTags.BLUE);
        if (r != null || g != null || b != null) {
            int red = 0;
            int green = 0;
            int blue = 0;
            if (r != null) red = Integer.parseInt(r);
            if (g != null) green = Integer.parseInt(g);
            if (b != null) blue = Integer.parseInt(b);
            color = new Color(red, green, blue);
        }
        else if ((value = attributes.getProperty(ElementTags.COLOR)) != null) {
            color = Markup.decodeColor(value);
        }
        if (fontname == null) {
            return getFont(null, encoding, embedded, size, style, color);
        }
        return getFont(fontname, encoding, embedded, size, style, color);
    }
    

    
    public Font getFont(String fontname, String encoding, boolean embedded, float size, int style) {
        return getFont(fontname, encoding, embedded, size, style, null);
    }
    

    
    public Font getFont(String fontname, String encoding, boolean embedded, float size) {
        return getFont(fontname, encoding, embedded, size, Font.UNDEFINED, null);
    }
    

    
    public Font getFont(String fontname, String encoding, boolean embedded) {
        return getFont(fontname, encoding, embedded, Font.UNDEFINED, Font.UNDEFINED, null);
    }
    

    
    public Font getFont(String fontname, String encoding, float size, int style, Color color) {
        return getFont(fontname, encoding, defaultEmbedding, size, style, color);
    }
    

    
    public Font getFont(String fontname, String encoding, float size, int style) {
        return getFont(fontname, encoding, defaultEmbedding, size, style, null);
    }
    

    
    public Font getFont(String fontname, String encoding, float size) {
        return getFont(fontname, encoding, defaultEmbedding, size, Font.UNDEFINED, null);
    }
    


    
    public Font getFont(String fontname, float size, Color color) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, Font.UNDEFINED, color);
    }
    

    
    public Font getFont(String fontname, String encoding) {
        return getFont(fontname, encoding, defaultEmbedding, Font.UNDEFINED, Font.UNDEFINED, null);
    }
    

    
    public Font getFont(String fontname, float size, int style, Color color) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, style, color);
    }
    

    
    public Font getFont(String fontname, float size, int style) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, style, null);
    }
    

    
    public Font getFont(String fontname, float size) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, size, Font.UNDEFINED, null);
    }
    

    
    public Font getFont(String fontname) {
        return getFont(fontname, defaultEncoding, defaultEmbedding, Font.UNDEFINED, Font.UNDEFINED, null);
    }
    
    
    public void registerFamily(String familyName, String fullName, String path) {
        if (path != null)
            trueTypeFonts.setProperty(fullName, path);
        ArrayList tmp = (ArrayList) fontFamilies.get(familyName);
        if (tmp == null) {
            tmp = new ArrayList();
            tmp.add(fullName);
            fontFamilies.put(familyName, tmp);
        }
        else {
            int fullNameLength = fullName.length();
            boolean inserted = false;
            for (int j = 0; j < tmp.size(); ++j) {
                if (((String)tmp.get(j)).length() >= fullNameLength) {
                    tmp.add(j, fullName);
                    inserted = true;
                    break;
                }
            }
            if (!inserted)
                tmp.add(fullName);
        }
    }
    

    
    public void register(String path) {
        register(path, null);
    }
    

    
    public void register(String path, String alias) {
        try {
            if (path.toLowerCase().endsWith(".ttf") || path.toLowerCase().endsWith(".otf") || path.toLowerCase().indexOf(".ttc,") > 0) {
                Object allNames[] = BaseFont.getAllFontNames(path, BaseFont.WINANSI, null);
                trueTypeFonts.setProperty(((String)allNames[0]).toLowerCase(), path);
                if (alias != null) {
                    trueTypeFonts.setProperty(alias.toLowerCase(), path);
                }
                
                String[][] names = (String[][])allNames[2]; 
                for (int i = 0; i < names.length; i++) {
                    trueTypeFonts.setProperty(names[i][3].toLowerCase(), path);
                }
                String fullName = null;
                String familyName = null;
                names = (String[][])allNames[1]; 
                for (int k = 0; k < TTFamilyOrder.length; k += 3) {
                    for (int i = 0; i < names.length; i++) {
                        if (TTFamilyOrder[k].equals(names[i][0]) && TTFamilyOrder[k + 1].equals(names[i][1]) && TTFamilyOrder[k + 2].equals(names[i][2])) {
                            familyName = names[i][3].toLowerCase();
                            k = TTFamilyOrder.length;
                            break;
                        }
                    }
                }
                if (familyName != null) {
                    String lastName = "";
                    names = (String[][])allNames[2]; 
                    for (int i = 0; i < names.length; i++) {
                        for (int k = 0; k < TTFamilyOrder.length; k += 3) {
                            if (TTFamilyOrder[k].equals(names[i][0]) && TTFamilyOrder[k + 1].equals(names[i][1]) && TTFamilyOrder[k + 2].equals(names[i][2])) {
                                fullName = names[i][3];
                                if (fullName.equals(lastName))
                                    continue;
                                lastName = fullName;
                                registerFamily(familyName, fullName, null);
                                break;
                            }
                        }
                    }
                }
            }
            else if (path.toLowerCase().endsWith(".ttc")) {
                if (alias != null)
                    System.err.println("class FontFactory: You can't define an alias for a true type collection.");
                String[] names = BaseFont.enumerateTTCNames(path);
                for (int i = 0; i < names.length; i++) {
                    register(path + "," + i);
                }
            }
            else if (path.toLowerCase().endsWith(".afm") || path.toLowerCase().endsWith(".pfm")) {
                BaseFont bf = BaseFont.createFont(path, BaseFont.CP1252, false);
                String fullName = bf.getFullFontName()[0][3].toLowerCase();
                String familyName = bf.getFamilyFontName()[0][3].toLowerCase();
                String psName = bf.getPostscriptFontName().toLowerCase();
                registerFamily(familyName, fullName, null);
                trueTypeFonts.setProperty(psName, path);
                trueTypeFonts.setProperty(fullName, path);
            }
        }
        catch(DocumentException de) {
            
            throw new ExceptionConverter(de);
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }

        
    public int registerDirectory(String dir) {
        return registerDirectory(dir, false);
    }

    
    public int registerDirectory(String dir, boolean scanSubdirectories) {
        int count = 0;
        try {
            File file = new File(dir);
            if (!file.exists() || !file.isDirectory())
                return 0;
            String files[] = file.list();
            if (files == null)
                return 0;
            for (int k = 0; k < files.length; ++k) {
                try {
                    file = new File(dir, files[k]);
                    if (file.isDirectory()) {
                        if (scanSubdirectories) {
                            count += registerDirectory(file.getAbsolutePath(), true);
                        }
                    } else {
                        String name = file.getPath();
                        String suffix = name.length() < 4 ? null : name.substring(name.length() - 4).toLowerCase();
                        if (".afm".equals(suffix) || ".pfm".equals(suffix)) {
                            
                            File pfb = new File(name.substring(0, name.length() - 4) + ".pfb");
                            if (pfb.exists()) {
                                register(name, null);
                                ++count;
                            }
                        } else if (".ttf".equals(suffix) || ".otf".equals(suffix) || ".ttc".equals(suffix)) {
                            register(name, null);
                            ++count;
                        }
                    }
                }
                catch (Exception e) {
                    
                }
            }
        }
        catch (Exception e) {
            
        }
        return count;
    }

        
    public int registerDirectories() {
        int count = 0;
        count += registerDirectory("c:/windows/fonts");
        count += registerDirectory("c:/winnt/fonts");
        count += registerDirectory("d:/windows/fonts");
        count += registerDirectory("d:/winnt/fonts");
        count += registerDirectory("/usr/share/X11/fonts", true);
        count += registerDirectory("/usr/X/lib/X11/fonts", true);
        count += registerDirectory("/usr/openwin/lib/X11/fonts", true);
        count += registerDirectory("/usr/share/fonts", true);
        count += registerDirectory("/usr/X11R6/lib/X11/fonts", true);
        count += registerDirectory("/Library/Fonts");
        count += registerDirectory("/System/Library/Fonts");
        return count;
    }


    
    public Set getRegisteredFonts() {
        return Utilities.getKeySet(trueTypeFonts);
    }
    

    
    public Set getRegisteredFamilies() {
        return Utilities.getKeySet(fontFamilies);
    }
    

    public boolean isRegistered(String fontname) {
        return trueTypeFonts.containsKey(fontname.toLowerCase());
    }
}
