
 
package com.lowagie.text.rtf.parser.destinations;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Properties;

import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.rtf.parser.RtfImportMgr;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public final class RtfDestinationFontTable extends RtfDestination {
    
    private RtfImportMgr importHeader = null;
    
    private String themeFont = "";
    
    private String fontNr = "";
    
    private String fontFamily = "";
    
    private String charset = "";
    
    private int fprq = 0;
    
    private String panose = "";
    
    private String nontaggedname = "";
    
    private String fontName = "";
    
    private String falt = "";
    
    private String fontemb = "";
    
    private String fontType = "";
    
    private String fontFile = "";
    
    private String fontFileCpg = "";
    
    private int fbias = 0;
    
    private String cpg = "";
    
    private String trueType = "";

    
    private int state = 0;
    
    
    private static final int SETTING_NORMAL = 0;
    
    private static final int SETTING_ALTERNATE = 1;
    
    private static final int SETTING_FONTNAME = 2;
    
    private static final int SETTING_PANOSE = 3;
    
    private static final int SETTING_FONT_EMBED = 4;
    
    private static final int SETTING_FONT_FILE = 5;
    
    
    private HashMap<String, Font> fontMap = null;
    
    
    public RtfDestinationFontTable() {
        super(null);
    }
    
    public RtfDestinationFontTable(RtfParser parser) {
        super(parser);
        this.init(true);
    }
    
    
    public void setParser(RtfParser parser) {
        if(this.rtfParser != null && this.rtfParser.equals(parser)) return;
        this.rtfParser = parser;
        this.init(true);
    }
    
    private void init(boolean importFonts) {
        fontMap = new HashMap<String, Font>();
        if(this.rtfParser != null) {
            this.importHeader = this.rtfParser.getImportManager();
        }
        this.setToDefaults();
        if(importFonts) {
            importSystemFonts();
        }
    }
    
    public boolean handleOpeningSubGroup() {
        return true;
    }
    
    public boolean closeDestination() {
        return true;
    }

    
    public boolean handleCloseGroup() {
        if(this.state == SETTING_NORMAL) {
            processFont();
        }
        this.state = SETTING_NORMAL;
        return true;
    }

    
    public boolean handleOpenGroup() {

        return true;
    }
    
    
    public boolean handleCharacter(int ch) {
        switch(this.state) {
        case SETTING_NORMAL:
            this.fontName += (char)ch;
            break;
        case SETTING_ALTERNATE:
            this.falt += (char)ch;
            break;
        case SETTING_PANOSE:
            this.panose += (char)ch;
            break;
        case SETTING_FONT_EMBED:
            break;
        case SETTING_FONT_FILE:
            break;
        case SETTING_FONTNAME:
            break;
            
        }
        return true;
    }
    
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        boolean result = true;
        
        
        if(ctrlWordData.ctrlWord.equals("f")) { this.setFontNumber(ctrlWordData.param); result=true;}
        if(ctrlWordData.ctrlWord.equals("fcharset")) { this.setCharset(ctrlWordData.param); result=true; }

        
        if(ctrlWordData.ctrlWord.equals("fnil")) { this.setFontFamily("roman"); result=true; }
        if(ctrlWordData.ctrlWord.equals("froman")) { this.setFontFamily("roman"); result=true; }
        if(ctrlWordData.ctrlWord.equals("fswiss")) { this.setFontFamily("swiss"); result=true; }
        if(ctrlWordData.ctrlWord.equals("fmodern")) { this.setFontFamily("modern"); result=true; }
        if(ctrlWordData.ctrlWord.equals("fscript")) { this.setFontFamily("script"); result=true; }
        if(ctrlWordData.ctrlWord.equals("fdecor")) { this.setFontFamily("decor"); result=true; }
        if(ctrlWordData.ctrlWord.equals("ftech")) { this.setFontFamily("tech"); result=true; }
        if(ctrlWordData.ctrlWord.equals("fbidi")) { this.setFontFamily("bidi"); result=true; }
        
        if(ctrlWordData.ctrlWord.equals("fprq")) { this.setPitch(ctrlWordData.param); result=true; }
        
        if(ctrlWordData.ctrlWord.equals("fbias")) { this.setBias(ctrlWordData.param); result=true; }
        
        if(ctrlWordData.ctrlWord.equals("flomajor")) { this.setThemeFont("flomajor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fhimajor")) { this.setThemeFont("fhimajor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fdbmajor")) { this.setThemeFont("fdbmajor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fbimajor")) { this.setThemeFont("fbimajor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("flominor")) { this.setThemeFont("flominor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fhiminor")) { this.setThemeFont("fhiminor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fdbminor")) { this.setThemeFont("fdbminor"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fbiminor")) { this.setThemeFont("fbiminor"); result= true; }

        
        if(ctrlWordData.ctrlWord.equals("panose")) {state = SETTING_PANOSE; result = true; }
        
        
        
        if(ctrlWordData.ctrlWord.equals("fname")) {state = SETTING_FONTNAME; result = true; }

        
        if(ctrlWordData.ctrlWord.equals("falt")) { state = SETTING_ALTERNATE; result = true; }
        
        
        if(ctrlWordData.ctrlWord.equals("fontemb")) { state = SETTING_FONT_EMBED; result = true; }

        
        if(ctrlWordData.ctrlWord.equals("ftnil")) { this.setTrueType("ftnil"); result= true; }
        if(ctrlWordData.ctrlWord.equals("fttruetype")) { this.setTrueType("fttruetype"); result= true; }
        
        
        if(ctrlWordData.ctrlWord.equals("fontemb")) { state = SETTING_FONT_FILE; result = true; }

        
        if(ctrlWordData.ctrlWord.equals("cpg")) { this.setCodePage(ctrlWordData.param); result= true; }
        
        this.lastCtrlWord = ctrlWordData;
        return result;
    }
    
    public void setCodePage(String value) {
        this.cpg = value;
    }
    
    public void setTrueType(String value) {
        this.trueType = value;
    }
    
    public void setPitch(String value) {
        this.fprq = Integer.parseInt(value);
    }
    
    public void setBias(String value) {
        this.fbias = Integer.parseInt(value);
    }
    
    public void setThemeFont(String themeFont) {
        this.themeFont = themeFont;
    }
    
    public void setFontName(String fontName) {
        this.fontName = fontName;
    }
    
    public void setFontFamily(String fontFamily) {
        this.fontFamily = fontFamily;
    }
    
    public void setFontNumber(String fontNr) {
        this.fontNr = fontNr;
    }
    
    public void setFontAlternate(String fontAlternate) {
        this.falt = fontAlternate;
    }
    
    public void setCharset(String charset) {
        if(charset.length() == 0) {
            charset = "0";
        }
        this.charset = charset;
    }

    
    public void setToDefaults() {
        this.themeFont = "";
        this.fontNr = "";
        this.fontName = "";
        this.fontFamily = "";
        
        this.charset = "";
        this.fprq = 0;
        this.panose = "";
        this.nontaggedname = "";
        this.falt = "";
        this.fontemb = "";
        this.fontType = "";
        this.fontFile = "";
        this.fontFileCpg = "";
        this.fbias = 0;
        this.cpg = "";
        this.trueType = "";
        this.state = SETTING_NORMAL;
    }
    
    private void processFont() {
        this.fontName = this.fontName.trim();
        if(fontName.length() == 0) return;
        if(fontNr.length() == 0) return;
        
        if(fontName.length()>0 && fontName.indexOf(';') >= 0) {
            fontName = fontName.substring(0,fontName.indexOf(';'));
        }

        if(this.rtfParser.isImport()) {
            
                
                
                
    
    
    
    
    
    
    
                    if(!this.importHeader.importFont(this.fontNr, this.fontName, Integer.parseInt(this.charset))) {
                        if(this.falt.length() > 0) {
                            this.importHeader.importFont(this.fontNr, this.falt, Integer.parseInt(this.charset));
                        }
                    }
    
            }
        if(this.rtfParser.isConvert()) {
            
            
            String fName = this.fontName;    
            Font f1 = createfont(fName);
            if(f1.getBaseFont() == null && this.falt.length()>0)
                f1 = createfont(this.falt);
            
            if(f1.getBaseFont() == null) {
                
                if(FontFactory.COURIER.indexOf(fName) > -1 ) {
                    f1 = FontFactory.getFont(FontFactory.COURIER);
                } else if(FontFactory.HELVETICA.indexOf(fName) > -1 ) {
                    f1 = FontFactory.getFont(FontFactory.HELVETICA);
                } else if(FontFactory.TIMES.indexOf(fName) > -1 ) {
                    f1 = FontFactory.getFont(FontFactory.TIMES);
                } else if(FontFactory.SYMBOL.indexOf(fName) > -1 ) {
                    f1 = FontFactory.getFont(FontFactory.SYMBOL);
                } else if(FontFactory.ZAPFDINGBATS.indexOf(fName) > -1 ) {
                    f1 = FontFactory.getFont(FontFactory.ZAPFDINGBATS);
                } else {
                    
                    
                    f1 = FontFactory.getFont(FontFactory.HELVETICA);
                }
            }
            fontMap.put(this.fontNr, f1);
            
        }
        this.setToDefaults();
    }
    
    private Font createfont(String fontName) {
        Font f1 = null;
        int pos=-1;
        do {
            f1 = FontFactory.getFont(fontName);
            
            if(f1.getBaseFont() != null) break;    
            
            pos = fontName.lastIndexOf(' ');    
            if(pos>0) {
                fontName = fontName.substring(0, pos );    
            }
        } while(pos>0);
        return f1;
    }
    
    public Font getFont(String key) {
        return fontMap.get(key);
    }
    
    private void importSystemFonts() {
        Properties pr = null;
        try {
            pr = getEnvironmentVariables();
        } catch (Throwable e) {
        }
        String systemRoot = pr.getProperty("SystemRoot");
        Runtime runtime = Runtime.getRuntime();
        String fileSeperator = System.getProperty("file.separator");
        int r = FontFactory.registerDirectory(systemRoot + fileSeperator + "fonts");
    }
    
    
     private Properties getEnvironmentVariables() throws Throwable {
        Properties environmentVariables = new Properties();
        String operatingSystem = System.getProperty("os.name").toLowerCase();
        Runtime runtime = Runtime.getRuntime();
        Process process = null;
        if (operatingSystem.indexOf("windows 95") > -1
                || operatingSystem.indexOf("windows 98") > -1
                || operatingSystem.indexOf("me") > -1) {
            process = runtime.exec("command.com /c set");
        } else if ((operatingSystem.indexOf("nt") > -1)
                || (operatingSystem.indexOf("windows 2000") > -1)
                || (operatingSystem.indexOf("windows xp") > -1)
                || (operatingSystem.indexOf("windows 2003") > -1)) {
            process = runtime.exec("cmd.exe /c set");
        } else {
            process = runtime.exec("env");
        }
        BufferedReader environmentStream = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String inputLine = "";
        int idx = -1;
        while ((inputLine = environmentStream.readLine()) != null) {
            idx = inputLine.indexOf('=');
            environmentVariables.setProperty(inputLine.substring(0, idx),
                    inputLine.substring(idx + 1));
        }
        return environmentVariables;
    }

}
