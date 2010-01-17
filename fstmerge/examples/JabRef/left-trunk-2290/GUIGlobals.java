

package net.sf.jabref;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JLabel;


public class GUIGlobals {

	
	public static String
	frameTitle = "JabRef",

	version = Globals.VERSION,
	stringsTitle = "Strings for database",
	
	untitledTitle = "untitled",
	helpTitle = "JabRef help",
	TYPE_HEADER = "entrytype",
	NUMBER_COL = "#",
	encPrefix = "Encoding: ", 
	linuxDefaultLookAndFeel = "com.jgoodies.looks.plastic.Plastic3DLookAndFeel",
	


	windowsDefaultLookAndFeel = "com.jgoodies.looks.windows.WindowsLookAndFeel";

	public static Font CURRENTFONT,
	typeNameFont,
	jabRefFont,
	fieldNameFont;

	
	public static final String SIGNATURE =
		"This file was created with JabRef";

	
	static Dimension
	helpSize = new Dimension(700, 600),
	aboutSize = new Dimension(600, 265),
	searchPaneSize = new Dimension(430, 70),
	searchFieldSize = new Dimension(215, 25);

	
	public static final int
	SPLIT_PANE_DIVIDER_SIZE = 4,
	SPLIT_PANE_DIVIDER_LOCATION = 145 + 15, 
	TABLE_ROW_PADDING = 4,
	KEYBIND_COL_0 = 200,
	KEYBIND_COL_1 = 80, 
	PREVIEW_PANEL_PADDING = 15, 
	PREVIEW_PANEL_HEIGHT = 200,
	MAX_CONTENT_SELECTOR_WIDTH = 240; 
	
	
	
	public static final double
	VERTICAL_DIVIDER_LOCATION = 0.4;

	
	public static String 
	backupExt = ".bak",
	tempExt = ".tmp",
	defaultDir = ".";

	
	public static String
	imageSize = "24",
	extension = ".gif",
	ex = imageSize + extension,
	pre = "/images/",
	helpPre = "/help/",
	fontPath = "/images/font/";

	static HashMap<String, JLabel> tableIcons = new HashMap<String, JLabel>(); 
	
	public static Color activeEditor = new Color(230, 230, 255);

	static HashMap<String, String> iconMap;

	public static JLabel getTableIcon(String fieldType) {
		Object o = tableIcons.get(fieldType);
		if (o == null) {
			Globals.logger("Error: no table icon defined for type '"+fieldType+"'.");
			return null;
		} else return (JLabel)o;
	}


	
	public static String
	baseFrameHelp = "BaseFrameHelp.html",
	entryEditorHelp = "EntryEditorHelp.html",
	stringEditorHelp = "StringEditorHelp.html",
	helpContents = "Contents.html",
	searchHelp = "SearchHelp.html",
	groupsHelp = "GroupsHelp.html",
	customEntriesHelp = "CustomEntriesHelp.html",
	contentSelectorHelp = "ContentSelectorHelp.html",
	labelPatternHelp = "LabelPatterns.html",
	ownerHelp = "OwnerHelp.html",
	timeStampHelp = "TimeStampHelp.html",
	pdfHelp = "ExternalFiles.html",
	exportCustomizationHelp = "CustomExports.html",
	importCustomizationHelp = "CustomImports.html",
	medlineHelp = "MedlineHelp.html",
	citeSeerHelp = "CiteSeerHelp.html",
	generalFieldsHelp = "GeneralFields.html",

	aboutPage = "About.html",
	shortPlainImport="ShortPlainImport.html",
	importInspectionHelp = "ImportInspectionDialog.html",
	shortIntegrityCheck="ShortIntegrityCheck.html",
	shortAuxImport="ShortAuxImport.html",
	remoteHelp = "RemoteHelp.html",
	journalAbbrHelp = "JournalAbbreviations.html",
	regularExpressionSearchHelp = "ExternalFiles.html#RegularExpressionSearch",
	nameFormatterHelp = "CustomExports.html#NameFormatter",
	previewHelp = "PreviewHelp.html";


	public static Color
	lightGray = new Color(230, 30, 30), 
	validFieldColor = new Color(100, 100, 150), 
	nullFieldColor = new Color(75, 130, 95), 
	invalidFieldColor = new Color(141, 0, 61), 

	validFieldBackground = Color.white, 

	invalidFieldBackground = new Color(255, 100, 100), 
	gradientGray = new Color(112, 121, 165),  
	gradientBlue = new Color(0, 27, 102),  
	
	
	activeTabbed = validFieldColor.darker(),  
	inActiveTabbed = Color.black,  
	infoField = new Color(254, 255, 225) 
	;

	public static String META_FLAG = "jabref-meta: ";
	public static String META_FLAG_OLD = "bibkeeper-meta: ";
	public static String ENTRYTYPE_FLAG = "jabref-entrytype: ";

	
	public static final double
	DEFAULT_FIELD_WEIGHT = 1,
	MAX_FIELD_WEIGHT = 2;

    
    public static final int
        STANDARD_EDITOR=1,
        FILE_LIST_EDITOR=2;

    public static final String FILE_FIELD = "file";

    public static final double
	SMALL_W = 0.30,
	MEDIUM_W = 0.5,
	LARGE_W = 1.5 ;

	public static final double PE_HEIGHT = 2;


	public static int[] FORM_WIDTH = new int[] { 500, 650, 820};
	public static int[] FORM_HEIGHT = new int[] { 90, 110, 130};


	public static final int
	INDENT = 4,
	LINE_LENGTH = 65; 

	public static int DEFAULT_FIELD_LENGTH = 100,
	NUMBER_COL_LENGTH = 32,
	WIDTH_ICON_COL = 19;

	
	public static final int
	EXPORT_DIALOG_COL_0_WIDTH = 50,
	EXPORT_DIALOG_COL_1_WIDTH = 200,
	EXPORT_DIALOG_COL_2_WIDTH = 30;

	
	public static final int
	IMPORT_DIALOG_COL_0_WIDTH = 200,
	IMPORT_DIALOG_COL_1_WIDTH = 80,
	IMPORT_DIALOG_COL_2_WIDTH = 200,
	IMPORT_DIALOG_COL_3_WIDTH = 200;

	public static final Map<String, String> LANGUAGES;

	static {
		LANGUAGES = new HashMap<String, String>();
		
		LANGUAGES.put("English", "en");
		LANGUAGES.put("Deutsch", "de");
		LANGUAGES.put("Fran\uis", "fr");
		LANGUAGES.put("Italiano", "it");
        LANGUAGES.put("Nederlands", "du");
        LANGUAGES.put("Norsk", "no");

	}

	
	public static void setUpIconTheme() {
		String defaultPrefix = "/images/crystal_16/", prefix = defaultPrefix;

		URL defaultResource = GUIGlobals.class.getResource(prefix+"Icons.properties");
		URL resource = defaultResource;

		if (Globals.prefs.getBoolean("useCustomIconTheme")) {
			String filename = Globals.prefs.get("customIconThemeFile");
			if (filename != null)
				try {
					File file = new File(filename);
					String parent = file.getParentFile().getAbsolutePath();
					prefix = "file://"+parent+System.getProperty("file.separator");
					resource = new URL("file://"+file.getAbsolutePath());
				} catch (MalformedURLException e) {
					e.printStackTrace();
				}
		}
		try {
			iconMap = readIconThemeFile(resource, prefix);
		} catch (IOException e) {
			System.err.println(Globals.lang("Unable to read icon theme file")+" '"+
				resource.toString()+"'");
			
			if (resource != defaultResource)
				try {
					iconMap = readIconThemeFile(defaultResource, defaultPrefix);
				} catch (IOException e2) {
					System.err.println(Globals.lang("Unable to read default icon theme."));
				}

		}


	}

	
	public static URL getIconUrl(String name) {
        if (iconMap.containsKey(name)) {
			String path = iconMap.get(name);
			URL url = GUIGlobals.class.getResource(path);
			if (url == null)
				
				try {
					url = new URL(path);
				} catch (MalformedURLException e) {
					url = null;
				}
				if (url == null)
					System.err.println(Globals.lang("Could not find image file")+" '"+path+"'");
				return url;
		}
		else return null;
	}

	
	public static ImageIcon getImage(String name) {
		URL u = getIconUrl(name);
		return u != null ? new ImageIcon(getIconUrl(name)) : null;
	}

	
	private static HashMap<String, String> readIconThemeFile(URL file, String prefix) throws IOException {
		HashMap<String, String> map = new HashMap<String, String>();
		InputStream in = null;
		try {
			in = file.openStream();
			StringBuffer buffer = new StringBuffer();
			int c;
			while ((c = in.read()) != -1)
				buffer.append((char)c);
			String[] lines = buffer.toString().split("\n");
			for (int i=0; i<lines.length; i++) {
				String line = lines[i].trim();
				int index = line.indexOf("=");
				if (index >= 0) {
					String key = line.substring(0, index).trim();
					String value = prefix+line.substring(index+1).trim();
					map.put(key, value);
				}
			}
		} catch (IOException ex) {
			throw ex;
		} finally {
			try {
				if (in != null) in.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
		return map;
	}

	
	public static String getLocaleHelpPath()
	{
		JabRefPreferences prefs = JabRefPreferences.getInstance() ;
		String middle = prefs.get("language")+"/";
		if (middle.equals("en/")) middle = ""; 

		return (helpPre + middle );
	}


	
	public static void init() {
		typeNameFont = new Font("arial", Font.ITALIC+Font.BOLD, 24);
		fieldNameFont = new Font("arial", Font.ITALIC+Font.BOLD, 14);
		JLabel lab;
		lab = new JLabel(getImage("pdfSmall"));
		lab.setToolTipText(Globals.lang("Open")+" PDF");
		tableIcons.put("pdf", lab);
		lab = new JLabel(getImage("wwwSmall"));
		lab.setToolTipText(Globals.lang("Open")+" URL");
		tableIcons.put("url", lab);
		lab = new JLabel(getImage("citeseer"));
		lab.setToolTipText(Globals.lang("Open")+" CiteSeer URL");
		tableIcons.put("citeseerurl", lab);
		lab = new JLabel(getImage("doiSmall"));
		lab.setToolTipText(Globals.lang("Open")+" DOI "+Globals.lang("web link"));
		tableIcons.put("doi", lab);
		lab = new JLabel(getImage("psSmall"));
		lab.setToolTipText(Globals.lang("Open")+" PS");
		tableIcons.put("ps", lab);
        lab = new JLabel(getImage("psSmall"));
        lab.setToolTipText(Globals.lang("Open file"));
        tableIcons.put(GUIGlobals.FILE_FIELD, lab);


        
	}

}
