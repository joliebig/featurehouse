
package net.sf.jabref;

import java.io.*;
import java.nio.charset.Charset;
import java.util.*;
import java.util.List;
import java.util.logging.*;
import java.util.logging.Filter;

import java.awt.*;
import javax.swing.*;

import net.sf.jabref.collab.*;
import net.sf.jabref.imports.*;
import net.sf.jabref.util.*;
import net.sf.jabref.journals.JournalAbbreviations;

public class Globals {

	public static int SHORTCUT_MASK,
		
		FUTURE_YEAR = 2050, 
		
		

		STANDARD_EXPORT_COUNT = 5, 
		METADATA_LINE_LENGTH = 70; 

	private static String resourcePrefix = "resource/JabRef", menuResourcePrefix = "resource/Menu",
		integrityResourcePrefix = "resource/IntegrityMessage";

	private static final String buildInfos = "/resource/build.properties";

	
	public static final String additionalFields = "/resource/fields/fields.xml";

	public static ResourceBundle messages, menuTitles, intMessages;

	public static FileUpdateMonitor fileUpdateMonitor = new FileUpdateMonitor();

	public static ImportFormatReader importFormatReader = new ImportFormatReader();

	public static ErrorConsole errorConsole;

	public static String VERSION, BUILD, BUILD_DATE;

	static {
		TBuildInfo bi = new TBuildInfo(buildInfos);
		VERSION = bi.getBUILD_VERSION();
		BUILD = bi.getBUILD_NUMBER();
		BUILD_DATE = bi.getBUILD_DATE();

		
		errorConsole = ErrorConsole.getInstance();
	}

	
	
	public static Locale locale;

	public static final String FILETYPE_PREFS_EXT = "_dir", SELECTOR_META_PREFIX = "selector_",
		LAYOUT_PREFIX = "/resource/layout/", MAC = "Mac OS X",
		DOI_LOOKUP_PREFIX = "http://dx.doi.org/", NONE = "_non__",
		FORMATTER_PACKAGE = "net.sf.jabref.export.layout.format.";

	public static float duplicateThreshold = 0.75f;

	private static Handler consoleHandler = new java.util.logging.ConsoleHandler();

	public static String[] ENCODINGS, ALL_ENCODINGS = 
		
		
		new String[] { "ISO8859_1", "UTF8", "UTF-16", "ASCII", "Cp1250", "Cp1251", "Cp1252",
			"Cp1253", "Cp1254", "Cp1257", "JIS", "SJIS",
			"EUC_JP", 
			"Big5", "Big5_HKSCS", "GBK", "ISO8859_2", "ISO8859_3", "ISO8859_4", "ISO8859_5",
			"ISO8859_6", "ISO8859_7", "ISO8859_8", "ISO8859_9", "ISO8859_13", "ISO8859_15" };

	
	public static String[] MONTHS = new String[] { "jan", "feb", "mar", "apr", "may", "jun", "jul",
		"aug", "sep", "oct", "nov", "dec" };

	
	public static Map<String, String> MONTH_STRINGS = new HashMap<String, String>();
	static {
		MONTH_STRINGS.put("jan", "January");
		MONTH_STRINGS.put("feb", "February");
		MONTH_STRINGS.put("mar", "March");
		MONTH_STRINGS.put("apr", "April");
		MONTH_STRINGS.put("may", "May");
		MONTH_STRINGS.put("jun", "June");
		MONTH_STRINGS.put("jul", "July");
		MONTH_STRINGS.put("aug", "August");
		MONTH_STRINGS.put("sep", "September");
		MONTH_STRINGS.put("oct", "October");
		MONTH_STRINGS.put("nov", "November");
		MONTH_STRINGS.put("dec", "December");

		
		
		List<String> encodings = new ArrayList<String>();
		for (int i = 0; i < ALL_ENCODINGS.length; i++) {
			if (Charset.isSupported(ALL_ENCODINGS[i])) {
				encodings.add(ALL_ENCODINGS[i]);
			}
		}
		ENCODINGS = encodings.toArray(new String[0]);

	}

	public static GlobalFocusListener focusListener = new GlobalFocusListener();

	public static JabRefPreferences prefs = null;

	public static HelpDialog helpDiag = null;

	public static String osName = System.getProperty("os.name", "def");

	public static boolean ON_MAC = (osName.equals(MAC)), ON_WIN = osName.startsWith("Windows");

	public static String[] SKIP_WORDS = { "a", "an", "the", "for", "on" };

	public static SidePaneManager sidePaneManager;

	public static final String NEWLINE = System.getProperty("line.separator");
    public static final int NEWLINE_LENGTH = System.getProperty("line.separator").length();

	
	public static final boolean UNIX_NEWLINE = NEWLINE.equals("\n");

	public static final String BIBTEX_STRING = "__string";

	
	

	public static void logger(String s) {
		Logger.global.info(s);
	}

	public static void turnOffLogging() { 
		Logger.global.setLevel(java.util.logging.Level.SEVERE);
	}

	
	public static void turnOnConsoleLogging() {
		Logger.global.addHandler(consoleHandler);

	}

	public static void turnOnFileLogging() {
		Logger.global.setLevel(java.util.logging.Level.ALL);
		java.util.logging.Handler handler;
		handler = new ConsoleHandler();
		
		Logger.global.addHandler(handler);

		handler.setFilter(new Filter() { 
				public boolean isLoggable(LogRecord record) {
					return true;
				}
			});
	}

	public static void setLanguage(String language, String country) {
		locale = new Locale(language, country);
		messages = ResourceBundle.getBundle(resourcePrefix, locale);
		menuTitles = ResourceBundle.getBundle(menuResourcePrefix, locale);
		intMessages = ResourceBundle.getBundle(integrityResourcePrefix, locale);
		Locale.setDefault(locale);
		javax.swing.JComponent.setDefaultLocale(locale);
	}

	public static JournalAbbreviations journalAbbrev;

	public static String lang(String key, String[] params) {
		String translation = null;
		try {
			if (Globals.messages != null) 
				translation = Globals.messages.getString(key.replaceAll(" ", "_"));
		} catch (MissingResourceException ex) {
			
		}
		if (translation == null)
			translation = key;

		if ((translation != null) && (translation.length() != 0)) {
			translation = translation.replaceAll("_", " ");
			StringBuffer sb = new StringBuffer();
			boolean b = false;
			char c;
			for (int i = 0; i < translation.length(); ++i) {
				c = translation.charAt(i);
				if (c == '%') {
					b = true;
				} else {
					if (!b) {
						sb.append(c);
					} else {
						b = false;
						try {
							int index = Integer.parseInt(String.valueOf(c));
							if (params != null && index >= 0 && index <= params.length)
								sb.append(params[index]);
						} catch (NumberFormatException e) {
							
							
							switch (c) {
							case 'c': 
								sb.append(':');
								break;
							case 'e': 
								sb.append('=');
								break;
							default: 
								sb.append(c);
							}
						}
					}
				}
			}
			return sb.toString();
		}
		return key;
	}

	public static String lang(String key) {
		return lang(key, (String[]) null);
	}

	public static String lang(String key, String s1) {
		return lang(key, new String[] { s1 });
	}

	public static String lang(String key, String s1, String s2) {
		return lang(key, new String[] { s1, s2 });
	}

	public static String lang(String key, String s1, String s2, String s3) {
		return lang(key, new String[] { s1, s2, s3 });
	}

	public static String menuTitle(String key) {
		String translation = null;
		try {
			if (Globals.messages != null) {
				translation = Globals.menuTitles.getString(key.replaceAll(" ", "_"));
			}
		} catch (MissingResourceException ex) {
			translation = key;

			
			

		}
		if ((translation != null) && (translation.length() != 0)) {
			return translation.replaceAll("_", " ");
		} else {
			return key;
		}
	}

	public static String getIntegrityMessage(String key) {
		String translation = null;
		try {
			if (Globals.intMessages != null) {
				translation = Globals.intMessages.getString(key);
			}
		} catch (MissingResourceException ex) {
			translation = key;

			
			
			
		}
		if ((translation != null) && (translation.length() != 0)) {
			return translation;
		} else {
			return key;
		}
	}

	
	
	
	public static BibtexEntryType getEntryType(String type) {
		
		Object o = BibtexEntryType.ALL_TYPES.get(type);
		if (o != null) {
			return (BibtexEntryType) o;
		} else {
			return BibtexEntryType.OTHER;
		}
		
	}

	
	public static String getOpeningBrace() {
		return "{";
		
	}

	
	public static String getClosingBrace() {
		return "}";
		

	}

	
	public static String[] getMultipleFiles(JFrame owner, File directory, String extension,
		boolean updateWorkingdirectory) {

		OpenFileFilter off = null;
		if (extension == null)
			off = new OpenFileFilter();
		else if (!extension.equals(NONE))
			off = new OpenFileFilter(extension);

		Object files = getNewFileImpl(owner, directory, extension, null, off,
			JFileChooser.OPEN_DIALOG, updateWorkingdirectory, false, true, null);

		if (files instanceof String[]) {
			return (String[]) files;
		}
		
		
		if (files != null) {
			return new String[] { (String) files };
		}
		return new String[0];
	}

	public static String getNewFile(JFrame owner, File directory, String extension, int dialogType,
		boolean updateWorkingDirectory) {
		return getNewFile(owner, directory, extension, null, dialogType, updateWorkingDirectory,
			false, null);
	}

        public static String getNewFile(JFrame owner, File directory, String extension, int dialogType,
		boolean updateWorkingDirectory, JComponent accessory) {
		return getNewFile(owner, directory, extension, null, dialogType, updateWorkingDirectory,
			false, accessory);
	}

        
	public static String getNewFile(JFrame owner, File directory, String extension,
		String description, int dialogType, boolean updateWorkingDirectory) {
		return getNewFile(owner, directory, extension, description, dialogType,
			updateWorkingDirectory, false, null);
	}

	public static String getNewDir(JFrame owner, File directory, String extension, int dialogType,
		boolean updateWorkingDirectory) {
		return getNewFile(owner, directory, extension, null, dialogType, updateWorkingDirectory,
			true, null);
	}

	public static String getNewDir(JFrame owner, File directory, String extension,
		String description, int dialogType, boolean updateWorkingDirectory) {
		return getNewFile(owner, directory, extension, description, dialogType,
			updateWorkingDirectory, true, null);
	}

	private static String getNewFile(JFrame owner, File directory, String extension,
		String description, int dialogType, boolean updateWorkingDirectory, boolean dirOnly,
                JComponent accessory) {

		OpenFileFilter off = null;

		if (extension == null)
			off = new OpenFileFilter();
		else if (!extension.equals(NONE))
			off = new OpenFileFilter(extension);

		return (String) getNewFileImpl(owner, directory, extension, description, off, dialogType,
			updateWorkingDirectory, dirOnly, false, accessory);
	}

	private static Object getNewFileImpl(JFrame owner, File directory, String extension,
		String description, OpenFileFilter off, int dialogType, boolean updateWorkingDirectory,
		boolean dirOnly, boolean multipleSelection, JComponent accessory) {

		if (ON_MAC && prefs.getBoolean("useNativeFileDialogOnMac")) {

			return getNewFileForMac(owner, directory, extension, dialogType,
				updateWorkingDirectory, dirOnly, off);
		}

		JFileChooser fc = null;
		try {
			fc = new JabRefFileChooser(directory);
                        if (accessory != null)
                            fc.setAccessory(accessory);
		} catch (InternalError errl) {
			
			
			
			
			
			return getNewFileForMac(owner, directory, extension, dialogType,
				updateWorkingDirectory, dirOnly, off);
		}

		if (dirOnly) {
			fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		}

		fc.setMultiSelectionEnabled(multipleSelection);

		fc.addChoosableFileFilter(off);
		fc.setDialogType(dialogType);
		int dialogResult;
		if (dialogType == JFileChooser.OPEN_DIALOG) {
			dialogResult = fc.showOpenDialog(owner);
		} else if (dialogType == JFileChooser.SAVE_DIALOG) {
			dialogResult = fc.showSaveDialog(owner);
		} else {
			dialogResult = fc.showDialog(owner, description);
		}

		
		
		if (dialogResult != JFileChooser.APPROVE_OPTION)
			return null;

		
		File selectedFile = fc.getSelectedFile();
		if (selectedFile == null) { 
			return null;
		}

		
		
		
		if ((extension != null) && (dialogType == JFileChooser.SAVE_DIALOG)
			&& (fc.getFileFilter() == off) && !off.accept(selectedFile)) {

			
			selectedFile = new File(selectedFile.getPath() + extension.split("[, ]+", 0)[0]);
		}

		if (updateWorkingDirectory) {
			prefs.put("workingDirectory", selectedFile.getPath());
		}

		if (!multipleSelection)
			return selectedFile.getAbsolutePath();
		else {
			File[] files = fc.getSelectedFiles();
			String[] filenames = new String[files.length];
			for (int i = 0; i < files.length; i++)
				filenames[i] = files[i].getAbsolutePath();
			return filenames;
		}
	}

	private static String getNewFileForMac(JFrame owner, File directory, String extensions,
		int dialogType, boolean updateWorkingDirectory, boolean dirOnly, FilenameFilter filter) {

		FileDialog fc = new FileDialog(owner);

		
		if (directory != null) {
			fc.setDirectory(directory.getParent());
		}
		if (dialogType == JFileChooser.OPEN_DIALOG) {
			fc.setMode(FileDialog.LOAD);
		} else {
			fc.setMode(FileDialog.SAVE);
		}

		fc.setVisible(true); 

		if (fc.getFile() != null) {
			Globals.prefs.put("workingDirectory", fc.getDirectory() + fc.getFile());
			return fc.getDirectory() + fc.getFile();
		} else {
			return null;
		}
	}

	public static String SPECIAL_COMMAND_CHARS = "\"`^~'c";

	public static HashMap<String, String> HTML_CHARS = new HashMap<String, String>(), HTMLCHARS = new HashMap<String, String>(),
		XML_CHARS = new HashMap<String, String>(), ASCII2XML_CHARS = new HashMap<String, String>(), UNICODE_CHARS = new HashMap<String, String>(),
		RTFCHARS = new HashMap<String, String>(), URL_CHARS = new HashMap<String,String>();

	static {

		
		
		
		
		
		

		
		
		fileUpdateMonitor.start();

		try {
			SHORTCUT_MASK = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
		} catch (Throwable t) {

		}

		
		
		URL_CHARS.put("<", "%3c");
		URL_CHARS.put(">", "%3e");
		URL_CHARS.put("(", "%28");
		URL_CHARS.put(")", "%29");
		URL_CHARS.put(" ", "%20");
		URL_CHARS.put("&", "%26");
		URL_CHARS.put("$", "%24");

		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		

		
		
		HTMLCHARS.put("`A", "&Agrave;"); 
		HTMLCHARS.put("'A", "&Aacute;"); 
		HTMLCHARS.put("^A", "&Acirc;"); 
		HTMLCHARS.put("~A", "&Atilde;"); 
		HTMLCHARS.put("\"A", "&Auml;"); 
		HTMLCHARS.put("AA", "&Aring;"); 
		HTMLCHARS.put("AE", "&AElig;"); 
		HTMLCHARS.put("cC", "&Ccedil;"); 
		HTMLCHARS.put("`E", "&Egrave;"); 
		HTMLCHARS.put("'E", "&Eacute;"); 
		HTMLCHARS.put("^E", "&Ecirc;"); 
		HTMLCHARS.put("\"E", "&Euml;"); 
		HTMLCHARS.put("`I", "&Igrave;"); 
		HTMLCHARS.put("'I", "&Iacute;"); 
		HTMLCHARS.put("^I", "&Icirc;"); 
		HTMLCHARS.put("\"I", "&Iuml;"); 
		HTMLCHARS.put("DH", "&ETH;"); 
		HTMLCHARS.put("~N", "&Ntilde;"); 
		HTMLCHARS.put("`O", "&Ograve;"); 
		HTMLCHARS.put("'O", "&Oacute;"); 
		HTMLCHARS.put("^O", "&Ocirc;"); 
		HTMLCHARS.put("~O", "&Otilde;"); 
		HTMLCHARS.put("\"O", "&Ouml;"); 
		
		
		
		HTMLCHARS.put("O", "&Oslash;"); 
		HTMLCHARS.put("`U", "&Ugrave;"); 
		HTMLCHARS.put("'U", "&Uacute;"); 
		HTMLCHARS.put("^U", "&Ucirc;"); 
		HTMLCHARS.put("\"U", "&Uuml;"); 
		HTMLCHARS.put("'Y", "&Yacute;"); 
		HTMLCHARS.put("TH", "&THORN;"); 
		HTMLCHARS.put("ss", "&szlig;"); 
		HTMLCHARS.put("`a", "&agrave;"); 
		HTMLCHARS.put("'a", "&aacute;"); 
		HTMLCHARS.put("^a", "&acirc;"); 
		HTMLCHARS.put("~a", "&atilde;"); 
		HTMLCHARS.put("\"a", "&auml;"); 
		HTMLCHARS.put("aa", "&aring;"); 
		HTMLCHARS.put("ae", "&aelig;"); 
		HTMLCHARS.put("cc", "&ccedil;"); 
		HTMLCHARS.put("`e", "&egrave;"); 
		HTMLCHARS.put("'e", "&eacute;"); 
		HTMLCHARS.put("^e", "&ecirc;"); 
		HTMLCHARS.put("\"e", "&euml;"); 
		HTMLCHARS.put("`i", "&igrave;"); 
		HTMLCHARS.put("'i", "&iacute;"); 
		HTMLCHARS.put("^i", "&icirc;"); 
		HTMLCHARS.put("\"i", "&iuml;"); 
		HTMLCHARS.put("dh", "&eth;"); 
		HTMLCHARS.put("~n", "&ntilde;"); 
		HTMLCHARS.put("`o", "&ograve;"); 
		HTMLCHARS.put("'o", "&oacute;"); 
		HTMLCHARS.put("^o", "&ocirc;"); 
		HTMLCHARS.put("~o", "&otilde;"); 
		HTMLCHARS.put("\"o", "&ouml;"); 
		
		
		
		HTMLCHARS.put("o", "&oslash;"); 
		HTMLCHARS.put("`u", "&ugrave;"); 
		HTMLCHARS.put("'u", "&uacute;"); 
		HTMLCHARS.put("^u", "&ucirc;"); 
		HTMLCHARS.put("\"u", "&uuml;"); 
		HTMLCHARS.put("'y", "&yacute;"); 
		HTMLCHARS.put("th", "&thorn;"); 
		HTMLCHARS.put("\"y", "&yuml;"); 

		
		
		HTMLCHARS.put("=A", "&#256;"); 
		HTMLCHARS.put("=a", "&#257;"); 
		HTMLCHARS.put("uA", "&#258;"); 
		HTMLCHARS.put("ua", "&#259;"); 
		HTMLCHARS.put("kA", "&#260;"); 
		HTMLCHARS.put("ka", "&#261;"); 
		HTMLCHARS.put("'C", "&#262;"); 
		HTMLCHARS.put("'c", "&#263;"); 
		HTMLCHARS.put("^C", "&#264;"); 
		HTMLCHARS.put("^c", "&#265;"); 
		HTMLCHARS.put(".C", "&#266;"); 
		HTMLCHARS.put(".c", "&#267;"); 
		HTMLCHARS.put("vC", "&#268;"); 
		HTMLCHARS.put("vc", "&#269;"); 
		HTMLCHARS.put("vD", "&#270;"); 
		
		HTMLCHARS.put("DJ", "&#272;"); 
		HTMLCHARS.put("dj", "&#273;"); 
		HTMLCHARS.put("=E", "&#274;"); 
		HTMLCHARS.put("=e", "&#275;"); 
		HTMLCHARS.put("uE", "&#276;"); 
		HTMLCHARS.put("ue", "&#277;"); 
		HTMLCHARS.put(".E", "&#278;"); 
		HTMLCHARS.put(".e", "&#279;"); 
		HTMLCHARS.put("kE", "&#280;"); 
		HTMLCHARS.put("ke", "&#281;"); 
		HTMLCHARS.put("vE", "&#282;"); 
		HTMLCHARS.put("ve", "&#283;"); 
		HTMLCHARS.put("^G", "&#284;"); 
		HTMLCHARS.put("^g", "&#285;"); 
		HTMLCHARS.put("uG", "&#286;"); 
		HTMLCHARS.put("ug", "&#287;"); 
		HTMLCHARS.put(".G", "&#288;"); 
		HTMLCHARS.put(".g", "&#289;"); 
		HTMLCHARS.put("cG", "&#290;"); 
		HTMLCHARS.put("'g", "&#291;"); 
		HTMLCHARS.put("^H", "&#292;"); 
		HTMLCHARS.put("^h", "&#293;"); 
		HTMLCHARS.put("Hstrok", "&#294;"); 
		HTMLCHARS.put("hstrok", "&#295;"); 
		HTMLCHARS.put("~I", "&#296;"); 
		HTMLCHARS.put("~i", "&#297;"); 
		HTMLCHARS.put("=I", "&#298;"); 
		HTMLCHARS.put("=i", "&#299;"); 
		HTMLCHARS.put("uI", "&#300;"); 
		HTMLCHARS.put("ui", "&#301;"); 
		HTMLCHARS.put("kI", "&#302;"); 
		HTMLCHARS.put("ki", "&#303;"); 
		HTMLCHARS.put(".I", "&#304;"); 
		HTMLCHARS.put("i", "&#305;"); 
		
		
		HTMLCHARS.put("^J", "&#308;"); 
		HTMLCHARS.put("^j", "&#309;"); 
		HTMLCHARS.put("cK", "&#310;"); 
		HTMLCHARS.put("ck", "&#311;"); 
		
		HTMLCHARS.put("'L", "&#313;"); 
		HTMLCHARS.put("'l", "&#314;"); 
		HTMLCHARS.put("cL", "&#315;"); 
		HTMLCHARS.put("cl", "&#316;"); 
		
		
		HTMLCHARS.put("Lmidot", "&#319;"); 
		HTMLCHARS.put("lmidot", "&#320;"); 
		HTMLCHARS.put("L", "&#321;"); 
		HTMLCHARS.put("l", "&#322;"); 
		HTMLCHARS.put("'N", "&#323;"); 
		HTMLCHARS.put("'n", "&#324;"); 
		HTMLCHARS.put("cN", "&#325;"); 
		HTMLCHARS.put("cn", "&#326;"); 
		HTMLCHARS.put("vN", "&#327;"); 
		HTMLCHARS.put("vn", "&#328;"); 
		
		HTMLCHARS.put("NG", "&#330;"); 
		HTMLCHARS.put("ng", "&#331;"); 
		HTMLCHARS.put("=O", "&#332;"); 
		HTMLCHARS.put("=o", "&#333;"); 
		HTMLCHARS.put("uO", "&#334;"); 
		HTMLCHARS.put("uo", "&#335;"); 
		HTMLCHARS.put("HO", "&#336;"); 
		HTMLCHARS.put("Ho", "&#337;"); 
		HTMLCHARS.put("OE", "&#338;"); 
		HTMLCHARS.put("oe", "&#339;"); 
		HTMLCHARS.put("'R", "&#340;"); 
		HTMLCHARS.put("'r", "&#341;"); 
		HTMLCHARS.put("cR", "&#342;"); 
		HTMLCHARS.put("cr", "&#343;"); 
		HTMLCHARS.put("vR", "&#344;"); 
		HTMLCHARS.put("vr", "&#345;"); 
		HTMLCHARS.put("'S", "&#346;"); 
		HTMLCHARS.put("'s", "&#347;"); 
		HTMLCHARS.put("^S", "&#348;"); 
		HTMLCHARS.put("^s", "&#349;"); 
		HTMLCHARS.put("cS", "&#350;"); 
		HTMLCHARS.put("cs", "&#351;"); 
		HTMLCHARS.put("vS", "&#352;"); 
		HTMLCHARS.put("vs", "&#353;"); 
		HTMLCHARS.put("cT", "&#354;"); 
		HTMLCHARS.put("ct", "&#355;"); 
		HTMLCHARS.put("vT", "&#356;"); 
		
		HTMLCHARS.put("Tstrok", "&#358;"); 
		HTMLCHARS.put("tstrok", "&#359;"); 
		HTMLCHARS.put("~U", "&#360;"); 
		HTMLCHARS.put("~u", "&#361;"); 
		HTMLCHARS.put("=U", "&#362;"); 
		HTMLCHARS.put("=u", "&#363;"); 
		HTMLCHARS.put("uU", "&#364;"); 
		HTMLCHARS.put("uu", "&#365;"); 
		HTMLCHARS.put("rU", "&#366;"); 
		HTMLCHARS.put("ru", "&#367;"); 
		HTMLCHARS.put("HU", "&#368;"); 
		HTMLCHARS.put("Hu", "&#369;"); 
		HTMLCHARS.put("kU", "&#370;"); 
		HTMLCHARS.put("ku", "&#371;"); 
		HTMLCHARS.put("^W", "&#372;"); 
		HTMLCHARS.put("^w", "&#373;"); 
		HTMLCHARS.put("^Y", "&#374;"); 
		HTMLCHARS.put("^y", "&#375;"); 
		HTMLCHARS.put("\"Y", "&#376;"); 
		HTMLCHARS.put("'Z", "&#377;"); 
		HTMLCHARS.put("'z", "&#378;"); 
		HTMLCHARS.put(".Z", "&#379;"); 
		HTMLCHARS.put(".z", "&#380;"); 
		HTMLCHARS.put("vZ", "&#381;"); 
		HTMLCHARS.put("vz", "&#382;"); 
		

		XML_CHARS.put("\\{\\\\\\\"\\{a\\}\\}", "&#x00E4;");
		XML_CHARS.put("\\{\\\\\\\"\\{A\\}\\}", "&#x00C4;");
		XML_CHARS.put("\\{\\\\\\\"\\{e\\}\\}", "&#x00EB;");
		XML_CHARS.put("\\{\\\\\\\"\\{E\\}\\}", "&#x00CB;");
		XML_CHARS.put("\\{\\\\\\\"\\{i\\}\\}", "&#x00EF;");
		XML_CHARS.put("\\{\\\\\\\"\\{I\\}\\}", "&#x00CF;");
		XML_CHARS.put("\\{\\\\\\\"\\{o\\}\\}", "&#x00F6;");
		XML_CHARS.put("\\{\\\\\\\"\\{O\\}\\}", "&#x00D6;");
		XML_CHARS.put("\\{\\\\\\\"\\{u\\}\\}", "&#x00FC;");
		XML_CHARS.put("\\{\\\\\\\"\\{U\\}\\}", "&#x00DC;");

		XML_CHARS.put("\\{\\\\\\`\\{e\\}\\}", "&#x00E8;");
		XML_CHARS.put("\\{\\\\\\`\\{E\\}\\}", "&#x00C8;");
		XML_CHARS.put("\\{\\\\\\`\\{i\\}\\}", "&#x00EC;");
		XML_CHARS.put("\\{\\\\\\`\\{I\\}\\}", "&#x00CC;");
		XML_CHARS.put("\\{\\\\\\`\\{o\\}\\}", "&#x00F2;");
		XML_CHARS.put("\\{\\\\\\`\\{O\\}\\}", "&#x00D2;");
		XML_CHARS.put("\\{\\\\\\`\\{u\\}\\}", "&#x00F9;");
		XML_CHARS.put("\\{\\\\\\`\\{U\\}\\}", "&#x00D9;");
		XML_CHARS.put("\\{\\\\\\'\\{e\\}\\}", "&#x00E9;");
		XML_CHARS.put("\\{\\\\\\\u\\{E\\}\\}", "&#x00C9;");
		XML_CHARS.put("\\{\\\\\\\u\\{i\\}\\}", "&#x00ED;");
		XML_CHARS.put("\\{\\\\\\\u\\{I\\}\\}", "&#x00CD;");
		XML_CHARS.put("\\{\\\\\\\u\\{o\\}\\}", "&#x00F3;");
		XML_CHARS.put("\\{\\\\\\\u\\{O\\}\\}", "&#x00D3;");
		XML_CHARS.put("\\{\\\\\\\u\\{u\\}\\}", "&#x00FA;");
		XML_CHARS.put("\\{\\\\\\\u\\{U\\}\\}", "&#x00DA;");
		XML_CHARS.put("\\{\\\\\\\u\\{a\\}\\}", "&#x00E1;");
		XML_CHARS.put("\\{\\\\\\\u\\{A\\}\\}", "&#x00C1;");

		XML_CHARS.put("\\{\\\\\\^\\{o\\}\\}", "&#x00F4;");
		XML_CHARS.put("\\{\\\\\\^\\{O\\}\\}", "&#x00D4;");
		XML_CHARS.put("\\{\\\\\\^\\{u\\}\\}", "&#x00F9;");
		XML_CHARS.put("\\{\\\\\\^\\{U\\}\\}", "&#x00D9;");
		XML_CHARS.put("\\{\\\\\\^\\{e\\}\\}", "&#x00EA;");
		XML_CHARS.put("\\{\\\\\\^\\{E\\}\\}", "&#x00CA;");
		XML_CHARS.put("\\{\\\\\\^\\{i\\}\\}", "&#x00EE;");
		XML_CHARS.put("\\{\\\\\\^\\{I\\}\\}", "&#x00CE;");
		XML_CHARS.put("\\{\\\\\\~\\{o\\}\\}", "&#x00F5;");
		XML_CHARS.put("\\{\\\\\\~\\{O\\}\\}", "&#x00D5;");
		XML_CHARS.put("\\{\\\\\\~\\{n\\}\\}", "&#x00F1;");
		XML_CHARS.put("\\{\\\\\\~\\{N\\}\\}", "&#x00D1;");
		XML_CHARS.put("\\{\\\\\\~\\{a\\}\\}", "&#x00E3;");
		XML_CHARS.put("\\{\\\\\\~\\{A\\}\\}", "&#x00C3;");

		XML_CHARS.put("\\{\\\\\\\"a\\}", "&#x00E4;");
		XML_CHARS.put("\\{\\\\\\\"A\\}", "&#x00C4;");
		XML_CHARS.put("\\{\\\\\\\"e\\}", "&#x00EB;");
		XML_CHARS.put("\\{\\\\\\\"E\\}", "&#x00CB;");
		XML_CHARS.put("\\{\\\\\\\"i\\}", "&#x00EF;");
		XML_CHARS.put("\\{\\\\\\\"I\\}", "&#x00CF;");
		XML_CHARS.put("\\{\\\\\\\"o\\}", "&#x00F6;");
		XML_CHARS.put("\\{\\\\\\\"O\\}", "&#x00D6;");
		XML_CHARS.put("\\{\\\\\\\"u\\}", "&#x00FC;");
		XML_CHARS.put("\\{\\\\\\\"U\\}", "&#x00DC;");

		XML_CHARS.put("\\{\\\\\\`e\\}", "&#x00E8;");
		XML_CHARS.put("\\{\\\\\\`E\\}", "&#x00C8;");
		XML_CHARS.put("\\{\\\\\\`i\\}", "&#x00EC;");
		XML_CHARS.put("\\{\\\\\\`I\\}", "&#x00CC;");
		XML_CHARS.put("\\{\\\\\\`o\\}", "&#x00F2;");
		XML_CHARS.put("\\{\\\\\\`O\\}", "&#x00D2;");
		XML_CHARS.put("\\{\\\\\\`u\\}", "&#x00F9;");
		XML_CHARS.put("\\{\\\\\\`U\\}", "&#x00D9;");
		XML_CHARS.put("\\{\\\\\\'e\\}", "&#x00E9;");
		XML_CHARS.put("\\{\\\\\\'E\\}", "&#x00C9;");
		XML_CHARS.put("\\{\\\\\\'i\\}", "&#x00ED;");
		XML_CHARS.put("\\{\\\\\\'I\\}", "&#x00CD;");
		XML_CHARS.put("\\{\\\\\\'o\\}", "&#x00F3;");
		XML_CHARS.put("\\{\\\\\\'O\\}", "&#x00D3;");
		XML_CHARS.put("\\{\\\\\\'u\\}", "&#x00FA;");
		XML_CHARS.put("\\{\\\\\\'U\\}", "&#x00DA;");
		XML_CHARS.put("\\{\\\\\\'a\\}", "&#x00E1;");
		XML_CHARS.put("\\{\\\\\\'A\\}", "&#x00C1;");

		XML_CHARS.put("\\{\\\\\\^a\\}", "&#x00F4;");
		XML_CHARS.put("\\{\\\\\\^A\\}", "&#x00D4;");
		XML_CHARS.put("\\{\\\\\\^o\\}", "&#x00F4;");
		XML_CHARS.put("\\{\\\\\\^O\\}", "&#x00D4;");
		XML_CHARS.put("\\{\\\\\\^u\\}", "&#x00F9;");
		XML_CHARS.put("\\{\\\\\\^U\\}", "&#x00D9;");
		XML_CHARS.put("\\{\\\\\\^e\\}", "&#x00EA;");
		XML_CHARS.put("\\{\\\\\\^E\\}", "&#x00CA;");
		XML_CHARS.put("\\{\\\\\\^i\\}", "&#x00EE;");
		XML_CHARS.put("\\{\\\\\\^I\\}", "&#x00CE;");
		XML_CHARS.put("\\{\\\\\\~o\\}", "&#x00F5;");
		XML_CHARS.put("\\{\\\\\\~O\\}", "&#x00D5;");
		XML_CHARS.put("\\{\\\\\\~n\\}", "&#x00F1;");
		XML_CHARS.put("\\{\\\\\\~N\\}", "&#x00D1;");
		XML_CHARS.put("\\{\\\\\\~a\\}", "&#x00E3;");
		XML_CHARS.put("\\{\\\\\\~A\\}", "&#x00C3;");

		ASCII2XML_CHARS.put("<", "&lt;");
		ASCII2XML_CHARS.put("\"", "&quot;");
		ASCII2XML_CHARS.put(">", "&gt;");

		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "Ae");
		UNICODE_CHARS.put("\u", "Aa");
		UNICODE_CHARS.put("\u", "Ae");
		UNICODE_CHARS.put("\u", "C");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "D");
		UNICODE_CHARS.put("\u", "N");
		UNICODE_CHARS.put("\u", "O");
		UNICODE_CHARS.put("\u", "O");
		UNICODE_CHARS.put("\u", "O");
		UNICODE_CHARS.put("\u", "O");
		UNICODE_CHARS.put("\u", "Oe");
		UNICODE_CHARS.put("\u", "Oe");
		UNICODE_CHARS.put("\u", "U");
		UNICODE_CHARS.put("\u", "U");
		UNICODE_CHARS.put("\u", "U");
		UNICODE_CHARS.put("\u", "Ue"); 
		UNICODE_CHARS.put("\u", "Y");
		UNICODE_CHARS.put("\u", "ss");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "ae");
		UNICODE_CHARS.put("\u", "aa");
		UNICODE_CHARS.put("\u", "ae");
		UNICODE_CHARS.put("\u", "c");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "i");
		UNICODE_CHARS.put("\u", "i");
		UNICODE_CHARS.put("\u", "i");
		UNICODE_CHARS.put("\u", "i");
		UNICODE_CHARS.put("\u", "o");
		UNICODE_CHARS.put("\u", "n");
		UNICODE_CHARS.put("\u", "o");
		UNICODE_CHARS.put("\u", "o");
		UNICODE_CHARS.put("\u", "o");
		UNICODE_CHARS.put("\u", "o");
		UNICODE_CHARS.put("\u", "oe");
		UNICODE_CHARS.put("\u", "oe");
		UNICODE_CHARS.put("\u", "u");
		UNICODE_CHARS.put("\u", "u");
		UNICODE_CHARS.put("\u", "u");
		UNICODE_CHARS.put("\u", "ue"); 
		UNICODE_CHARS.put("\u", "y");
		UNICODE_CHARS.put("\u", "y");
		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "A");
		UNICODE_CHARS.put("\u", "a");
		UNICODE_CHARS.put("\u", "C");
		UNICODE_CHARS.put("\u", "c");
		UNICODE_CHARS.put("\u", "C");
		UNICODE_CHARS.put("\u", "c");
		UNICODE_CHARS.put("\u", "C");
		UNICODE_CHARS.put("\u", "c");
		UNICODE_CHARS.put("\u", "C");
		UNICODE_CHARS.put("\u", "c");
		UNICODE_CHARS.put("\u", "D");
		UNICODE_CHARS.put("\u", "d");
		UNICODE_CHARS.put("\u", "D");
		UNICODE_CHARS.put("\u", "d");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "E");
		UNICODE_CHARS.put("\u", "e");
		UNICODE_CHARS.put("\u", "G");
		UNICODE_CHARS.put("\u", "g");
		UNICODE_CHARS.put("\u", "G");
		UNICODE_CHARS.put("\u", "g");
		UNICODE_CHARS.put("\u", "G");
		UNICODE_CHARS.put("\u", "g");
		UNICODE_CHARS.put("\u", "G");
		UNICODE_CHARS.put("\u", "g");
		UNICODE_CHARS.put("\u", "H");
		UNICODE_CHARS.put("\u", "h");
		UNICODE_CHARS.put("\u", "h");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "i");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "i");
		UNICODE_CHARS.put("\u", "I");
		UNICODE_CHARS.put("\u", "i");
		

		RTFCHARS.put("`a", "\\'e0");
		RTFCHARS.put("`e", "\\'e8");
		RTFCHARS.put("`i", "\\'ec");
		RTFCHARS.put("`o", "\\'f2");
		RTFCHARS.put("`u", "\\'f9");
		RTFCHARS.put("?a", "\\'e1");
		RTFCHARS.put("?e", "\\'e9");
		RTFCHARS.put("?i", "\\'ed");
		RTFCHARS.put("?o", "\\'f3");
		RTFCHARS.put("?u", "\\'fa");
		RTFCHARS.put("^a", "\\'e2");
		RTFCHARS.put("^e", "\\'ea");
		RTFCHARS.put("^i", "\\'ee");
		RTFCHARS.put("^o", "\\'f4");
		RTFCHARS.put("^u", "\\'fa");
		RTFCHARS.put("\"a", "\\'e4");
		RTFCHARS.put("\"e", "\\'eb");
		RTFCHARS.put("\"i", "\\'ef");
		RTFCHARS.put("\"o", "\\'f6");
		RTFCHARS.put("\"u", "\\u");
		RTFCHARS.put("~n", "\\'f1");
		RTFCHARS.put("`A", "\\'c0");
		RTFCHARS.put("`E", "\\'c8");
		RTFCHARS.put("`I", "\\'cc");
		RTFCHARS.put("`O", "\\'d2");
		RTFCHARS.put("`U", "\\'d9");
		RTFCHARS.put("?A", "\\'c1");
		RTFCHARS.put("?E", "\\'c9");
		RTFCHARS.put("?I", "\\'cd");
		RTFCHARS.put("?O", "\\'d3");
		RTFCHARS.put("?U", "\\'da");
		RTFCHARS.put("^A", "\\'c2");
		RTFCHARS.put("^E", "\\'ca");
		RTFCHARS.put("^I", "\\'ce");
		RTFCHARS.put("^O", "\\'d4");
		RTFCHARS.put("^U", "\\'db");
		RTFCHARS.put("\"A", "\\'c4");
		RTFCHARS.put("\"E", "\\'cb");
		RTFCHARS.put("\"I", "\\'cf");
		RTFCHARS.put("\"O", "\\'d6");
		RTFCHARS.put("\"U", "\\'dc");
		
		
		

		
		
		RTFCHARS.put("'A", "\\u"); 
		
		
		RTFCHARS.put("~A", "\\u"); 
		
		
		RTFCHARS.put("AA", "\\u"); 
		RTFCHARS.put("AE", "{\\u\\u}"); 
		RTFCHARS.put("cC", "\\u"); 
		
		
		RTFCHARS.put("'E", "\\u"); 
		
		
		
		
		
		
		RTFCHARS.put("'I", "\\u"); 
		
		
		
		
		RTFCHARS.put("DH", "\\u"); 
		RTFCHARS.put("~N", "\\u"); 
		
		
		RTFCHARS.put("'O", "\\u"); 
		
		
		RTFCHARS.put("~O", "\\u"); 
		
		
		
		
		
		RTFCHARS.put("O", "\\u"); 
		
		
		RTFCHARS.put("'U", "\\u"); 
		
		
		
		
		RTFCHARS.put("'Y", "\\u"); 
		RTFCHARS.put("TH", "{\\u\\u}"); 
		RTFCHARS.put("ss", "{\\u\\u"); 
		
		
		RTFCHARS.put("'a", "\\u"); 
		
		
		RTFCHARS.put("~a", "\\u"); 
		
		
		RTFCHARS.put("aa", "\\u"); 
		RTFCHARS.put("ae", "{\\u\\u}"); 
		RTFCHARS.put("cc", "\\u"); 
		
		
		RTFCHARS.put("'e", "\\u"); 
		
		
		
		
		
		
		RTFCHARS.put("'i", "\\u"); 
		
		
		
		
		RTFCHARS.put("dh", "\\u"); 
		
		
		
		
		RTFCHARS.put("'o", "\\u"); 
		
		
		RTFCHARS.put("~o", "\\u"); 
		
		
		
		
		
		RTFCHARS.put("o", "\\u"); 
		
		
		RTFCHARS.put("'u", "\\u"); 
		
		
		
		
		RTFCHARS.put("'y", "\\u"); 
		RTFCHARS.put("th", "{\\u\\u}"); 
		RTFCHARS.put("\"y", "\\u"); 

		RTFCHARS.put("=A", "\\u"); 
		RTFCHARS.put("=a", "\\u"); 
		RTFCHARS.put("uA", "\\u"); 
		RTFCHARS.put("ua", "\\u"); 
		RTFCHARS.put("kA", "\\u"); 
		RTFCHARS.put("ka", "\\u"); 
		RTFCHARS.put("'C", "\\u"); 
		RTFCHARS.put("'c", "\\u"); 
		RTFCHARS.put("^C", "\\u"); 
		RTFCHARS.put("^c", "\\u"); 
		RTFCHARS.put(".C", "\\u"); 
		RTFCHARS.put(".c", "\\u"); 
		RTFCHARS.put("vC", "\\u"); 
		RTFCHARS.put("vc", "\\u"); 
		RTFCHARS.put("vD", "\\u"); 
		
		RTFCHARS.put("DJ", "\\u"); 
		RTFCHARS.put("dj", "\\u"); 
		RTFCHARS.put("=E", "\\u"); 
		RTFCHARS.put("=e", "\\u"); 
		RTFCHARS.put("uE", "\\u"); 
		RTFCHARS.put("ue", "\\u"); 
		RTFCHARS.put(".E", "\\u"); 
		RTFCHARS.put(".e", "\\u"); 
		RTFCHARS.put("kE", "\\u"); 
		RTFCHARS.put("ke", "\\u"); 
		RTFCHARS.put("vE", "\\u"); 
		RTFCHARS.put("ve", "\\u"); 
		RTFCHARS.put("^G", "\\u"); 
		RTFCHARS.put("^g", "\\u"); 
		RTFCHARS.put("uG", "\\u"); 
		RTFCHARS.put("ug", "\\u"); 
		RTFCHARS.put(".G", "\\u"); 
		RTFCHARS.put(".g", "\\u"); 
		RTFCHARS.put("cG", "\\u"); 
		RTFCHARS.put("'g", "\\u"); 
		RTFCHARS.put("^H", "\\u"); 
		RTFCHARS.put("^h", "\\u"); 
		RTFCHARS.put("Hstrok", "\\u"); 
		RTFCHARS.put("hstrok", "\\u"); 
		RTFCHARS.put("~I", "\\u"); 
		RTFCHARS.put("~i", "\\u"); 
		RTFCHARS.put("=I", "\\u"); 
		RTFCHARS.put("=i", "\\u"); 
		RTFCHARS.put("uI", "\\u"); 
		RTFCHARS.put("ui", "\\u"); 
		RTFCHARS.put("kI", "\\u"); 
		RTFCHARS.put("ki", "\\u"); 
		RTFCHARS.put(".I", "\\u"); 
		RTFCHARS.put("i", "\\u"); 
		
		
		RTFCHARS.put("^J", "\\u"); 
		RTFCHARS.put("^j", "\\u"); 
		RTFCHARS.put("cK", "\\u"); 
		RTFCHARS.put("ck", "\\u"); 
		
		RTFCHARS.put("'L", "\\u"); 
		RTFCHARS.put("'l", "\\u"); 
		RTFCHARS.put("cL", "\\u"); 
		RTFCHARS.put("cl", "\\u"); 
		
		
		RTFCHARS.put("Lmidot", "\\u"); 
		RTFCHARS.put("lmidot", "\\u"); 
		RTFCHARS.put("L", "\\u"); 
		RTFCHARS.put("l", "\\u"); 
		RTFCHARS.put("'N", "\\u"); 
		RTFCHARS.put("'n", "\\u"); 
		RTFCHARS.put("cN", "\\u"); 
		RTFCHARS.put("cn", "\\u"); 
		RTFCHARS.put("vN", "\\u"); 
		RTFCHARS.put("vn", "\\u"); 
		
		RTFCHARS.put("NG", "\\u"); 
		RTFCHARS.put("ng", "\\u"); 
		RTFCHARS.put("=O", "\\u"); 
		RTFCHARS.put("=o", "\\u"); 
		RTFCHARS.put("uO", "\\u"); 
		RTFCHARS.put("uo", "\\u"); 
		RTFCHARS.put("HO", "\\u"); 
		RTFCHARS.put("Ho", "\\u"); 
		RTFCHARS.put("OE", "{\\u\\u}"); 
		RTFCHARS.put("oe", "{\\u\\u}"); 
		RTFCHARS.put("'R", "\\u"); 
		RTFCHARS.put("'r", "\\u"); 
		RTFCHARS.put("cR", "\\u"); 
		RTFCHARS.put("cr", "\\u"); 
		RTFCHARS.put("vR", "\\u"); 
		RTFCHARS.put("vr", "\\u"); 
		RTFCHARS.put("'S", "\\u"); 
		RTFCHARS.put("'s", "\\u"); 
		RTFCHARS.put("^S", "\\u"); 
		RTFCHARS.put("^s", "\\u"); 
		RTFCHARS.put("cS", "\\u"); 
		RTFCHARS.put("cs", "\\u"); 
		RTFCHARS.put("vS", "\\u"); 
		RTFCHARS.put("vs", "\\u"); 
		RTFCHARS.put("cT", "\\u"); 
		RTFCHARS.put("ct", "\\u"); 
		RTFCHARS.put("vT", "\\u"); 
		
		RTFCHARS.put("Tstrok", "\\u"); 
		RTFCHARS.put("tstrok", "\\u"); 
		RTFCHARS.put("~U", "\\u"); 
		RTFCHARS.put("~u", "\\u"); 
		RTFCHARS.put("=U", "\\u"); 
		RTFCHARS.put("=u", "\\u"); 
		RTFCHARS.put("uU", "\\u"); 
		RTFCHARS.put("uu", "\\u"); 
		RTFCHARS.put("rU", "\\u"); 
		RTFCHARS.put("ru", "\\u"); 
		RTFCHARS.put("HU", "\\u"); 
		RTFCHARS.put("Hu", "\\u"); 
		RTFCHARS.put("kU", "\\u"); 
		RTFCHARS.put("ku", "\\u"); 
		RTFCHARS.put("^W", "\\u"); 
		RTFCHARS.put("^w", "\\u"); 
		RTFCHARS.put("^Y", "\\u"); 
		RTFCHARS.put("^y", "\\u"); 
		RTFCHARS.put("\"Y","\\u"); 
		RTFCHARS.put("'Z", "\\u"); 
		RTFCHARS.put("'z", "\\u"); 
		RTFCHARS.put(".Z", "\\u"); 
		RTFCHARS.put(".z", "\\u"); 
		RTFCHARS.put("vZ", "\\u"); 
		RTFCHARS.put("vz", "\\u"); 
		

		
	}

	public static void initializeJournalNames() {
		journalAbbrev = new JournalAbbreviations();

		
		
		String[] lists = prefs.getStringArray("externalJournalLists");
		if ((lists != null) && (lists.length > 0)) {
			for (int i = lists.length - 1; i >= 0; i--) {
				try {
					journalAbbrev.readJournalList(new File(lists[i]));
				} catch (FileNotFoundException e) {
					
					Globals.logger(e.getMessage());
				}
			}
		}

		
		if (prefs.get("personalJournalList") != null) {
			try {
				journalAbbrev.readJournalList(new File(prefs.get("personalJournalList")));
			} catch (FileNotFoundException e) {
				Globals.logger("Personal journal list file '" + prefs.get("personalJournalList")
					+ "' not found.");
			}
		}

	}

}
