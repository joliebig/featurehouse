
package net.sf.jabref;

import java.awt.FileDialog;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.ConsoleHandler;
import java.util.logging.Filter;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.event.UndoableEditListener;

import net.sf.jabref.collab.FileUpdateMonitor;
import net.sf.jabref.imports.ImportFormatReader;
import net.sf.jabref.journals.JournalAbbreviations;
import net.sf.jabref.util.ErrorConsole;
import net.sf.jabref.util.TBuildInfo;

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
        PROTECTED_FLAG_META = "protectedFlag",
        LAYOUT_PREFIX = "/resource/layout/", MAC = "Mac OS X",
		DOI_LOOKUP_PREFIX = "http://dx.doi.org/", NONE = "_non__",
		FORMATTER_PACKAGE = "net.sf.jabref.export.layout.format.";

	public static float duplicateThreshold = 0.75f;

	private static Handler consoleHandler = new java.util.logging.ConsoleHandler();

	public static String[] ENCODINGS, ALL_ENCODINGS = 
		
		
		new String[] { "ISO8859_1", "UTF8", "UTF-16", "ASCII", "Cp1250", "Cp1251", "Cp1252",
			"Cp1253", "Cp1254", "Cp1257", "SJIS",
			"EUC_JP", 
			"Big5", "Big5_HKSCS", "GBK", "ISO8859_2", "ISO8859_3", "ISO8859_4", "ISO8859_5",
			"ISO8859_6", "ISO8859_7", "ISO8859_8", "ISO8859_9", "ISO8859_13", "ISO8859_15" };
    public static Map<String,String> ENCODING_NAMES_LOOKUP;

    
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
        
        ENCODING_NAMES_LOOKUP = new HashMap<String,String>();
        ENCODING_NAMES_LOOKUP.put("Cp1250", "windows-1250");
        ENCODING_NAMES_LOOKUP.put("Cp1251", "windows-1251");
        ENCODING_NAMES_LOOKUP.put("Cp1252", "windows-1252");
        ENCODING_NAMES_LOOKUP.put("Cp1253", "windows-1253");
        ENCODING_NAMES_LOOKUP.put("Cp1254", "windows-1254");
        ENCODING_NAMES_LOOKUP.put("Cp1257", "windows-1257");
        ENCODING_NAMES_LOOKUP.put("ISO8859_1", "ISO-8859-1");
        ENCODING_NAMES_LOOKUP.put("ISO8859_2", "ISO-8859-2");
        ENCODING_NAMES_LOOKUP.put("ISO8859_3", "ISO-8859-3");
        ENCODING_NAMES_LOOKUP.put("ISO8859_4", "ISO-8859-4");
        ENCODING_NAMES_LOOKUP.put("ISO8859_5", "ISO-8859-5");
        ENCODING_NAMES_LOOKUP.put("ISO8859_6", "ISO-8859-6");
        ENCODING_NAMES_LOOKUP.put("ISO8859_7", "ISO-8859-7");
        ENCODING_NAMES_LOOKUP.put("ISO8859_8", "ISO-8859-8");
        ENCODING_NAMES_LOOKUP.put("ISO8859_9", "ISO-8859-9");
        ENCODING_NAMES_LOOKUP.put("ISO8859_13", "ISO-8859-13");
        ENCODING_NAMES_LOOKUP.put("ISO8859_15", "ISO-8859-15");
        ENCODING_NAMES_LOOKUP.put("KOI8_R", "KOI8-R");
        ENCODING_NAMES_LOOKUP.put("UTF8", "UTF-8");
        ENCODING_NAMES_LOOKUP.put("UTF-16", "UTF-16");
        ENCODING_NAMES_LOOKUP.put("SJIS", "Shift_JIS");
        ENCODING_NAMES_LOOKUP.put("GBK", "GBK");
        ENCODING_NAMES_LOOKUP.put("Big5_HKSCS", "Big5-HKSCS");
        ENCODING_NAMES_LOOKUP.put("Big5", "Big5");
        ENCODING_NAMES_LOOKUP.put("EUC_JP", "EUC-JP");
        ENCODING_NAMES_LOOKUP.put("ASCII", "US-ASCII");
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

    
    
    
    private static Logger logger = Logger.getLogger("global");

    
	public static final boolean UNIX_NEWLINE = NEWLINE.equals("\n");

	
	public static final String BIBTEX_STRING = "__string";

	public static void logger(String s) {
		logger.info(s);
	}

	public static void turnOffLogging() { 
		logger.setLevel(java.util.logging.Level.SEVERE);
	}

	
	public static void turnOnConsoleLogging() {
		logger.addHandler(consoleHandler);
	}

	
	public static void turnOnFileLogging() {
		logger.setLevel(java.util.logging.Level.ALL);
		java.util.logging.Handler handler;
		handler = new ConsoleHandler();
		logger.addHandler(handler);

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

        
        
        if (!dirOnly && prefs.getBoolean("useNativeFileDialogOnMac")) {

			return getNewFileForMac(owner, directory, extension, dialogType,
				updateWorkingDirectory, dirOnly, off);
		}

		JFileChooser fc;
		try {
			fc = new JFileChooser(directory);
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
		
        HTMLCHARS.put("%", "%"); 

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
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{E\\}\\}", "&#x00C9;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{i\\}\\}", "&#x00ED;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{I\\}\\}", "&#x00CD;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{o\\}\\}", "&#x00F3;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{O\\}\\}", "&#x00D3;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{u\\}\\}", "&#x00FA;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{U\\}\\}", "&#x00DA;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{a\\}\\}", "&#x00E1;");
		XML_CHARS.put("\\{\\\\\\\uFFFD\\{A\\}\\}", "&#x00C1;");

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

		UNICODE_CHARS.put("\u00C0", "A");
		UNICODE_CHARS.put("\u00C1", "A");
		UNICODE_CHARS.put("\u00C2", "A");
		UNICODE_CHARS.put("\u00C3", "A");
		UNICODE_CHARS.put("\u00C4", "Ae");
		UNICODE_CHARS.put("\u00C5", "Aa");
		UNICODE_CHARS.put("\u00C6", "Ae");
		UNICODE_CHARS.put("\u00C7", "C");
		UNICODE_CHARS.put("\u00C8", "E");
		UNICODE_CHARS.put("\u00C9", "E");
		UNICODE_CHARS.put("\u00CA", "E");
		UNICODE_CHARS.put("\u00CB", "E");
		UNICODE_CHARS.put("\u00CC", "I");
		UNICODE_CHARS.put("\u00CD", "I");
		UNICODE_CHARS.put("\u00CE", "I");
		UNICODE_CHARS.put("\u00CF", "I");
		UNICODE_CHARS.put("\u00D0", "D");
		UNICODE_CHARS.put("\u00D1", "N");
		UNICODE_CHARS.put("\u00D2", "O");
		UNICODE_CHARS.put("\u00D3", "O");
		UNICODE_CHARS.put("\u00D4", "O");
		UNICODE_CHARS.put("\u00D5", "O");
		UNICODE_CHARS.put("\u00D6", "Oe");
		UNICODE_CHARS.put("\u00D8", "Oe");
		UNICODE_CHARS.put("\u00D9", "U");
		UNICODE_CHARS.put("\u00DA", "U");
		UNICODE_CHARS.put("\u00DB", "U");
		UNICODE_CHARS.put("\u00DC", "Ue"); 
		UNICODE_CHARS.put("\u00DD", "Y");
		UNICODE_CHARS.put("\u00DF", "ss");
		UNICODE_CHARS.put("\u00E0", "a");
		UNICODE_CHARS.put("\u00E1", "a");
		UNICODE_CHARS.put("\u00E2", "a");
		UNICODE_CHARS.put("\u00E3", "a");
		UNICODE_CHARS.put("\u00E4", "ae");
		UNICODE_CHARS.put("\u00E5", "aa");
		UNICODE_CHARS.put("\u00E6", "ae");
		UNICODE_CHARS.put("\u00E7", "c");
		UNICODE_CHARS.put("\u00E8", "e");
		UNICODE_CHARS.put("\u00E9", "e");
		UNICODE_CHARS.put("\u00EA", "e");
		UNICODE_CHARS.put("\u00EB", "e");
		UNICODE_CHARS.put("\u00EC", "i");
		UNICODE_CHARS.put("\u00ED", "i");
		UNICODE_CHARS.put("\u00EE", "i");
		UNICODE_CHARS.put("\u00EF", "i");
		UNICODE_CHARS.put("\u00F0", "o");
		UNICODE_CHARS.put("\u00F1", "n");
		UNICODE_CHARS.put("\u00F2", "o");
		UNICODE_CHARS.put("\u00F3", "o");
		UNICODE_CHARS.put("\u00F4", "o");
		UNICODE_CHARS.put("\u00F5", "o");
		UNICODE_CHARS.put("\u00F6", "oe");
		UNICODE_CHARS.put("\u00F8", "oe");
		UNICODE_CHARS.put("\u00F9", "u");
		UNICODE_CHARS.put("\u00FA", "u");
		UNICODE_CHARS.put("\u00FB", "u");
		UNICODE_CHARS.put("\u00FC", "ue"); 
		UNICODE_CHARS.put("\u00FD", "y");
		UNICODE_CHARS.put("\u00FF", "y");
		UNICODE_CHARS.put("\u0100", "A");
		UNICODE_CHARS.put("\u0101", "a");
		UNICODE_CHARS.put("\u0102", "A");
		UNICODE_CHARS.put("\u0103", "a");
		UNICODE_CHARS.put("\u0104", "A");
		UNICODE_CHARS.put("\u0105", "a");
		UNICODE_CHARS.put("\u0106", "C");
		UNICODE_CHARS.put("\u0107", "c");
		UNICODE_CHARS.put("\u0108", "C");
		UNICODE_CHARS.put("\u0109", "c");
		UNICODE_CHARS.put("\u010A", "C");
		UNICODE_CHARS.put("\u010B", "c");
		UNICODE_CHARS.put("\u010C", "C");
		UNICODE_CHARS.put("\u010D", "c");
		UNICODE_CHARS.put("\u010E", "D");
		UNICODE_CHARS.put("\u010F", "d");
		UNICODE_CHARS.put("\u0110", "D");
		UNICODE_CHARS.put("\u0111", "d");
		UNICODE_CHARS.put("\u0112", "E");
		UNICODE_CHARS.put("\u0113", "e");
		UNICODE_CHARS.put("\u0114", "E");
		UNICODE_CHARS.put("\u0115", "e");
		UNICODE_CHARS.put("\u0116", "E");
		UNICODE_CHARS.put("\u0117", "e");
		UNICODE_CHARS.put("\u0118", "E");
		UNICODE_CHARS.put("\u0119", "e");
		UNICODE_CHARS.put("\u011A", "E");
		UNICODE_CHARS.put("\u011B", "e");
		UNICODE_CHARS.put("\u011C", "G");
		UNICODE_CHARS.put("\u011D", "g");
		UNICODE_CHARS.put("\u011E", "G");
		UNICODE_CHARS.put("\u011F", "g");
		UNICODE_CHARS.put("\u0120", "G");
		UNICODE_CHARS.put("\u0121", "g");
		UNICODE_CHARS.put("\u0122", "G");
		UNICODE_CHARS.put("\u0123", "g");
		UNICODE_CHARS.put("\u0124", "H");
		UNICODE_CHARS.put("\u0125", "h");
		UNICODE_CHARS.put("\u0127", "h");
		UNICODE_CHARS.put("\u0128", "I");
		UNICODE_CHARS.put("\u0129", "i");
		UNICODE_CHARS.put("\u012A", "I");
		UNICODE_CHARS.put("\u012B", "i");
		UNICODE_CHARS.put("\u012C", "I");
		UNICODE_CHARS.put("\u012D", "i");
        UNICODE_CHARS.put("\u0147", "N");
        UNICODE_CHARS.put("\u0148", "n");
        UNICODE_CHARS.put("\u0160", "S");
        UNICODE_CHARS.put("\u0161", "s");
        UNICODE_CHARS.put("\u017D", "Z");
        UNICODE_CHARS.put("\u017E", "z");
        UNICODE_CHARS.put("\u0158", "R");
        UNICODE_CHARS.put("\u0159", "r");
        UNICODE_CHARS.put("\u0164", "T");
        UNICODE_CHARS.put("\u008C", "AE"); 
        UNICODE_CHARS.put("\u016E", "U");
        UNICODE_CHARS.put("\u016F", "u");

        UNICODE_CHARS.put("\u0178", "Y");
        UNICODE_CHARS.put("\u0153", "ae");
        UNICODE_CHARS.put("\u00FE", ""); 
        
        

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
		RTFCHARS.put("\"u", "\\u252u");
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
		
		
		

		
		
		RTFCHARS.put("'A", "\\u193A"); 
		
		
		RTFCHARS.put("~A", "\\u195A"); 
		
		
		RTFCHARS.put("AA", "\\u197A"); 
		RTFCHARS.put("AE", "{\\uc2\\u198AE}"); 
		RTFCHARS.put("cC", "\\u199C"); 
		
		
		RTFCHARS.put("'E", "\\u201E"); 
		
		
		
		
		
		
		RTFCHARS.put("'I", "\\u205I"); 
		
		
		
		
		RTFCHARS.put("DH", "\\u208D"); 
		RTFCHARS.put("~N", "\\u209N"); 
		
		
		RTFCHARS.put("'O", "\\u211O"); 
		
		
		RTFCHARS.put("~O", "\\u213O"); 
		
		
		
		
		
		RTFCHARS.put("O", "\\u216O"); 
		
		
		RTFCHARS.put("'U", "\\u218U"); 
		
		
		
		
		RTFCHARS.put("'Y", "\\u221Y"); 
		RTFCHARS.put("TH", "{\\uc2\\u222TH}"); 
		RTFCHARS.put("ss", "{\\uc2\\u223ss}"); 
        
        
		
		RTFCHARS.put("'a", "\\u225a"); 
		
		
		RTFCHARS.put("~a", "\\u227a"); 
		
		
		RTFCHARS.put("aa", "\\u229a"); 
		RTFCHARS.put("ae", "{\\uc2\\u230ae}"); 
		RTFCHARS.put("cc", "\\u231c"); 
		
		
		RTFCHARS.put("'e", "\\u233e"); 
		
		
		
		
		
		
		RTFCHARS.put("'i", "\\u237i"); 
		
		
		
		
		RTFCHARS.put("dh", "\\u240d"); 
		
		
		
		
		RTFCHARS.put("'o", "\\u243o"); 
		
		
		RTFCHARS.put("~o", "\\u245o"); 
		
		
		
		
		
		RTFCHARS.put("o", "\\u248o"); 
		
		
		RTFCHARS.put("'u", "\\u250u"); 
		
		
		
		
		RTFCHARS.put("'y", "\\u253y"); 
		RTFCHARS.put("th", "{\\uc2\\u254th}"); 
		RTFCHARS.put("\"y", "\\u255y"); 

		RTFCHARS.put("=A", "\\u256A"); 
		RTFCHARS.put("=a", "\\u257a"); 
		RTFCHARS.put("uA", "\\u258A"); 
		RTFCHARS.put("ua", "\\u259a"); 
		RTFCHARS.put("kA", "\\u260A"); 
		RTFCHARS.put("ka", "\\u261a"); 
		RTFCHARS.put("'C", "\\u262C"); 
		RTFCHARS.put("'c", "\\u263c"); 
		RTFCHARS.put("^C", "\\u264C"); 
		RTFCHARS.put("^c", "\\u265c"); 
		RTFCHARS.put(".C", "\\u266C"); 
		RTFCHARS.put(".c", "\\u267c"); 
		RTFCHARS.put("vC", "\\u268C"); 
		RTFCHARS.put("vc", "\\u269c"); 
		RTFCHARS.put("vD", "\\u270D"); 
		
		RTFCHARS.put("DJ", "\\u272D"); 
		RTFCHARS.put("dj", "\\u273d"); 
		RTFCHARS.put("=E", "\\u274E"); 
		RTFCHARS.put("=e", "\\u275e"); 
		RTFCHARS.put("uE", "\\u276E"); 
		RTFCHARS.put("ue", "\\u277e"); 
		RTFCHARS.put(".E", "\\u278E"); 
		RTFCHARS.put(".e", "\\u279e"); 
		RTFCHARS.put("kE", "\\u280E"); 
		RTFCHARS.put("ke", "\\u281e"); 
		RTFCHARS.put("vE", "\\u282E"); 
		RTFCHARS.put("ve", "\\u283e"); 
		RTFCHARS.put("^G", "\\u284G"); 
		RTFCHARS.put("^g", "\\u285g"); 
		RTFCHARS.put("uG", "\\u286G"); 
		RTFCHARS.put("ug", "\\u287g"); 
		RTFCHARS.put(".G", "\\u288G"); 
		RTFCHARS.put(".g", "\\u289g"); 
		RTFCHARS.put("cG", "\\u290G"); 
		RTFCHARS.put("'g", "\\u291g"); 
		RTFCHARS.put("^H", "\\u292H"); 
		RTFCHARS.put("^h", "\\u293h"); 
		RTFCHARS.put("Hstrok", "\\u294H"); 
		RTFCHARS.put("hstrok", "\\u295h"); 
		RTFCHARS.put("~I", "\\u296I"); 
		RTFCHARS.put("~i", "\\u297i"); 
		RTFCHARS.put("=I", "\\u298I"); 
		RTFCHARS.put("=i", "\\u299i"); 
		RTFCHARS.put("uI", "\\u300I"); 
		RTFCHARS.put("ui", "\\u301i"); 
		RTFCHARS.put("kI", "\\u302I"); 
		RTFCHARS.put("ki", "\\u303i"); 
		RTFCHARS.put(".I", "\\u304I"); 
		RTFCHARS.put("i", "\\u305i"); 
		
		
		RTFCHARS.put("^J", "\\u308J"); 
		RTFCHARS.put("^j", "\\u309j"); 
		RTFCHARS.put("cK", "\\u310K"); 
		RTFCHARS.put("ck", "\\u311k"); 
		
		RTFCHARS.put("'L", "\\u313L"); 
		RTFCHARS.put("'l", "\\u314l"); 
		RTFCHARS.put("cL", "\\u315L"); 
		RTFCHARS.put("cl", "\\u316l"); 
		
		
		RTFCHARS.put("Lmidot", "\\u319L"); 
		RTFCHARS.put("lmidot", "\\u320l"); 
		RTFCHARS.put("L", "\\u321L"); 
		RTFCHARS.put("l", "\\u322l"); 
		RTFCHARS.put("'N", "\\u323N"); 
		RTFCHARS.put("'n", "\\u324n"); 
		RTFCHARS.put("cN", "\\u325N"); 
		RTFCHARS.put("cn", "\\u326n"); 
		RTFCHARS.put("vN", "\\u327N"); 
		RTFCHARS.put("vn", "\\u328n"); 
		
		RTFCHARS.put("NG", "\\u330G"); 
		RTFCHARS.put("ng", "\\u331g"); 
		RTFCHARS.put("=O", "\\u332O"); 
		RTFCHARS.put("=o", "\\u333o"); 
		RTFCHARS.put("uO", "\\u334O"); 
		RTFCHARS.put("uo", "\\u335o"); 
		RTFCHARS.put("HO", "\\u336?"); 
		RTFCHARS.put("Ho", "\\u337?"); 
		RTFCHARS.put("OE", "{\\uc2\\u338OE}"); 
		RTFCHARS.put("oe", "{\\uc2\\u339oe}"); 
		RTFCHARS.put("'R", "\\u340R"); 
		RTFCHARS.put("'r", "\\u341r"); 
		RTFCHARS.put("cR", "\\u342R"); 
		RTFCHARS.put("cr", "\\u343r"); 
		RTFCHARS.put("vR", "\\u344R"); 
		RTFCHARS.put("vr", "\\u345r"); 
		RTFCHARS.put("'S", "\\u346S"); 
		RTFCHARS.put("'s", "\\u347s"); 
		RTFCHARS.put("^S", "\\u348S"); 
		RTFCHARS.put("^s", "\\u349s"); 
		RTFCHARS.put("cS", "\\u350S"); 
		RTFCHARS.put("cs", "\\u351s"); 
		RTFCHARS.put("vS", "\\u352S"); 
		RTFCHARS.put("vs", "\\u353s"); 
		RTFCHARS.put("cT", "\\u354T"); 
		RTFCHARS.put("ct", "\\u355t"); 
		RTFCHARS.put("vT", "\\u356T"); 
		
		RTFCHARS.put("Tstrok", "\\u358T"); 
		RTFCHARS.put("tstrok", "\\u359t"); 
		RTFCHARS.put("~U", "\\u360U"); 
		RTFCHARS.put("~u", "\\u361u"); 
		RTFCHARS.put("=U", "\\u362U"); 
		RTFCHARS.put("=u", "\\u363u"); 
		RTFCHARS.put("uU", "\\u364U"); 
		RTFCHARS.put("uu", "\\u365u"); 
		RTFCHARS.put("rU", "\\u366U"); 
		RTFCHARS.put("ru", "\\u367u"); 
		RTFCHARS.put("HU", "\\u368?"); 
		RTFCHARS.put("Hu", "\\u369?"); 
		RTFCHARS.put("kU", "\\u370U"); 
		RTFCHARS.put("ku", "\\u371u"); 
		RTFCHARS.put("^W", "\\u372W"); 
		RTFCHARS.put("^w", "\\u373w"); 
		RTFCHARS.put("^Y", "\\u374Y"); 
		RTFCHARS.put("^y", "\\u375y"); 
		RTFCHARS.put("\"Y","\\u376Y"); 
		RTFCHARS.put("'Z", "\\u377Z"); 
		RTFCHARS.put("'z", "\\u378z"); 
		RTFCHARS.put(".Z", "\\u379Z"); 
		RTFCHARS.put(".z", "\\u380z"); 
		RTFCHARS.put("vZ", "\\u381Z"); 
		RTFCHARS.put("vz", "\\u382z"); 
		

		
	}

	public static void initializeJournalNames() {
		if (prefs.getBoolean("useIEEEAbrv"))
			journalAbbrev = new JournalAbbreviations("/resource/IEEEJournalList.txt");
        else
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
