/* (C) 2003 Nizar N. Batada, Morten O. Alver

 All programs in this directory and
 subdirectories are published under the GNU General Public License as
 described below.

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or (at
 your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 USA

 Further information about the GNU GPL is available at:
 http://www.gnu.org/copyleft/gpl.ja.html

 */
package net.sf.jabref;

import java.awt.FileDialog;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.nio.charset.Charset;
import java.util.*;
import java.util.logging.*;

import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;

import net.sf.jabref.collab.FileUpdateMonitor;
import net.sf.jabref.imports.ImportFormatReader;
import net.sf.jabref.journals.JournalAbbreviations;
import net.sf.jabref.util.ErrorConsole;
import net.sf.jabref.util.TBuildInfo;

public class Globals {

	public static int SHORTCUT_MASK,// =
		// Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
		FUTURE_YEAR = 2050, // Needs to give a year definitely in the future.
		// Used for guessing the
		// year field when parsing textual data. :-)

		STANDARD_EXPORT_COUNT = 5, // The number of standard export formats.
		METADATA_LINE_LENGTH = 70; // The line length used to wrap metadata.

	private static String resourcePrefix = "resource/JabRef", menuResourcePrefix = "resource/Menu",
		integrityResourcePrefix = "resource/IntegrityMessage";

	private static final String buildInfos = "/resource/build.properties";

	/*
	 * some extra field definitions
	 */
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

		// TODO: Error console initialization here. When should it be used?
		errorConsole = ErrorConsole.getInstance();
	}

	// public static ResourceBundle preferences =
	// ResourceBundle.getBundle("resource/defaultPrefs");
	public static Locale locale;

	public static final String FILETYPE_PREFS_EXT = "_dir", SELECTOR_META_PREFIX = "selector_",
		LAYOUT_PREFIX = "/resource/layout/", MAC = "Mac OS X",
		DOI_LOOKUP_PREFIX = "http://dx.doi.org/", NONE = "_non__",
		FORMATTER_PACKAGE = "net.sf.jabref.export.layout.format.";

	public static float duplicateThreshold = 0.75f;

	private static Handler consoleHandler = new java.util.logging.ConsoleHandler();

	public static String[] ENCODINGS, ALL_ENCODINGS = // (String[])
		// Charset.availableCharsets().keySet().toArray(new
		// String[]{});
		new String[] { "ISO8859_1", "UTF8", "UTF-16", "ASCII", "Cp1250", "Cp1251", "Cp1252",
			"Cp1253", "Cp1254", "Cp1257", "JIS", "SJIS",
			"EUC_JP", // Added Japanese encodings.
			"Big5", "Big5_HKSCS", "GBK", "ISO8859_2", "ISO8859_3", "ISO8859_4", "ISO8859_5",
			"ISO8859_6", "ISO8859_7", "ISO8859_8", "ISO8859_9", "ISO8859_13", "ISO8859_15" };

	// String array that maps from month number to month string label:
	public static String[] MONTHS = new String[] { "jan", "feb", "mar", "apr", "may", "jun", "jul",
		"aug", "sep", "oct", "nov", "dec" };

	// Map that maps from month string labels to
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

		// Build list of encodings, by filtering out all that are not supported
		// on this system:
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

	/**
	 * true if we have unix newlines
	 */
	public static final boolean UNIX_NEWLINE = NEWLINE.equals("\n");

	/**
	 * 	"Fieldname" to indicate that a field should be treated as a bibtex 
	 * string. Used when writing database to file.
	 */
	public static final String BIBTEX_STRING = "__string";

	public static void logger(String s) {
		Logger.global.info(s);
	}

	public static void turnOffLogging() { // only log exceptions
		Logger.global.setLevel(java.util.logging.Level.SEVERE);
	}

	/**
	 * Should be only called once
	 */
	public static void turnOnConsoleLogging() {
		Logger.global.addHandler(consoleHandler);
	}

	/**
	 * Should be only called once
	 */
	public static void turnOnFileLogging() {
		Logger.global.setLevel(java.util.logging.Level.ALL);
		java.util.logging.Handler handler;
		handler = new ConsoleHandler();
		Logger.global.addHandler(handler);

		handler.setFilter(new Filter() { // select what gets logged
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
			//logger("Warning: could not get translation for \"" + key + "\"");
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
							// append literally (for quoting) or insert special
							// symbol
							switch (c) {
							case 'c': // colon
								sb.append(':');
								break;
							case 'e': // equal
								sb.append('=');
								break;
							default: // anything else, e.g. %
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

			//System.err.println("Warning: could not get menu item translation for \""
			//+ key + "\"");

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

			// System.err.println("Warning: could not get menu item translation
			// for \""
			// + key + "\"");
		}
		if ((translation != null) && (translation.length() != 0)) {
			return translation;
		} else {
			return key;
		}
	}

	// ============================================================
	// Using the hashmap of entry types found in BibtexEntryType
	// ============================================================
	public static BibtexEntryType getEntryType(String type) {
		// decide which entryType object to return
		Object o = BibtexEntryType.ALL_TYPES.get(type);
		if (o != null) {
			return (BibtexEntryType) o;
		} else {
			return BibtexEntryType.OTHER;
		}
		/*
		 * if(type.equals("article")) return BibtexEntryType.ARTICLE; else
		 * if(type.equals("book")) return BibtexEntryType.BOOK; else
		 * if(type.equals("inproceedings")) return
		 * BibtexEntryType.INPROCEEDINGS;
		 */
	}

	/**
	 * This method provides the correct opening brace to use when writing a
	 * field to BibTeX format.
	 * 
	 * @return A String containing the braces to use.
	 */
	public static String getOpeningBrace() {
		return "{";
		/*
		 * As of version 2.0, storing all fields with double braces is no longer
		 * supported, because it causes problems with e.g. the author field.
		 * 
		 * if (prefs.getBoolean("autoDoubleBraces")) return "{{"; else return
		 * "{";
		 */
	}

	/**
	 * This method provides the correct closing brace to use when writing a
	 * field to BibTeX format.
	 * 
	 * @return A String containing the braces to use.
	 */
	public static String getClosingBrace() {
		return "}";
		/*
		 * As of version 2.0, storing all fields with double braces is no longer
		 * supported, because it causes problems with e.g. the author field.
		 * 
		 * if (prefs.getBoolean("autoDoubleBraces")) return "}}"; else
		 */

	}

	/**
	 * Will return the names of multiple files selected in the given directory
	 * and the given extensions.
	 * 
	 * Will return an empty String array if no entry is found.
	 * 
	 * @param owner
	 * @param directory
	 * @param extension
	 * @param updateWorkingdirectory
	 * @return
	 */
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
		// Fix for:
		// http://sourceforge.net/tracker/index.php?func=detail&aid=1538769&group_id=92314&atid=600306
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
			// This try/catch clause was added because a user reported an
			// InternalError getting thrown on WinNT, presumably because of a
			// bug in JGoodies Windows PLAF. This clause can be removed if the
			// bug is fixed, but for now we just resort to the native file
			// dialog, using the same method as is always used on Mac:
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

		// the getSelectedFile method returns a valid fileselection
		// (if something is selected) indepentently from dialog return status
		if (dialogResult != JFileChooser.APPROVE_OPTION)
			return null;

		// okay button
		File selectedFile = fc.getSelectedFile();
		if (selectedFile == null) { // cancel
			return null;
		}

		// If this is a save dialog, and the user has not chosen "All files" as
		// filter
		// we enforce the given extension. But only if extension is not null.
		if ((extension != null) && (dialogType == JFileChooser.SAVE_DIALOG)
			&& (fc.getFileFilter() == off) && !off.accept(selectedFile)) {

			// add the first extension if there are multiple extensions
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

		// fc.setFilenameFilter(filter);
		if (directory != null) {
			fc.setDirectory(directory.getParent());
		}
		if (dialogType == JFileChooser.OPEN_DIALOG) {
			fc.setMode(FileDialog.LOAD);
		} else {
			fc.setMode(FileDialog.SAVE);
		}

		fc.setVisible(true); // fc.show(); -> deprecated since 1.5

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

		// System.out.println(journalAbbrev.getAbbreviatedName("Journal of Fish
		// Biology", true));
		// System.out.println(journalAbbrev.getAbbreviatedName("Journal of Fish
		// Biology", false));
		// System.out.println(journalAbbrev.getFullName("Aquaculture Eng."));
		/*
		 * for (Iterator i=journalAbbrev.fullNameIterator(); i.hasNext();) {
		 * String s = (String)i.next();
		 * System.out.println(journalAbbrev.getFullName(s)+" :
		 * "+journalAbbrev.getAbbreviatedName(s, true)); }
		 */

		// Start the thread that monitors file time stamps.
		// Util.pr("Starting FileUpdateMonitor thread. Globals line 293.");
		fileUpdateMonitor.start();

		try {
			SHORTCUT_MASK = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
		} catch (Throwable t) {

		}

		// Special characters in URLs need to be replaced to ensure that the URL
		// opens properly on all platforms:
		URL_CHARS.put("<", "%3c");
		URL_CHARS.put(">", "%3e");
		URL_CHARS.put("(", "%28");
		URL_CHARS.put(")", "%29");
		URL_CHARS.put(" ", "%20");
		URL_CHARS.put("&", "%26");
		URL_CHARS.put("$", "%24");

		// HTMLCHARS.put("\"a", "&auml;");
		// HTMLCHARS.put("\"A", "&Auml;");
		// HTMLCHARS.put("\"e", "&euml;");
		// HTMLCHARS.put("\"E", "&Euml;");
		// HTMLCHARS.put("\"i", "&iuml;");
		// HTMLCHARS.put("\"I", "&Iuml;");
		// HTMLCHARS.put("\"o", "&ouml;");
		// HTMLCHARS.put("\"O", "&Ouml;");
		// HTMLCHARS.put("\"u", "&uuml;");
		// HTMLCHARS.put("\"U", "&Uuml;");
		// HTMLCHARS.put("`a", "&agrave;");
		// HTMLCHARS.put("`A", "&Agrave;");
		// HTMLCHARS.put("`e", "&egrave;");
		// HTMLCHARS.put("`E", "&Egrave;");
		// HTMLCHARS.put("`i", "&igrave;");
		// HTMLCHARS.put("`I", "&Igrave;");
		// HTMLCHARS.put("`o", "&ograve;");
		// HTMLCHARS.put("`O", "&Ograve;");
		// HTMLCHARS.put("`u", "&ugrave;");
		// HTMLCHARS.put("`U", "&Ugrave;");
		// HTMLCHARS.put("'e", "&eacute;");
		// HTMLCHARS.put("'E", "&Eacute;");
		// HTMLCHARS.put("'i", "&iacute;");
		// HTMLCHARS.put("'I", "&Iacute;");
		// HTMLCHARS.put("'o", "&oacute;");
		// HTMLCHARS.put("'O", "&Oacute;");
		// HTMLCHARS.put("'u", "&uacute;");
		// HTMLCHARS.put("'U", "&Uacute;");
		// HTMLCHARS.put("'a", "&aacute;");
		// HTMLCHARS.put("'A", "&Aacute;");
		// HTMLCHARS.put("^a", "&ocirc;");
		// HTMLCHARS.put("^A", "&Ocirc;");
		// HTMLCHARS.put("^o", "&ocirc;");
		// HTMLCHARS.put("^O", "&Ocirc;");
		// HTMLCHARS.put("^u", "&ucirc;");
		// HTMLCHARS.put("^U", "&Ucirc;");
		// HTMLCHARS.put("^e", "&ecirc;");
		// HTMLCHARS.put("^E", "&Ecirc;");
		// HTMLCHARS.put("^i", "&icirc;");
		// HTMLCHARS.put("^I", "&Icirc;");
		// HTMLCHARS.put("~o", "&otilde;");
		// HTMLCHARS.put("~O", "&Otilde;");
		// HTMLCHARS.put("~n", "&ntilde;");
		// HTMLCHARS.put("~N", "&Ntilde;");
		// HTMLCHARS.put("~a", "&atilde;");
		// HTMLCHARS.put("~A", "&Atilde;");
		// HTMLCHARS.put("cc", "&ccedil;");
		// HTMLCHARS.put("cC", "&Ccedil;");

		// Following character definitions contributed by Ervin Kolenovic:
		// HTML named entities from #192 - #255 (UNICODE Latin-1)
		HTMLCHARS.put("`A", "&Agrave;"); // #192
		HTMLCHARS.put("'A", "&Aacute;"); // #193
		HTMLCHARS.put("^A", "&Acirc;"); // #194
		HTMLCHARS.put("~A", "&Atilde;"); // #195
		HTMLCHARS.put("\"A", "&Auml;"); // #196
		HTMLCHARS.put("AA", "&Aring;"); // #197
		HTMLCHARS.put("AE", "&AElig;"); // #198
		HTMLCHARS.put("cC", "&Ccedil;"); // #199
		HTMLCHARS.put("`E", "&Egrave;"); // #200
		HTMLCHARS.put("'E", "&Eacute;"); // #201
		HTMLCHARS.put("^E", "&Ecirc;"); // #202
		HTMLCHARS.put("\"E", "&Euml;"); // #203
		HTMLCHARS.put("`I", "&Igrave;"); // #204
		HTMLCHARS.put("'I", "&Iacute;"); // #205
		HTMLCHARS.put("^I", "&Icirc;"); // #206
		HTMLCHARS.put("\"I", "&Iuml;"); // #207
		HTMLCHARS.put("DH", "&ETH;"); // #208
		HTMLCHARS.put("~N", "&Ntilde;"); // #209
		HTMLCHARS.put("`O", "&Ograve;"); // #210
		HTMLCHARS.put("'O", "&Oacute;"); // #211
		HTMLCHARS.put("^O", "&Ocirc;"); // #212
		HTMLCHARS.put("~O", "&Otilde;"); // #213
		HTMLCHARS.put("\"O", "&Ouml;"); // #214
		// According to ISO 8859-1 the "\times" symbol should be placed here
		// (#215).
		// Omitting this, because it is a mathematical symbol.
		HTMLCHARS.put("O", "&Oslash;"); // #216
		HTMLCHARS.put("`U", "&Ugrave;"); // #217
		HTMLCHARS.put("'U", "&Uacute;"); // #218
		HTMLCHARS.put("^U", "&Ucirc;"); // #219
		HTMLCHARS.put("\"U", "&Uuml;"); // #220
		HTMLCHARS.put("'Y", "&Yacute;"); // #221
		HTMLCHARS.put("TH", "&THORN;"); // #222
		HTMLCHARS.put("ss", "&szlig;"); // #223
		HTMLCHARS.put("`a", "&agrave;"); // #224
		HTMLCHARS.put("'a", "&aacute;"); // #225
		HTMLCHARS.put("^a", "&acirc;"); // #226
		HTMLCHARS.put("~a", "&atilde;"); // #227
		HTMLCHARS.put("\"a", "&auml;"); // #228
		HTMLCHARS.put("aa", "&aring;"); // #229
		HTMLCHARS.put("ae", "&aelig;"); // #230
		HTMLCHARS.put("cc", "&ccedil;"); // #231
		HTMLCHARS.put("`e", "&egrave;"); // #232
		HTMLCHARS.put("'e", "&eacute;"); // #233
		HTMLCHARS.put("^e", "&ecirc;"); // #234
		HTMLCHARS.put("\"e", "&euml;"); // #235
		HTMLCHARS.put("`i", "&igrave;"); // #236
		HTMLCHARS.put("'i", "&iacute;"); // #237
		HTMLCHARS.put("^i", "&icirc;"); // #238
		HTMLCHARS.put("\"i", "&iuml;"); // #239
		HTMLCHARS.put("dh", "&eth;"); // #240
		HTMLCHARS.put("~n", "&ntilde;"); // #241
		HTMLCHARS.put("`o", "&ograve;"); // #242
		HTMLCHARS.put("'o", "&oacute;"); // #243
		HTMLCHARS.put("^o", "&ocirc;"); // #244
		HTMLCHARS.put("~o", "&otilde;"); // #245
		HTMLCHARS.put("\"o", "&ouml;"); // #246
		// According to ISO 8859-1 the "\div" symbol should be placed here
		// (#247).
		// Omitting this, because it is a mathematical symbol.
		HTMLCHARS.put("o", "&oslash;"); // #248
		HTMLCHARS.put("`u", "&ugrave;"); // #249
		HTMLCHARS.put("'u", "&uacute;"); // #250
		HTMLCHARS.put("^u", "&ucirc;"); // #251
		HTMLCHARS.put("\"u", "&uuml;"); // #252
		HTMLCHARS.put("'y", "&yacute;"); // #253
		HTMLCHARS.put("th", "&thorn;"); // #254
		HTMLCHARS.put("\"y", "&yuml;"); // #255

		// HTML special characters without names (UNICODE Latin Extended-A),
		// indicated by UNICODE number
		HTMLCHARS.put("=A", "&#256;"); // "Amacr"
		HTMLCHARS.put("=a", "&#257;"); // "amacr"
		HTMLCHARS.put("uA", "&#258;"); // "Abreve"
		HTMLCHARS.put("ua", "&#259;"); // "abreve"
		HTMLCHARS.put("kA", "&#260;"); // "Aogon"
		HTMLCHARS.put("ka", "&#261;"); // "aogon"
		HTMLCHARS.put("'C", "&#262;"); // "Cacute"
		HTMLCHARS.put("'c", "&#263;"); // "cacute"
		HTMLCHARS.put("^C", "&#264;"); // "Ccirc"
		HTMLCHARS.put("^c", "&#265;"); // "ccirc"
		HTMLCHARS.put(".C", "&#266;"); // "Cdot"
		HTMLCHARS.put(".c", "&#267;"); // "cdot"
		HTMLCHARS.put("vC", "&#268;"); // "Ccaron"
		HTMLCHARS.put("vc", "&#269;"); // "ccaron"
		HTMLCHARS.put("vD", "&#270;"); // "Dcaron"
		// Symbol #271 (d´) has no special Latex command
		HTMLCHARS.put("DJ", "&#272;"); // "Dstrok"
		HTMLCHARS.put("dj", "&#273;"); // "dstrok"
		HTMLCHARS.put("=E", "&#274;"); // "Emacr"
		HTMLCHARS.put("=e", "&#275;"); // "emacr"
		HTMLCHARS.put("uE", "&#276;"); // "Ebreve"
		HTMLCHARS.put("ue", "&#277;"); // "ebreve"
		HTMLCHARS.put(".E", "&#278;"); // "Edot"
		HTMLCHARS.put(".e", "&#279;"); // "edot"
		HTMLCHARS.put("kE", "&#280;"); // "Eogon"
		HTMLCHARS.put("ke", "&#281;"); // "eogon"
		HTMLCHARS.put("vE", "&#282;"); // "Ecaron"
		HTMLCHARS.put("ve", "&#283;"); // "ecaron"
		HTMLCHARS.put("^G", "&#284;"); // "Gcirc"
		HTMLCHARS.put("^g", "&#285;"); // "gcirc"
		HTMLCHARS.put("uG", "&#286;"); // "Gbreve"
		HTMLCHARS.put("ug", "&#287;"); // "gbreve"
		HTMLCHARS.put(".G", "&#288;"); // "Gdot"
		HTMLCHARS.put(".g", "&#289;"); // "gdot"
		HTMLCHARS.put("cG", "&#290;"); // "Gcedil"
		HTMLCHARS.put("'g", "&#291;"); // "gacute"
		HTMLCHARS.put("^H", "&#292;"); // "Hcirc"
		HTMLCHARS.put("^h", "&#293;"); // "hcirc"
		HTMLCHARS.put("Hstrok", "&#294;"); // "Hstrok"
		HTMLCHARS.put("hstrok", "&#295;"); // "hstrok"
		HTMLCHARS.put("~I", "&#296;"); // "Itilde"
		HTMLCHARS.put("~i", "&#297;"); // "itilde"
		HTMLCHARS.put("=I", "&#298;"); // "Imacr"
		HTMLCHARS.put("=i", "&#299;"); // "imacr"
		HTMLCHARS.put("uI", "&#300;"); // "Ibreve"
		HTMLCHARS.put("ui", "&#301;"); // "ibreve"
		HTMLCHARS.put("kI", "&#302;"); // "Iogon"
		HTMLCHARS.put("ki", "&#303;"); // "iogon"
		HTMLCHARS.put(".I", "&#304;"); // "Idot"
		HTMLCHARS.put("i", "&#305;"); // "inodot"
		// Symbol #306 (IJ) has no special Latex command
		// Symbol #307 (ij) has no special Latex command
		HTMLCHARS.put("^J", "&#308;"); // "Jcirc"
		HTMLCHARS.put("^j", "&#309;"); // "jcirc"
		HTMLCHARS.put("cK", "&#310;"); // "Kcedil"
		HTMLCHARS.put("ck", "&#311;"); // "kcedil"
		// Symbol #312 (k) has no special Latex command
		HTMLCHARS.put("'L", "&#313;"); // "Lacute"
		HTMLCHARS.put("'l", "&#314;"); // "lacute"
		HTMLCHARS.put("cL", "&#315;"); // "Lcedil"
		HTMLCHARS.put("cl", "&#316;"); // "lcedil"
		// Symbol #317 (L´) has no special Latex command
		// Symbol #318 (l´) has no special Latex command
		HTMLCHARS.put("Lmidot", "&#319;"); // "Lmidot"
		HTMLCHARS.put("lmidot", "&#320;"); // "lmidot"
		HTMLCHARS.put("L", "&#321;"); // "Lstrok"
		HTMLCHARS.put("l", "&#322;"); // "lstrok"
		HTMLCHARS.put("'N", "&#323;"); // "Nacute"
		HTMLCHARS.put("'n", "&#324;"); // "nacute"
		HTMLCHARS.put("cN", "&#325;"); // "Ncedil"
		HTMLCHARS.put("cn", "&#326;"); // "ncedil"
		HTMLCHARS.put("vN", "&#327;"); // "Ncaron"
		HTMLCHARS.put("vn", "&#328;"); // "ncaron"
		// Symbol #329 (´n) has no special Latex command
		HTMLCHARS.put("NG", "&#330;"); // "ENG"
		HTMLCHARS.put("ng", "&#331;"); // "eng"
		HTMLCHARS.put("=O", "&#332;"); // "Omacr"
		HTMLCHARS.put("=o", "&#333;"); // "omacr"
		HTMLCHARS.put("uO", "&#334;"); // "Obreve"
		HTMLCHARS.put("uo", "&#335;"); // "obreve"
		HTMLCHARS.put("HO", "&#336;"); // "Odblac"
		HTMLCHARS.put("Ho", "&#337;"); // "odblac"
		HTMLCHARS.put("OE", "&#338;"); // "OElig"
		HTMLCHARS.put("oe", "&#339;"); // "oelig"
		HTMLCHARS.put("'R", "&#340;"); // "Racute"
		HTMLCHARS.put("'r", "&#341;"); // "racute"
		HTMLCHARS.put("cR", "&#342;"); // "Rcedil"
		HTMLCHARS.put("cr", "&#343;"); // "rcedil"
		HTMLCHARS.put("vR", "&#344;"); // "Rcaron"
		HTMLCHARS.put("vr", "&#345;"); // "rcaron"
		HTMLCHARS.put("'S", "&#346;"); // "Sacute"
		HTMLCHARS.put("'s", "&#347;"); // "sacute"
		HTMLCHARS.put("^S", "&#348;"); // "Scirc"
		HTMLCHARS.put("^s", "&#349;"); // "scirc"
		HTMLCHARS.put("cS", "&#350;"); // "Scedil"
		HTMLCHARS.put("cs", "&#351;"); // "scedil"
		HTMLCHARS.put("vS", "&#352;"); // "Scaron"
		HTMLCHARS.put("vs", "&#353;"); // "scaron"
		HTMLCHARS.put("cT", "&#354;"); // "Tcedil"
		HTMLCHARS.put("ct", "&#355;"); // "tcedil"
		HTMLCHARS.put("vT", "&#356;"); // "Tcaron"
		// Symbol #357 (t´) has no special Latex command
		HTMLCHARS.put("Tstrok", "&#358;"); // "Tstrok"
		HTMLCHARS.put("tstrok", "&#359;"); // "tstrok"
		HTMLCHARS.put("~U", "&#360;"); // "Utilde"
		HTMLCHARS.put("~u", "&#361;"); // "utilde"
		HTMLCHARS.put("=U", "&#362;"); // "Umacr"
		HTMLCHARS.put("=u", "&#363;"); // "umacr"
		HTMLCHARS.put("uU", "&#364;"); // "Ubreve"
		HTMLCHARS.put("uu", "&#365;"); // "ubreve"
		HTMLCHARS.put("rU", "&#366;"); // "Uring"
		HTMLCHARS.put("ru", "&#367;"); // "uring"
		HTMLCHARS.put("HU", "&#368;"); // "Odblac"
		HTMLCHARS.put("Hu", "&#369;"); // "odblac"
		HTMLCHARS.put("kU", "&#370;"); // "Uogon"
		HTMLCHARS.put("ku", "&#371;"); // "uogon"
		HTMLCHARS.put("^W", "&#372;"); // "Wcirc"
		HTMLCHARS.put("^w", "&#373;"); // "wcirc"
		HTMLCHARS.put("^Y", "&#374;"); // "Ycirc"
		HTMLCHARS.put("^y", "&#375;"); // "ycirc"
		HTMLCHARS.put("\"Y", "&#376;"); // "Yuml"
		HTMLCHARS.put("'Z", "&#377;"); // "Zacute"
		HTMLCHARS.put("'z", "&#378;"); // "zacute"
		HTMLCHARS.put(".Z", "&#379;"); // "Zdot"
		HTMLCHARS.put(".z", "&#380;"); // "zdot"
		HTMLCHARS.put("vZ", "&#381;"); // "Zcaron"
		HTMLCHARS.put("vz", "&#382;"); // "zcaron"
		// Symbol #383 (f) has no special Latex command

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
		UNICODE_CHARS.put("\u", "Ue"); // U umlaut ..
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
		UNICODE_CHARS.put("\u", "ue"); // u umlaut...
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
		// UNICODE_CHARS.put("\u", "");

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
		
		// Use UNICODE characters for RTF-Chars which can not be found in the
		// standard codepage

		// RTFCHARS.put("`A", "\\u"); // "Agrave" exists in standard
		// codepage
		RTFCHARS.put("'A", "\\u"); // "Aacute"
		// RTFCHARS.put("^A", "\\u"); // "Acirc" exists in standard
		// codepage
		RTFCHARS.put("~A", "\\u"); // "Atilde"
		// RTFCHARS.put("\"A", "\\u"); // "Auml" exists in standard
		// codepage
		RTFCHARS.put("AA", "\\u"); // "Aring"
		RTFCHARS.put("AE", "{\\u\\u}"); // "AElig"
		RTFCHARS.put("cC", "\\u"); // "Ccedil"
		// RTFCHARS.put("`E", "\\u"); // "Egrave" exists in standard
		// codepage
		RTFCHARS.put("'E", "\\u"); // "Eacute"
		// RTFCHARS.put("^E", "\\u"); // "Ecirc" exists in standard
		// codepage
		// RTFCHARS.put("\"E", "\\u"); // "Euml" exists in standard
		// codepage
		// RTFCHARS.put("`I", "\\u"); // "Igrave" exists in standard
		// codepage
		RTFCHARS.put("'I", "\\u"); // "Iacute"
		// RTFCHARS.put("^I", "\\u"); // "Icirc" exists in standard
		// codepage
		// RTFCHARS.put("\"I", "\\u"); // "Iuml" exists in standard
		// codepage
		RTFCHARS.put("DH", "\\u"); // "ETH"
		RTFCHARS.put("~N", "\\u"); // "Ntilde"
		// RTFCHARS.put("`O", "\\u"); // "Ograve" exists in standard
		// codepage
		RTFCHARS.put("'O", "\\u"); // "Oacute"
		// RTFCHARS.put("^O", "\\u"); // "Ocirc" exists in standard
		// codepage
		RTFCHARS.put("~O", "\\u"); // "Otilde"
		// RTFCHARS.put("\"O", "\\u"); // "Ouml" exists in standard
		// codepage
		// According to ISO 8859-1 the "\times" symbol should be placed here
		// (#215).
		// Omitting this, because it is a mathematical symbol.
		RTFCHARS.put("O", "\\u"); // "Oslash"
		// RTFCHARS.put("`U", "\\u"); // "Ugrave" exists in standard
		// codepage
		RTFCHARS.put("'U", "\\u"); // "Uacute"
		// RTFCHARS.put("^U", "\\u"); // "Ucirc" exists in standard
		// codepage
		// RTFCHARS.put("\"U", "\\u"); // "Uuml" exists in standard
		// codepage
		RTFCHARS.put("'Y", "\\u"); // "Yacute"
		RTFCHARS.put("TH", "{\\u\\u}"); // "THORN"
		RTFCHARS.put("ss", "{\\u\\u"); // "szlig"
		// RTFCHARS.put("`a", "\\u"); // "agrave" exists in standard
		// codepage
		RTFCHARS.put("'a", "\\u"); // "aacute"
		// RTFCHARS.put("^a", "\\u"); // "acirc" exists in standard
		// codepage
		RTFCHARS.put("~a", "\\u"); // "atilde"
		// RTFCHARS.put("\"a", "\\u"); // "auml" exists in standard
		// codepage
		RTFCHARS.put("aa", "\\u"); // "aring"
		RTFCHARS.put("ae", "{\\u\\u}"); // "aelig"
		RTFCHARS.put("cc", "\\u"); // "ccedil"
		// RTFCHARS.put("`e", "\\u"); // "egrave" exists in standard
		// codepage
		RTFCHARS.put("'e", "\\u"); // "eacute"
		// RTFCHARS.put("^e", "\\u"); // "ecirc" exists in standard
		// codepage
		// RTFCHARS.put("\"e", "\\u"); // "euml" exists in standard
		// codepage
		// RTFCHARS.put("`i", "\\u"); // "igrave" exists in standard
		// codepage
		RTFCHARS.put("'i", "\\u"); // "iacute"
		// RTFCHARS.put("^i", "\\u"); // "icirc" exists in standard
		// codepage
		// RTFCHARS.put("\"i", "\\u"); // "iuml" exists in standard
		// codepage
		RTFCHARS.put("dh", "\\u"); // "eth"
		// RTFCHARS.put("~n", "\\u"); // "ntilde" exists in standard
		// codepage
		// RTFCHARS.put("`o", "\\u"); // "ograve" exists in standard
		// codepage
		RTFCHARS.put("'o", "\\u"); // "oacute"
		// RTFCHARS.put("^o", "\\u"); // "ocirc" exists in standard
		// codepage
		RTFCHARS.put("~o", "\\u"); // "otilde"
		// RTFCHARS.put("\"o", "\\u"); // "ouml" exists in standard
		// codepage
		// According to ISO 8859-1 the "\div" symbol should be placed here
		// (#247).
		// Omitting this, because it is a mathematical symbol.
		RTFCHARS.put("o", "\\u"); // "oslash"
		// RTFCHARS.put("`u", "\\u"); // "ugrave" exists in standard
		// codepage
		RTFCHARS.put("'u", "\\u"); // "uacute"
		// RTFCHARS.put("^u", "\\u"); // "ucirc" exists in standard
		// codepage
		// RTFCHARS.put("\"u", "\\u"); // "uuml" exists in standard
		// codepage
		RTFCHARS.put("'y", "\\u"); // "yacute"
		RTFCHARS.put("th", "{\\u\\u}"); // "thorn"
		RTFCHARS.put("\"y", "\\u"); // "yuml"

		RTFCHARS.put("=A", "\\u"); // "Amacr"
		RTFCHARS.put("=a", "\\u"); // "amacr"
		RTFCHARS.put("uA", "\\u"); // "Abreve"
		RTFCHARS.put("ua", "\\u"); // "abreve"
		RTFCHARS.put("kA", "\\u"); // "Aogon"
		RTFCHARS.put("ka", "\\u"); // "aogon"
		RTFCHARS.put("'C", "\\u"); // "Cacute"
		RTFCHARS.put("'c", "\\u"); // "cacute"
		RTFCHARS.put("^C", "\\u"); // "Ccirc"
		RTFCHARS.put("^c", "\\u"); // "ccirc"
		RTFCHARS.put(".C", "\\u"); // "Cdot"
		RTFCHARS.put(".c", "\\u"); // "cdot"
		RTFCHARS.put("vC", "\\u"); // "Ccaron"
		RTFCHARS.put("vc", "\\u"); // "ccaron"
		RTFCHARS.put("vD", "\\u"); // "Dcaron"
		// Symbol #271 (d´) has no special Latex command
		RTFCHARS.put("DJ", "\\u"); // "Dstrok"
		RTFCHARS.put("dj", "\\u"); // "dstrok"
		RTFCHARS.put("=E", "\\u"); // "Emacr"
		RTFCHARS.put("=e", "\\u"); // "emacr"
		RTFCHARS.put("uE", "\\u"); // "Ebreve"
		RTFCHARS.put("ue", "\\u"); // "ebreve"
		RTFCHARS.put(".E", "\\u"); // "Edot"
		RTFCHARS.put(".e", "\\u"); // "edot"
		RTFCHARS.put("kE", "\\u"); // "Eogon"
		RTFCHARS.put("ke", "\\u"); // "eogon"
		RTFCHARS.put("vE", "\\u"); // "Ecaron"
		RTFCHARS.put("ve", "\\u"); // "ecaron"
		RTFCHARS.put("^G", "\\u"); // "Gcirc"
		RTFCHARS.put("^g", "\\u"); // "gcirc"
		RTFCHARS.put("uG", "\\u"); // "Gbreve"
		RTFCHARS.put("ug", "\\u"); // "gbreve"
		RTFCHARS.put(".G", "\\u"); // "Gdot"
		RTFCHARS.put(".g", "\\u"); // "gdot"
		RTFCHARS.put("cG", "\\u"); // "Gcedil"
		RTFCHARS.put("'g", "\\u"); // "gacute"
		RTFCHARS.put("^H", "\\u"); // "Hcirc"
		RTFCHARS.put("^h", "\\u"); // "hcirc"
		RTFCHARS.put("Hstrok", "\\u"); // "Hstrok"
		RTFCHARS.put("hstrok", "\\u"); // "hstrok"
		RTFCHARS.put("~I", "\\u"); // "Itilde"
		RTFCHARS.put("~i", "\\u"); // "itilde"
		RTFCHARS.put("=I", "\\u"); // "Imacr"
		RTFCHARS.put("=i", "\\u"); // "imacr"
		RTFCHARS.put("uI", "\\u"); // "Ibreve"
		RTFCHARS.put("ui", "\\u"); // "ibreve"
		RTFCHARS.put("kI", "\\u"); // "Iogon"
		RTFCHARS.put("ki", "\\u"); // "iogon"
		RTFCHARS.put(".I", "\\u"); // "Idot"
		RTFCHARS.put("i", "\\u"); // "inodot"
		// Symbol #306 (IJ) has no special Latex command
		// Symbol #307 (ij) has no special Latex command
		RTFCHARS.put("^J", "\\u"); // "Jcirc"
		RTFCHARS.put("^j", "\\u"); // "jcirc"
		RTFCHARS.put("cK", "\\u"); // "Kcedil"
		RTFCHARS.put("ck", "\\u"); // "kcedil"
		// Symbol #312 (k) has no special Latex command
		RTFCHARS.put("'L", "\\u"); // "Lacute"
		RTFCHARS.put("'l", "\\u"); // "lacute"
		RTFCHARS.put("cL", "\\u"); // "Lcedil"
		RTFCHARS.put("cl", "\\u"); // "lcedil"
		// Symbol #317 (L´) has no special Latex command
		// Symbol #318 (l´) has no special Latex command
		RTFCHARS.put("Lmidot", "\\u"); // "Lmidot"
		RTFCHARS.put("lmidot", "\\u"); // "lmidot"
		RTFCHARS.put("L", "\\u"); // "Lstrok"
		RTFCHARS.put("l", "\\u"); // "lstrok"
		RTFCHARS.put("'N", "\\u"); // "Nacute"
		RTFCHARS.put("'n", "\\u"); // "nacute"
		RTFCHARS.put("cN", "\\u"); // "Ncedil"
		RTFCHARS.put("cn", "\\u"); // "ncedil"
		RTFCHARS.put("vN", "\\u"); // "Ncaron"
		RTFCHARS.put("vn", "\\u"); // "ncaron"
		// Symbol #329 (´n) has no special Latex command
		RTFCHARS.put("NG", "\\u"); // "ENG"
		RTFCHARS.put("ng", "\\u"); // "eng"
		RTFCHARS.put("=O", "\\u"); // "Omacr"
		RTFCHARS.put("=o", "\\u"); // "omacr"
		RTFCHARS.put("uO", "\\u"); // "Obreve"
		RTFCHARS.put("uo", "\\u"); // "obreve"
		RTFCHARS.put("HO", "\\u"); // "Odblac"
		RTFCHARS.put("Ho", "\\u"); // "odblac"
		RTFCHARS.put("OE", "{\\u\\u}"); // "OElig"
		RTFCHARS.put("oe", "{\\u\\u}"); // "oelig"
		RTFCHARS.put("'R", "\\u"); // "Racute"
		RTFCHARS.put("'r", "\\u"); // "racute"
		RTFCHARS.put("cR", "\\u"); // "Rcedil"
		RTFCHARS.put("cr", "\\u"); // "rcedil"
		RTFCHARS.put("vR", "\\u"); // "Rcaron"
		RTFCHARS.put("vr", "\\u"); // "rcaron"
		RTFCHARS.put("'S", "\\u"); // "Sacute"
		RTFCHARS.put("'s", "\\u"); // "sacute"
		RTFCHARS.put("^S", "\\u"); // "Scirc"
		RTFCHARS.put("^s", "\\u"); // "scirc"
		RTFCHARS.put("cS", "\\u"); // "Scedil"
		RTFCHARS.put("cs", "\\u"); // "scedil"
		RTFCHARS.put("vS", "\\u"); // "Scaron"
		RTFCHARS.put("vs", "\\u"); // "scaron"
		RTFCHARS.put("cT", "\\u"); // "Tcedil"
		RTFCHARS.put("ct", "\\u"); // "tcedil"
		RTFCHARS.put("vT", "\\u"); // "Tcaron"
		// Symbol #357 (t´) has no special Latex command
		RTFCHARS.put("Tstrok", "\\u"); // "Tstrok"
		RTFCHARS.put("tstrok", "\\u"); // "tstrok"
		RTFCHARS.put("~U", "\\u"); // "Utilde"
		RTFCHARS.put("~u", "\\u"); // "utilde"
		RTFCHARS.put("=U", "\\u"); // "Umacr"
		RTFCHARS.put("=u", "\\u"); // "umacr"
		RTFCHARS.put("uU", "\\u"); // "Ubreve"
		RTFCHARS.put("uu", "\\u"); // "ubreve"
		RTFCHARS.put("rU", "\\u"); // "Uring"
		RTFCHARS.put("ru", "\\u"); // "uring"
		RTFCHARS.put("HU", "\\u"); // "Odblac"
		RTFCHARS.put("Hu", "\\u"); // "odblac"
		RTFCHARS.put("kU", "\\u"); // "Uogon"
		RTFCHARS.put("ku", "\\u"); // "uogon"
		RTFCHARS.put("^W", "\\u"); // "Wcirc"
		RTFCHARS.put("^w", "\\u"); // "wcirc"
		RTFCHARS.put("^Y", "\\u"); // "Ycirc"
		RTFCHARS.put("^y", "\\u"); // "ycirc"
		RTFCHARS.put("\"Y","\\u"); // "Yuml"
		RTFCHARS.put("'Z", "\\u"); // "Zacute"
		RTFCHARS.put("'z", "\\u"); // "zacute"
		RTFCHARS.put(".Z", "\\u"); // "Zdot"
		RTFCHARS.put(".z", "\\u"); // "zdot"
		RTFCHARS.put("vZ", "\\u"); // "Zcaron"
		RTFCHARS.put("vz", "\\u"); // "zcaron"
		// Symbol #383 (f) has no special Latex command

		// XML_CHARS.put("\\u", "&#x00E1;");
	}

	public static void initializeJournalNames() {
		journalAbbrev = new JournalAbbreviations();// "/resource/journalList.txt");

		// Read external lists, if any (in reverse order, so the upper lists
		// override the lower):
		String[] lists = prefs.getStringArray("externalJournalLists");
		if ((lists != null) && (lists.length > 0)) {
			for (int i = lists.length - 1; i >= 0; i--) {
				try {
					journalAbbrev.readJournalList(new File(lists[i]));
				} catch (FileNotFoundException e) {
					// The file couldn't be found... should we tell anyone?
					Globals.logger(e.getMessage());
				}
			}
		}

		// Read personal list, if set up:
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
