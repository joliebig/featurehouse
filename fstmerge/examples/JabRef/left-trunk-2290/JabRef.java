
package net.sf.jabref;

import gnu.dtools.ritopt.BooleanOption;
import gnu.dtools.ritopt.Options;
import gnu.dtools.ritopt.StringOption;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Vector;

import javax.swing.*;
import javax.swing.plaf.metal.MetalLookAndFeel;

import net.sf.jabref.export.*;
import net.sf.jabref.imports.OpenDatabaseAction;
import net.sf.jabref.imports.ParserResult;
import net.sf.jabref.remote.RemoteListener;
import net.sf.jabref.util.Pair;
import net.sf.jabref.wizard.auximport.AuxCommandLine;

import com.jgoodies.looks.FontPolicies;
import com.jgoodies.looks.FontPolicy;
import com.jgoodies.looks.FontSet;
import com.jgoodies.looks.FontSets;
import com.jgoodies.looks.plastic.Plastic3DLookAndFeel;
import com.jgoodies.looks.windows.WindowsLookAndFeel;


public class JabRef {
    
	public static JabRef singleton;
    public static RemoteListener remoteListener = null;
    public JabRefFrame jrf;
    public Options options;
    public Frame splashScreen = null;

    boolean graphicFailure = false;

    StringOption importFile, exportFile, exportPrefs, importPrefs, auxImExport, importToOpenBase;
    BooleanOption helpO, disableGui, blank, loadSess, showVersion, disableSplash;
    
    public static void main(String[] args) {
        new JabRef(args);
    }

    protected JabRef(String[] args) {

		singleton = this;

		
		
		System.setProperty("java.net.useSystemProxies", "true");
		System.getProperties().put("proxySet", "true");

		JabRefPreferences prefs = JabRefPreferences.getInstance();
		Globals.prefs = prefs;
		Globals.setLanguage(prefs.get("language"), "");

		
		
		
		Globals.importFormatReader.resetImportFormats();
		BibtexEntryType.loadCustomEntryTypes(prefs);
		ExportFormats.initAllExports();
		
		
		Globals.initializeJournalNames();

		
		if (Globals.prefs.getBoolean("useRemoteServer")) {
			remoteListener = RemoteListener.openRemoteListener(this);

			if (remoteListener == null) {
				
				if (RemoteListener.sendToActiveJabRefInstance(args)) {

					
					System.out
							.println(Globals
									.lang("Arguments passed on to running JabRef instance. Shutting down."));
					System.exit(0);
				}
			} else {
				
				
				remoteListener.start();
			}
		}

		
		String personalJournalList = prefs.get("personalJournalList");
		if (personalJournalList != null) {
			try {
				Globals.journalAbbrev.readJournalList(new File(
						personalJournalList));
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
		}

		
		
		openWindow(processArguments(args, true));
	}

    private void setupOptions() {

        importFile = new StringOption("");
        exportFile = new StringOption("");
        helpO = new BooleanOption();
        disableGui = new BooleanOption();
        disableSplash = new BooleanOption();
        blank = new BooleanOption();
        loadSess = new BooleanOption();
        showVersion = new BooleanOption();
        exportPrefs = new StringOption("jabref_prefs.xml");
        importPrefs = new StringOption("jabref_prefs.xml");
        auxImExport = new StringOption("");
        importToOpenBase = new StringOption("");

        options = new Options("JabRef "); 
        options.setVersion(GUIGlobals.version);

        importFile.setDescription("imopoepuoeu"); 
        options.register("version", 'v',
                Globals.lang("Display version"), showVersion);
        options.register("nogui", 'n',
            Globals.lang("No GUI. Only process command line options."), disableGui);
        options.register("nosplash", 's',
                Globals.lang("Do not show splash window at startup"), disableSplash);
        options.register("import", 'i',
            Globals.lang("Import file") + ": " + Globals.lang("filename")
            + "[,import format]", importFile);
        options.register("output", 'o',
            Globals.lang("Output or export file") + ": " + Globals.lang("filename")
            + "[,export format]", exportFile);
        options.register("help", 'h',
            Globals.lang("Display help on command line options"), helpO);
        options.register("loads", 'l', Globals.lang("Load session"), loadSess);
        options.register("prexp", 'x', Globals.lang("Export preferences to file"),
            exportPrefs);
        options.register("primp", 'p', Globals.lang("Import preferences from file"),
            importPrefs);
        options.register("aux", 'a',
            Globals.lang("Subdatabase from aux") + ": " + Globals.lang("file")+"[.aux]" + ","+Globals.lang("new")+"[.bib]",
            auxImExport);
        options.register("blank", 'b', Globals.lang("Do not open any files at startup"), blank);

        options.register("importToOpen", '\0', Globals.lang("Import to open tab"), importToOpenBase);

        options.setUseMenu(false);
    }

    public Vector<ParserResult> processArguments(String[] args, boolean initialStartup) {

        setupOptions();
        String[] leftOver = options.process(args);

        if (initialStartup && showVersion.isInvoked()) {
            options.displayVersion();
            disableGui.setInvoked(true);
        }

        if (initialStartup && helpO.isInvoked()) {
            System.out.println("jabref [options] [bibtex-file]\n");
            System.out.println(options.getHelp());

            String importFormats = Globals.importFormatReader.getImportFormatList();
            System.out.println(Globals.lang("Available import formats") + ":\n"
                + importFormats);

            String outFormats = ExportFormats.getConsoleExportList(70, 20, "\t");
            System.out.println(Globals.lang("Available export formats") + ": " + outFormats
                + ".");
            System.exit(0);
        }

        
        
        
        
        if (initialStartup && !disableGui.isInvoked() && !disableSplash.isInvoked()) {
            try {
                splashScreen = SplashScreen.splash();
            } catch (Throwable ex) {
                graphicFailure = true;
                System.err.println(Globals.lang("Unable to create graphical interface")
                    + ".");
            }
        }

        
        Vector<ParserResult> loaded = new Vector<ParserResult>();
        Vector<String> toImport = new Vector<String>();
        if (!blank.isInvoked() && (leftOver.length > 0))  {
            for (int i = 0; i < leftOver.length; i++) {
                
                
                
                boolean bibExtension = leftOver[i].toLowerCase().endsWith("bib");
                ParserResult pr = null;
                if (bibExtension)
                    pr = openBibFile(leftOver[i]);

                if ((pr == null) || (pr == ParserResult.INVALID_FORMAT)) {
                    
                    
                    
                    
                    
                    
                    if (initialStartup) {
                        toImport.add(leftOver[i]);
                    } else {
                        ParserResult res = importToOpenBase(leftOver[i]);
                        if (res != null)
                            loaded.add(res);
                    }
                }
                else
                    loaded.add(pr);

            }
        }

        if (!blank.isInvoked() && importFile.isInvoked()) {
            toImport.add(importFile.getStringValue());
        }

        for (String filenameString : toImport) {
			ParserResult pr = importFile(filenameString);
			if (pr != null)
				loaded.add(pr);
		}

        if (!blank.isInvoked() && importToOpenBase.isInvoked()) {
            ParserResult res = importToOpenBase(importToOpenBase.getStringValue());
            if (res != null)
                loaded.add(res);
        }

        if (exportFile.isInvoked()) {
            if (loaded.size() > 0) {
                String[] data = exportFile.getStringValue().split(",");

                if (data.length == 1) {
                    
                    
                    if (loaded.size() > 0) {
                        ParserResult pr =
                            loaded.elementAt(loaded.size() - 1);

                        try {
                            System.out.println(Globals.lang("Saving") + ": " + data[0]);
                            SaveSession session = FileActions.saveDatabase(pr.getDatabase(),
                                new MetaData(pr.getMetaData(),pr.getDatabase()), new File(data[0]), Globals.prefs,
                                false, false, Globals.prefs.get("defaultEncoding"));
                            
                            if (!session.getWriter().couldEncodeAll())
                                System.err.println(Globals.lang("Warning")+": "+
                                    Globals.lang("The chosen encoding '%0' could not encode the following characters: ",
                                    session.getEncoding())+session.getWriter().getProblemCharacters());
                            session.commit();
                        } catch (SaveException ex) {
                            System.err.println(Globals.lang("Could not save file") + " '"
                                + data[0] + "': " + ex.getMessage());
                        }
                    } else
                        System.err.println(Globals.lang(
                                "The output option depends on a valid import option."));
                } else if (data.length == 2) {
                    
                    
                    ParserResult pr = loaded.elementAt(loaded.size() - 1);
                    System.out.println(Globals.lang("Exporting") + ": " + data[0]);
                    IExportFormat format = ExportFormats.getExportFormat(data[1]);
                    if (format != null) {
                        
                        try {
                            format.performExport(pr.getDatabase(), data[0], pr.getEncoding(), null);
                        } catch (Exception ex) {
                            System.err.println(Globals.lang("Could not export file")
                                + " '" + data[0] + "': " + ex.getMessage());
                        }
                    }
                    else
                        System.err.println(Globals.lang("Unknown export format")
                                + ": " + data[1]);

                }
            } else
                System.err.println(Globals.lang(
                        "The output option depends on a valid import option."));
        }

        

        if (exportPrefs.isInvoked()) {
            try {
                Globals.prefs.exportPreferences(exportPrefs.getStringValue());
            } catch (IOException ex) {
                Util.pr(ex.getMessage());
            }
        }


	if (importPrefs.isInvoked()) {
	    try {
		Globals.prefs.importPreferences(importPrefs.getStringValue());
		BibtexEntryType.loadCustomEntryTypes(Globals.prefs);
	    }
	    catch (IOException ex) {
		Util.pr(ex.getMessage());
	    }
	}

        if (!blank.isInvoked() && auxImExport.isInvoked()) {
            boolean usageMsg = false;

            if (loaded.size() > 0) 
             {
                String[] data = auxImExport.getStringValue().split(",");

                if (data.length == 2) {
                    ParserResult pr = loaded.firstElement();
                    AuxCommandLine acl = new AuxCommandLine(data[0], pr.getDatabase());
                    BibtexDatabase newBase = acl.perform();

                    boolean notSavedMsg = false;

                    
                    if (newBase != null) {
                        if (newBase.getEntryCount() > 0) {
                            String subName = Util.getCorrectFileName(data[1], "bib");

                            try {
                                System.out.println(Globals.lang("Saving") + ": "
                                    + subName);
                                SaveSession session = FileActions.saveDatabase(newBase, new MetaData(), 
                                    new File(subName), Globals.prefs, false, false,
                                    Globals.prefs.get("defaultEncoding"));
                                
                                if (!session.getWriter().couldEncodeAll())
                                    System.err.println(Globals.lang("Warning")+": "+
                                        Globals.lang("The chosen encoding '%0' could not encode the following characters: ",
                                        session.getEncoding())+session.getWriter().getProblemCharacters());
                                session.commit();
                            } catch (SaveException ex) {
                                System.err.println(Globals.lang("Could not save file")
                                    + " '" + subName + "': " + ex.getMessage());
                            }

                            notSavedMsg = true;
                        }
                    }

                    if (!notSavedMsg)
                        System.out.println(Globals.lang("no database generated"));
                } else
                    usageMsg = true;
            } else
                usageMsg = true;

            if (usageMsg) {
                System.out.println(Globals.lang("no base-bibtex-file specified"));
                System.out.println(Globals.lang("usage") + " :");
                System.out.println(
                    "jabref --aux infile[.aux],outfile[.bib] base-bibtex-file");
            }
        }

        return loaded;
    }

    public void openWindow(Vector<ParserResult> loaded) {
        if (!graphicFailure && !disableGui.isInvoked()) {
            
            
            
            Util.performCompatibilityUpdate();


            
            GUIGlobals.setUpIconTheme();

            
            Globals.prefs.updateExternalFileTypes();

           
            
            
            System.setProperty("apple.laf.useScreenMenuBar", "true");

            
            
            
            
            if (!Globals.ON_MAC) {
                int fontSizes = Globals.prefs.getInt("menuFontSize");
                boolean overrideDefaultFonts = Globals.prefs.getBoolean("overrideDefaultFonts");
                String defaultLookAndFeel;

                if (Globals.ON_WIN)
                    defaultLookAndFeel = GUIGlobals.windowsDefaultLookAndFeel;
                else
                    defaultLookAndFeel = GUIGlobals.linuxDefaultLookAndFeel;

                String lookAndFeel = null;

                if (!Globals.prefs.getBoolean("useDefaultLookAndFeel"))
                    lookAndFeel = Globals.prefs.get("lookAndFeel");
                else
                    lookAndFeel = defaultLookAndFeel;

                LookAndFeel lnf = null;
                Object objLnf = null;


                try {
                    if (lookAndFeel != null)
                        objLnf = Class.forName(lookAndFeel).newInstance();
                    else
                        objLnf = Class.forName(defaultLookAndFeel).newInstance();
                } catch (Exception ex) {
                    ex.printStackTrace();

                    try {
                        objLnf = Class.forName(defaultLookAndFeel).newInstance();
                    } catch (Exception ex2) {
                    }
                }

                if (objLnf != null)
                    lnf = (LookAndFeel) objLnf;

                
                if ((lnf != null) && (lnf instanceof Plastic3DLookAndFeel)) {

                    
                    MetalLookAndFeel.setCurrentTheme(new
                     com.jgoodies.looks.plastic.theme.SkyBluer());

                    
                    
                    
                    int defaultIconSize = GUIGlobals.getImage("open").getIconWidth();
                    com.jgoodies.looks.Options.setDefaultIconSize
                            (new Dimension(defaultIconSize, defaultIconSize));


                    if (overrideDefaultFonts) {
                        FontSet fontSet = FontSets.createDefaultFontSet(
                            new Font("Tahoma", Font.PLAIN, fontSizes),    
                            new Font("Tahoma", Font.PLAIN, fontSizes),    
                            new Font("Tahoma", Font.BOLD, fontSizes)     
                            );
                        FontPolicy fixedPolicy = FontPolicies.createFixedPolicy(fontSet);
                        Plastic3DLookAndFeel.setFontPolicy(fixedPolicy);
                    }

                    
                }
                else if ((lnf != null) && (lnf instanceof WindowsLookAndFeel)) {

                    
                    
                    
                    int defaultIconSize = GUIGlobals.getImage("open").getIconWidth();
                    com.jgoodies.looks.Options.setDefaultIconSize
                        (new Dimension(defaultIconSize, defaultIconSize));

                    if (overrideDefaultFonts) {
                        FontSet fontSet = FontSets.createDefaultFontSet(
                            new Font("Tahoma", Font.PLAIN, fontSizes),    
                            new Font("Tahoma", Font.PLAIN, fontSizes),    
                            new Font("Tahoma", Font.BOLD, fontSizes)     
                            );
                        FontPolicy fixedPolicy = FontPolicies.createFixedPolicy(fontSet);
                        WindowsLookAndFeel.setFontPolicy(fixedPolicy);
                    }

                    
                }

                if (lnf != null) {
                    try {

                        UIManager.setLookAndFeel(lnf);

                        if (!Globals.ON_WIN) {
                            UIManager.put("SimpleInternalFrame.activeTitleBackground", GUIGlobals.gradientBlue);
                        }

                        if (!Globals.ON_WIN && !Globals.ON_MAC) {
                            
                            UIDefaults def = UIManager.getDefaults();
                            InputMap im = (InputMap)def.get("Button.focusInputMap");
                            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false), "pressed");
                            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true), "released");
                        }
                    } catch (Throwable ex) {
                        ex.printStackTrace();
                        System.err.println("Trying to set system default Look&Feel...");

                        
                        try {
                            UIManager.setLookAndFeel(UIManager
                                .getSystemLookAndFeelClassName());
                        } catch (Throwable e) {
                            e.printStackTrace();
                        }
                    }

                    
                    
                    
                    
                    
                }

            }


            
            if (!blank.isInvoked() && Globals.prefs.getBoolean("openLastEdited") && (Globals.prefs.get("lastEdited") != null)) {
                
                String[] names = Globals.prefs.getStringArray("lastEdited");
lastEdLoop: 
                for (int i = 0; i < names.length; i++) {
                    File fileToOpen = new File(names[i]);

                    for (int j = 0; j < loaded.size(); j++) {
                        ParserResult pr = loaded.elementAt(j);

                        if ((pr.getFile() != null) &&pr.getFile().equals(fileToOpen))
                            continue lastEdLoop;
                    }

                    if (fileToOpen.exists()) {
                        ParserResult pr = openBibFile(names[i]);

                        if (pr != null) {

			    if (pr == ParserResult.INVALID_FORMAT) {
				System.out.println(Globals.lang("Error opening file")+" '"+fileToOpen.getPath()+"'");
			    }
			    else
				loaded.add(pr);

			}
                    }
                }
            }

            GUIGlobals.init();
            GUIGlobals.CURRENTFONT =
                new Font(Globals.prefs.get("fontFamily"), Globals.prefs.getInt("fontStyle"),
                    Globals.prefs.getInt("fontSize"));

            
            jrf = new JabRefFrame();

            
	    boolean first = true;
            if (loaded.size() > 0) {
                for (ParserResult pr : loaded){
		            jrf.addTab(pr.getDatabase(), pr.getFile(),
                            pr.getMetaData(), pr.getEncoding(), first);
                    first = false;
                }
            }

            if (loadSess.isInvoked())
                jrf.loadSessionAction.actionPerformed(new java.awt.event.ActionEvent(
                        jrf, 0, ""));

            if (splashScreen != null) {
                splashScreen.dispose();
                splashScreen = null;
            }

            
            jrf.setVisible(true);

            
            startOOPlugin(jrf);

            for (int i = 0; i < loaded.size(); i++) {
                ParserResult pr = loaded.elementAt(i);
                if (Globals.prefs.getBoolean("displayKeyWarningDialogAtStartup") && pr.hasWarnings()) {
                    String[] wrns = pr.warnings();
                    StringBuffer wrn = new StringBuffer();
                    for (int j = 0; j<wrns.length; j++)
                        wrn.append(j + 1).append(". ").append(wrns[j]).append("\n");
                    if (wrn.length() > 0)
                        wrn.deleteCharAt(wrn.length() - 1);
                    jrf.showBaseAt(i);
                    JOptionPane.showMessageDialog(jrf, wrn.toString(),
                        Globals.lang("Warnings"),
                        JOptionPane.WARNING_MESSAGE);
                }
            }

            
            
            
            
            
            for (int i = 0; i < loaded.size(); i++) {
                ParserResult pr = loaded.elementAt(i);
                BasePanel panel = jrf.baseAt(i);
                OpenDatabaseAction.performPostOpenActions(panel, pr, true);
            }

            

            if (loaded.size() > 0) {
                jrf.tabbedPane.setSelectedIndex(0);
                new FocusRequester(((BasePanel) jrf.tabbedPane.getComponentAt(0)).mainTable);
            }
        } else
            System.exit(0);
    }

    
    private void startOOPlugin(JabRefFrame jrf) {


        try {
            Class<?> c = Class.forName("net.sf.jabref.oo.OOTestPanel");
            Method m = c.getDeclaredMethod("open",
                    new Class[] {JabRefFrame.class});
            Object i = c.newInstance();
            m.invoke(i, new Object[] {jrf});
        } catch (Exception e) {
            
            
            
        }
    }

    public static ParserResult openBibFile(String name) {
        System.out.println(Globals.lang("Opening") + ": " + name);

        try {
            File file = new File(name);
            String encoding = Globals.prefs.get("defaultEncoding");
            ParserResult pr = OpenDatabaseAction.loadDatabase(file, encoding);
            if (pr == null)
                return ParserResult.INVALID_FORMAT;
            pr.setFile(file);
            if (pr.hasWarnings()) {
                String[] warn = pr.warnings();
                for (int i=0; i<warn.length; i++)
                    System.out.println(Globals.lang("Warning")+": "+warn[i]);

            }
            return pr;
        } catch (Throwable ex) {
            
            
            System.err.println(Globals.lang("Error opening file") + ": "
                + ex.getMessage());
        }

        return null;
    }

    public static ParserResult importFile(String argument){
    	String[] data = argument.split(",");
        try {
            if ((data.length > 1) && !"*".equals(data[1])) {
                System.out.println(Globals.lang("Importing") + ": " + data[0]);
                List<BibtexEntry> entries =
                        Globals.importFormatReader.importFromFile(data[1],
                                data[0].replaceAll("~", System.getProperty("user.home")));
                return new ParserResult(entries);
            } else {
                
                System.out.println(Globals.lang("Importing in unknown format")
                        + ": " + data[0]);
                
            	Pair<String, ParserResult> importResult = 
                    Globals.importFormatReader.importUnknownFormat(data[0]
                            .replaceAll("~", System.getProperty("user.home")));
            	
            	if (importResult != null){
            		System.out.println(Globals.lang("Format used") + ": "
                        + importResult.p);
            		
            		return importResult.v;
            	} else {
            		System.out.println(Globals.lang(
                                "Could not find a suitable import format."));
                }
            }
        } catch (IOException ex) {
            System.err.println(Globals.lang("Error opening file") + " '"
                    + data[0] + "': " + ex.getLocalizedMessage());
        }
        return null;
    }

    
    public static ParserResult importToOpenBase(String argument) {
    	ParserResult result = importFile(argument);
    	
    	if (result != null)
    		result.setToOpenTab(true);
    	
    	return result;
    }
}
