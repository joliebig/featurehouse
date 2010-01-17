import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.InputMap;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;

class Foo {
    public void openWindow(Vector loaded) {
        if (!graphicFailure && !disableGui.isInvoked()) {
            // Call the method performCompatibilityUpdate(), which does any
            // necessary changes for users with a preference set from an older
            // Jabref version.
            Util.performCompatibilityUpdate();


            // Set up custom or default icon theme:
            GUIGlobals.setUpIconTheme();

            // TODO: remove temporary registering of external file types?
            Globals.prefs.updateExternalFileTypes();

           // This property is set to make the Mac OSX Java VM move the menu bar to
            // the top
            // of the screen, where Mac users expect it to be.
            System.setProperty("apple.laf.useScreenMenuBar", "true");

            // Set antialiasing on everywhere. This only works in JRE >= 1.5.
            // Or... it doesn't work, period.
            //System.setProperty("swing.aatext", "true");
            // If we are not on Mac, deal with font sizes and LookAndFeels:
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

                // Set font sizes if we are using a JGoodies look and feel.
                if ((lnf != null) && (lnf instanceof Plastic3DLookAndFeel)) {

                    //UIManager.put("jgoodies.popupDropShadowEnabled", Boolean.TRUE);
                    MetalLookAndFeel.setCurrentTheme(new
                     com.jgoodies.looks.plastic.theme.SkyBluer());

                    // Set a "model" icon size, so menu items are evenly spaced even though
                    // only some items have icons. We load an arbitrary icon and look at
                    // its size to determine what size to use:
                    int defaultIconSize = GUIGlobals.getImage("open").getIconWidth();
                    com.jgoodies.looks.Options.setDefaultIconSize
                            (new Dimension(defaultIconSize, defaultIconSize));


                    if (overrideDefaultFonts) {
                        FontSet fontSet = FontSets.createDefaultFontSet(
                            new Font("Tahoma", Font.PLAIN, fontSizes),    // control font
                            new Font("Tahoma", Font.PLAIN, fontSizes),    // menu font
                            new Font("Tahoma", Font.BOLD, fontSizes)     // title font
                            );
                        FontPolicy fixedPolicy = FontPolicies.createFixedPolicy(fontSet);
                        Plastic3DLookAndFeel.setFontPolicy(fixedPolicy);
                    }

                    //Plastic3DLookAndFeel plLnf = (Plastic3DLookAndFeel) lnf;
                }
                else if ((lnf != null) && (lnf instanceof WindowsLookAndFeel)) {

                    // Set a "model" icon size, so menu items are evenly spaced even though
                    // only some items have icons. We load an arbitrary icon and look at
                    // its size to determine what size to use:
                    int defaultIconSize = GUIGlobals.getImage("open").getIconWidth();
                    com.jgoodies.looks.Options.setDefaultIconSize
                        (new Dimension(defaultIconSize, defaultIconSize));

                    if (overrideDefaultFonts) {
                        FontSet fontSet = FontSets.createDefaultFontSet(
                            new Font("Tahoma", Font.PLAIN, fontSizes),    // control font
                            new Font("Tahoma", Font.PLAIN, fontSizes),    // menu font
                            new Font("Tahoma", Font.BOLD, fontSizes)     // title font
                            );
                        FontPolicy fixedPolicy = FontPolicies.createFixedPolicy(fontSet);
                        WindowsLookAndFeel.setFontPolicy(fixedPolicy);
                    }

                    //WindowsLookAndFeel plLnf = (WindowsLookAndFeel) lnf;
                }

                if (lnf != null) {
                    try {

                        UIManager.setLookAndFeel(lnf);

                        if (!Globals.ON_WIN) {
                            UIManager.put("SimpleInternalFrame.activeTitleBackground", GUIGlobals.gradientBlue);
                        }

                        if (!Globals.ON_WIN && !Globals.ON_MAC) {
                            // For Linux, add Enter as button click key:
                            UIDefaults def = UIManager.getDefaults();
                            InputMap im = (InputMap)def.get("Button.focusInputMap");
                            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false), "pressed");
                            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true), "released");
                        }
                    } catch (Throwable ex) {
                        ex.printStackTrace();
                        System.err.println("Trying to set system default Look&Feel...");

                        // if desired lnf could not be set, try system default
                        try {
                            UIManager.setLookAndFeel(UIManager
                                .getSystemLookAndFeelClassName());
                        } catch (Throwable e) {
                            e.printStackTrace();
                        }
                    }

                    //LookAndFeel lnf = new com.sun.java.swing.plaf.gtk.GTKLookAndFeel();
                    //Look1AndFeel lnf = new
                    // com.incors.plaf.kunststoff.KunststoffLookAndFeel();
                    //com.incors.plaf.kunststoff.KunststoffLookAndFeel.setCurrentTheme(new
                    // com.incors.plaf.kunststoff.themes.KunststoffDesktopTheme());
                }

            }


            // If the option is enabled, open the last edited databases, if any.
            if (!blank.isInvoked() && Globals.prefs.getBoolean("openLastEdited") && (Globals.prefs.get("lastEdited") != null)) {
                // How to handle errors in the databases to open?
                String[] names = Globals.prefs.getStringArray("lastEdited");
lastEdLoop: 
                for (int i = 0; i < names.length; i++) {
                    File fileToOpen = new File(names[i]);

                    for (int j = 0; j < loaded.size(); j++) {
                        ParserResult pr = (ParserResult) loaded.elementAt(j);

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

            //Util.pr(": Initializing frame");
            jrf = new JabRefFrame();

            // Add all loaded databases to the frame:
	    boolean first = true;
            if (loaded.size() > 0) {
                for (Iterator i=loaded.iterator(); i.hasNext();) {
                    ParserResult pr = (ParserResult)i.next();
		            BasePanel panel = jrf.addTab(pr.getDatabase(), pr.getFile(),
                            pr.getMetaData(), pr.getEncoding(), first);
                    first = false;
                }
            }

            if (loadSess.isInvoked())
                jrf.loadSessionAction.actionPerformed(new java.awt.event.ActionEvent(
                        jrf, 0, ""));

            if (splashScreen != null) {// do this only if splashscreen was actually created
                splashScreen.dispose();
                splashScreen = null;
            }

            //Util.pr(": Showing frame");
            jrf.setVisible(true);

            // TEST TEST TEST TEST TEST TEST
            startOOPlugin(jrf);

            for (int i = 0; i < loaded.size(); i++) {
                ParserResult pr = (ParserResult) loaded.elementAt(i);
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

            // After adding the databases, go through each and see if
            // any post open actions need to be done. For instance, checking
            // if we found new entry types that can be imported, or checking
            // if the database contents should be modified due to new features
            // in this version of JabRef:
            for (int i = 0; i < loaded.size(); i++) {
                ParserResult pr = (ParserResult) loaded.elementAt(i);
                BasePanel panel = jrf.baseAt(i);
                OpenDatabaseAction.performPostOpenActions(panel, pr, true);
            }

            //Util.pr(": Finished adding panels");

            if (loaded.size() > 0) {
                jrf.tabbedPane.setSelectedIndex(0);
                new FocusRequester(((BasePanel) jrf.tabbedPane.getComponentAt(0)).mainTable);
            }
        } else
            System.exit(0);
    }
}