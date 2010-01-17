package net.sf.jabref.external;

import net.sf.jabref.imports.ParserResult;
import net.sf.jabref.imports.PostOpenAction;
import net.sf.jabref.*;
import net.sf.jabref.undo.NamedCompound;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.regex.Pattern;
import java.util.Iterator;

import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;


public class FileLinksUpgradeWarning implements PostOpenAction {

    private static final String[] FIELDS_TO_LOOK_FOR = new String[] {"pdf", "ps"};

    
    public boolean isActionNecessary(ParserResult pr) {
        
        if (!Globals.prefs.getBoolean("showFileLinksUpgradeWarning"))
            return false;
        if (pr.getJabrefMajorVersion() < 0)
            return false; 
        if (pr.getJabrefMajorVersion() < 2)
            return true; 
        if (pr.getJabrefMajorVersion() > 2)
            return false; 
        return (pr.getJabrefMinorVersion() <= 2);
    }

    
    public void performAction(BasePanel panel, ParserResult pr) {
        
        
        boolean offerChangeSettings = !Globals.prefs.getBoolean("fileColumn");
        
        boolean offerChangeDatabase = linksFound(pr.getDatabase(), FIELDS_TO_LOOK_FOR);
        
        boolean offerSetFileDir = !Globals.prefs.hasKey(GUIGlobals.FILE_FIELD+"Directory")
                && (Globals.prefs.hasKey("pdfDirectory") || Globals.prefs.hasKey("psDirectory"));

        if (!offerChangeDatabase && !offerChangeSettings && !offerSetFileDir)
                    return; 
                
        JCheckBox changeSettings = new JCheckBox(Globals.lang("Change table column and General fields settings to use the new feature"),
                offerChangeSettings);
        JCheckBox changeDatabase = new JCheckBox(Globals.lang("Upgrade old external file links to use the new feature"),
                offerChangeDatabase);
        JCheckBox setFileDir = new JCheckBox(Globals.lang("Set main external file directory")+":", offerSetFileDir);
        JTextField fileDir = new JTextField(30);
        JCheckBox doNotShowDialog = new JCheckBox(Globals.lang("Do not show these options in the future"),
                false);

        StringBuilder sb = new StringBuilder("<html>");
        sb.append(Globals.lang("This database was written using an older version of JabRef."));
        sb.append("<br>");
        sb.append(Globals.lang("The current version features a new way of handling links to external files.<br>"
            +"To take advantage of this, your links must be changed into the new format, and<br>"
            +"JabRef must be configured to show the new links."));
        sb.append("<p>");
        sb.append(Globals.lang("Do you want JabRef to do the following operations?"));
        sb.append("</html>");

        JPanel message = new JPanel();
        DefaultFormBuilder b = new DefaultFormBuilder(message,
                new FormLayout("left:pref", ""));
        b.append(new JLabel(sb.toString()));
        b.nextLine();
        if (offerChangeSettings) {
            b.append(changeSettings);
            b.nextLine();
        }
        if (offerChangeDatabase) {
            b.append(changeDatabase);
            b.nextLine();
        }
        if (offerSetFileDir) {
            if (Globals.prefs.hasKey("pdfDirectory"))
                fileDir.setText(Globals.prefs.get("pdfDirectory"));
            else
                fileDir.setText(Globals.prefs.get("psDirectory"));
            JPanel pan = new JPanel();
            pan.add(setFileDir);
            pan.add(fileDir);
            b.append(pan);
            b.nextLine();
        }
        b.append("");
        b.nextLine();
        b.append(doNotShowDialog);

        int answer = JOptionPane.showConfirmDialog(panel.frame(),
                message, Globals.lang("Upgrade file"), JOptionPane.YES_NO_OPTION);
        if (doNotShowDialog.isSelected())
            Globals.prefs.putBoolean("showFileLinksUpgradeWarning", false);

        if (answer == JOptionPane.YES_OPTION)
            makeChanges(panel, pr, changeSettings.isSelected(), changeDatabase.isSelected(),
                    setFileDir.isSelected() ? fileDir.getText() : null);
    }

    
    public boolean linksFound(BibtexDatabase database, String[] fields) {
        for (Iterator iterator = database.getEntries().iterator(); iterator.hasNext();) {
            BibtexEntry entry = (BibtexEntry)iterator.next();
            for (int i = 0; i < fields.length; i++) {
                if (entry.getField(fields[i]) != null)
                    return true;
            }
        }
        return false;
    }

    
    public void makeChanges(BasePanel panel, ParserResult pr, boolean upgradePrefs,
                            boolean upgradeDatabase, String fileDir) {

        if (upgradeDatabase) {
            
            NamedCompound ce = Util.upgradePdfPsToFile(pr.getDatabase(), FIELDS_TO_LOOK_FOR);
            panel.undoManager.addEdit(ce);
            panel.markBaseChanged();
        }

        if (fileDir != null) {
            Globals.prefs.put(GUIGlobals.FILE_FIELD+"Directory", fileDir);
        }

        if (upgradePrefs) {
            
            Globals.prefs.putBoolean("pdfColumn", Boolean.FALSE);
            Globals.prefs.putBoolean("fileColumn", Boolean.TRUE);

            
            String genF = Globals.prefs.get(Globals.prefs.CUSTOM_TAB_FIELDS+"_def0");
            if (!Pattern.compile("\\b"+GUIGlobals.FILE_FIELD+"\\b").matcher(genF)
                .matches()) {

                StringBuilder sb = new StringBuilder(GUIGlobals.FILE_FIELD).append(';').
                    append(genF);
                

                Globals.prefs.put(Globals.prefs.CUSTOM_TAB_FIELDS+"_def0", sb.toString());
                System.out.println(sb.toString());
                Globals.prefs.updateEntryEditorTabList();
                panel.frame().removeCachedEntryEditors();
            }

            

            panel.frame().setupAllTables();
        }
    }

}
