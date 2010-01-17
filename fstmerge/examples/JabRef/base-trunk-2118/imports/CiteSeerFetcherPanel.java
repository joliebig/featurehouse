package net.sf.jabref.imports;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Hashtable;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexEntryType;
import net.sf.jabref.BibtexFields;
import net.sf.jabref.GUIGlobals;
import net.sf.jabref.Globals;
import net.sf.jabref.HelpAction;
import net.sf.jabref.SidePaneComponent;
import net.sf.jabref.SidePaneManager;
import net.sf.jabref.Util;
import net.sf.jabref.gui.ImportInspectionDialog;
import net.sf.jabref.undo.NamedCompound;



public class CiteSeerFetcherPanel extends SidePaneComponent implements ActionListener {


    String idList;
    JTextField tf = new JTextField();
    JPanel pan = new JPanel();
    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints con = new GridBagConstraints();
    CiteSeerFetcher citeSeerFetcher;
    AuthorDialog authorDialog;
    JFrame jFrame; 
    JButton go = new JButton(Globals.lang("Fetch")),
    helpBut = new JButton(GUIGlobals.getImage("helpSmall"));
    HelpAction help;
    CiteSeerFetcherPanel ths = this;

    public CiteSeerFetcherPanel(SidePaneManager p0, final CiteSeerFetcher fetcher) {
        super(p0, GUIGlobals.getIconUrl("citeseer"), Globals.lang("Fetch CiteSeer"));

        help = new HelpAction(Globals.helpDiag, GUIGlobals.citeSeerHelp, "Help");

        this.citeSeerFetcher = fetcher;
        helpBut.addActionListener(help);
        helpBut.setMargin(new Insets(0, 0, 0, 0));
        tf.setPreferredSize(new Dimension(1,tf.getPreferredSize().height));
        
        
        JPanel main = new JPanel();
        main.setLayout(gbl);
        con.fill = GridBagConstraints.BOTH;
        con.insets = new Insets(0, 0, 2, 0);
        con.gridwidth = GridBagConstraints.REMAINDER;
        con.weightx = 1;
        con.weighty = 0;
        
        
        con.weighty = 1;
        con.insets = new Insets(0, 0, 0, 0);
        
        con.fill = GridBagConstraints.BOTH;
        gbl.setConstraints(tf, con);
        main.add(tf);
        con.weighty = 0;
        con.gridwidth = 1;
        gbl.setConstraints(go, con);
        main.add(go);
        con.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(helpBut, con);
        main.add(helpBut);
        main.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
        add(main, BorderLayout.CENTER);
        go.addActionListener(this);
        tf.addActionListener(this);
    }

    public JTextField getTextField() {
        return tf;
    }

    public void actionPerformed(ActionEvent e) {
        if (citeSeerFetcher.activateImportFetcher()) {
            (new Thread() {

                BibtexEntry entry;

                class Update implements ImportInspectionDialog.CallBack {

                    
                    
                    
                    public void done(int entriesImported) {
                        if (entriesImported > 0)
                            panel.output(Globals.lang("Completed import from CiteSeer."));
                        else
                            panel.output(Globals.lang("No entries imported."));
                    }

                    public void cancelled() {
                        panel.output(Globals.lang("%0 import cancelled.", "CiteSeer"));
                    }

                    public void stopFetching() {
                    }

                    

                }

                public void run() {

                    
                    ImportInspectionDialog diag = null;
                    if (Globals.prefs.getBoolean("useImportInspectionDialog")) {
                        diag = new ImportInspectionDialog(panel.frame(), panel,
                            BibtexFields.DEFAULT_INSPECTION_FIELDS, Globals.lang("Fetch CiteSeer"), false);
                        diag.addCallBack(new Update());
                        Util.placeDialog(diag, panel.frame());
                        diag.setVisible(true);
                        diag.setProgress(0, 1);
                    }
                    NamedCompound undoEdit =
                            new NamedCompound(Globals.lang("CiteSeer import entries")),
                            
                            
                            dummyCompound = new NamedCompound(Globals.lang("Ok"));
                    BooleanAssign overwriteAll = new BooleanAssign(true),
                            overwriteNone = new BooleanAssign(false),
                            newValue = new BooleanAssign(false);
                    Hashtable rejectedEntries = new Hashtable();
                    String text = tf.getText().replaceAll(",", ";");
                    String[] ids = text.split(";");
                    BibtexEntry[] entries = new BibtexEntry[ids.length];
                    citeSeerFetcher.activateImportFetcher();

                    for (int i = 0; i < entries.length; i++) {
                        
                        entries[i] = new BibtexEntry(Util.createNeutralId(), BibtexEntryType.getType("article"));
                        
                        entries[i].setField("citeseerurl", ids[i].trim());
                        
                        boolean newValues = citeSeerFetcher.importCiteSeerEntry
                                (entries[i], dummyCompound, overwriteAll, overwriteNone,
                                        newValue, rejectedEntries);

                        
                        if (diag != null) {
                            diag.addEntry(entries[i]);
                            diag.setProgress(i+1, entries.length);
                        } else {
                            
                        }
                    }
                    citeSeerFetcher.deactivateImportFetcher();

                    if (diag != null) {
                        
                        diag.entryListComplete();
                    } else {
                        panel.frame().addBibEntries(Arrays.asList(entries), null, false);
                        (new Update()).done(entries.length);
                    }
                }
            }).start();
        } else {
            JOptionPane.showMessageDialog(panel.frame(),
                    Globals.lang("A CiteSeer import operation is currently in progress.") + "  " +
                    Globals.lang("Please wait until it has finished."),
                    Globals.lang("CiteSeer Import Error"),
                    JOptionPane.WARNING_MESSAGE);
        }
    }
}

