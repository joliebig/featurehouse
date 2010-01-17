package net.sf.jabref.external;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import net.sf.jabref.*;


public class PushToApplicationAction extends AbstractAction implements Runnable {
    private PushToApplication operation;
    private JabRefFrame frame;
    private BasePanel panel;
    private BibtexEntry[] entries;
    
    public PushToApplicationAction(JabRefFrame frame, PushToApplication operation) {
        this.frame = frame;
        putValue(SMALL_ICON, operation.getIcon());
        putValue(NAME, operation.getName());
        putValue(SHORT_DESCRIPTION, operation.getTooltip());
        if (operation.getKeyStrokeName() != null)
            putValue(ACCELERATOR_KEY, Globals.prefs.getKey(operation.getKeyStrokeName()));
        this.operation = operation;
    }

    public void actionPerformed(ActionEvent e) {
        panel = frame.basePanel();

        
        if (panel == null)
            return;

        
        entries = panel.getSelectedEntries();
        if (entries.length == 0) {
            JOptionPane.showMessageDialog(frame, Globals.lang("This operation requires one or more entries to be selected."),
                    (String)getValue(NAME), JOptionPane.ERROR_MESSAGE);
            return;
        }

        
        if (operation.requiresBibtexKeys())
            for (int i=0; i<entries.length; i++) {
                if ((entries[i].getCiteKey() == null) || (entries[i].getCiteKey().trim().length() == 0)) {
                    JOptionPane.showMessageDialog(frame, Globals.lang("This operation requires all selected entries to have BibTex keys defined."),
                        (String)getValue(NAME), JOptionPane.ERROR_MESSAGE);
                    return;
                }
        }

        
        Thread t = new Thread(this);
        t.start();

    }

    public void run() {
        
        operation.pushEntries(entries, getKeyString(entries));

        
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                operation.operationCompleted(panel);
            }
        });
    }

    protected String getKeyString(BibtexEntry[] entries) {
        StringBuffer result = new StringBuffer();
        String citeKey = "";
        boolean first = true;
        for (int i=0; i<entries.length; i++) {
            BibtexEntry bes = entries[i];
            citeKey = (String) bes.getField(BibtexFields.KEY_FIELD);
            
            if (citeKey == null || citeKey.equals(""))
                continue;
            if (first) {
                result.append(citeKey);
                first = false;
            } else {
                result.append(",").append(citeKey);
            }
        }
        return result.toString();
    }
}
