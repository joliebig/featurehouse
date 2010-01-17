

package net.sf.jabref.export;

import java.awt.event.ActionEvent;
import net.sf.jabref.BasePanel;
import net.sf.jabref.GUIGlobals;
import net.sf.jabref.Globals;
import net.sf.jabref.JabRefFrame;
import net.sf.jabref.MnemonicAwareAction;
import net.sf.jabref.Worker;
import spin.Spin;


public class SaveAllAction extends MnemonicAwareAction implements Worker {
    
    private JabRefFrame frame;
    private int databases=0, saved=0;
    
    
    public SaveAllAction(JabRefFrame frame) {
        super(GUIGlobals.getImage("saveAll"));
        this.frame = frame;
        putValue(ACCELERATOR_KEY, Globals.prefs.getKey("Save all"));
        putValue(SHORT_DESCRIPTION, Globals.lang("Save all open databases"));
        putValue(NAME, "Save all");
    }

    public void actionPerformed(ActionEvent e) {
        databases = frame.getTabbedPane().getTabCount();
        saved = 0;
        frame.output(Globals.lang("Saving all databases..."));
        Worker worker = (Worker)Spin.off(this);
        run();
        
        frame.output(Globals.lang("Save all finished."));
        
    }

    public void run() {
        for (int i=0; i<databases; i++) {
            if (i < frame.getTabbedPane().getTabCount()) {
                
                BasePanel panel = frame.baseAt(i);
                if (panel.getFile() == null) {
                    frame.showBaseAt(i);
                }
                panel.runCommand("save");
                
                saved++;
            }
        }
    }

    
    
    
}
