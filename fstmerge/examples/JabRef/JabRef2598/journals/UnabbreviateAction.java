package net.sf.jabref.journals; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Globals; 
import net.sf.jabref.BasePanel; 
import net.sf.jabref.AbstractWorker; 
import net.sf.jabref.undo.NamedCompound; 


public  class  UnabbreviateAction  extends AbstractWorker {
	
    BasePanel panel;

	
    String message = "";

	

    public UnabbreviateAction(BasePanel panel) {
        this.panel = panel;
    }


	


    public void init() {
        
        panel.output("Unabbreviating...");
    }


	

    public void run() {
        


        BibtexEntry[] entries = panel.getSelectedEntries();
        if (entries == null)
            return;
        NamedCompound ce = new NamedCompound("Unabbreviate journal names");
        int count = 0;
        for (int i = 0; i < entries.length; i++) {
            if (Globals.journalAbbrev.unabbreviate(entries[i], "journal", ce))
                count++;
        }
        if (count > 0) {
            ce.end();
            panel.undoManager.addEdit(ce);
            panel.markBaseChanged();
            message = Globals.lang("Unabbreviated %0 journal names.", String.valueOf(count));
        } else {
            message = Globals.lang("No journal names could be unabbreviated.");
        }
    }


	

    public void update() {
        panel.output(message);
    }



}
