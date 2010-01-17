

package net.sf.jabref; 

import java.util.HashSet; 
import java.util.Iterator; 

import javax.swing.JOptionPane; 
import javax.swing.SwingUtilities; 

import net.sf.jabref.undo.NamedCompound; 
import net.sf.jabref.undo.UndoableRemoveEntry; 


public  class  StrictDuplicateSearch  extends Thread {
	
    
    BasePanel panel;

	
    
    
    public StrictDuplicateSearch(BasePanel bp) {
        this.panel = bp;
        
    }


	
    
    public void run() {
        HashSet<BibtexEntry> toRemove = new HashSet<BibtexEntry>();
        NamedCompound ce = new NamedCompound(Globals.lang("Remove duplicates"));
        panel.output(Globals.lang("Searching for duplicates..."));
        Object[] keys = panel.database.getKeySet().toArray();
        if ((keys == null) || (keys.length < 2))
            return;
        BibtexEntry[] bes = new BibtexEntry[keys.length];
        for (int i=0; i<keys.length; i++)
            bes[i] = panel.database.getEntryById((String)keys[i]);
        
        for (int i = 0; i<bes.length-1; i++) {
            for (int j = i + 1; j<bes.length; j++) {
                
                if (!toRemove.contains(bes[i]) && !toRemove.contains(bes[j]) && 
                    DuplicateCheck.compareEntriesStrictly(bes[i], bes[j]) > 1) {
                    
                    if (!toRemove.contains(bes[i]) && !toRemove.contains(bes[j])) {
                        toRemove.add(bes[j]);
                    }
                }
            }
         }
    
        if (toRemove.size() == 0) {
            panel.output(Globals.lang("No duplicates found")+".");
            return;
        }
               
        
        int answer = JOptionPane.showConfirmDialog(panel.frame(), Globals.lang("Duplicates found")+": "+
            toRemove.size()+". "+Globals.lang("Remove all?"), Globals.lang("Remove duplicates"),
            JOptionPane.OK_CANCEL_OPTION);
        if (answer == JOptionPane.CANCEL_OPTION)
            return;
        
        panel.output(Globals.lang("Duplicates removed")+": "+toRemove.size());
        
        for (Iterator<BibtexEntry> i=toRemove.iterator(); i.hasNext();) {
            BibtexEntry entry = i.next();
            panel.database.removeEntry(entry.getId());        
            ce.addEdit(new UndoableRemoveEntry(panel.database, entry, panel));
        }
     
        ce.end();
        panel.undoManager.addEdit(ce);
        
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {                
                panel.markBaseChanged();
            }
        });

        
    }



}
