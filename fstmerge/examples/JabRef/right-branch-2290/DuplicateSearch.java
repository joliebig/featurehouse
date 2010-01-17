







package net.sf.jabref;

import java.util.Vector;
import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.*;

import net.sf.jabref.undo.NamedCompound;
import net.sf.jabref.undo.UndoableRemoveEntry;
import spin.Spin;

public class DuplicateSearch extends Thread {

  BasePanel panel;
  BibtexEntry[] bes;
  final Vector duplicates = new Vector();
  boolean autoRemoveExactDuplicates = false;
  
  public DuplicateSearch(BasePanel bp) {
    panel = bp;
  }

public void run() {
  final NamedCompound ce = new NamedCompound(Globals.lang("duplicate removal"));
  int duplicateCounter = 0;
  
  autoRemoveExactDuplicates = false;
  panel.output(Globals.lang("Searching for duplicates..."));
  Object[] keys = panel.database.getKeySet().toArray();
  if ((keys == null) || (keys.length < 2))
    return;
  bes = new BibtexEntry[keys.length];
  for (int i=0; i<keys.length; i++)
    bes[i] = panel.database.getEntryById((String)keys[i]);

  SearcherThread st = new SearcherThread();
  st.setPriority(Thread.MIN_PRIORITY);
  st.start();
  int current = 0;
  DuplicateResolverDialog drd = null;



   final ArrayList toRemove = new ArrayList();
  while (!st.finished() || (current < duplicates.size()))
  {

    if (current >= duplicates.size() )
    {
      
      
      synchronized(duplicates)
      {
         try
         {
           duplicates.wait();
         }
         catch (Exception e) {}
      }
    } else  
    {


        BibtexEntry[] be = (BibtexEntry[]) duplicates.get(current);
        current++;
        if (!toRemove.contains(be[0]) && !toRemove.contains(be[1])) {
            
            boolean askAboutExact = false;
            if (Util.compareEntriesStrictly(be[0], be[1]) > 1) {
                if (autoRemoveExactDuplicates) {
                    toRemove.add(be[1]);
                    duplicateCounter++;
                    continue;
                } else {
                    askAboutExact = true;
                }
            }

            DuplicateCallBack cb = new DuplicateCallBack(panel.frame, be[0], be[1],
                    askAboutExact ? DuplicateResolverDialog.DUPLICATE_SEARCH_WITH_EXACT :
                            DuplicateResolverDialog.DUPLICATE_SEARCH);
            ((CallBack)(Spin.over(cb))).update();

            duplicateCounter++;
            int answer = cb.getSelected();
            if ((answer == DuplicateResolverDialog.KEEP_UPPER)
                    || (answer == DuplicateResolverDialog.AUTOREMOVE_EXACT)) {
                toRemove.add(be[1]);
                if (answer == DuplicateResolverDialog.AUTOREMOVE_EXACT)
                    autoRemoveExactDuplicates = true; 
            } else if (answer == DuplicateResolverDialog.KEEP_LOWER) {
                toRemove.add(be[0]);
            } else if (answer == DuplicateResolverDialog.BREAK) {
                st.setFinished(); 
                current = Integer.MAX_VALUE;
                duplicateCounter--; 
            }
        }
    }
  }

  if (drd != null)
    drd.dispose();
    final int dupliC = duplicateCounter;
    SwingUtilities.invokeLater(new Runnable() {
        public void run() {
            
            if (toRemove.size() > 0) {
                for (Iterator iterator = toRemove.iterator(); iterator.hasNext();) {
                    BibtexEntry entry = (BibtexEntry) iterator.next();
                    panel.database.removeEntry(entry.getId());
                    ce.addEdit(new UndoableRemoveEntry(panel.database, entry, panel));
                }
                panel.markBaseChanged();
            }
            panel.output(Globals.lang("Duplicate pairs found") + ": " + duplicates.size()
                       +" " +Globals.lang("pairs processed") +": " +dupliC );

            if (ce != null)
            {
                ce.end();
                panel.undoManager.addEdit(ce);
            }

        }

    });


}


class SearcherThread extends Thread {

  private boolean finished = false;

  public void run() {
    for (int i = 0; (i < bes.length - 1) && !finished ; i++) {
      for (int j = i + 1; (j < bes.length) && !finished ; j++) {
        boolean eq = Util.isDuplicate(bes[i], bes[j],
                                      Globals.duplicateThreshold);

        
        if (eq)
        {
          synchronized (duplicates)
          {
            duplicates.add( new BibtexEntry[] {bes[i], bes[j]} ) ;
            duplicates.notifyAll(); 
          }
        }
      }
    }
    finished = true;

    
    synchronized(duplicates)
    {
      duplicates.notifyAll();
    }
  }

  public boolean finished() {
    return finished;
  }

  
  
  public void setFinished()
  {
    finished = true ;
  }
}

    class DuplicateCallBack implements CallBack {
        private int reply = -1;
        DuplicateResolverDialog diag;
        private JabRefFrame frame;
        private BibtexEntry one;
        private BibtexEntry two;
        private int dialogType;

        public DuplicateCallBack(JabRefFrame frame, BibtexEntry one, BibtexEntry two,
                                 int dialogType) {

            this.frame = frame;
            this.one = one;
            this.two = two;
            this.dialogType = dialogType;
        }
        public int getSelected() {
            return reply;
        }
        public void update() {
            diag = new DuplicateResolverDialog(frame, one, two, dialogType);
            diag.setVisible(true);
            diag.dispose();
            reply = diag.getSelected();
        }
    }

}
