package net.sf.jabref.gui;

import net.sf.jabref.*;
import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.event.ListEventAssembler;
import ca.odell.glazedlists.event.ListEvent;
import ca.odell.glazedlists.event.ListEventListener;

import java.util.*;


public class GlazedEntrySorter implements DatabaseChangeListener {


    
    EventList list;

    Comparator comp;
    String[] idArray;
    BibtexEntry[] entryArray;
    
    private boolean outdated = false;
    private boolean changed = false;

    public GlazedEntrySorter(Map entries, Comparator comp) {
        
        list = new BasicEventList();
        
        this.comp = comp;
        list.getReadWriteLock().writeLock().lock();
        Set keySet = entries.keySet();
        if (keySet != null) {
            Iterator i = keySet.iterator();
            while (i.hasNext()) {
                list.add(entries.get(i.next()));
            }
        }

        
        
        Collections.sort(list, new IdComparator());
        
        list.getReadWriteLock().writeLock().unlock();

    }

    public EventList getTheList() {
        return list;
    }

    public void databaseChanged(DatabaseChangeEvent e) {
        list.getReadWriteLock().writeLock().lock();
        if (e.getType() == DatabaseChangeEvent.ADDED_ENTRY) {
            
            list.add(e.getEntry());
            

        } else if (e.getType() == DatabaseChangeEvent.REMOVED_ENTRY) {
            list.remove(e.getEntry());
            
        } else if (e.getType() == DatabaseChangeEvent.CHANGED_ENTRY) {
            int index = list.indexOf(e.getEntry());
            list.set(index, e.getEntry());
        }
        list.getReadWriteLock().writeLock().unlock();

    }


}
