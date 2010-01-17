package net.sf.jabref.collab;

import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import net.sf.jabref.*;
import net.sf.jabref.groups.*;
import net.sf.jabref.imports.*;


public class ChangeScanner extends Thread {

    final double MATCH_THRESHOLD = 0.4;
    final String[] sortBy = new String[] {"year", "author", "title" };
    File f;
    BibtexDatabase inMem;
    MetaData mdInMem;
    BasePanel panel;
    JabRefFrame frame;

    
    
    DefaultMutableTreeNode changes = new DefaultMutableTreeNode(Globals.lang("External changes"));

    

    public ChangeScanner(JabRefFrame frame, BasePanel bp) { 
        panel = bp;
        this.frame = frame;
        this.inMem = bp.database();
        this.mdInMem = bp.metaData();
        
        setPriority(Thread.MIN_PRIORITY);

    }

    public void changeScan(File f) {
        this.f = f;
        start();
    }

    public void run() {
        try {
            

            
            File tempFile = Globals.fileUpdateMonitor.getTempFile(panel.fileMonitorHandle());
            ParserResult pr = OpenDatabaseAction.loadDatabase(tempFile,
            Globals.prefs.get("defaultEncoding"));
            BibtexDatabase inTemp = pr.getDatabase();
            MetaData mdInTemp = new MetaData(pr.getMetaData(),inTemp);
            
            pr = OpenDatabaseAction.loadDatabase(f, Globals.prefs.get("defaultEncoding"));
            BibtexDatabase onDisk = pr.getDatabase();
            MetaData mdOnDisk = new MetaData(pr.getMetaData(),onDisk);

            
            EntryComparator comp = new EntryComparator(false, true, sortBy[2]);
            comp = new EntryComparator(false, true, sortBy[1], comp);
            comp = new EntryComparator(false, true, sortBy[0], comp);
            EntrySorter sInTemp = inTemp.getSorter(comp);
            comp = new EntryComparator(false, true, sortBy[2]);
            comp = new EntryComparator(false, true, sortBy[1], comp);
            comp = new EntryComparator(false, true, sortBy[0], comp);
            EntrySorter sOnDisk = onDisk.getSorter(comp);
            comp = new EntryComparator(false, true, sortBy[2]);
            comp = new EntryComparator(false, true, sortBy[1], comp);
            comp = new EntryComparator(false, true, sortBy[0], comp);
            EntrySorter sInMem = inMem.getSorter(comp);

            

            scanPreamble(inMem, inTemp, onDisk);
            scanStrings(inMem, inTemp, onDisk);


            scanEntries(sInMem, sInTemp, sOnDisk);
            
            scanGroups(mdInMem, mdInTemp, mdOnDisk);


        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public boolean changesFound() {
        return changes.getChildCount() > 0;
    }

    public void displayResult() {
        if (changes.getChildCount() > 0) {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    ChangeDisplayDialog dial = new ChangeDisplayDialog(frame, panel, changes);
                    Util.placeDialog(dial, frame);
                    dial.setVisible(true); 
                }
            });

        } else {
            JOptionPane.showMessageDialog(frame, Globals.lang("No actual changes found."),
            Globals.lang("External changes"), JOptionPane.INFORMATION_MESSAGE);
        }
    }

    private void scanEntries(EntrySorter mem, EntrySorter tmp, EntrySorter disk) {

        
        
        
        int piv1 = 0, piv2 = 0;

        
        
        HashSet used = new HashSet(disk.getEntryCount());
        HashSet notMatched = new HashSet(tmp.getEntryCount());

        
        
        
        mainLoop: for (piv1=0; piv1<tmp.getEntryCount(); piv1++) {

            
            double comp = -1;
            
            
            if (!used.contains(""+piv2) && (piv2<disk.getEntryCount())) {
                comp = Util.compareEntriesStrictly(tmp.getEntryAt(piv1), disk.getEntryAt(piv2));
            }
            if (comp > 1) {
                used.add(""+piv2);
                piv2++;
                continue mainLoop;
            }

            
            if (piv2 < disk.getEntryCount()-1) {
                for (int i = piv2+1; i < disk.getEntryCount(); i++) {
                    if (!used.contains(""+i))
                        comp = Util.compareEntriesStrictly(tmp.getEntryAt(piv1), disk.getEntryAt(i));
                    else
                        comp = -1;

                    if (comp > 1) {
                        used.add("" + i);
                        continue mainLoop;
                    }
                }
            }

            
            notMatched.add(new Integer(piv1));
        }


        
        
        if (notMatched.size() > 0) {

            fuzzyLoop: for (Iterator it=notMatched.iterator(); it.hasNext();) {

                Integer integ = (Integer)it.next();
                piv1 = integ.intValue();


                
                
                int bestMatchI = -1;
                double bestMatch = 0;
                double comp = -1;

                if (piv2 < disk.getEntryCount()-1) {
                    for (int i = piv2; i < disk.getEntryCount(); i++) {
                        if (!used.contains(""+i)) {
                            comp = Util.compareEntriesStrictly(tmp.getEntryAt(piv1),
                            disk.getEntryAt(i));
                        }
                        else
                            comp = -1;

                        if (comp > bestMatch) {
                            bestMatch = comp;
                            bestMatchI = i;
                        }
                    }
                }

                if (bestMatch > MATCH_THRESHOLD) {
                    used.add(""+bestMatchI);
                    it.remove();

                    EntryChange ec = new EntryChange(bestFit(tmp, mem, piv1), tmp.getEntryAt(piv1),
                    disk.getEntryAt(bestMatchI));
                    changes.add(ec);

                    
                    
                    
                    
                    
                    

                    
                    

                }
                else {
                    EntryDeleteChange ec = new EntryDeleteChange(bestFit(tmp, mem, piv1), tmp.getEntryAt(piv1));
                    changes.add(ec);
          

                }

            }

        }

        
        
        if (used.size() < disk.getEntryCount()) {
            for (int i=0; i<disk.getEntryCount(); i++) {
                if (!used.contains(""+i)) {

                    
                    boolean hasAlready = false;
                    for (int j = 0; j < mem.getEntryCount(); j++) {
                        if (Util.compareEntriesStrictly(mem.getEntryAt(j),
                            disk.getEntryAt(i)) >= 1) {
                            hasAlready = true;
                            break;
                        }
                    }
                    if (!hasAlready) {
                        EntryAddChange ec = new EntryAddChange(disk.getEntryAt(i));
                        changes.add(ec);
                    }
          
                }
            }
            
        }
    }

    
    private BibtexEntry bestFit(EntrySorter old, EntrySorter neu, int index) {
        double comp = -1;
        int found = 0;
        loop: for (int i=0; i<neu.getEntryCount(); i++) {
            double res = Util.compareEntriesStrictly(old.getEntryAt(index),
            neu.getEntryAt(i));
            if (res > comp) {
                comp = res;
                found = i;
            }
            if (comp > 1)
                break loop;
        }
        return neu.getEntryAt(found);
    }

    private void scanPreamble(BibtexDatabase inMem, BibtexDatabase onTmp, BibtexDatabase onDisk) {
        String mem = inMem.getPreamble(),
        tmp = onTmp.getPreamble(),
        disk = onDisk.getPreamble();
        if (tmp != null) {
            if ((disk == null) || !tmp.equals(disk))
                changes.add(new PreambleChange(tmp, mem, disk));
        }
        else if ((disk != null) && !disk.equals("")) {
            changes.add(new PreambleChange(tmp, mem, disk));
        }
    }

    private void scanStrings(BibtexDatabase inMem, BibtexDatabase onTmp, BibtexDatabase onDisk) {
        int nTmp = onTmp.getStringCount(),
        nDisk = onDisk.getStringCount();
        if ((nTmp == 0) && (nDisk == 0))
            return;

        HashSet used = new HashSet();
        HashSet usedInMem = new HashSet();
        HashSet notMatched = new HashSet(onTmp.getStringCount());

        
        
        mainLoop: for (Iterator i=onTmp.getStringKeySet().iterator(); i.hasNext();) {
            Object tmpId = i.next();
            BibtexString tmp = onTmp.getString(tmpId);

            
            for (Iterator j=onDisk.getStringKeySet().iterator(); j.hasNext();) {
                Object diskId = j.next();
                if (!used.contains(diskId)) {
                    BibtexString disk = onDisk.getString(diskId);
                    if (disk.getName().equals(tmp.getName())) {
                        
                        if ((tmp.getContent() != null) && !tmp.getContent().equals(disk.getContent())) {
                            
                            BibtexString mem = findString(inMem, tmp.getName(), usedInMem);
                            if (mem != null)
                                changes.add(new StringChange(mem, tmp.getName(),
                                mem.getContent(),
                                tmp.getContent(), disk.getContent()));
                            else
                                changes.add(new StringChange(null, tmp.getName(), null, tmp.getContent(), disk.getContent()));
                        }
                        used.add(diskId);
                        
                        
                        continue mainLoop;
                    }

                }
            }
            
            notMatched.add(tmp.getId());
        }

        
        if (notMatched.size() > 0) {
            for (Iterator i = notMatched.iterator(); i.hasNext(); ) {
                Object nmId = i.next();
                BibtexString tmp = onTmp.getString(nmId);

                
                
                String tmpContent = tmp.getContent();
                
                for (Iterator j=onDisk.getStringKeySet().iterator(); j.hasNext();) {
                    Object diskId = j.next();
                    
                    if (!used.contains(diskId)) {
                        BibtexString disk = onDisk.getString(diskId);

                        if (disk.getContent().equals(tmp.getContent())) {
                            
                            

                            
                            BibtexString bsMem = null;
                            findInMem: for (Iterator k=inMem.getStringKeySet().iterator(); k.hasNext();) {
                                Object memId = k.next();
                                
                                BibtexString bsMem_cand = inMem.getString(memId);
                                if (bsMem_cand.getContent().equals(disk.getContent()) &&
                                !usedInMem.contains(memId)) {
                                    usedInMem.add(memId);
                                    bsMem = bsMem_cand;
                                    break findInMem;
                                }
                            }

                            changes.add(new StringNameChange(bsMem, bsMem.getName(),
                            tmp.getName(), disk.getName(),
                            tmp.getContent()));
                            i.remove();
                            used.add(diskId);

                        }
                    }
                }
            }
        }

        if (notMatched.size() > 0) {
            
            for (Iterator i = notMatched.iterator(); i.hasNext(); ) {
                Object nmId = i.next();
                BibtexString tmp = onTmp.getString(nmId);
                BibtexString mem = findString(inMem, tmp.getName(), usedInMem);
                if (mem != null) { 
                    changes.add(new StringRemoveChange(tmp, mem));
                }
            }
        }


        
        
        for (Iterator i=onDisk.getStringKeySet().iterator(); i.hasNext();) {
            Object diskId = i.next();
            if (!used.contains(diskId)) {
                BibtexString disk = onDisk.getString(diskId);
                
                used.add(diskId);
                changes.add(new StringAddChange(disk));
            }
        }
    }

    private BibtexString findString(BibtexDatabase base, String name, HashSet used) {
        if (!base.hasStringLabel(name))
            return null;
        for (Iterator i=base.getStringKeySet().iterator(); i.hasNext();) {
            Object key = i.next();
            BibtexString bs = base.getString(key);
            if (bs.getName().equals(name) && !used.contains(key)) {
                used.add(key);
                return bs;
            }
        }
        return null;
    }

    
    public void scanGroups(MetaData inMem, MetaData onTmp, MetaData onDisk) {
        final GroupTreeNode groupsMem = inMem.getGroups();
        final GroupTreeNode groupsTmp = onTmp.getGroups();
        final GroupTreeNode groupsDisk = onDisk.getGroups();
        if (groupsTmp == null && groupsDisk == null)
            return;
        if ((groupsTmp != null && groupsDisk == null)
                || (groupsTmp == null && groupsDisk != null)) {
            changes.add(new GroupChange(groupsDisk));
            return;
        }
        if (groupsTmp.equals(groupsDisk))
            return;
        changes.add(new GroupChange(groupsDisk));
        return;

















































    }

}
