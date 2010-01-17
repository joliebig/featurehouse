package net.sf.jabref.collab;

import java.util.Enumeration;
import java.util.TreeSet;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;

import net.sf.jabref.*;
import net.sf.jabref.undo.NamedCompound;
import net.sf.jabref.undo.UndoableFieldChange;

public class EntryChange extends Change {

  BibtexEntry memEntry, tmpEntry, diskEntry;
  boolean isModifiedLocally, modificationsAgree;

  public EntryChange(BibtexEntry memEntry, BibtexEntry tmpEntry, BibtexEntry diskEntry) {
    super();
    String key = tmpEntry.getCiteKey();
    if (key == null)
      name = "Modified entry";
    else
      name = "Modified entry: '"+key+"'";
    this.memEntry = memEntry;
    this.tmpEntry = tmpEntry;
    this.diskEntry = diskEntry;

    
    
    isModifiedLocally = ! (DuplicateCheck.compareEntriesStrictly(memEntry, tmpEntry) > 1);

    
    
    modificationsAgree = (DuplicateCheck.compareEntriesStrictly(memEntry, diskEntry) > 1);

    
    

    TreeSet<String> allFields = new TreeSet<String>();
    allFields.addAll(memEntry.getAllFields());
    allFields.addAll(tmpEntry.getAllFields());
    allFields.addAll(diskEntry.getAllFields());
  
    for (String field : allFields){
      String mem = memEntry.getField(field),
          tmp = tmpEntry.getField(field),
          disk = diskEntry.getField(field);

      if ((tmp != null) && (disk != null)) {
        if (!tmp.equals(disk)) {
          
          add(new FieldChange(field, memEntry, tmpEntry, mem, tmp, disk));
        }
      } else if ((tmp == null) && (disk != null) && !disk.equals("")) {
        
        add(new FieldChange(field, memEntry, tmpEntry, mem, tmp, disk));
      } else if ((disk == null) && (tmp != null) && !tmp.equals("")
                 && (mem != null) && !mem.equals("")) {
        
        add(new FieldChange(field, memEntry, tmpEntry, mem, tmp, disk));
      }

      
    }
  }

  
public void makeChange(BasePanel panel, BibtexDatabase secondary, NamedCompound undoEdit) {


    Enumeration<Change> e = children();
    for (; e.hasMoreElements();) {
      Change c = e.nextElement();
      if (c.isAcceptable() && c.isAccepted())
        c.makeChange(panel, secondary, undoEdit);
    }

    
  }

  JComponent description() {
    return new JLabel(name);
  }




  class FieldChange extends Change {

    BibtexEntry entry, tmpEntry;
    String field, inMem, onTmp, onDisk;
    InfoPane tp = new InfoPane();
    JScrollPane sp = new JScrollPane(tp);

    public FieldChange(String field, BibtexEntry memEntry, BibtexEntry tmpEntry, String inMem, String onTmp, String onDisk) {
      entry = memEntry;
      this.tmpEntry = tmpEntry;
      name = field;
      this.field = field;
      this.inMem = inMem;
      this.onTmp = onTmp;
      this.onDisk = onDisk;

      StringBuffer text = new StringBuffer();
      text.append("<FONT SIZE=10>");
        text.append("<H2>").append(Globals.lang("Modification of field")).append(" <I>").append(field).append("</I></H2>");

      if ((onDisk != null) && !onDisk.equals(""))
          text.append("<H3>").append(Globals.lang("Value set externally")).append(":</H3>" + " ").append(onDisk);
      else
          text.append("<H3>").append(Globals.lang("Value cleared externally")).append("</H3>");

      if ((inMem != null) && !inMem.equals(""))
          text.append("<H3>").append(Globals.lang("Current value")).append(":</H3>" + " ").append(inMem);
      if ((onTmp != null) && !onTmp.equals(""))
          text.append("<H3>").append(Globals.lang("Current tmp value")).append(":</H3>" + " ").append(onTmp);
      else {
        
        
      }
      tp.setContentType("text/html");
      tp.setText(text.toString());
    }

    public void makeChange(BasePanel panel, BibtexDatabase secondary, NamedCompound undoEdit) {
      
      entry.setField(field, onDisk);
      undoEdit.addEdit(new UndoableFieldChange(entry, field, inMem, onDisk));
      tmpEntry.setField(field, onDisk);
    }

    JComponent description() {
      return sp;
    }

  }
}
