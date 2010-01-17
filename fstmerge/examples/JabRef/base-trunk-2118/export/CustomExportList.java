package net.sf.jabref.export;

import java.util.TreeSet;
import java.util.Comparator;
import java.util.TreeMap;

import net.sf.jabref.Globals;
import net.sf.jabref.JabRefPreferences;



public class CustomExportList extends TreeSet {

    private TreeMap formats = new TreeMap();
  private Object[] array;
  JabRefPreferences prefs;


  public CustomExportList(JabRefPreferences prefs_, Comparator comp) {
    super(comp);
    
    prefs = prefs_;
    readPrefs();
    sort();
  }

  public TreeMap getCustomExportFormats() {
      return formats;
  }


  private void readPrefs() {
    int i=0;
    String[] s = null;
    while ((s = prefs.getStringArray("customExportFormat"+i)) != null) {
        ExportFormat format = createFormat(s);
        formats.put(format.getConsoleName(), format);
      super.add(s);
      i++;
    }
  }

    private ExportFormat createFormat(String[] s) {
        String lfFileName;
        if (s[1].endsWith(".layout"))
            lfFileName = s[1].substring(0, s[1].length()-7);
        else
            lfFileName = s[1];
        ExportFormat format = new ExportFormat(s[0], s[0], lfFileName, null, s[2]);
        format.setCustomExport(true);
        return format;
    }

  public String[] getElementAt(int pos) {
    return (String[])(array[pos]);
  }

  public void addFormat(String[] s) {
    super.add(s);
      ExportFormat format = createFormat(s);
      formats.put(format.getConsoleName(), format);
    sort();
  }

  public void remove(int pos) {
      String[] toRemove = (String[])array[pos];
      formats.remove(toRemove[0]);
    super.remove(array[pos]);
    sort();
  }

  public void sort() {
    array = toArray();
  }

  public void store() {

    if (array.length == 0)
      purge(0);
    else {
      for (int i=0; i<array.length; i++) {
        
        Globals.prefs.putStringArray("customExportFormat"+i, (String[])(array[i]));
      }
      purge(array.length);
    }
  }

  private void purge(int from) {
    String[] s = null;
    int i = from;
    while ((s = Globals.prefs.getStringArray("customExportFormat"+i)) != null) {
      Globals.prefs.remove("customExportFormat"+i);
      i++;
    }
  }

}
