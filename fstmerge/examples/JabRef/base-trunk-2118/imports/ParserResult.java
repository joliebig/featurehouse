
package net.sf.jabref.imports;

import java.util.HashMap;
import java.io.File;
import net.sf.jabref.*;
import java.util.Vector;
import java.util.ArrayList;
import java.awt.event.ActionListener;

public class ParserResult {

    public static ParserResult INVALID_FORMAT = new ParserResult(null, null, null);
    private BibtexDatabase base;
    private HashMap metaData, entryTypes;
    private File file = null;
    private ArrayList warnings = new ArrayList();
    private String encoding = null; 


    private String jabrefVersion = null; 
    private int jabrefMajorVersion = 0, jabrefMinorVersion = 0; 
    private boolean toOpenTab = false;

    public ParserResult(BibtexDatabase base, HashMap metaData, HashMap entryTypes) {
	this.base = base;
	this.metaData = metaData;
	this.entryTypes = entryTypes;
    }

    
    public boolean toOpenTab() {
        return toOpenTab;
    }

    public void setToOpenTab(boolean toOpenTab) {
        this.toOpenTab = toOpenTab;
    }


    
    public String getJabrefVersion() {
        return jabrefVersion;
    }

    
    public void setJabrefVersion(String jabrefVersion) {
        this.jabrefVersion = jabrefVersion;
    }


    public int getJabrefMajorVersion() {
        return jabrefMajorVersion;
    }

    public void setJabrefMajorVersion(int jabrefMajorVersion) {
        this.jabrefMajorVersion = jabrefMajorVersion;
    }

    public int getJabrefMinorVersion() {
        return jabrefMinorVersion;
    }

    public void setJabrefMinorVersion(int jabrefMinorVersion) {
        this.jabrefMinorVersion = jabrefMinorVersion;
    }

    public BibtexDatabase getDatabase() {
    	return base;
    }

    public HashMap getMetaData() {
	return metaData;
    }

    public HashMap getEntryTypes() {
	return entryTypes;
    }

    public File getFile() {
      return file;
    }

    public void setFile(File f) {
      file = f;
    }

    
    public void setEncoding(String enc) {
      encoding = enc;
    }

    
    public String getEncoding() {
      return encoding;
    }

    
    public void addWarning(String s) {
        if (!warnings.contains(s))
            warnings.add(s);
    }

    public boolean hasWarnings() {
      return (warnings.size() > 0);
    }

    public String[] warnings() {
      String[] s = new String[warnings.size()];
      for (int i=0; i<warnings.size(); i++)
        s[i] = (String)warnings.get(i);
      return s;
    }

}
