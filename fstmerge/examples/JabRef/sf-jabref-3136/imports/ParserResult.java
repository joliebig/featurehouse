
package net.sf.jabref.imports;

import java.io.File;
import net.sf.jabref.BibtexEntryType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;

public class ParserResult {

    public static ParserResult INVALID_FORMAT = new ParserResult(null, null, null);
    public static ParserResult FILE_LOCKED = new ParserResult(null, null, null);
    private BibtexDatabase base;
    private HashMap<String, String> metaData;
    private HashMap<String, BibtexEntryType> entryTypes;


    private File file = null;
    private ArrayList<String> warnings = new ArrayList<String>();
    private String errorMessage = null;
    private String encoding = null; 

    private boolean postponedAutosaveFound = false;
    private boolean invalid = false;

    private String jabrefVersion = null; 
    private int jabrefMajorVersion = 0, jabrefMinorVersion = 0; 
    private boolean toOpenTab = false;

    public ParserResult(Collection<BibtexEntry> entries){
    	this(ImportFormatReader.createDatabase(entries), null, new HashMap<String, BibtexEntryType>());
    }
    
    public ParserResult(BibtexDatabase base, HashMap<String, String> metaData, HashMap<String, BibtexEntryType> entryTypes) {
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

    public HashMap<String, String> getMetaData() {
	return metaData;
    }

    public HashMap<String, BibtexEntryType> getEntryTypes() {
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
        s[i] = warnings.get(i);
      return s;
    }

    public boolean isPostponedAutosaveFound() {
        return postponedAutosaveFound;
    }

    public void setPostponedAutosaveFound(boolean postponedAutosaveFound) {
        this.postponedAutosaveFound = postponedAutosaveFound;
    }

    public boolean isInvalid() {
        return invalid;
    }

    public void setInvalid(boolean invalid) {
        this.invalid = invalid;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }
}
