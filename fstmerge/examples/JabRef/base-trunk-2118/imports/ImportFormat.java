package net.sf.jabref.imports;

import java.io.InputStream;
import java.io.IOException;
import java.util.List;


public abstract class ImportFormat implements Comparable {

    private boolean isCustomImporter;
    
    
    public ImportFormat() {
      this.isCustomImporter = false;
    }

    
    public abstract boolean isRecognizedFormat(InputStream in) throws IOException;

    
    public abstract List importEntries(InputStream in) throws IOException;


    
    public abstract String getFormatName();
    
    
    public String getExtensions() {
      return null;
    }
    
    
    public String getCLIId() {
      String id = getFormatName();
      StringBuffer result = new StringBuffer(id.length());
      for (int i = 0; i < id.length(); i++) {
        char c = id.charAt(i);
        if (Character.isLetterOrDigit(c)) {
          result.append(Character.toLowerCase(c));
        }
      }
      return result.toString();
    }
    
    
    public String getDescription() {
      return "No description available for " + getFormatName() + ".";
    }
    
    
    public final void setIsCustomImporter(boolean isCustomImporter) {
      this.isCustomImporter = isCustomImporter;
    }
    
    
    public final boolean getIsCustomImporter() {
      return this.isCustomImporter; 
    }
        
    
    public int hashCode() {
      return getFormatName().hashCode();
    }
    
    
    public boolean equals(Object o) {
      return o != null 
          && o instanceof ImportFormat
          && ((ImportFormat)o).getIsCustomImporter() == getIsCustomImporter() 
          && ((ImportFormat)o).getFormatName().equals(getFormatName());
    }
    
    
    public String toString() {
      return getFormatName();
    }
    
    
    public int compareTo(Object o) {
      int result = 0;
      ImportFormat importer = (ImportFormat)o;
      if (getIsCustomImporter() == importer.getIsCustomImporter()) {
        result = getFormatName().compareTo(importer.getFormatName());
      } else {
        result = getIsCustomImporter() ? 1 : -1;
      }
      return result;
    }
}
