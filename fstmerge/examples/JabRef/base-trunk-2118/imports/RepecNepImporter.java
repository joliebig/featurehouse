
package net.sf.jabref.imports;
import java.io.InputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.ArrayList;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexEntryType;
import net.sf.jabref.Util;
import net.sf.jabref.imports.ImportFormat;
import net.sf.jabref.imports.ImportFormatReader;



public class RepecNepImporter extends ImportFormat {

  private final static Collection recognizedFields = Arrays.asList(new String[]{"Keywords", "JEL", "Date", "URL", "By"});
  
  private int line = 0;
  private String lastLine = "";
  private String preLine = "";
  private BufferedReader in = null;
  private boolean inOverviewSection = false;
  
  
  public String getFormatName() {
    return "REPEC New Economic Papers (NEP)";
  }

  
  public String getCLIId() {
    return "repecnep";
  }
  
    
  public String getExtensions() {
    return ".txt";
  }
  
  
  public String getDescription() {
    return 
      "Imports a New Economics Papers-Message (see http://nep.repec.org)\n"
    + "from the REPEC-NEP Service (see http://www.repec.org).\n"
    + "To import papers either save a NEP message as a text file and then import or\n"
    + "copy&paste the papers you want to import and make sure, one of the first lines\n"
    + "contains the line \"nep.repec.org\".";
  }
  
  
  public boolean isRecognizedFormat(InputStream stream) throws IOException {
    
    
    
    BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
    String startOfMessage = "";
    String line = in.readLine();
    for (int i = 0; i < 25 && line != null; i++) {
      startOfMessage += line;
      line = in.readLine();
    }
    return startOfMessage.indexOf("NEP: New Economics Papers") >= 0 || startOfMessage.indexOf("nep.repec.org") >= 0;
  }

  private boolean startsWithKeyword(Collection keywords) {
    boolean result = this.lastLine.indexOf(':') > 0;
    if (result) {
      String possibleKeyword = this.lastLine.substring(0, this.lastLine.indexOf(':'));
      result = keywords.contains(possibleKeyword);
    }
    return result;
  }
  
  private void readLine() throws IOException {
    this.line++;
    this.preLine = this.lastLine;
    this.lastLine = this.in.readLine();
  }
  
  
  private String readMultipleLines() throws IOException {
    String result = this.lastLine.trim();
    readLine();
    while (this.lastLine != null && !this.lastLine.trim().equals("") && !startsWithKeyword(recognizedFields) && !isStartOfWorkingPaper()) {
      result += this.lastLine.length() == 0 ? this.lastLine.trim() : " " + this.lastLine.trim();
      readLine();
    }
    return result;
  }

  
  private void parseTitleString(BibtexEntry be) throws IOException {
    
    this.lastLine = this.lastLine.substring(this.lastLine.indexOf('.') + 1, this.lastLine.length());
    be.setField("title", readMultipleLines());
  }
  
  
  private void parseAuthors(BibtexEntry be) throws IOException {
    
    String authors = "";
    String institutions = "";
    while (this.lastLine != null && !this.lastLine.equals("") && !startsWithKeyword(recognizedFields)) {
      
      
      String author = null;
      String institution = null;
      boolean institutionDone = false;
      if (this.lastLine.indexOf('(') >= 0) {
        author = this.lastLine.substring(0, this.lastLine.indexOf('(')).trim();
        institutionDone = this.lastLine.indexOf(')') > 0;
        institution = this.lastLine.substring(this.lastLine.indexOf('(') + 1, institutionDone && this.lastLine.indexOf(')') > this.lastLine.indexOf('(') + 1 ? this.lastLine.indexOf(')') : this.lastLine.length()).trim();
      } else {
        author = this.lastLine.substring(0, this.lastLine.length()).trim();
        institutionDone = true;
      }
      
      readLine();
      while (!institutionDone && this.lastLine!= null) {
        institutionDone = this.lastLine.indexOf(')') > 0;
        institution += this.lastLine.substring(0, institutionDone ? this.lastLine.indexOf(')') : this.lastLine.length()).trim();
        readLine();
      }
      
      if (author != null) {
        authors += !authors.equals("") ? " and " + author : "" + author;
      }
      if (institution != null) {
        institutions += !institutions.equals("") ? " and " + institution : "" + institution;
      }            
    }
    
    if (!authors.equals("")) {
      be.setField("author", authors);
    }
    if (!institutions.equals("")) {
      be.setField("institution", institutions);
    }
  }
  
  
  private void parseAbstract(BibtexEntry be) throws IOException {
    String theabstract = readMultipleLines();
    
    if (!theabstract.equals("")) {
      be.setField("abstract", theabstract);
    }
  }
    
  
  private void parseAdditionalFields(BibtexEntry be, boolean multilineUrlFieldAllowed) throws IOException {
    
    
    if (this.lastLine != null && this.lastLine.trim().equals("")) {
      readLine();  
    }
    
    
    while (this.lastLine != null && !isStartOfWorkingPaper() && (startsWithKeyword(recognizedFields) || this.lastLine.equals(""))) {
      
      
      String keyword = this.lastLine.equals("") ? "" : this.lastLine.substring(0, this.lastLine.indexOf(':')).trim();
      
      this.lastLine = this.lastLine.equals("") ? "" : this.lastLine.substring(this.lastLine.indexOf(':')+1, this.lastLine.length()).trim();
      
      
      if (keyword.equals("Keywords")) {
        String content = readMultipleLines();
        String[] keywords = content.split("[,;]");
        String keywordStr = "";
        for (int i = 0; i < keywords.length; i++) {
          keywordStr += " '" + keywords[i].trim() + "'";
        }
        be.setField("keywords", keywordStr.trim());
        
      
      } else if (keyword.equals("JEL")) {
        be.setField("jel", readMultipleLines());
        
      
      } else if (keyword.startsWith("Date")) {
        Date date = null;
        String content = readMultipleLines();
        String[] recognizedDateFormats = new String[] {"yyyy-MM-dd","yyyy-MM","yyyy"};
        int i = 0;
        for (; i < recognizedDateFormats.length && date == null; i++) {
          try {            
            date = new SimpleDateFormat(recognizedDateFormats[i]).parse(content);
          } catch (ParseException e) {
            
          }
        }
        
        Calendar cal = new GregorianCalendar();              
        cal.setTime(date != null ? date : new Date());
        be.setField("year", "" + cal.get(Calendar.YEAR));
        if (date != null && recognizedDateFormats[i-1].indexOf("MM") >= 0) {
          be.setField("month", "" + cal.get(Calendar.MONTH));
        }
        
      
      } else if (keyword.startsWith("URL")) {
        String content = null;
        if (multilineUrlFieldAllowed) {
          content = readMultipleLines(); 
        } else {
          content = this.lastLine;
          readLine();
        }
        be.setField("url", content);
        
      
      } else if (keyword.startsWith("By")) {
        
        parseAuthors(be); 
      } else {
        readLine();
      }
    }
  }

  
  private boolean isStartOfWorkingPaper() {
    return this.lastLine.matches("\\d+\\.\\s.*") && !this.inOverviewSection && this.preLine.trim().equals("");
  }
  
  
  public List importEntries(InputStream stream) throws IOException {    
  	ArrayList bibitems = new ArrayList();
    String paperNoStr = null;
    this.line = 0;
    
    try {
    	this.in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
      
      readLine(); 
    	while (this.lastLine != null) {
  
        if (this.lastLine.startsWith("-----------------------------")) {
          this.inOverviewSection = this.preLine.startsWith("In this issue we have");
        } 
        if (isStartOfWorkingPaper()) {
          BibtexEntry be = new BibtexEntry(Util.createNeutralId());
          be.setType(BibtexEntryType.getType("techreport"));
          paperNoStr = this.lastLine.substring(0, this.lastLine.indexOf('.'));  
          parseTitleString(be);
          if (startsWithKeyword(recognizedFields)) {
            parseAdditionalFields(be, false);
          } else {
            readLine(); 
            parseAuthors(be);
            readLine(); 
          }
          if (!startsWithKeyword(recognizedFields)) {
            parseAbstract(be);
          }
          parseAdditionalFields(be, true);
          
          bibitems.add(be);
          paperNoStr = null;
          
        } else {        
          this.preLine = this.lastLine;
          readLine();
        }
      }
      
    } catch (Exception e) {
      String message = "Error in REPEC-NEP import on line " + this.line;
      if (paperNoStr != null) {
        message += ", paper no. " + paperNoStr + ": ";
      }
      message += e.getMessage();
      System.err.println(message);
      if (!(e instanceof IOException)) {
        e.printStackTrace();
        e = new IOException(message);
      }
      throw (IOException)e;
    }

  	return bibitems;	  	
  }
}


