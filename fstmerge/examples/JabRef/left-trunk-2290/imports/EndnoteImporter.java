package net.sf.jabref.imports;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Pattern;

import net.sf.jabref.*;


public class EndnoteImporter extends ImportFormat {

    
    public String getFormatName() {
    return "Refer/Endnote";
    }

    
    public String getCLIId() {
      return "refer";
    }

    
    public boolean isRecognizedFormat(InputStream stream) throws IOException {

    
    BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
    Pattern pat1 = Pattern.compile("%A .*"),
            pat2 = Pattern.compile("%E .*");
    String str;
    while ((str = in.readLine()) != null){
        if (pat1.matcher(str).matches() || pat2.matcher(str).matches())
            return true;
    }
    return false;
    }

    
    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {
    ArrayList<BibtexEntry> bibitems = new ArrayList<BibtexEntry>();
    StringBuffer sb = new StringBuffer();
    BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
    String ENDOFRECORD = "__EOREOR__";

    String str;
    boolean first = true;
    while ((str = in.readLine()) != null){
        str = str.trim();
        
        if (str.indexOf("%0") == 0){
        if (first){
            first = false;
        }else{
            sb.append(ENDOFRECORD);
        }
        sb.append(str);
        }else sb.append(str);
        sb.append("\n");
    }

    String[] entries = sb.toString().split(ENDOFRECORD);
    HashMap<String, String> hm = new HashMap<String, String>();
    String author = "", Type = "", editor = "";
    for (int i = 0; i < entries.length; i++){
        hm.clear();
        author = "";
        Type = "";
        editor = "";
        boolean IsEditedBook = false;
        String[] fields = entries[i].trim().substring(1).split("\n%");
        
        for (int j = 0; j < fields.length; j++){

        if (fields[j].length() < 3) continue;

        

        String prefix = fields[j].substring(0, 1);

        String val = fields[j].substring(2);

        if (prefix.equals("A")){
            if (author.equals("")) author = val;
            else author += " and " + val;
        }else if (prefix.equals("E")){
            if (editor.equals("")) editor = val;
            else editor += " and " + val;
        }else if (prefix.equals("T")) hm.put("title", val);
        else if (prefix.equals("0")){
            if (val.indexOf("Journal") == 0) Type = "article";
            else if ((val.indexOf("Book Section") == 0)) Type = "incollection";
            else if ((val.indexOf("Book") == 0)) Type = "book";
            else if (val.indexOf("Edited Book") == 0) {
                Type = "book";
                IsEditedBook = true;
            }else if (val.indexOf("Conference") == 0) 
            Type = "inproceedings";
            else if (val.indexOf("Report") == 0) 
            Type = "techreport";
            else if (val.indexOf("Review") == 0)
                Type = "article";
            else if (val.indexOf("Thesis") == 0)
                Type = "phdthesis";
            else Type = "misc"; 
        }else if (prefix.equals("7")) hm.put("edition", val);
        else if (prefix.equals("C")) hm.put("address", val);
        else if (prefix.equals("D")) hm.put("year", val);
        else if (prefix.equals("8")) hm.put("date", val);
        else if (prefix.equals("J")){
            
            
            if (hm.get("journal") == null) hm.put("journal", val);
        }else if (prefix.equals("B")){
            
            
            if (Type.equals("article")) hm.put("journal", val);
            else if (Type.equals("book") || Type.equals("inbook")) hm.put(
                                          "series", val);
            else 
            hm.put("booktitle", val);
        }else if (prefix.equals("I")) {
            if (Type.equals("phdthesis"))
                hm.put("school", val);
            else
                 hm.put("publisher", val);
        }
            
        else if (prefix.equals("P")) hm.put("pages", val.replaceAll("([0-9]) *- *([0-9])","$1--$2"));
        else if (prefix.equals("V")) hm.put("volume", val);
        else if (prefix.equals("N")) hm.put("number", val);
        else if (prefix.equals("U")) hm.put("url", val);
        else if (prefix.equals("O")) hm.put("note", val);
        else if (prefix.equals("K")) hm.put("keywords", val);
        else if (prefix.equals("X")) hm.put("abstract", val);
        else if (prefix.equals("9")){
            
            if (val.indexOf("Ph.D.") == 0) Type = "phdthesis";
            if (val.indexOf("Masters") == 0) Type = "mastersthesis";
        }else if (prefix.equals("F")) hm.put(BibtexFields.KEY_FIELD, Util
                             .checkLegalKey(val));
        }

        
        
        if (IsEditedBook && editor.equals("")) {
           editor = author;
           author = "";
        }

        
        if (!author.equals("")) hm.put("author", fixAuthor(author));
        if (!editor.equals("")) hm.put("editor", fixAuthor(editor));
        BibtexEntry b = new BibtexEntry(BibtexFields.DEFAULT_BIBTEXENTRY_ID, Globals
                        .getEntryType(Type)); 
        
        b.setField(hm);
        
        if (b.getAllFields().size() > 0)
        	bibitems.add(b);

    }
    return bibitems;

    }

    
    private String fixAuthor(String s) {
        int index = s.indexOf(" and ");
        if (index >= 0)
            return AuthorList.fixAuthor_lastNameFirst(s);
        
        index = s.lastIndexOf(",");
        if (index == s.length()-1) {
            String mod = s.substring(0, s.length()-1).replaceAll(", ", " and ");
            return AuthorList.fixAuthor_lastNameFirst(mod);
        } else
            return AuthorList.fixAuthor_lastNameFirst(s);
    }

}
