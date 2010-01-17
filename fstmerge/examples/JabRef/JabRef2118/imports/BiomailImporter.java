package net.sf.jabref.imports; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Globals; 

import java.io.BufferedReader; 
import java.io.IOException; 
import java.io.InputStream; 

import java.util.ArrayList; 
import java.util.HashMap; 
import java.util.List; 
import java.util.regex.Pattern; 
import net.sf.jabref.BibtexFields; 



public  class  BiomailImporter  extends ImportFormat {
	
    
    public String getFormatName() {
        return "Biomail";
    }


	

    
    public String getCLIId() {
      return "biomail";
    }


	

    
    public boolean isRecognizedFormat(InputStream stream)
            throws IOException {
        
        BufferedReader in =
                new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
        Pattern pat1 = Pattern.compile("BioMail");

        String str;

        while ((str = in.readLine()) != null) {

            if (pat1.matcher(str).find())
                return true;
        }

        return false;
    }


	


    
    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {
        ArrayList<BibtexEntry> bibitems = new ArrayList<BibtexEntry>();
        StringBuffer sb = new StringBuffer();

        BufferedReader in =
                new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));

        String str;

        while ((str = in.readLine()) != null) {
            if (str.length() < 3)
                continue;

            
            if (str.substring(0, 6).equals("PMID- "))
                sb.append("::").append(str);
            else {
                String beg = str.substring(0, 6);

                if (beg.indexOf(" ") > 0) {
                    sb.append(" ## "); 
                    sb.append(str);
                } else {
                    sb.append("EOLEOL"); 
                    sb.append(str.trim());
                }
            }
        }

        String[] entries = sb.toString().split("::");

        
        HashMap<String, String> hm = new HashMap<String, String>();

        for (int i = 0; i < entries.length; i++) {
            String[] fields = entries[i].split(" ## ");

            if (fields.length == 0)
                fields = entries[i].split("\n");

            String Type = "";
            String pages = "";
            String shortauthor = "";
            String fullauthor = "";
            hm.clear();

            for (int j = 0; j < fields.length; j++) {
                System.out.println(">>>"+fields[j]+"<<<");

                
                if (fields[j].length() <= 2)
                    continue;

                String beg = fields[j].substring(0, 6);
                String value = fields[j].substring(6);
                value = value.trim();

                if (beg.equals("PT  - ")) {
                    
                    Type = "article"; 
                } else if (beg.equals("TY  - ")) {
                    if ("CONF".equals(value))
                        Type = "inproceedings";
                } else if (beg.equals("JO  - "))
                    hm.put("booktitle", value);
                else if (beg.equals("FAU - ")) {
                    String tmpauthor = value.replaceAll("EOLEOL", " and ");

                    
                    if (!"".equals(fullauthor))
                        fullauthor = fullauthor + " and " + tmpauthor;
                    else
                        fullauthor = tmpauthor;
                } else if (beg.equals("AU  - ")) {
                    String tmpauthor = value.replaceAll("EOLEOL", " and ").replaceAll(" ", ", ");

                    
                    if (!"".equals(shortauthor))
                        shortauthor = shortauthor + " and " + tmpauthor;
                    else
                        shortauthor = tmpauthor;
                } else if (beg.equals("TI  - "))
                    hm.put("title", value.replaceAll("EOLEOL", " "));
                else if (beg.equals("TA  - "))
                    hm.put("journal", value.replaceAll("EOLEOL", " "));
                else if (beg.equals("AB  - "))
                    hm.put("abstract", value.replaceAll("EOLEOL", " "));
                else if (beg.equals("PG  - "))
                    pages = value.replaceAll("-", "--");
                else if (beg.equals("IP  - "))
                    hm.put("number", value);
                else if (beg.equals("DP  - ")) {
                    String[] parts = value.split(" "); 
                    hm.put("year", parts[0]);
                } else if (beg.equals("VI  - "))
                    hm.put("volume", value);
                else if (beg.equals("AID - ")) {
                    String[] parts = value.split(" ");
                    if ("[doi]".equals(parts[1])) {
                        hm.put("doi", parts[0]);
                        hm.put("url", "http://dx.doi.org/" + parts[0]);
                    }
                }
            }

            if (!"".equals(pages))
                hm.put("pages", pages);
            if (!"".equals(fullauthor))
                hm.put("author", fullauthor);
            else if (!"".equals(shortauthor))
                hm.put("author", shortauthor);

            BibtexEntry b =
                    new BibtexEntry(BibtexFields.DEFAULT_BIBTEXENTRY_ID, Globals.getEntryType(Type)); 

            
            b.setField(hm);

            
            
            
            if (hm.get("author") != null || hm.get("title") != null)
                bibitems.add(b);
        }

        return bibitems;
    }



}
