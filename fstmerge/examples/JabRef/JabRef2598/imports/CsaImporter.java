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
import java.util.regex.Matcher; 
import net.sf.jabref.BibtexFields; 



public  class  CsaImporter  extends ImportFormat {
	

    
    private int line;

	

    
    private final static Pattern FIELD_PATTERN =
        Pattern.compile("^([A-Z][A-Z]): ([A-Z].*)$");

	
    private final static Pattern VOLNOPP_PATTERN =
        Pattern.compile("[;,\\.]\\s+(\\d+[A-Za-z]?)\\((\\d+(?:-\\d+)?)\\)(?:,\\s+|:)(\\d+-\\d+)");

	
    private final static Pattern PAGES_PATTERN =
        Pattern.compile("[;,\\.]\\s+(?:(\\[?[vn]\\.?p\\.?\\]?)|(?:pp?\\.?\\s+)(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)|(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)(?:\\s+pp?))");

	
    private final static Pattern VOLUME_PATTERN =
        Pattern.compile("[;,\\.]?\\s+[vV][oO][lL]\\.?\\s+(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)");

	
    private final static Pattern NUMBER_PATTERN =
        Pattern.compile("[;,\\.]\\s+(?:No|no|Part|part|NUMB)\\.?\\s+([A-Z]?\\d+(?:[/-]\\d+)?)");

	
    private final static Pattern DATE_PATTERN =
        Pattern.compile("[;,\\.]\\s+(?:(\\d+)\\s)?(?:([A-Z][a-z][a-z])[\\.,]*\\s)?\\(?(\\d\\d\\d\\d)\\)?(?:\\s([A-Z][a-z][a-z]))?(?:\\s+(\\d+))?");

	
    private final static Pattern LT_PATTERN =
        Pattern.compile("\\[Lt\\]");

	

    
    private static final String MONS =
        "jan feb mar apr may jun jul aug sep oct nov dec";

	
    private static final String[] MONTHS =
        { "January", "February", "March", "April", "May", "June",
          "July", "August", "September", "October", "November", "December" };

	

    
    public String getFormatName() {
        return "CSA";
    }


	

    
    public String getCLIId() {
      return "csa";
    }


	

    
    private String readLine(BufferedReader file) throws IOException {
        String str = file.readLine();
        if (str != null)
            line++;
        return str;
    }


	

    
    private void addNote(HashMap<String, String> hm, String note) {

        StringBuffer notebuf = new StringBuffer();
        if (hm.get("note") != null) {
            notebuf.append(hm.get("note"));
            notebuf.append("\n");
        }
        notebuf.append(note);
        hm.put("note", notebuf.toString());
    }


	

    
    private String parseDate(HashMap<String, String> hm, String fstr) {

        
        int match = -1;
        Matcher pm = DATE_PATTERN.matcher(fstr);
        while (pm.find()) {
            match = pm.start();

        }

        if (match == -1) {

            return fstr;
        }

        if (!pm.find(match)) {

            return fstr;
        }

        StringBuffer date = new StringBuffer();

        String day = pm.group(1);
        if (day == null)
            day = pm.group(5);
        else if (pm.group(5) != null)
            return fstr;	

        if (day != null && !day.equals("0")) {
            date.append(day);
            date.append(" ");
        } else
            day = null;

        String mon = pm.group(2);
        if (mon == null)
            mon = pm.group(4);
        else if (pm.group(4) != null)
            return fstr;	

        int idx = -1;
        if (mon != null) {
            String lmon = mon.toLowerCase();
            idx = MONS.indexOf(lmon);
            if (idx == -1)  
                return fstr;
            date.append(mon);
            date.append(" ");
            idx = idx / 4;
            hm.put("month", MONTHS[idx]);

        } else if (day != null) 
            return fstr;

        String year = pm.group(3);
        date.append(year);

        StringBuffer note = new StringBuffer();
        if (day != null && !day.equals("0")) {
            note.append("Source Date: ");
            note.append(date);
            note.append(".");
            addNote(hm, note.toString());
        }

        
        if (hm.get("year") != null) {
            String oyear = hm.get("year");
            if (!year.equals(oyear)) {
                note.setLength(0);
                note.append("Source Year: ");
                note.append(year);
                note.append(".");
                addNote(hm, note.toString());

            }
        } else
            hm.put("year", year);

        int len = fstr.length();
        StringBuffer newf = new StringBuffer();
        if (pm.start() > 0)
            newf.append(fstr.substring(0, pm.start()));
        if (pm.end() < len)
            newf.append(fstr.substring(pm.end(), len));
        return newf.toString();
    }


	

    
    public boolean isRecognizedFormat(InputStream stream) throws IOException {
        
        BufferedReader in =
            new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
        String str;
        while ((str = in.readLine()) != null) {
            if (str.equals("DN: Database Name"))
                return true;
        }

        return false;
    }


	

    
    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {
        ArrayList<BibtexEntry> bibitems = new ArrayList<BibtexEntry>();
        StringBuffer sb = new StringBuffer();
        HashMap<String, String> hm = new HashMap<String, String>();

        BufferedReader in =
            new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));

        String Type = null;
        String str;
        boolean first = true;
        line = 1;
        str = readLine(in);
        while (true) {
            if (str == null || str.length() == 0) {	
                if (!hm.isEmpty()) { 
                    if (Type == null) {
                        addNote(hm, "Publication Type: [NOT SPECIFIED]");
                        addNote(hm, "[PERHAPS NOT FULL FORMAT]");
                        Type = "article";
                    }

                    
                    if (Type.equals("article") &&
                        hm.get("booktitle") != null) {
                        String booktitle = hm.get("booktitle");
                        hm.remove("booktitle");
                        hm.put("journal", booktitle);
                    }

                    BibtexEntry b =
                        new BibtexEntry(BibtexFields.DEFAULT_BIBTEXENTRY_ID,
                                        Globals.getEntryType(Type));

                    
                    b.setField(hm);

                    bibitems.add(b);
                }
                hm.clear();	
                first = true;
                if (str == null)
                    break;	
                str = readLine(in);
                continue;
            }

            int fline = line;	
            Matcher fm = FIELD_PATTERN.matcher(str);
            if (fm.find()) {

                
                String fabbr = fm.group(1);
                String fname = fm.group(2);

                
                sb.setLength(0); 
                while ((str = readLine(in)) != null) {
                    if (! str.startsWith("    ")) 
                        break;	
                    if (sb.length() > 0) {
                        sb.append(" ");
                    }
                    sb.append(str.substring(4)); 
                }
                String fstr = sb.toString();
                if (fstr == null || fstr.length() == 0) {
                    int line1 = line - 1;
                    throw new IOException("illegal empty field at line " +
                                          line1);
                }
                
                fm = LT_PATTERN.matcher(fstr);
                if (fm.find())
                    fstr = fm.replaceAll("<");

                
                if (fabbr.equals("DN") &&
                    fname.equalsIgnoreCase("Database Name")) {
                    if (first == false) {
                        throw new IOException("format error at line " + fline +
                                              ": DN out of order");
                    }
                    first = false;
                } else if (first == true) {
                    throw new IOException("format error at line " + fline +
                                              ": missing DN");
                }

                if (fabbr.equals("PT")) {
                    Type = null;
                    String flow = fstr.toLowerCase();
                    String[] types = flow.split("; ");
                    for (int ii = 0; ii < types.length; ++ii) {
                        if ((types[ii].indexOf("article")>=0) ||
                            (types[ii].indexOf("journal article")>=0)) {
                            Type = "article";
                            break;
                        } else if (types[ii].equals("dissertation")) {
                            Type = "phdthesis";
                            break;
                        } else if (types[ii].equals("conference")) {
                            Type = "inproceedings";
                            break;
                        } else if (types[ii].equals("book monograph") &&
                                   Type == null) {
                            Type = "book";
                            break;
                        } else if (types[ii].equals("report") &&
                                   Type == null) {
                            Type = "techreport";
                            break;
                        }
                    }
                    if (Type == null) {
                        Type = "misc";
                    }

                }

                String ftype = null;
                if (fabbr.equals("AB"))
                    ftype = "abstract";
                else if (fabbr.equals("AF"))
                    ftype = "affiliation";
                else if (fabbr.equals("AU")) {
                    ftype = "author";
                    if (fstr.indexOf(";") >= 0)
                        fstr = fstr.replaceAll("; ", " and ");
                }
                else if (fabbr.equals("CA"))
                    ftype = "organization";
                else if (fabbr.equals("DE"))
                    ftype = "keywords";
                else if (fabbr.equals("DO"))
                    ftype = "doi";
                else if (fabbr.equals("ED"))
                    ftype = "editor";
                else if (fabbr.equals("IB"))
                    ftype = "ISBN";
                else if (fabbr.equals("IS"))
                    ftype = "ISSN";
                else if (fabbr.equals("JN"))
                    ftype = "journal";
                else if (fabbr.equals("LA"))
                    ftype = "language";
                else if (fabbr.equals("PB"))
                    ftype = "publisher";
                else if (fabbr.equals("PY")) {
                    ftype = "year";
                    if (hm.get("year") != null) {
                        String oyear = hm.get("year");
                        if (!fstr.equals(oyear)) {
                            StringBuffer note = new StringBuffer();
                            note.append("Source Year: ");
                            note.append(oyear);
                            note.append(".");
                            addNote(hm, note.toString());

                        }
                    }
                } else if (fabbr.equals("RL")) {
                    ftype = "url";
                    String[] lines = fstr.split(" ");
                    StringBuffer urls = new StringBuffer();
                    for (int ii = 0; ii < lines.length; ++ii) {
                        if (lines[ii].startsWith("[URL:"))
                            urls.append(lines[ii].substring(5));
                        else if (lines[ii].endsWith("]")) {
                            int len = lines[ii].length();
                            urls.append(lines[ii].substring(0, len - 1));
                            if (ii < lines.length - 1)
                                urls.append("\n");
                        } else
                            urls.append(lines[ii]);
                    }
                    fstr = urls.toString();
                } else if (fabbr.equals("SO")) {
                    ftype = "booktitle";

                    

                    
                    Matcher pm = VOLNOPP_PATTERN.matcher(fstr);
                    if (pm.find()) {
                        hm.put("volume", pm.group(1));
                        hm.put("number", pm.group(2));
                        hm.put("pages", pm.group(3));
                        fstr = pm.replaceFirst("");
                    }

                    
                    pm = PAGES_PATTERN.matcher(fstr);
                    StringBuffer pages = new StringBuffer();
                    while (pm.find()) {
                        if (pages.length() > 0)
                            pages.append(",");
                        String pp = pm.group(1);
                        if (pp == null)
                            pp = pm.group(2);
                        if (pp == null)
                            pp = pm.group(3);
                        pages.append(pp);
                        fstr = pm.replaceFirst("");
                        pm = PAGES_PATTERN.matcher(fstr);
                    }
                    if (pages.length() > 0)
                        hm.put("pages", pages.toString());

                    
                    pm = VOLUME_PATTERN.matcher(fstr);
                    if (pm.find()) {
                        hm.put("volume", pm.group(1));
                        fstr = pm.replaceFirst("");
                    }

                    
                    pm = NUMBER_PATTERN.matcher(fstr);
                    if (pm.find()) {
                        hm.put("number", pm.group(1));
                        fstr = pm.replaceFirst("");
                    }

                    
                    fstr = parseDate(hm, fstr);

                    
                    Pattern pp = Pattern.compile(",?\\s*$");
                    pm = pp.matcher(fstr);
                    if (pm.find())
                        fstr = pm.replaceFirst("");

                    if (fstr.equals(""))
                        continue;

                } else if (fabbr.equals("TI"))
                    ftype = "title";
                else if (fabbr.equals("RE"))
                    continue;	

                if (ftype != null) {
                    hm.put(ftype, fstr);
                } else {
                    StringBuffer val = new StringBuffer();
                    val.append(fname);
                    val.append(": ");
                    val.append(fstr);
                    val.append(".");
                    addNote(hm, val.toString());
                }
            } else
                str = readLine(in);
        }

        return bibitems;
    }



}
