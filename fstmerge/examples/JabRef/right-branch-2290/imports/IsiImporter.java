package net.sf.jabref.imports;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexFields;
import net.sf.jabref.Globals;
import net.sf.jabref.Util;
import net.sf.jabref.util.CaseChanger;


public class IsiImporter extends ImportFormat {
	
	public String getFormatName() {
		return "ISI";
	}

	
	public String getCLIId() {
		return "isi";
	}

    
    
    static final Pattern isiPattern = Pattern.compile("FN ISI Export Format|VR 1.|PY \\d{4}");

	
	public boolean isRecognizedFormat(InputStream stream) throws IOException {

		BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));

		String str;

        while ((str = in.readLine()) != null) {

			
			if (isiPattern.matcher(str).find())
				return true;
		}

		return false;
	}

	static Pattern subsupPattern = Pattern.compile("/(sub|sup)\\s+(.*?)\\s*/");

	static public void processSubSup(HashMap map) {

		String[] subsup = { "title", "abstract", "review", "notes" };

		for (int i = 0; i < subsup.length; i++) {
			if (map.containsKey(subsup[i])) {

				Matcher m = subsupPattern.matcher((String) map.get(subsup[i]));
				StringBuffer sb = new StringBuffer();

				while (m.find()) {

					String group2 = m.group(2);
					group2 = group2.replaceAll("\\$", "\\\\\\\\\\\\\\$"); 
					
					
					if (group2.length() > 1) {
						group2 = "{" + group2 + "}";
					}
					if (m.group(1).equals("sub")) {
						m.appendReplacement(sb, "\\$_" + group2 + "\\$");
					} else {
						m.appendReplacement(sb, "\\$^" + group2 + "\\$");
					}
				}
				m.appendTail(sb);
				map.put(subsup[i], sb.toString());
			}
		}
	}

	static public void processCapitalization(HashMap map) {

		String[] subsup = { "title", "journal", "publisher" };

		for (int i = 0; i < subsup.length; i++) {

			if (map.containsKey(subsup[i])) {

				String s = (String) map.get(subsup[i]);

				if (s.toUpperCase().equals(s)) {
					s = CaseChanger.changeCase(s, CaseChanger.UPPER_EACH_FIRST);
					map.put(subsup[i], s);
				}
			}
		}
	}

	
	public List importEntries(InputStream stream) throws IOException {
		if (stream == null) {
			throw new IOException("No stream given.");
		}

		ArrayList bibitems = new ArrayList();
		StringBuffer sb = new StringBuffer();

		BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));

		
		
		String str;

		while ((str = in.readLine()) != null) {
			if (str.length() < 3)
				continue;

			
			if (str.substring(0, 3).equals("PT "))
				sb.append("::").append(str);
			else {
				String beg = str.substring(0, 3).trim();

				
				
				
				if (beg.length() == 2) {
					sb.append(" ## "); 
					sb.append(str);
				} else {
					sb.append("EOLEOL"); 
					sb.append(str.trim()); 
				}
			}
		}

		String[] entries = sb.toString().split("::");

		HashMap hm = new HashMap();

		
		for (int i = 0; i < entries.length; i++) {
			String[] fields = entries[i].split(" ## ");

			if (fields.length == 0)
				fields = entries[i].split("\n");

			String Type = "";
			String PT = "";
			String pages = "";
			hm.clear();

			nextField: for (int j = 0; j < fields.length; j++) {
				
				if (fields[j].length() <= 2)
					continue;

				String beg = fields[j].substring(0, 2);
				String value = fields[j].substring(3);
				if (value.startsWith(" - ")) {
					value = value.substring(3);
				}
				value = value.trim();

				if (beg.equals("PT")) {
					if (value.startsWith("J")) {
						PT = "article";
					} else {
						PT = value;
					}
					Type = "article"; 
				} else if (beg.equals("TY")) {
					if ("JOUR".equals(value))
						Type = "article";
					else if ("CONF".equals(value))
						Type = "inproceedings";
				} else if (beg.equals("JO"))
					hm.put("booktitle", value);
				else if (beg.equals("AU")) {
					String author = isiAuthorsConvert(value.replaceAll("EOLEOL", " and "));

					
					if (hm.get("author") != null)
						author = (String) hm.get("author") + " and " + author;

					hm.put("author", author);
				} else if (beg.equals("TI"))
					hm.put("title", value.replaceAll("EOLEOL", " "));
				else if (beg.equals("SO") || beg.equals("JA"))
					hm.put("journal", value.replaceAll("EOLEOL", " "));
				else if (beg.equals("ID") || beg.equals("KW")) {
				
					value = value.replaceAll("EOLEOL", " ");
					String existingKeywords = (String) hm.get("keywords");
					if (existingKeywords != null && existingKeywords.indexOf(value) == -1) {
						existingKeywords += ", " + value;
					} else {
						existingKeywords = value;
					}
					hm.put("keywords", existingKeywords);

				} else if (beg.equals("AB"))
					hm.put("abstract", value.replaceAll("EOLEOL", " "));
				else if (beg.equals("BP") || beg.equals("BR") || beg.equals("SP"))
					pages = value;
				else if (beg.equals("EP")) {
					int detpos = value.indexOf(' ');

					
					if (detpos != -1 && value.substring(0, detpos).trim().length() > 0)
						value = value.substring(0, detpos);

					pages = pages + "--" + value;
				} else if (beg.equals("PS")) {
					pages = parsePages(value);
				} else if (beg.equals("AR"))
					pages = value;
				else if (beg.equals("IS"))
					hm.put("number", value);
				else if (beg.equals("PY"))
					hm.put("year", value);
				else if (beg.equals("VL"))
					hm.put("volume", value);
				else if (beg.equals("PU"))
					hm.put("publisher", value);
				else if (beg.equals("PD")) {

					String month = parseMonth(value);
					if (month != null) {
						hm.put("month", month);
						continue nextField;
					}

				} else if (beg.equals("DT")) {
					Type = value;
					if (Type.equals("Review")) {
						Type = "article"; 
					} else if (Type.startsWith("Article") || Type.startsWith("Journal")
						|| PT.equals("article")) {
						Type = "article";
						continue;
					} else {
						Type = "misc";
					}
				} else if (beg.equals("CR")) {
					hm.put("CitedReferences", value.replaceAll("EOLEOL", " ; ").trim());
				} else {
					
					if (beg.equals("ER") || beg.equals("EF") || beg.equals("VR")
						|| beg.equals("FN"))
						continue nextField;
					hm.put(beg, value);
				}
			}

			if (!"".equals(pages))
				hm.put("pages", pages);

			
			if (hm.size() == 0)
				continue;

			BibtexEntry b = new BibtexEntry(BibtexFields.DEFAULT_BIBTEXENTRY_ID, Globals
				.getEntryType(Type));
			

			
			ArrayList toRemove = new ArrayList();
			for (Iterator it = hm.keySet().iterator(); it.hasNext();) {
				Object key = it.next();
				String content = (String) hm.get(key);
				if ((content == null) || (content.trim().length() == 0))
					toRemove.add(key);
			}
			for (Iterator iterator = toRemove.iterator(); iterator.hasNext();) {
				hm.remove(iterator.next());

			}

			
			processSubSup(hm);
			processCapitalization(hm);

			b.setField(hm);

			bibitems.add(b);
		}

		return bibitems;
	}

	public static String parsePages(String value) {
		int lastDash = value.lastIndexOf("-");
		return value.substring(0, lastDash) + "--" + value.substring(lastDash + 1);
	}

	public static String parseMonth(String value) {

		String[] parts = value.split("\\s|\\-");
		for (int ii = 0; ii < parts.length; ii++) {
			if (Globals.MONTH_STRINGS.containsKey(parts[ii].toLowerCase())) {
				return "#" + parts[ii].toLowerCase() + "#";
			}
		}

		
		for (int ii = 0; ii < parts.length; ii++) {
			int number;
			try {
				number = Integer.parseInt(parts[ii]);
				if (number >= 1 && number <= 12) {
					return "#" + Globals.MONTHS[number - 1] + "#";
				}
			} catch (NumberFormatException e) {

			}
		}
		return null;
	}

	
	public static String isiAuthorConvert(String author) {

		String[] s = author.split(",");
		if (s.length != 2)
			return author;

		StringBuffer sb = new StringBuffer();

		String last = s[0].trim();
		sb.append(last).append(", ");

		String first = s[1].trim();

		String[] firstParts = first.split("\\s+");

		for (int i = 0; i < firstParts.length; i++) {

			first = firstParts[i];

			
			if (first.toUpperCase().equals(first)) {
				first = first.replaceAll("\\.", "");
				for (int j = 0; j < first.length(); j++) {
					sb.append(first.charAt(j)).append(".");

					if (j < first.length() - 1)
						sb.append(" ");
				}
			} else {
				sb.append(first);
			}
			if (i < firstParts.length - 1) {
				sb.append(" ");
			}
		}
		return sb.toString();

	}

	public static String[] isiAuthorsConvert(String[] authors) {

		String[] result = new String[authors.length];
		for (int i = 0; i < result.length; i++) {
			result[i] = isiAuthorConvert(authors[i]);
		}
		return result;
	}

	public static String isiAuthorsConvert(String authors) {
		String[] s = isiAuthorsConvert(authors.split(" and |;"));
		return Util.join(s, " and ", 0, s.length);
	}

}
