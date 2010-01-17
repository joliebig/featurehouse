
package net.sf.jabref.export; 

import javax.xml.transform.*; 
import javax.xml.transform.dom.*; 
import javax.xml.transform.stream.*; 
import javax.swing.*; 

import java.io.*; 
import java.net.URL; 
import java.util.*; 
import java.util.regex.Pattern; 
import java.util.regex.Matcher; 
import java.nio.charset.UnsupportedCharsetException; 

import net.sf.jabref.export.layout.Layout; 
import net.sf.jabref.export.layout.LayoutHelper; 
import net.sf.jabref.export.layout.LayoutFormatter; 
import net.sf.jabref.export.layout.format.*; 

import net.sf.jabref.*; 
import net.sf.jabref.mods.*; 
import ca.odell.glazedlists.BasicEventList; 
import ca.odell.glazedlists.SortedList; 
import net.sf.jabref.export.layout.format.AuthorLastFirst; 
import net.sf.jabref.export.layout.format.RemoveLatexCommands; 
import net.sf.jabref.export.layout.format.ResolvePDF; 

public  class  FileActions {
	

    private static Pattern refPat = Pattern.compile("(#[A-Za-z]+#)");

	 


    private static void writePreamble(Writer fw, String preamble) throws IOException {
    if (preamble != null) {
        fw.write("@PREAMBLE{");
        fw.write(preamble);
        fw.write("}"+Globals.NEWLINE +Globals.NEWLINE);
    }
    }


	

    
    private static void writeStrings(Writer fw, BibtexDatabase database) throws IOException {
        List<BibtexString> strings = new ArrayList<BibtexString>();
        for (String s : database.getStringKeySet()) {
            strings.add(database.getString(s));
        }
        Collections.sort(strings, new BibtexStringComparator(false));
        
        HashMap<String, BibtexString> remaining = new HashMap<String, BibtexString>();
        for (Iterator<BibtexString> i=strings.iterator(); i.hasNext();) {
            BibtexString string = i.next();
            remaining.put(string.getName(), string);
        }
        for (Iterator<BibtexString> i = strings.iterator(); i.hasNext();) {
            BibtexString bs = i.next();
            if (remaining.containsKey(bs.getName()))
                writeString(fw, bs, remaining);
        }
    }


	

    


	

    
    private static void writeBibFileHeader(Writer out, String encoding) throws IOException {
        out.write("% ");
      out.write(GUIGlobals.SIGNATURE);
      out.write(" "+GUIGlobals.version+"."+Globals.NEWLINE + "% " + 
              GUIGlobals.encPrefix+encoding+Globals.NEWLINE +Globals.NEWLINE);
    }


	

    
    public static SaveSession saveDatabase(BibtexDatabase database,
		MetaData metaData, File file, JabRefPreferences prefs,
		boolean checkSearch, boolean checkGroup, String encoding)
		throws SaveException {
    	
		TreeMap<String, BibtexEntryType> types = new TreeMap<String, BibtexEntryType>();
		
		boolean backup = prefs.getBoolean("backup");

		SaveSession session;
		BibtexEntry exceptionCause = null;
		try {
			session = new SaveSession(file, encoding, backup);
		} catch (Throwable e) {
			System.err.println("Error from encoding: '" + encoding + "' Len: "
				+ encoding.length());
			
			
			
			
			e.printStackTrace();
			throw new SaveException(e.getMessage());
		}

		try {

			
			
			VerifyingWriter fw = session.getWriter();

			
			writeBibFileHeader(fw, encoding);

			
			writePreamble(fw, database.getPreamble());

			
			writeStrings(fw, database);

			
			
			
			
			List<BibtexEntry> sorter = getSortedEntries(database, null, true);

			FieldFormatter ff = new LatexFieldFormatter();

			for (BibtexEntry be : sorter) {
				exceptionCause = be;

				
				
				
				BibtexEntryType tp = be.getType();

				if (BibtexEntryType.getStandardType(tp.getName()) == null) {
					types.put(tp.getName(), tp);
				}

				
				boolean write = true;

				if (checkSearch && !nonZeroField(be, BibtexFields.SEARCH)) {
					write = false;
				}

				if (checkGroup && !nonZeroField(be, BibtexFields.GROUPSEARCH)) {
					write = false;
				}

				if (write) {
					be.write(fw, ff, true);
					fw.write(Globals.NEWLINE);
				}
			}

			
			if (metaData != null) {
				metaData.writeMetaData(fw);
			}

			
			if (types.size() > 0) {
				for (Iterator<String> i = types.keySet().iterator(); i
					.hasNext();) {
					CustomEntryType tp = (CustomEntryType) types.get(i.next());
					tp.save(fw);
					fw.write(Globals.NEWLINE);
				}

			}

			fw.close();
		} catch (Throwable ex) {
			ex.printStackTrace();
			try {
				session.cancel();
				
			} catch (IOException e) {
				
				e.printStackTrace();
				throw new SaveException(
					ex.getMessage()
						+ "\n"
						+ Globals
							.lang("Warning: could not complete file repair; your file may "
								+ "have been corrupted. Error message: ")
						+ e.getMessage());
			}
			throw new SaveException(ex.getMessage(), exceptionCause);
		}

		return session;

	}


	

    
    @SuppressWarnings("unchecked")
	public static SaveSession savePartOfDatabase(BibtexDatabase database, MetaData metaData,
                                                 File file, JabRefPreferences prefs, BibtexEntry[] bes, String encoding) throws SaveException
    {

    TreeMap<String, BibtexEntryType> types = new TreeMap<String, BibtexEntryType>(); 
																						
																						
																						
																						
																						
    

        BibtexEntry be = null;
        boolean backup = prefs.getBoolean("backup");

        SaveSession session;
        try {
            session = new SaveSession(file, encoding, backup);
        } catch (IOException e) {
            throw new SaveException(e.getMessage());
        }

        try
        {

            
            VerifyingWriter fw = session.getWriter();

            
            writeBibFileHeader(fw, encoding);

            
            writePreamble(fw, database.getPreamble());

            
        writeStrings(fw, database);

            
            
            
            
        String pri, sec, ter;

        boolean priD, secD, terD;
        if (!prefs.getBoolean("saveInStandardOrder")) {
        
        pri = prefs.get("priSort");
        sec = prefs.get("secSort");
        
        ter = prefs.get("terSort");
        priD = prefs.getBoolean("priDescending");
        secD = prefs.getBoolean("secDescending");
        terD = prefs.getBoolean("terDescending");
        } else {
        
        pri = "author";
        sec = "editor";
        ter = "year";
        priD = false;
        secD = false;
        terD = true;
        }

        List<Comparator<BibtexEntry>> comparators = new ArrayList<Comparator<BibtexEntry>>();
        comparators.add(new CrossRefEntryComparator());
        comparators.add(new FieldComparator(pri, priD));
        comparators.add(new FieldComparator(sec, secD));
        comparators.add(new FieldComparator(ter, terD));
        comparators.add(new FieldComparator(BibtexFields.KEY_FIELD));
        
        BasicEventList entryList = new BasicEventList();
        SortedList sorter = new SortedList(entryList, new FieldComparatorStack<BibtexEntry>(comparators));

        if ((bes != null) && (bes.length > 0))
        for (int i=0; i<bes.length; i++) {
            sorter.add(bes[i]);
        }

            FieldFormatter ff = new LatexFieldFormatter();

            for (Iterator<BibtexEntry> i = sorter.iterator(); i.hasNext();)
            {
                be = (i.next());

        
        
        
        BibtexEntryType tp = be.getType();
        if (BibtexEntryType.getStandardType(tp.getName()) == null) {
            types.put(tp.getName(), tp);
        }

        be.write(fw, ff, true);
        fw.write(Globals.NEWLINE);
        }

            
            if (metaData != null)
            {
                metaData.writeMetaData(fw);
            }

        
        if (types.size() > 0) {
        for (Iterator<String> i=types.keySet().iterator(); i.hasNext();) {
            CustomEntryType tp = (CustomEntryType)types.get(i.next());
            tp.save(fw);
            fw.write(Globals.NEWLINE);
        }

        }

            fw.close();
        }
         catch (Throwable ex)
        {
            try {
                session.cancel();
                
            } catch (IOException e) {
                
                e.printStackTrace();
                throw new SaveException(ex.getMessage()+"\n"+
                        Globals.lang("Warning: could not complete file repair; your file may "
                        +"have been corrupted. Error message: ")+e.getMessage());
            }
            throw new SaveException(ex.getMessage(), be);
    }

        return session;

    }


	

    public static void exportToCSV(BibtexDatabase database,
                                   File outFile, JabRefPreferences prefs)
        throws Exception {

    HashMap<String, Object> fieldFormatters = new HashMap<String, Object>();
    fieldFormatters.put("default", new RemoveLatexCommands());
    fieldFormatters.put("author", new Object[] {new AuthorLastFirst(),
                            new RemoveLatexCommands()});
    fieldFormatters.put("pdf", new ResolvePDF());

    String SEPARATOR = "\t";
    List<BibtexEntry> sorted = getSortedEntries(database, null, true);
    Set<String> fields = new TreeSet<String>();
    for (int i=0, len=BibtexFields.numberOfPublicFields(); i<len; i++)
        fields.add(BibtexFields.getFieldName(i));

    
    Object[] o = fields.toArray();
    FileWriter out = new FileWriter(outFile);
    out.write((String)o[0]);
    for (int i=1; i<o.length; i++) {
        out.write(SEPARATOR+(String)o[i]);
    }
    out.write(Globals.NEWLINE);

    for (BibtexEntry entry : sorted){
        writeField(database, entry, (String)o[0], fieldFormatters, out);
        for (int j=1; j<o.length; j++) {
        out.write(SEPARATOR);
        writeField(database, entry, (String)o[j], fieldFormatters, out);
        }
        out.write(Globals.NEWLINE);
    }


    out.close();
    


    }


	



    


	

    
    public static Reader getReader(String name) throws IOException {
      Reader reader = null;
      
      URL reso = Globals.class.getResource(name);

      
      if (reso != null) {
        try {
          reader = new InputStreamReader(reso.openStream());
        } catch (FileNotFoundException ex) {
          throw new IOException(Globals.lang("Could not find layout file")+": '"+name+"'.");
        }
      } else {
        File f = new File(name);
        try {
          reader = new FileReader(f);
        } catch (FileNotFoundException ex) {
          throw new IOException(Globals.lang("Could not find layout file")+": '"+name+"'.");
        }
      }

      return reader;
    }


	

    
    


	

    
    private static boolean nonZeroField(BibtexEntry be, String field)
    {
        String o = (String) (be.getField(field));

        return ((o != null) && !o.equals("0"));
    }


	

    private static void writeString(Writer fw, BibtexString bs, HashMap<String, BibtexString> remaining) throws IOException {
        
        remaining.remove(bs.getName());

        
        
        
        String content = bs.getContent();
        Matcher m;
        while ((m = refPat.matcher(content)).find()) {
            String foundLabel = m.group(1);
            int restIndex = content.indexOf(foundLabel)+foundLabel.length();
            content = content.substring(restIndex);
            Object referred = remaining.get(foundLabel.substring(1, foundLabel.length()-1));
            
            if (referred != null)
                writeString(fw, (BibtexString)referred, remaining);
        }

        fw.write("@STRING{" + bs.getName() + " = ");
        if (!bs.getContent().equals(""))
            fw.write((new LatexFieldFormatter()).format(bs.getContent(), Globals.BIBTEX_STRING));
        else
            fw.write("{}");

        fw.write("}" + Globals.NEWLINE + Globals.NEWLINE);
    }

	



    private static void writeField(BibtexDatabase database, BibtexEntry entry, String field,
                                   HashMap<String, Object> fieldFormatters, Writer out)
    throws IOException {

    String s = BibtexDatabase.getResolvedField(field, entry, database);
    	
    Object form = fieldFormatters.get(field);
    if (form == null)
        form = fieldFormatters.get("default");

    if (form instanceof LayoutFormatter) {
        s = ((LayoutFormatter)form).format(s);
    } else if (form instanceof Object[]) {
        Object[] forms = (Object[])form;
        for (int i=0; i<forms.length; i++) {
        s = ((LayoutFormatter)(forms[i])).format(s);
        }
    }

    out.write(s);
    }

	

    
    @SuppressWarnings("unchecked")
	public static List<BibtexEntry> getSortedEntries(BibtexDatabase database, Set<String> keySet, boolean isSaveOperation) {
        FieldComparatorStack<BibtexEntry> comparatorStack = null;

        if (Globals.prefs.getBoolean("saveInOriginalOrder")) {
            
            
            List<Comparator<BibtexEntry>> comparators = new ArrayList<Comparator<BibtexEntry>>();
            comparators.add(new CrossRefEntryComparator());
            comparators.add(new IdComparator());
            comparatorStack = new FieldComparatorStack<BibtexEntry>(comparators);

        } else {
            String pri, sec, ter;
            boolean priD, secD, terD = false;


            if (!isSaveOperation || !Globals.prefs.getBoolean("saveInStandardOrder")) {
                
                pri = Globals.prefs.get("priSort");
                sec = Globals.prefs.get("secSort");
                
                ter = Globals.prefs.get("terSort");
                priD = Globals.prefs.getBoolean("priDescending");
                secD = Globals.prefs.getBoolean("secDescending");
                terD = Globals.prefs.getBoolean("terDescending");
            } else {
                
                pri = "author";
                sec = "editor";
                ter = "year";
                priD = false;
                secD = false;
                terD = true;
            }

            List<Comparator<BibtexEntry>> comparators = new ArrayList<Comparator<BibtexEntry>>();
            if (isSaveOperation)
                comparators.add(new CrossRefEntryComparator());
            comparators.add(new FieldComparator(pri, priD));
            comparators.add(new FieldComparator(sec, secD));
            comparators.add(new FieldComparator(ter, terD));
            comparators.add(new FieldComparator(BibtexFields.KEY_FIELD));

            comparatorStack = new FieldComparatorStack<BibtexEntry>(comparators);
        }
        
        BasicEventList entryList = new BasicEventList();
        SortedList sorter = new SortedList(entryList, comparatorStack);

        if (keySet == null)
            keySet = database.getKeySet();

        if (keySet != null) {
            Iterator<String> i = keySet.iterator();

            for (; i.hasNext();) {
                sorter.add(database.getEntryById((i.next())));
            }
        }
        return sorter;
    }


}
