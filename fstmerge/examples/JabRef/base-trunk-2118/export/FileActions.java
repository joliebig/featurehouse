
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

public class FileActions
{

    private static Pattern refPat = Pattern.compile("(#[A-Za-z]+#)"); 


    private static void writePreamble(Writer fw, String preamble) throws IOException {
    if (preamble != null) {
        fw.write("@PREAMBLE{");
        fw.write(preamble);
        fw.write("}"+Globals.NEWLINE +Globals.NEWLINE);
    }
    }

    
    private static void writeStrings(Writer fw, BibtexDatabase database) throws IOException {
        List strings = new ArrayList();
        for (Iterator i = database.getStringKeySet().iterator(); i.hasNext();) {
            strings.add(database.getString(i.next()));
        }
        Collections.sort(strings, new BibtexStringComparator(false));
        
        HashMap remaining = new HashMap();
        for (Iterator i=strings.iterator(); i.hasNext();) {
            BibtexString string = (BibtexString)i.next();
            remaining.put(string.getName(), string);
        }
        for (Iterator i = strings.iterator(); i.hasNext();) {
            BibtexString bs = (BibtexString) i.next();
            if (remaining.containsKey(bs.getName()))
                writeString(fw, bs, remaining);
        }
    }

    private static void writeString(Writer fw, BibtexString bs, HashMap remaining) throws IOException {
        
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

    
    private static void writeBibFileHeader(Writer out, String encoding) throws IOException {
      out.write(GUIGlobals.SIGNATURE);
      out.write(" "+GUIGlobals.version+"."+Globals.NEWLINE +GUIGlobals.encPrefix+encoding+Globals.NEWLINE +Globals.NEWLINE);
    }

    
    public static SaveSession saveDatabase(BibtexDatabase database, MetaData metaData,
                                           File file, JabRefPreferences prefs, boolean checkSearch,
                                           boolean checkGroup, String encoding) throws SaveException
    {
        BibtexEntry be = null;
        TreeMap types = new TreeMap(); 
        

        boolean backup = prefs.getBoolean("backup");

        SaveSession session;
        try {
            session = new SaveSession(file, encoding, backup);
        } catch (Throwable e) {
            System.err.println("Error from encoding: '"+encoding+"' Len: "+encoding.length());
            
            
            
            
            e.printStackTrace();
            throw new SaveException(e.getMessage());
        }

        try
        {

            
            VerifyingWriter fw = session.getWriter();

            
            writeBibFileHeader(fw, encoding);

            
            writePreamble(fw, database.getPreamble());

            
            writeStrings(fw, database);

            
            
            
            
            List sorter = getSortedEntries(database, null, true);

            FieldFormatter ff = new LatexFieldFormatter();

            for (Iterator i = sorter.iterator(); i.hasNext();) {
                be = (BibtexEntry) (i.next());

        
        
        
        BibtexEntryType tp = be.getType();

        if (BibtexEntryType.getStandardType(tp.getName()) == null) {
            types.put(tp.getName(), tp);
        }

                
                boolean write = true;

                if (checkSearch && !nonZeroField(be, BibtexFields.SEARCH))
                {
                    write = false;
                }

                if (checkGroup && !nonZeroField(be, BibtexFields.GROUPSEARCH))
                {
                    write = false;
                }

                if (write)
                {
                    be.write(fw, ff, true);
                    fw.write(Globals.NEWLINE);
                }
            }

            
            if (metaData != null)
            {
                metaData.writeMetaData(fw);
            }

        
        if (types.size() > 0) {
        for (Iterator i=types.keySet().iterator(); i.hasNext();) {
            CustomEntryType tp = (CustomEntryType)types.get(i.next());
            tp.save(fw);
            fw.write(Globals.NEWLINE);
        }

        }


            fw.close();
        }
         catch (Throwable ex)
        {
            ex.printStackTrace();
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

    
    public static SaveSession savePartOfDatabase(BibtexDatabase database, MetaData metaData,
                                                 File file, JabRefPreferences prefs, BibtexEntry[] bes, String encoding) throws SaveException
    {

    TreeMap types = new TreeMap(); 
    

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

        boolean priD, secD, terD, priBinary=false;
        if (!prefs.getBoolean("saveInStandardOrder")) {
        
            priBinary = prefs.getBoolean("priBinary");
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

        List comparators = new ArrayList();
        comparators.add(new CrossRefEntryComparator());
        comparators.add(new FieldComparator(pri, priD));
        comparators.add(new FieldComparator(sec, secD));
        comparators.add(new FieldComparator(ter, terD));
        comparators.add(new FieldComparator(BibtexFields.KEY_FIELD));
        
        BasicEventList entryList = new BasicEventList();
        SortedList sorter = new SortedList(entryList, new FieldComparatorStack(comparators));

        if ((bes != null) && (bes.length > 0))
        for (int i=0; i<bes.length; i++) {
            sorter.add(bes[i]);
        }

            FieldFormatter ff = new LatexFieldFormatter();

            for (Iterator i = sorter.iterator(); i.hasNext();)
            {
                be = (BibtexEntry) (i.next());

        
        
        
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
        for (Iterator i=types.keySet().iterator(); i.hasNext();) {
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

    HashMap fieldFormatters = new HashMap();
    fieldFormatters.put("default", new RemoveLatexCommands());
    fieldFormatters.put("author", new Object[] {new AuthorLastFirst(),
                            new RemoveLatexCommands()});
    fieldFormatters.put("pdf", new ResolvePDF());

    String SEPARATOR = "\t";
    List sorted = getSortedEntries(database, null, true);
    Set fields = new TreeSet();
    for (int i=0, len=BibtexFields.numberOfPublicFields(); i<len; i++)
        fields.add(BibtexFields.getFieldName(i));

    
    Object[] o = fields.toArray();
    FileWriter out = new FileWriter(outFile);
    out.write((String)o[0]);
    for (int i=1; i<o.length; i++) {
        out.write(SEPARATOR+(String)o[i]);
    }
    out.write(Globals.NEWLINE);

    for (Iterator i=sorted.iterator(); i.hasNext();) {
        BibtexEntry entry = (BibtexEntry)i.next();
        writeField(database, entry, (String)o[0], fieldFormatters, out);
        for (int j=1; j<o.length; j++) {
        out.write(SEPARATOR);
        writeField(database, entry, (String)o[j], fieldFormatters, out);
        }
        out.write(Globals.NEWLINE);
    }


    out.close();
    


    }



    private static void writeField(BibtexDatabase database, BibtexEntry entry, String field,
                                   HashMap fieldFormatters, Writer out)
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

    
    public static List getSortedEntries(BibtexDatabase database, Set keySet, boolean isSaveOperation) {
        FieldComparatorStack comparatorStack = null;

        if (Globals.prefs.getBoolean("saveInOriginalOrder")) {
            
            
            List comparators = new ArrayList();
            comparators.add(new CrossRefEntryComparator());
            comparators.add(new IdComparator());
            comparatorStack = new FieldComparatorStack(comparators);

        } else {
            String pri, sec, ter;
            boolean priD, secD, terD, priBinary = false;


            if (!isSaveOperation || !Globals.prefs.getBoolean("saveInStandardOrder")) {
                
                priBinary = Globals.prefs.getBoolean("priBinary");
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

            List comparators = new ArrayList();
            if (isSaveOperation)
                comparators.add(new CrossRefEntryComparator());
            comparators.add(new FieldComparator(pri, priD));
            comparators.add(new FieldComparator(sec, secD));
            comparators.add(new FieldComparator(ter, terD));
            comparators.add(new FieldComparator(BibtexFields.KEY_FIELD));

            comparatorStack = new FieldComparatorStack(comparators);
        }
        
        BasicEventList entryList = new BasicEventList();
        SortedList sorter = new SortedList(entryList, comparatorStack);

        if (keySet == null)
            keySet = database.getKeySet();

        if (keySet != null) {
            Iterator i = keySet.iterator();

            for (; i.hasNext();) {
                sorter.add(database.getEntryById((String) (i.next())));
            }
        }
        return sorter;
    }

    
    private static boolean nonZeroField(BibtexEntry be, String field)
    {
        String o = (String) (be.getField(field));

        return ((o != null) && !o.equals("0"));
    }
}






