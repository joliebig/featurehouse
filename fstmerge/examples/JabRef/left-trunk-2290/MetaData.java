
package net.sf.jabref;

import java.io.*;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.Vector;

import net.sf.jabref.groups.GroupTreeNode;
import net.sf.jabref.groups.VersionHandling;

public class MetaData implements Iterable<String> {
    private HashMap<String, Vector<String>> metaData = new HashMap<String, Vector<String>>();
    private StringReader data;
    private GroupTreeNode groupsRoot = null;
    private File file = null; 
    
    
    public MetaData(HashMap<String, String> inData, BibtexDatabase db) {
        boolean groupsTreePresent = false;
        Vector<String> flatGroupsData = null;
        Vector<String> treeGroupsData = null;
        
        
        int groupsVersionOnDisk = 0;
        
        if (inData != null) 
        	for (String key : inData.keySet()){
            data = new StringReader(inData.get(key));
            String unit;
            Vector<String> orderedData = new Vector<String>();
            
            try {
                while ((unit = getNextUnit(data)) != null) {
                    orderedData.add(unit);
                }
            } catch (IOException ex) {
                System.err.println("Weird error while parsing meta data.");
            }
            if (key.equals("groupsversion")) {
                if (orderedData.size() >= 1)
                    groupsVersionOnDisk = Integer.parseInt(orderedData.firstElement().toString());
            } else if (key.equals("groupstree")) {
                groupsTreePresent = true;
                treeGroupsData = orderedData; 
                
                
            } else if (key.equals("groups")) {
                flatGroupsData = orderedData;
            } else {
                putData(key, orderedData);
            }
        }
        
        
        if (groupsTreePresent)
            putGroups(treeGroupsData, db, groupsVersionOnDisk);
        
        if (!groupsTreePresent && flatGroupsData != null) {
            groupsRoot = VersionHandling.importFlatGroups(flatGroupsData);
        }
    }

    
    public MetaData() {

    }

    
    public void initializeNewDatabase() {
        metaData.put(Globals.SELECTOR_META_PREFIX + "keywords", new Vector<String>());
        metaData.put(Globals.SELECTOR_META_PREFIX + "author", new Vector<String>());
        metaData.put(Globals.SELECTOR_META_PREFIX + "journal", new Vector<String>());
        metaData.put(Globals.SELECTOR_META_PREFIX + "publisher", new Vector<String>());
    }

    public Iterator<String> iterator() {
        return metaData.keySet().iterator();
    }

    public Vector<String> getData(String key) {
        return metaData.get(key);
    }

    public void remove(String key) {
        metaData.remove(key);
    }

    
    public void putData(String key, Vector<String> orderedData) {
        metaData.put(key, orderedData);
    }

    
    public String getFileDirectory(String fieldName) {
        
        
        
        String key = fieldName + "Directory";
        String dir;
        Vector<String> vec = getData(key);
        if ((vec != null) && (vec.size() > 0)) {
            dir = vec.get(0);
            
            
            if (!(new File(dir)).isAbsolute() && (file != null)) {
                String relDir = new StringBuffer(file.getParent()).
                        append(System.getProperty("file.separator")).
                        append(dir).toString();
                
                
                if ((new File(relDir)).exists())
                    dir = relDir;
            }
        }
        else
            dir = Globals.prefs.get(key);


        
        return dir;
    }

    private void putGroups(Vector<String> orderedData, BibtexDatabase db, int version) {
        try {
            groupsRoot = VersionHandling.importGroups(orderedData, db, 
                    version);
        } catch (Exception e) {
            
            System.err.println(e);
        }
    }

    public GroupTreeNode getGroups() {
        return groupsRoot;
    }
    
    
    public void setGroups(GroupTreeNode root) {
        groupsRoot = root;
    }

    
    public void writeMetaData(Writer out) throws IOException {
        
        for (Iterator<String> i = metaData.keySet().iterator(); i.hasNext();) {
            String key = i.next();
            StringBuffer sb = new StringBuffer();
            Vector<String> orderedData = metaData.get(key);
            if (orderedData.size() >= 0) {
                sb.append("@comment{").append(GUIGlobals.META_FLAG).append(key).append(":");
                for (int j = 0; j < orderedData.size(); j++) {
                    sb.append(Util.quote((String) orderedData.elementAt(j), ";", '\\')).append(";");
                }
                sb.append("}");
                sb.append(Globals.NEWLINE);
                sb.append(Globals.NEWLINE);
            }
            wrapStringBuffer(sb, Globals.METADATA_LINE_LENGTH);
            out.write(sb.toString());
        }
        
        
        if (groupsRoot != null && groupsRoot.getChildCount() > 0) {
            StringBuffer sb = new StringBuffer();
            
            sb.append("@comment{").append(GUIGlobals.META_FLAG).append("groupsversion:");
            sb.append(""+VersionHandling.CURRENT_VERSION+";");
            sb.append("}");
            sb.append(Globals.NEWLINE);
            sb.append(Globals.NEWLINE);
            out.write(sb.toString());
            
            
            sb = new StringBuffer();
            sb.append("@comment{").append(GUIGlobals.META_FLAG).append("groupstree:");
            sb.append(Globals.NEWLINE);
            
            StringTokenizer tok = new StringTokenizer(groupsRoot.getTreeAsString(),Globals.NEWLINE);
            while (tok.hasMoreTokens()) {
                StringBuffer s = 
                    new StringBuffer(Util.quote(tok.nextToken(), ";", '\\') + ";");
                wrapStringBuffer(s, Globals.METADATA_LINE_LENGTH);
                sb.append(s);
                sb.append(Globals.NEWLINE);
            }
            sb.append("}");
            sb.append(Globals.NEWLINE);
            sb.append(Globals.NEWLINE);
            out.write(sb.toString());
        }
    }

    private void wrapStringBuffer(StringBuffer sb, int lineLength) {
        for (int i=lineLength; i<sb.length(); i+=lineLength+Globals.NEWLINE_LENGTH) {
            sb.insert(i, Globals.NEWLINE);
        }
    }
    
    
    private String getNextUnit(Reader reader) throws IOException {
        int c;
        boolean escape = false;
        StringBuffer res = new StringBuffer();
        while ((c = reader.read()) != -1) {
            if (escape) {
                res.append((char)c);
                escape = false;
            } else if (c == '\\') {
                escape = true;
            } else if (c == ';') {
                break;
            } else {
                res.append((char)c);
            }
        }
        if (res.length() > 0)
            return res.toString();
        return null;
    }

    public File getFile() {
        return file;
    }

    public void setFile(File file) {
        this.file = file;
    }
}
