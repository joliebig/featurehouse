
package net.sf.jabref;

import java.io.*;
import java.util.*;

import net.sf.jabref.groups.*;

public class MetaData {
    private HashMap metaData = new HashMap();
    private StringReader data;
    private GroupTreeNode groupsRoot = null;
    private File file = null; 
    
    
    public MetaData(HashMap inData, BibtexDatabase db) {
        boolean groupsTreePresent = false;
        Vector flatGroupsData = null;
        Vector treeGroupsData = null;
        
        
        int groupsVersionOnDisk = 0;
        
        if (inData != null) for (Iterator i = inData.keySet().iterator(); i.hasNext();) {
            String key = (String) i.next();
            data = new StringReader((String) inData.get(key));
            String unit;
            Vector orderedData = new Vector();
            
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
        metaData.put(Globals.SELECTOR_META_PREFIX + "keywords", new Vector());
        metaData.put(Globals.SELECTOR_META_PREFIX + "author", new Vector());
        metaData.put(Globals.SELECTOR_META_PREFIX + "journal", new Vector());
        metaData.put(Globals.SELECTOR_META_PREFIX + "publisher", new Vector());
    }

    public Iterator iterator() {
        return metaData.keySet().iterator();
    }

    public Vector getData(String key) {
        return (Vector) metaData.get(key);
    }

    public void remove(String key) {
        metaData.remove(key);
    }

    
    public void putData(String key, Vector orderedData) {
        metaData.put(key, orderedData);
    }

    
    public String getFileDirectory(String fieldName) {
        
        
        
        String key = fieldName + "Directory";
        String dir;
        Vector vec = getData(key);
        if ((vec != null) && (vec.size() > 0)) {
            dir = (String)vec.get(0);
            
            
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

    private void putGroups(Vector orderedData, BibtexDatabase db, int version) {
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
        
        for (Iterator i = metaData.keySet().iterator(); i.hasNext();) {
            String key = (String) i.next();
            StringBuffer sb = new StringBuffer();
            Vector orderedData = (Vector) metaData.get(key);
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
