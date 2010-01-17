

package net.sf.jabref.sql; 

import java.io.BufferedOutputStream; 
import java.io.File; 
import java.io.FileOutputStream; 
import java.io.PrintStream; 
import java.sql.Connection; 
import java.sql.DriverManager; 
import java.sql.ResultSet; 
import java.sql.SQLException; 
import java.sql.SQLWarning; 
import java.sql.Statement; 
import java.util.ArrayList; 
import java.util.Enumeration; 
import java.util.HashMap; 
import java.util.Iterator; 
import java.util.LinkedHashMap; 
import java.util.List; 
import java.util.ListIterator; 
import java.util.Set; 

import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.BibtexEntryType; 
import net.sf.jabref.BibtexFields; 
import net.sf.jabref.BibtexString; 
import net.sf.jabref.Globals; 
import net.sf.jabref.MetaData; 
import net.sf.jabref.Util; 
import net.sf.jabref.export.FileActions; 
import net.sf.jabref.groups.AbstractGroup; 
import net.sf.jabref.groups.AllEntriesGroup; 
import net.sf.jabref.groups.ExplicitGroup; 
import net.sf.jabref.groups.GroupTreeNode; 
import net.sf.jabref.groups.KeywordGroup; 
import net.sf.jabref.groups.SearchGroup; 


public  class  SQLutil {
	

    public enum  DBTYPE {
        MYSQL , 
        MYSQL}

	 

    private static ArrayList<String> fields = null;

	
    private static String fieldstr = null;

	

    
    public static DBTYPE getDBType (DBStrings dbstrings) {

        DBTYPE dbtype = null;
        String srvtype = dbstrings.getServerType();

        if (srvtype.equalsIgnoreCase("mysql")) {
            dbtype = DBTYPE.MYSQL;
        }
        if (srvtype.equalsIgnoreCase("derby")) {
            dbtype = DBTYPE.DERBY;
        }

        return dbtype;
    }


	

    
    public static String createJDBCurl (DBStrings dbs) {

        String url = "";
        String servertype = dbs.getServerType();

        if (servertype.equalsIgnoreCase("mysql")) {
            url = "jdbc:" + dbs.getServerType().toLowerCase() + "://" 
                          + dbs.getServerHostname() + "/" 
                          + dbs.getDatabase();
        }

        if (servertype.equalsIgnoreCase("derby")) {
            url = "jdbc:" + dbs.getServerType().toLowerCase() + ":"
                          + dbs.getDatabase() + ";create=true";
        }

        return url;
    }


	

    public static String getJDBCdriver (DBStrings dbstrings) {

        String driver = "";
        String servertype = dbstrings.getServerType();

        if (servertype.equalsIgnoreCase("mysql")) {
            driver ="com.mysql.jdbc.Driver";
        }

        if (servertype.equalsIgnoreCase("derby")) {
            driver = "org.apache.derby.jdbc.EmbeddedDriver";
        }

        return driver;
            
    }


	

    
    public static Connection connectToDB (DBStrings dbstrings)
        throws Exception {

        String url = createJDBCurl(dbstrings);
        String drv = getJDBCdriver(dbstrings);

        Class.forName (drv).newInstance ();
        Connection conn = DriverManager.getConnection (url,
                           dbstrings.getUsername(), dbstrings.getPassword());
       
        return conn;

    }


	    
   

    
    private static Object processDMLWithResults ( Object out, String dml)
                            throws SQLException {

        if ( out instanceof PrintStream) {
            
            PrintStream fout = (PrintStream) out;
            fout.println(dml);
            return null;
        }

        if ( out instanceof Connection) {
            Connection conn = (Connection) out;
            return execDMLWithResults(conn, dml);
        }

        return null;
    }


	

    private static String processDMLWithSingleResult ( Connection conn, String query) throws SQLException {
        Object res = execDMLWithResults(conn, query);
        if (res instanceof Statement) {
            Statement st = (Statement)res;
            ResultSet rs = st.getResultSet();
            rs.next();
            String returned = rs.getString(1);
            st.close();
            return returned;
        }
        else return null;
    }


	

    
    public static Statement execDMLWithResults(Connection conn, String dml) throws SQLException {
        
        Statement stmnt = conn.createStatement();
        stmnt.executeQuery(dml);
        SQLWarning warn = stmnt.getWarnings();
        if (warn!=null) {
            
            System.out.println(warn.toString());
            System.out.println("("+dml+")");
        }
        return stmnt;
    }


	


    
    private static void processDML ( Object out, String dml) 
                            throws SQLException {

        if ( out instanceof PrintStream) {
            PrintStream fout = (PrintStream) out;
            fout.println(dml);
        }

        if ( out instanceof Connection) {
            Connection conn = (Connection) out;
            execDML(conn, dml);
        }

    }


	


    
    public static void execDML(Connection conn, String dml) throws SQLException {
        
        Statement stmnt = conn.createStatement();
        stmnt.execute(dml);
        SQLWarning warn = stmnt.getWarnings();
        if (warn!=null) {
            
            System.out.println(warn.toString());
            System.out.println("("+dml+")");
        }
        stmnt.close();
    }


	

    
    public static ArrayList<String> getFields() {
        if (fields == null) {
            refreshFields();
        }
        return fields;
    }


	


    
    public static void refreshFields() {

        if (fields==null) {
            fields = new ArrayList<String>();
        } else {
            fields.clear();
        }

        for (BibtexEntryType val : BibtexEntryType.ALL_TYPES.values()) {
            fields = uniqueInsert(fields, val.getRequiredFields());
            fields = uniqueInsert(fields, val.getOptionalFields());
            fields = uniqueInsert(fields, val.getGeneralFields());
            fields = uniqueInsert(fields, val.getUtilityFields());
        }
        

        
        fieldstr = "";
        for (int i = 0; i < fields.size(); i++) {
            if (i > 0)
                fieldstr = fieldstr + ", ";
            fieldstr = fieldstr + fields.get(i);
        }

    }


	


    
    private static ArrayList<String> uniqueInsert(ArrayList<String> list, 
            String[] array) {

        if (array != null) {
            for (int i = 0; i < array.length; i++) {
                if (!list.contains(array[i]))
                    list.add(array[i]);
            }
        }
        return list;
    }


	


    
    public static void exportDatabase(final BibtexDatabase database,
        final MetaData metaData, Set<String> keySet, String file, DBTYPE dbtype ) 
        throws Exception {

        
        File outfile = new File(file);
        if (outfile.exists())
            outfile.delete();

        BufferedOutputStream writer = null;
        writer = new BufferedOutputStream( new FileOutputStream( outfile ) );
        PrintStream fout = null;
        fout = new PrintStream( writer );


        exportDatabase_worker(dbtype, database, metaData, keySet, fout);

        fout.close();

    }


	

    public static Object[] importDatabase(Set<String> keySet,
                                      DBStrings dbStrings)
        throws Exception {

                DBTYPE dbtype = getDBType(dbStrings);

        Object[] result = null;
        Connection conn = null;

        try {

            conn = SQLutil.connectToDB(dbStrings);

            

            result = importDatabase_worker(dbtype, keySet, conn);

            

            conn.close();

        } catch (SQLException ex) {

            throw ex;
        }

        return result;
    }


	

     private static Object[] importDatabase_worker (DBTYPE dbtype,
            Set<String> keySet, Connection conn) throws Exception {

         BibtexDatabase database = new BibtexDatabase();

         
         HashMap<String, BibtexEntryType> types = new HashMap<String, BibtexEntryType>();
         Object res = processDMLWithResults(conn,"SELECT entry_types_id,label FROM entry_types;");
         if (res instanceof Statement) {
             Statement statement = (Statement)res;
             ResultSet rs = statement.getResultSet();
             while ( rs.next()) {
                types.put(rs.getString(1), BibtexEntryType.getType(rs.getString(2)));
             }
             statement.close();
         }
         for (Iterator<String> iterator = types.keySet().iterator(); iterator.hasNext();) {
             iterator.next();
         }

          
         res = processDMLWithResults(conn, "SHOW columns FROM entries;");
         ArrayList<String> colNames = new ArrayList<String>();
         if (res instanceof Statement) {
             Statement statement = (Statement)res;
             ResultSet rs = statement.getResultSet();
             boolean started = false;
             while ( rs.next()) {
                if (started)
                    colNames.add(rs.getString(1));
                 else if (rs.getString(1).equals("cite_key"))
                    started = true;
             }
             statement.close();
         }

         
         HashMap<String,BibtexEntry> entries = new HashMap<String, BibtexEntry>();
         res = processDMLWithResults(conn, "SELECT * FROM entries;");
         if (res instanceof Statement) {
             Statement statement = (Statement)res;
             ResultSet rs = statement.getResultSet();
             while ( rs.next()) {
                 String id = rs.getString("entries_id");
                 BibtexEntry entry = new BibtexEntry(Util.createNeutralId(),
                         types.get(rs.getString(3)));
                 entry.setField(BibtexFields.KEY_FIELD, rs.getString("cite_key"));
                 for (Iterator<String> iterator = colNames.iterator(); iterator.hasNext();) {
                     String col = iterator.next();
                     String value = rs.getString(col);
                     if (value != null)
                        entry.setField(col, value);
                     
                 }
                 entries.put(id, entry);
                 database.insertEntry(entry);
             }
             statement.close();
         }

         
         res = processDMLWithResults(conn, "SELECT * FROM strings;");
         if (res instanceof Statement) {
             Statement statement = (Statement)res;
             ResultSet rs = statement.getResultSet();
             while ( rs.next()) {
                 String label = rs.getString("label"), content = rs.getString("content");
                 if (label.equals("@PREAMBLE")) {
                     database.setPreamble(content);
                 }
                 else {
                     BibtexString string = new BibtexString(Util.createNeutralId(), label, content);
                     database.addString(string);
                 }
             }
             statement.close();
         }

         MetaData metaData = new MetaData();
         metaData.initializeNewDatabase();

         
         importGroupsTree(dbtype, metaData, entries, conn);

         return new Object[] {database, metaData};

     }


	

    public static void importGroupsTree(DBTYPE dbtype, MetaData metaData, HashMap<String,BibtexEntry> entries,
                                        Connection conn) throws SQLException {
        Object res = processDMLWithResults(conn, "SELECT * FROM groups ORDER BY groups_id;");
        if (res instanceof Statement) {
            Statement statement = (Statement)res;
            ResultSet rs = statement.getResultSet();
            GroupTreeNode rootNode = new GroupTreeNode(new AllEntriesGroup());
            
            HashMap<String, GroupTreeNode> groups = new HashMap<String, GroupTreeNode>();
            LinkedHashMap<GroupTreeNode, String> parentIds = new LinkedHashMap<GroupTreeNode, String>();
            
            while ( rs.next()) {
                AbstractGroup group = null;
                String typeId = findGroupTypeName(rs.getString("group_types_id"), conn);
                if (typeId.equals(AllEntriesGroup.ID)) {
                    
                    groups.put(rs.getString("groups_id"), rootNode);
                }
                else if (typeId.equals(ExplicitGroup.ID)) {
                    group = new ExplicitGroup(rs.getString("label"),
                            rs.getInt("hierarchical_context"));
                }
                else if (typeId.equals(KeywordGroup.ID)) {
                    System.out.println("Keyw: "+ rs.getBoolean("case_sensitive"));
                    group = new KeywordGroup(rs.getString("label"),
                            Util.unquote(rs.getString("search_field"), '\\'),
                            Util.unquote(rs.getString("search_expression"), '\\'),
                            rs.getBoolean("case_sensitive"), rs.getBoolean("reg_exp"),
                            rs.getInt("hierarchical_context"));
                }
                else if (typeId.equals(SearchGroup.ID)) {
                    System.out.println("Search: "+ rs.getBoolean("case_sensitive"));
                    group = new SearchGroup(rs.getString("label"),
                            Util.unquote(rs.getString("search_expression"), '\\'),
                            rs.getBoolean("case_sensitive"), rs.getBoolean("reg_exp"),
                            rs.getInt("hierarchical_context"));
                }

                if (group != null) {
                    GroupTreeNode node = new GroupTreeNode(group);
                    parentIds.put(node, rs.getString("parent_id"));
                    groups.put(rs.getString("groups_id"), node);
                }
            }
            statement.close();

            
            
            
            for (Iterator<GroupTreeNode> i=parentIds.keySet().iterator(); i.hasNext();) {
                GroupTreeNode node = i.next();
                String parentId = parentIds.get(node);
                
                GroupTreeNode parent = groups.get(parentId);
                if (parent == null) {
                    
                }
                else {
                    parent.add(node);
                }
            }

            
            res = processDMLWithResults(conn, "SELECT * FROM entry_group;");
            if (res instanceof Statement) {
                statement = (Statement)res;
                rs = statement.getResultSet();
                while ( rs.next()) {
                    String entryId = rs.getString("entries_id"),
                            groupId = rs.getString("groups_id");
                    GroupTreeNode node = groups.get(groupId);
                    if ((node != null) && (node.getGroup() instanceof ExplicitGroup)) {
                        ExplicitGroup group = (ExplicitGroup)node.getGroup();
                        group.addEntry(entries.get(entryId));
                    } else {
                        
                    }
                }
                statement.close();
            }

            
            metaData.setGroups(rootNode);
        }
    }


	

    
    public static String findGroupTypeName(String groupId, Connection conn) throws SQLException {
        return processDMLWithSingleResult(conn, "SELECT label FROM group_types WHERE group_types_id=\""+groupId+"\";");
    }


	

    
    public static void exportDatabase(final BibtexDatabase database,
        final MetaData metaData, Set<String> keySet, DBStrings dbStrings)
        throws Exception {

        DBTYPE dbtype = getDBType(dbStrings);

        Connection conn = null;

        try {

            conn = SQLutil.connectToDB(dbStrings);

            

            exportDatabase_worker(dbtype, database, metaData, keySet, conn);

            if (!conn.getAutoCommit()) {
                conn.commit();
                conn.setAutoCommit(true);
            }

            conn.close();

        } catch (SQLException ex) {

            if (conn != null) {
                if (!conn.getAutoCommit()) {
                    conn.rollback();
                }
            }

            throw ex;
        }
    }


	


   
    private static void exportDatabase_worker (DBTYPE dbtype, 
            final BibtexDatabase database, final MetaData metaData, 
            Set<String> keySet, Object out) throws Exception{

        List<BibtexEntry> entries = FileActions.getSortedEntries(database,
            keySet, false);

        
        dmlCreateTables(dbtype,out);

        
        dmlPopTab_ET(out);

        
        dmlPopTab_FD(entries,out);

        
        dmlPopTab_ST(database,out);

        GroupTreeNode gtn = metaData.getGroups();

        
        dmlPopTab_GT(out);


        
        dmlPopTab_GP(gtn,out);
        
		
        dmlPopTab_EG(gtn,out);
    }


	

    
    
    private static void dmlCreateTables(DBTYPE dbtype, Object out)
                                throws SQLException{

        
        if (fields==null) {
            refreshFields();
        }

        
        String dml1 = "", dml2 = "";
        switch (dbtype) {
            case MYSQL:

                
                processDML(out,"DROP TABLE IF EXISTS entry_types;");
                processDML(out,"DROP TABLE IF EXISTS entries;");
                processDML(out,"DROP TABLE IF EXISTS strings;");
                processDML(out,"DROP TABLE IF EXISTS group_types;");
                processDML(out,"DROP TABLE IF EXISTS groups;");
                processDML(out,"DROP TABLE IF EXISTS entry_group;");

                
                dml1 = SQLutil.fieldsAsCols(fields, " VARCHAR(3) DEFAULT NULL");
                dml2 = SQLutil.fieldsAsCols(fields, " TEXT DEFAULT NULL");

                
                dmlTable_mysql(dml1, dml2, out);

                break;

            case DERBY:

                
                if (out instanceof Connection) {

                    Connection conn = (Connection) out;
                    boolean commitNow = conn.getAutoCommit();
                    conn.setAutoCommit(true);

                    

                    conn.setAutoCommit(commitNow);

                }

                
                dml1 = SQLutil.fieldsAsCols(fields, " VARCHAR(3) DEFAULT NULL");
                dml2 = SQLutil.fieldsAsCols(fields, " LONG VARCHAR DEFAULT NULL");

                
                dmlTable_derby(dml1, dml2, out);

                break;

            default:
                System.err.println("Error: Do not recognize database enumeration.");
                System.exit(0);
        }

        return;
    }


	


    
    private static String fieldsAsCols(ArrayList<String> fields, String datatype) {
        String str = "";
        ListIterator<String> li = fields.listIterator();
        while (li.hasNext()) {
            str = str + li.next() + " " + datatype;
            if (li.hasNext())
                str = str + ", ";
        }
        return str;
    }


	

    
    private static void dmlTable_mysql(String dml1, String dml2, Object out)
            throws SQLException {

        processDML(out,"CREATE TABLE entry_types ( \n"
            + "entry_types_id    INT UNSIGNED  NOT NULL AUTO_INCREMENT, \n"
            + "label			 TEXT, \n"
            + dml1
            + ", \n"
            + "PRIMARY KEY (entry_types_id) \n"
            + ");" );
           			
        processDML(out,"CREATE TABLE entries ( \n"
            + "entries_id      INTEGER         NOT NULL AUTO_INCREMENT, \n"
			+ "jabref_eid      VARCHAR("
			+  Util.getMinimumIntegerDigits()
		    + ")   DEFAULT NULL, \n"
<<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_49657
            + "entry_types_id  INT UNSIGNED         DEFAULT NULL, \n"
            + "cite_key        VARCHAR(100)     DEFAULT NULL, \n"
=======
            + "entry_types_id  INTEGER         DEFAULT NULL, \n"
            + "cite_key        VARCHAR(100)     DEFAULT NULL, \n"
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_49659
            + dml2
            + ",\n"
            + "PRIMARY KEY (entries_id), \n"
			+ "INDEX(entry_types_id), \n"
            + "FOREIGN KEY (entry_types_id) REFERENCES entry_types(entry_types_id) \n"
            + ");");

        processDML(out,"CREATE TABLE strings ( \n"
            + "strings_id      INTEGER         NOT NULL AUTO_INCREMENT, \n"
			+ "label      VARCHAR(100)  DEFAULT NULL, \n"
		    + "content    VARCHAR(200)  DEFAULT NULL, \n"
            + "PRIMARY KEY (strings_id) \n"
            + ");");
<<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_49657

        processDML(out,"CREATE TABLE group_types ( \n"
                 + "group_types_id  INTEGER     NOT NULL AUTO_INCREMENT, \n"
                 + "label   VARCHAR(100)    DEFAULT NULL, \n"
                 + "PRIMARY KEY (group_types_id) \n"
                 + ");" );

=======

        processDML(out,"CREATE TABLE strings ( \n"
            + "strings_id      INTEGER         NOT NULL AUTO_INCREMENT, \n"
			+ "label      VARCHAR(100)  DEFAULT NULL, \n"
		    + "content    VARCHAR(200)  DEFAULT NULL, \n"
            + "PRIMARY KEY (strings_id) \n"
            + ");");

        processDML(out,"CREATE TABLE group_types ( \n"
                 + "group_types_id  INTEGER     NOT NULL AUTO_INCREMENT, \n"
                 + "label   VARCHAR(100)    DEFAULT NULL, \n"
                 + "PRIMARY KEY (group_types_id) \n"
                 + ");" );

>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_49659
        processDML(out,"CREATE TABLE groups ( \n"
            + "groups_id       INTEGER         NOT NULL AUTO_INCREMENT, \n"
            + "group_types_id  INTEGER         DEFAULT NULL, \n"
            + "label           VARCHAR(100)    DEFAULT NULL, \n"
            + "parent_id       INTEGER         DEFAULT NULL, \n"
            + "search_field       VARCHAR(100)          DEFAULT NULL, \n"
            + "search_expression  VARCHAR(200)          DEFAULT NULL, \n"
            + "case_sensitive  BOOL          DEFAULT NULL, \n"
            + "reg_exp BOOL DEFAULT NULL, \n"
            + "hierarchical_context INTEGER DEFAULT NULL, \n"
            + "PRIMARY KEY (groups_id) \n"
            + ");");
           
        processDML(out,"CREATE TABLE entry_group ( \n"
            + "entries_id       INTEGER        NOT NULL AUTO_INCREMENT, \n"
            + "groups_id        INTEGER        DEFAULT NULL, \n"
			+ "INDEX(entries_id), \n"
			+ "INDEX(groups_id), \n"
            + "FOREIGN KEY (entries_id) REFERENCES entries(entries_id), \n"
            + "FOREIGN KEY (groups_id)  REFERENCES groups(groups_id) \n"
            + ");");

        return;

    }


	

    
    private static void dmlTable_derby(String dml1, String dml2, Object out)
            throws SQLException {

        processDML(out,"CREATE TABLE entry_types ( "
            + "entry_types_id INT  NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
            + dml1 + ", "
            + "label LONG VARCHAR"
            + ")" );

        processDML(out,"CREATE TABLE entries ( "
            + "entries_id      INTEGER         NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
			+ "jabref_eid      VARCHAR("
			+  Util.getMinimumIntegerDigits()
		    + ")   DEFAULT NULL, "
            + "entry_types_id  INTEGER         DEFAULT NULL, "
            + "cite_key        VARCHAR(100)     DEFAULT NULL, "
            + dml2
            + ")");
          
        processDML(out,"ALTER TABLE entries ADD CONSTRAINT entries_fk "
                     + "FOREIGN KEY (\"entry_types_id\") REFERENCES \"entry_types\" (\"entry_types_id\")");

        processDML(out,"CREATE TABLE group_types ( "
            + "group_types_id INT  NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
            + "label LONG VARCHAR"
            + ")" );

        processDML(out,"CREATE TABLE group_types ( "
            + "group_types_id INT  NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
            + "label LONG VARCHAR"
            + ")" );

        processDML(out,"CREATE TABLE groups ( "
            + "groups_id       INTEGER         NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
            + "group_types_id  INTEGER         DEFAULT NULL, "
            + "label           VARCHAR(100)    DEFAULT NULL, "
            + "parent_id       INTEGER         DEFAULT NULL  "
            + "search_field       VARCHAR(100)          DEFAULT NULL, "
            + "search_expression  VARCHAR(200)          DEFAULT NULL, "
            + "case_sensitive  BOOL          DEFAULT NULL, "
            + "reg_exp BOOL DEFAULT NULL"
            + "hierarchical_context INTEGER DEFAULT NULL, "
            + ")");
           
        processDML(out,"CREATE TABLE entry_group ( "
            + "entries_id       INTEGER        NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
            + "groups_id        INTEGER        DEFAULT NULL "
            + ")");

        processDML(out,"ALTER TABLE entry_group ADD CONSTRAINT entries_group_fk"
                     + "FOREIGN KEY (\"entries_id\") REFERENCES \"entry_fields\" (\"entries_id\")");

        processDML(out,"ALTER TABLE entry_group ADD CONSTRAINT groups_fk"
                     + "FOREIGN KEY (\"groups_id\") REFERENCES \"groups\" (\"groups_id\")");

        return;

    }


	

    
    private static void dmlPopTab_GT( Object out) throws SQLException{
        String[] typeNames = new String[] {
                AllEntriesGroup.ID, ExplicitGroup.ID, KeywordGroup.ID, SearchGroup.ID};
        for (int i = 0; i < typeNames.length; i++) {
            String typeName = typeNames[i];
            String insert = "INSERT INTO group_types (label) VALUES (\""+typeName+"\");";
            
            processDML(out, insert);
        }


    }


	
     
    private static void dmlPopTab_ET( Object out) throws SQLException{

        String dml = "";
        String insert = "INSERT INTO entry_types (label, "+fieldstr+") VALUES (";

        ArrayList<String> fieldID = new ArrayList<String>();
        for (int i = 0; i < fields.size(); i++)
            fieldID.add(null);

        
        for (BibtexEntryType val : BibtexEntryType.ALL_TYPES.values()) {

            
            
            for (int i = 0; i < fieldID.size(); i++) {
                fieldID.set(i, "");
            }
            fieldID = setFieldID(fields, fieldID, val.getRequiredFields(),
                "req");
            fieldID = setFieldID(fields, fieldID, val.getOptionalFields(),
                "opt");
            fieldID = setFieldID(fields, fieldID, val.getGeneralFields(), "gen");
            fieldID = setFieldID(fields, fieldID, val.getUtilityFields(), "uti");

            
            dml = insert + "\"" + val.getName().toLowerCase() + "\"";
            for (int i = 0; i < fieldID.size(); i++) {
                dml = dml + ", ";
                if (fieldID.get(i) != "") {
                    dml = dml + "\"" + fieldID.get(i) + "\"";
                } else {
                    dml = dml + "NULL";
                }
            }
            dml = dml + ");";

            
            processDML(out, dml);

        }

        return;

    }


	


     
    private static ArrayList<String> setFieldID(ArrayList<String> fields,
        ArrayList<String> fieldID, String[] fieldstr, String ID) {
        if (fieldstr != null) {
            for (int i = 0; i < fieldstr.length; i++) {
                fieldID.set(fields.indexOf(fieldstr[i]), ID);
            }
        }
        return fieldID;
    }


	


     
    private static void dmlPopTab_FD(List<BibtexEntry> entries, Object out) 
                            throws SQLException {

        String dml = "";
        String val = "";
        String insert = "INSERT INTO entries (jabref_eid, entry_types_id, cite_key, "
            + fieldstr
            + ") VALUES (";

        
        for (BibtexEntry entry : entries) {

            
            dml = insert 
			      + "\"" + entry.getId() + "\""
			      + ", (SELECT entry_types_id FROM entry_types WHERE label=\""
			      + entry.getType().getName().toLowerCase() + "\"), \""
                  + entry.getCiteKey() + "\"";

            for (int i = 0; i < fields.size(); i++) {
                dml = dml + ", ";
                val = entry.getField(fields.get(i));
                if (val != null) {
                    
                	val = val.replace("\\", "\\\\");
                	val = val.replace("\"", "\\\"");
                	val = val.replace("\'", "\\\'");
                	val = val.replace("`", "\\`");
                	dml = dml + "\"" + val + "\"";
                } else {
                    dml = dml + "NULL";
                }
            }
            dml = dml + ");";

            
            processDML(out, dml);

        }

        return;

    }


	

    private static void dmlPopTab_ST(BibtexDatabase database, Object out)
                            throws SQLException {

        String insert = "INSERT INTO strings (label, content) VALUES (";

        
        if (database.getPreamble() != null) {
            String dml = insert + "\"@PREAMBLE\", "
                    + "\""+Util.quote(database.getPreamble(), "\"", '\\')+"\""
                    + ");";
            processDML(out, dml);
        }

        Set<String> keys = database.getStringKeySet();
        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            String key = iterator.next();
            BibtexString string = database.getString(key);
            String dml = insert + "\""+Util.quote(string.getName(), "\"", '\\')+"\", "
                    + "\""+Util.quote(string.getContent(), "\"", '\\')+"\""
                    + ");";
            processDML(out, dml);
        }
    }


	

     
	private static int dmlPopTab_GP (GroupTreeNode cursor, Object out) 
                        throws Exception {
        int cnt = dmlPopTab_GP_worker(cursor, 1, 1, out);
        return cnt;
    }


	

    
	private static int dmlPopTab_GP_worker (GroupTreeNode cursor, int parentID,
            int ID, Object out) throws SQLException{

        AbstractGroup group = cursor.getGroup();
        String searchField = null, searchExpr = null, caseSensitive = null, reg_exp = null;
        int hierContext = group.getHierarchicalContext();
        if (group instanceof KeywordGroup) {
            searchField = ((KeywordGroup)group).getSearchField();
            searchExpr = ((KeywordGroup)group).getSearchExpression();
            caseSensitive = ((KeywordGroup)group).isCaseSensitive() ? "1" : "0";
            reg_exp = ((KeywordGroup)group).isRegExp() ? "1" : "0";
        }
        else if (group instanceof SearchGroup) {
            searchExpr = ((SearchGroup)group).getSearchExpression();
            caseSensitive = ((SearchGroup)group).isCaseSensitive() ? "1" : "0";
            reg_exp = ((SearchGroup)group).isRegExp() ? "1" : "0";
        }

        
        if (searchField != null)
            searchField = Util.quote(searchField, "\"", '\\');
        if (searchExpr != null)
            searchExpr = Util.quote(searchExpr, "\"", '\\');

        
        processDML(out, "INSERT INTO groups (groups_id, label, parent_id, group_types_id, search_field, "
            +"search_expression, case_sensitive, reg_exp, hierarchical_context) "
				      + "VALUES (" + ID + ", \"" + cursor.getGroup().getName() 
				      + "\", " + parentID
                      +", (SELECT group_types_id FROM group_types where label=\""+group.getTypeId()+"\")"
                      +", "+(searchField != null ? "\""+searchField+"\"" : "NULL")
                      +", "+(searchExpr != null ? "\""+searchExpr+"\"" : "NULL")
                      +", "+(caseSensitive != null ? "\""+caseSensitive+"\"" : "NULL")
                      +", "+(reg_exp != null ? "\""+reg_exp+"\"" : "NULL")
                      +", "+hierContext
                      + ");");

		
	    int myID = ID;
	    for (Enumeration<GroupTreeNode> e = cursor.children(); e.hasMoreElements();) 
			ID = dmlPopTab_GP_worker(e.nextElement(),myID,++ID,out);
	    return ID;
	}


	


    
	private static int dmlPopTab_EG(GroupTreeNode cursor, Object fout) 
                        throws SQLException{

            int cnt = dmlPopTab_EG_worker(cursor, 1, 1, fout);
            return cnt;
    }


	

    

	private static int dmlPopTab_EG_worker(GroupTreeNode cursor, int parentID, int ID, 
			Object out) throws SQLException{

		
		if ( cursor.getGroup() instanceof ExplicitGroup) {

			
			ExplicitGroup grp = (ExplicitGroup)cursor.getGroup();
			
			for (BibtexEntry be : grp.getEntries()){

                
                processDML(out, "INSERT INTO entry_group (entries_id, groups_id) " 
						   + "VALUES (" 
						   + "(SELECT entries_id FROM entries WHERE jabref_eid="
						   + "\"" + be.getId() + "\""
						   + "), "
						   + "(SELECT groups_id FROM groups WHERE groups_id=" 
						   + "\"" + ID + "\")"
						   + ");");
			}
		}

		
	    int myID = ID;
	    for (Enumeration<GroupTreeNode> e = cursor.children(); e.hasMoreElements();) 
			ID = dmlPopTab_EG_worker(e.nextElement(),myID,++ID,out);

	    return ID;
	}


	

    

    public static String getExceptionMessage (Exception ex, DBTYPE dbtype) {
        
        String errorMessage = null;

        switch (dbtype) {
            case MYSQL:
                errorMessage = getExceptionMessage_MySQL(ex);
                break;
            case DERBY:
                errorMessage = getExceptionMessage_MySQL(ex);
                break;
            default:
                errorMessage = Globals.lang("Could not determine exception message.");
                break;
        }

        return errorMessage;

    }


	

    
    public static String getExceptionMessage_MySQL (Exception ex) {
      
        String msg = null;

        
        if (ex instanceof SQLException) {

            SQLException sqlex = (SQLException) ex;

            
            
            
            
            
            

            if (sqlex.getSQLState().equals("42000")) {
                msg = Globals.lang(sqlex.getMessage());
            }


            
            
            
            
            

            if (sqlex.getSQLState().equals("42000")) {
                msg = Globals.lang("User does not have sufficient privileges.\n");
                msg = msg + Globals.lang("(" + sqlex.getMessage() + ")");
            }


            
            
            
            
            
            

            if (sqlex.getSQLState().equals("28000")) {
                msg = Globals.lang(sqlex.getMessage());
            }


            
            
            
            
            
            

            if (sqlex.getSQLState().equals("08S01")) {
                msg = Globals.lang("Cannot connect to SQL server at the specified host.");
            } 

            
            if (false) {
                System.out.println("-------------------------------------");
                System.out.println(sqlex.getErrorCode());
                System.out.println(sqlex.getSQLState());
                System.out.println(sqlex.getMessage());
                System.out.println("-------------------------------------");
            }

        } 
        
        
        if (msg == null) {

            if (ex.getMessage()==null) {
                msg = ex.toString();
            } else {
                msg = ex.getMessage();
            }

        }


        return msg;

    }



}
