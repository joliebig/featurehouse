

package net.sf.jabref.sql;

import java.io.File;
import java.io.PrintStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexEntryType;
import net.sf.jabref.MetaData;
import net.sf.jabref.Util;
import net.sf.jabref.Globals;
import net.sf.jabref.export.FileActions;
import net.sf.jabref.groups.ExplicitGroup;
import net.sf.jabref.groups.GroupTreeNode;


public class SQLutil {

    public enum DBTYPE {
        MYSQL, DERBY
    } 

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

        PrintStream fout = null;
        fout = new PrintStream(outfile);

        exportDatabase_worker(dbtype, database, metaData, keySet, fout);

        fout.close();

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

		GroupTreeNode gtn = metaData.getGroups();

		
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
            + "entry_types_id  INTEGER         DEFAULT NULL, \n"
            + "cite_key        VARCHAR(30)     DEFAULT NULL, \n"
            + dml2
            + ",\n"
            + "PRIMARY KEY (entries_id), \n"
            + "FOREIGN KEY (entry_types_id) REFERENCES entry_type(entry_types_id) \n"
            + ");");
           
        processDML(out,"CREATE TABLE groups ( \n"
            + "groups_id       INTEGER         NOT NULL AUTO_INCREMENT, \n"
            + "label           VARCHAR(100)     DEFAULT NULL, \n"
            + "parent_id       INTEGER          DEFAULT NULL, \n"
            + "PRIMARY KEY (groups_id) \n"
            + ");");
           
        processDML(out,"CREATE TABLE entry_group ( \n"
            + "entries_id       INTEGER        NOT NULL AUTO_INCREMENT, \n"
            + "groups_id        INTEGER        DEFAULT NULL, \n"
            + "FOREIGN KEY (entries_id) REFERENCES entry_fields(entries_id), \n"
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
            + "cite_key        VARCHAR(30)     DEFAULT NULL, "
            + dml2
            + ")");
          
        processDML(out,"ALTER TABLE entries ADD CONSTRAINT entries_fk "
                     + "FOREIGN KEY (\"entry_types_id\") REFERENCES \"entry_type\" (\"entry_types_id\")");

        processDML(out,"CREATE TABLE groups ( "
            + "groups_id       INTEGER         NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, "
            + "label           VARCHAR(100)     DEFAULT NULL, "
            + "parent_id       INTEGER          DEFAULT NULL  "
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
                    dml = dml + "\"" + val.replaceAll("\"", "\\\\\"") + "\"";
                } else {
                    dml = dml + "NULL";
                }
            }
            dml = dml + ");";

            
            processDML(out, dml);

        }

        return;

    }

     
	private static int dmlPopTab_GP (GroupTreeNode cursor, Object out) 
                        throws Exception {
        int cnt = dmlPopTab_GP_worker(cursor, 1, 1, out);
        return cnt;
    }

    
	private static int dmlPopTab_GP_worker (GroupTreeNode cursor, int parentID,
            int ID, Object out) throws SQLException{

        
        processDML(out, "INSERT INTO groups (groups_id, label, parent_id) " 
				      + "VALUES (" + ID + ", \"" + cursor.getGroup().getName() 
				      + "\", " + parentID + ");");

		
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
