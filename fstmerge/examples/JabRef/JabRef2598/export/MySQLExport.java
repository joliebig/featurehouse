package net.sf.jabref.export; 



import java.util.Set; 

import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.Globals; 
import net.sf.jabref.MetaData; 
import net.sf.jabref.sql.SQLutil; 


public  class  MySQLExport  extends ExportFormat {
	

    public MySQLExport() {
        super(Globals.lang("MySQL database"), "mysql", null, null, ".sql");
    }


	

    
    public void performExport(final BibtexDatabase database,
        final MetaData metaData, final String file, final String encoding,
        Set<String> keySet) throws Exception {

        SQLutil.exportDatabase(database, metaData, keySet, file, SQLutil.DBTYPE.MYSQL);

    }



}
