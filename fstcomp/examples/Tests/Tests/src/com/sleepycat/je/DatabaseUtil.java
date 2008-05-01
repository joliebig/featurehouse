package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
 
class  DatabaseUtil {
	 static void checkForNullParam( Object param, String name){ if (param == null) { throw new NullPointerException(name + " cannot be null"); } }

	 static void checkForNullDbt( DatabaseEntry dbt, String name, boolean checkData){ if (dbt == null) { throw new NullPointerException("DatabaseEntry " + name + " cannot be null"); } if (checkData) { if (dbt.getData() == null) { throw new NullPointerException("Data field for DatabaseEntry " + name + " cannot be null"); } } }

	 static void checkForPartialKey( DatabaseEntry dbt){ if (dbt.getPartial()) { throw new IllegalArgumentException("A partial key DatabaseEntry is not allowed"); } }


}
