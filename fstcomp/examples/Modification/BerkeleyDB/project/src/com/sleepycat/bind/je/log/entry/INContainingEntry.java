package com.sleepycat.je.log.entry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.tree.IN; 
import de.ovgu.cide.jakutil.*; 
public  interface  INContainingEntry {
	 public IN getIN( EnvironmentImpl env) throws DatabaseException ;

	 public DatabaseId getDbId();

	 public long getLsnOfIN( long lastReadLsn);


}
