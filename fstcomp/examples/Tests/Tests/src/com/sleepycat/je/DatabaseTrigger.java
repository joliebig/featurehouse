package com.sleepycat.je; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
 
interface  DatabaseTrigger {
	 void triggerAdded( Database db);

	 void triggerRemoved( Database db);

	 void databaseUpdated( Database db, Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException ;


}
