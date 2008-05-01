package com.sleepycat.je.log; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  interface  LoggableObject  extends LogWritable {
	 public LogEntryType getLogType();

	 public void postLogWork( long justLoggedLsn) throws DatabaseException ;

	 public boolean marshallOutsideWriteLatch();

	 public boolean countAsObsoleteWhenLogged();


}
