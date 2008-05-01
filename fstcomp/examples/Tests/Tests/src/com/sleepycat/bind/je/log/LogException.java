package com.sleepycat.je.log; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  LogException  extends DatabaseException {
	 public LogException( String message){ super(message); }

	 public LogException( String message, Exception e){ super(message,e); }


}
