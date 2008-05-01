package com.sleepycat.je.log.entry; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  interface  LogEntry  extends Cloneable {
	 void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException ;

	 StringBuffer dumpEntry( StringBuffer sb, boolean verbose);

	 Object getMainItem();

	 public Object clone() throws CloneNotSupportedException ;

	 public boolean isTransactional();

	 public long getTransactionId();


}
