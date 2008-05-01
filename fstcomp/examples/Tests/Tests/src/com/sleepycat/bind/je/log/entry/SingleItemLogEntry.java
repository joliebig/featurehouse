package com.sleepycat.je.log.entry; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.log.LogReadable; 
import de.ovgu.cide.jakutil.*; 
public  class  SingleItemLogEntry  implements LogEntry {
	 private Class logClass;

	 LogReadable item;

	 public SingleItemLogEntry( Class logClass){ this.logClass=logClass; }

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { try { item=(LogReadable)logClass.newInstance(); item.readFromLog(entryBuffer,entryTypeVersion); } catch ( IllegalAccessException e) { throw new DatabaseException(e); }
catch ( InstantiationException e) { throw new DatabaseException(e); } }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ item.dumpLog(sb,verbose); return sb; }

	 public Object getMainItem(){ return item; }

	 public Object clone() throws CloneNotSupportedException { return super.clone(); }

	 public boolean isTransactional(){ return item.logEntryIsTransactional(); }

	 public long getTransactionId(){ return item.getTransactionId(); }

	 public LogEntry getNewInstance() throws DatabaseException { try { return (LogEntry)logClass.newInstance(); } catch ( InstantiationException e) { throw new DatabaseException(e); }
catch ( IllegalAccessException e) { throw new DatabaseException(e); } }


}
