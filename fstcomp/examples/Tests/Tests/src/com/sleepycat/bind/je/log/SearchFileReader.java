package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.entry.LogEntry; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  SearchFileReader  extends FileReader {
	 private LogEntryType targetType;

	 private LogEntry logEntry;

	 public SearchFileReader( EnvironmentImpl env, int readBufferSize, boolean forward, long startLsn, long endOfFileLsn, LogEntryType targetType) throws IOException, DatabaseException { super(env,readBufferSize,forward,startLsn,null,endOfFileLsn,DbLsn.NULL_LSN); this.targetType=targetType; logEntry=targetType.getNewLogEntry(); }

	 protected boolean isTargetEntry( byte logEntryTypeNumber, byte logEntryTypeVersion){ return (targetType.equalsType(logEntryTypeNumber,logEntryTypeVersion)); }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { logEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); return true; }

	 public Object getLastObject(){ return logEntry.getMainItem(); }


}
