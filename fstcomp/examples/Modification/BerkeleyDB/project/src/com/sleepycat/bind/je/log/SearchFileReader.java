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

	 protected boolean isTargetEntry__wrappee__base( byte logEntryTypeNumber, byte logEntryTypeVersion){ return (targetType.equalsType(logEntryTypeNumber,logEntryTypeVersion)); }

	 protected boolean isTargetEntry( byte logEntryTypeNumber, byte logEntryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTargetEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean processEntry__wrappee__base( ByteBuffer entryBuffer) throws DatabaseException { logEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); return true; }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object getLastObject__wrappee__base(){ return logEntry.getMainItem(); }

	 public Object getLastObject(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
