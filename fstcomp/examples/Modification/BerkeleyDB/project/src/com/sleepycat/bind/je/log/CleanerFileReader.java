package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.HashMap; 
import java.util.Map; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.entry.INLogEntry; 
import com.sleepycat.je.log.entry.LNLogEntry; 
import com.sleepycat.je.log.entry.LogEntry; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  CleanerFileReader  extends FileReader {
	 private static final byte IS_IN=0;

	 private static final byte IS_LN=1;

	 private static final byte IS_ROOT=2;

	 private Map targetEntryMap;

	 private LogEntry targetLogEntry;

	 private byte targetCategory;

	 public CleanerFileReader( EnvironmentImpl env, int readBufferSize, long startLsn, Long fileNum) throws IOException, DatabaseException { super(env,readBufferSize,true,startLsn,fileNum,DbLsn.NULL_LSN,DbLsn.NULL_LSN); targetEntryMap=new HashMap(); addTargetType(IS_LN,LogEntryType.LOG_LN_TRANSACTIONAL); addTargetType(IS_LN,LogEntryType.LOG_LN); addTargetType(IS_LN,LogEntryType.LOG_NAMELN_TRANSACTIONAL); addTargetType(IS_LN,LogEntryType.LOG_NAMELN); addTargetType(IS_LN,LogEntryType.LOG_MAPLN_TRANSACTIONAL); addTargetType(IS_LN,LogEntryType.LOG_MAPLN); addTargetType(IS_LN,LogEntryType.LOG_DEL_DUPLN_TRANSACTIONAL); addTargetType(IS_LN,LogEntryType.LOG_DEL_DUPLN); addTargetType(IS_LN,LogEntryType.LOG_DUPCOUNTLN_TRANSACTIONAL); addTargetType(IS_LN,LogEntryType.LOG_DUPCOUNTLN); addTargetType(IS_LN,LogEntryType.LOG_FILESUMMARYLN); addTargetType(IS_IN,LogEntryType.LOG_IN); addTargetType(IS_IN,LogEntryType.LOG_BIN); addTargetType(IS_IN,LogEntryType.LOG_DIN); addTargetType(IS_IN,LogEntryType.LOG_DBIN); addTargetType(IS_ROOT,LogEntryType.LOG_ROOT); }

	
private static  class  EntryInfo {
		 public LogEntry targetLogEntry;

		 public byte targetCategory;

		 EntryInfo( LogEntry targetLogEntry, byte targetCategory){ this.targetLogEntry=targetLogEntry; this.targetCategory=targetCategory; }


	}

	 private void addTargetType__wrappee__base( byte category, LogEntryType entryType) throws DatabaseException { targetEntryMap.put(entryType,new EntryInfo(entryType.getNewLogEntry(),category)); }

	 private void addTargetType( byte category, LogEntryType entryType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addTargetType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void initStartingPosition__wrappee__base( long endOfFileLsn, Long fileNum) throws IOException, DatabaseException { eof=false; readBufferFileNum=fileNum.longValue(); readBufferFileEnd=0; nextEntryOffset=readBufferFileEnd; }

	 protected void initStartingPosition( long endOfFileLsn, Long fileNum) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initStartingPosition__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isTargetEntry__wrappee__base( byte entryTypeNum, byte entryTypeVersion){ LogEntryType fromLogType=new LogEntryType(entryTypeNum,entryTypeVersion); EntryInfo info=(EntryInfo)targetEntryMap.get(fromLogType); if (info == null) { return false; } else { targetCategory=info.targetCategory; targetLogEntry=info.targetLogEntry; return true; } }

	 protected boolean isTargetEntry( byte entryTypeNum, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTargetEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean processEntry__wrappee__base( ByteBuffer entryBuffer) throws DatabaseException { targetLogEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); return true; }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isIN__wrappee__base(){ return (targetCategory == IS_IN); }

	 public boolean isIN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isLN__wrappee__base(){ return (targetCategory == IS_LN); }

	 public boolean isLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isRoot__wrappee__base(){ return (targetCategory == IS_ROOT); }

	 public boolean isRoot(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LN getLN__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getLN(); }

	 public LN getLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public IN getIN__wrappee__base() throws DatabaseException { return ((INLogEntry)targetLogEntry).getIN(env); }

	 public IN getIN() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDatabaseId__wrappee__base(){ if (targetCategory == IS_LN) { return ((LNLogEntry)targetLogEntry).getDbId(); } else if (targetCategory == IS_IN) { return ((INLogEntry)targetLogEntry).getDbId(); } else { return null; } }

	 public DatabaseId getDatabaseId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getKey(); }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupTreeKey__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getDupKey(); }

	 public byte[] getDupTreeKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupTreeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
