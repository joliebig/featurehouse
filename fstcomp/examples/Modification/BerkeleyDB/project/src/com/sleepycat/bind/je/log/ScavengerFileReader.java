package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.HashSet; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.entry.LogEntry; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
abstract public  class  ScavengerFileReader  extends FileReader {
	 private Set targetEntryTypes;

	 private int readBufferSize;

	 private boolean dumpCorruptedBounds;

	 public ScavengerFileReader( EnvironmentImpl env, int readBufferSize, long startLsn, long finishLsn, long endOfFileLsn) throws IOException, DatabaseException { super(env,readBufferSize,false,startLsn,null,endOfFileLsn,finishLsn); this.readBufferSize=readBufferSize; anticipateChecksumErrors=true; targetEntryTypes=new HashSet(); dumpCorruptedBounds=false; }

	 public void setDumpCorruptedBounds__wrappee__base( boolean dumpCorruptedBounds){ this.dumpCorruptedBounds=dumpCorruptedBounds; }

	 public void setDumpCorruptedBounds( boolean dumpCorruptedBounds){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDumpCorruptedBounds__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTargetType__wrappee__base( LogEntryType type){ targetEntryTypes.add(new Byte(type.getTypeNum())); }

	 public void setTargetType( LogEntryType type){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTargetType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean processEntry__wrappee__base( ByteBuffer entryBuffer) throws DatabaseException { LogEntryType lastEntryType=LogEntryType.findType(currentEntryTypeNum,currentEntryTypeVersion); LogEntry entry=lastEntryType.getSharedLogEntry(); entry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); processEntryCallback(entry,lastEntryType); return true; }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected void processEntryCallback__wrappee__base( LogEntry entry, LogEntryType entryType) throws DatabaseException ;

	 abstract protected void processEntryCallback( LogEntry entry, LogEntryType entryType) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntryCallback__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean readNextEntry__wrappee__base() throws DatabaseException, IOException { long saveCurrentEntryOffset=currentEntryOffset; try { return super.readNextEntry(); } catch ( DbChecksumException DCE) { resyncReader(DbLsn.makeLsn(readBufferFileNum,saveCurrentEntryOffset),dumpCorruptedBounds); return super.readNextEntry(); } }

	 public boolean readNextEntry() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readNextEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean resyncReader__wrappee__base( long nextGoodRecordPostCorruption, boolean showCorruptedBounds) throws DatabaseException, IOException { LastFileReader reader=null; long tryReadBufferFileNum=DbLsn.getFileNumber(nextGoodRecordPostCorruption); while (tryReadBufferFileNum >= 0) { try { reader=new LastFileReader(env,readBufferSize,new Long(tryReadBufferFileNum)); break; } catch ( DbChecksumException DCE) { tryReadBufferFileNum--; continue; } } boolean switchedFiles=tryReadBufferFileNum != DbLsn.getFileNumber(nextGoodRecordPostCorruption); if (!switchedFiles) { while (reader.readNextEntry()) { } } long lastUsedLsn=reader.getLastValidLsn(); long nextAvailableLsn=reader.getEndOfLog(); if (showCorruptedBounds) { System.err.println("A checksum error was found in the log."); System.err.println("Corruption begins at LSN:\n   " + DbLsn.toString(nextAvailableLsn)); System.err.println("Last known good record before corruption is at LSN:\n   " + DbLsn.toString(lastUsedLsn)); System.err.println("Next known good record after corruption is at LSN:\n   " + DbLsn.toString(nextGoodRecordPostCorruption)); } startLsn=lastUsedLsn; initStartingPosition(nextAvailableLsn,null); if (switchedFiles) { currentEntryPrevOffset=0; } return true; }

	 protected boolean resyncReader( long nextGoodRecordPostCorruption, boolean showCorruptedBounds) throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	resyncReader__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isTargetEntry__wrappee__base( byte logEntryTypeNumber, byte logEntryTypeVersion){ if (targetEntryTypes.size() == 0) { return true; } else { return targetEntryTypes.contains(new Byte(logEntryTypeNumber)); } }

	 protected boolean isTargetEntry( byte logEntryTypeNumber, byte logEntryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTargetEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
