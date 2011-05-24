package com.sleepycat.je.log; 
import java.io.File; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Map; 
import java.util.Set; 
import java.util.logging.Level; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  LastFileReader  extends FileReader {
	 private Set trackableEntries;

	 private long nextUnprovenOffset;

	 private long lastValidOffset;

	 private LogEntryType entryType;

	 private Map lastOffsetSeen;

	 public LastFileReader( EnvironmentImpl env, int readBufferSize) throws IOException, DatabaseException { super(env,readBufferSize,true,DbLsn.NULL_LSN,new Long(-1),DbLsn.NULL_LSN,DbLsn.NULL_LSN); trackableEntries=new HashSet(); lastOffsetSeen=new HashMap(); lastValidOffset=0; nextUnprovenOffset=0; anticipateChecksumErrors=true; }

	 public LastFileReader( EnvironmentImpl env, int readBufferSize, Long specificFileNumber) throws IOException, DatabaseException { super(env,readBufferSize,true,DbLsn.NULL_LSN,specificFileNumber,DbLsn.NULL_LSN,DbLsn.NULL_LSN); trackableEntries=new HashSet(); lastOffsetSeen=new HashMap(); lastValidOffset=0; nextUnprovenOffset=0; anticipateChecksumErrors=true; }

	 protected void initStartingPosition__wrappee__base( long endOfFileLsn, Long singleFileNum) throws IOException, DatabaseException { eof=false; Long lastNum=((singleFileNum != null) && (singleFileNum.longValue() >= 0)) ? singleFileNum : fileManager.getLastFileNum(); FileHandle fileHandle=null; readBufferFileEnd=0; long fileLen=0; while ((fileHandle == null) && !eof) { if (lastNum == null) { eof=true; } else { try { readBufferFileNum=lastNum.longValue(); fileHandle=fileManager.getFileHandle(readBufferFileNum); fileLen=fileHandle.getFile().length(); if (fileLen <= FileManager.firstLogEntryOffset()) { lastNum=fileManager.getFollowingFileNum(lastNum.longValue(),false); this.hook477(fileHandle); fileHandle=null; } } catch ( DatabaseException e) { lastNum=attemptToMoveBadFile(e); fileHandle=null; } finally { this.hook478(fileHandle); } } } nextEntryOffset=0; }

	 protected void initStartingPosition( long endOfFileLsn, Long singleFileNum) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initStartingPosition__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private Long attemptToMoveBadFile__wrappee__base( DatabaseException origException) throws DatabaseException, IOException { String fileName=fileManager.getFullFileNames(readBufferFileNum)[0]; File problemFile=new File(fileName); Long lastNum=null; if (problemFile.length() <= FileManager.firstLogEntryOffset()) { fileManager.clear(); lastNum=fileManager.getFollowingFileNum(readBufferFileNum,false); fileManager.renameFile(readBufferFileNum,FileManager.BAD_SUFFIX); } else { throw origException; } return lastNum; }

	 private Long attemptToMoveBadFile( DatabaseException origException) throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	attemptToMoveBadFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setEndOfFile__wrappee__base() throws IOException, DatabaseException { fileManager.truncateLog(readBufferFileNum,nextUnprovenOffset); }

	 public void setEndOfFile() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setEndOfFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getEndOfLog__wrappee__base(){ return DbLsn.makeLsn(readBufferFileNum,nextUnprovenOffset); }

	 public long getEndOfLog(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEndOfLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLastValidLsn__wrappee__base(){ return DbLsn.makeLsn(readBufferFileNum,lastValidOffset); }

	 public long getLastValidLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastValidLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getPrevOffset__wrappee__base(){ return lastValidOffset; }

	 public long getPrevOffset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getEntryType__wrappee__base(){ return entryType; }

	 public LogEntryType getEntryType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEntryType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTargetType__wrappee__base( LogEntryType type){ trackableEntries.add(type); }

	 public void setTargetType( LogEntryType type){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTargetType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLastSeen__wrappee__base( LogEntryType type){ Long typeNumber=(Long)lastOffsetSeen.get(type); if (typeNumber != null) { return DbLsn.makeLsn(readBufferFileNum,typeNumber.longValue()); } else { return DbLsn.NULL_LSN; } }

	 public long getLastSeen( LogEntryType type){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastSeen__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean processEntry__wrappee__base( ByteBuffer entryBuffer){ entryBuffer.position(entryBuffer.position() + currentEntrySize); entryType=new LogEntryType(currentEntryTypeNum,currentEntryTypeVersion); if (trackableEntries.contains(entryType)) { lastOffsetSeen.put(entryType,new Long(currentEntryOffset)); } return true; }

	 protected boolean processEntry( ByteBuffer entryBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean readNextEntry__wrappee__base() throws DatabaseException, IOException { boolean foundEntry=false; nextUnprovenOffset=nextEntryOffset; try { foundEntry=super.readNextEntry(); lastValidOffset=currentEntryOffset; } catch ( DbChecksumException e) { Tracer.trace(Level.INFO,env,"Found checksum exception while searching " + " for end of log. Last valid entry is at " + DbLsn.toString(DbLsn.makeLsn(readBufferFileNum,lastValidOffset)) + " Bad entry is at "+ DbLsn.makeLsn(readBufferFileNum,currentEntryOffset)); } return foundEntry; }

	 public boolean readNextEntry() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readNextEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook477__wrappee__base( FileHandle fileHandle) throws IOException, DatabaseException { }

	 protected void hook477( FileHandle fileHandle) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook477__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook478__wrappee__base( FileHandle fileHandle) throws IOException, DatabaseException { }

	 protected void hook478( FileHandle fileHandle) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook478__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
