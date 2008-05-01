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

	 protected void initStartingPosition( long endOfFileLsn, Long singleFileNum) throws IOException, DatabaseException { eof=false; Long lastNum=((singleFileNum != null) && (singleFileNum.longValue() >= 0)) ? singleFileNum : fileManager.getLastFileNum(); FileHandle fileHandle=null; readBufferFileEnd=0; long fileLen=0; while ((fileHandle == null) && !eof) { if (lastNum == null) { eof=true; } else { try { readBufferFileNum=lastNum.longValue(); fileHandle=fileManager.getFileHandle(readBufferFileNum); fileLen=fileHandle.getFile().length(); if (fileLen <= FileManager.firstLogEntryOffset()) { lastNum=fileManager.getFollowingFileNum(lastNum.longValue(),false); this.hook477(fileHandle); fileHandle=null; } } catch ( DatabaseException e) { lastNum=attemptToMoveBadFile(e); fileHandle=null; } finally { this.hook478(fileHandle); } } } nextEntryOffset=0; }

	 private Long attemptToMoveBadFile( DatabaseException origException) throws DatabaseException, IOException { String fileName=fileManager.getFullFileNames(readBufferFileNum)[0]; File problemFile=new File(fileName); Long lastNum=null; if (problemFile.length() <= FileManager.firstLogEntryOffset()) { fileManager.clear(); lastNum=fileManager.getFollowingFileNum(readBufferFileNum,false); fileManager.renameFile(readBufferFileNum,FileManager.BAD_SUFFIX); } else { throw origException; } return lastNum; }

	 public void setEndOfFile() throws IOException, DatabaseException { fileManager.truncateLog(readBufferFileNum,nextUnprovenOffset); }

	 public long getEndOfLog(){ return DbLsn.makeLsn(readBufferFileNum,nextUnprovenOffset); }

	 public long getLastValidLsn(){ return DbLsn.makeLsn(readBufferFileNum,lastValidOffset); }

	 public long getPrevOffset(){ return lastValidOffset; }

	 public LogEntryType getEntryType(){ return entryType; }

	 public void setTargetType( LogEntryType type){ trackableEntries.add(type); }

	 public long getLastSeen( LogEntryType type){ Long typeNumber=(Long)lastOffsetSeen.get(type); if (typeNumber != null) { return DbLsn.makeLsn(readBufferFileNum,typeNumber.longValue()); } else { return DbLsn.NULL_LSN; } }

	 protected boolean processEntry( ByteBuffer entryBuffer){ entryBuffer.position(entryBuffer.position() + currentEntrySize); entryType=new LogEntryType(currentEntryTypeNum,currentEntryTypeVersion); if (trackableEntries.contains(entryType)) { lastOffsetSeen.put(entryType,new Long(currentEntryOffset)); } return true; }

	 public boolean readNextEntry() throws DatabaseException, IOException { boolean foundEntry=false; nextUnprovenOffset=nextEntryOffset; try { foundEntry=super.readNextEntry(); lastValidOffset=currentEntryOffset; } catch ( DbChecksumException e) { Tracer.trace(Level.INFO,env,"Found checksum exception while searching " + " for end of log. Last valid entry is at " + DbLsn.toString(DbLsn.makeLsn(readBufferFileNum,lastValidOffset)) + " Bad entry is at "+ DbLsn.makeLsn(readBufferFileNum,currentEntryOffset)); } return foundEntry; }

	 protected void hook477( FileHandle fileHandle) throws IOException, DatabaseException { }

	 protected void hook478( FileHandle fileHandle) throws IOException, DatabaseException { }


}
