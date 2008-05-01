package com.sleepycat.je.log; 
import java.io.File; 
import java.io.FileNotFoundException; 
import java.io.IOException; 
import java.io.RandomAccessFile; 
import java.nio.ByteBuffer; 
import java.nio.channels.ClosedChannelException; 
import java.nio.channels.FileChannel; 
import java.nio.channels.FileLock; 
import java.nio.channels.OverlappingFileLockException; 
import java.util.Arrays; 
import java.util.HashMap; 
import java.util.Hashtable; 
import java.util.Iterator; 
import java.util.LinkedList; 
import java.util.Map; 
import java.util.Random; 
import java.util.Set; 
import java.util.zip.Checksum; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.RunRecoveryException; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.entry.LogEntry; 
import com.sleepycat.je.utilint.Adler32; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.HexFormatter; 
import de.ovgu.cide.jakutil.*; 
public  class  FileManager {
	
public static  class  FileMode {
		 public static final FileMode READ_MODE=new FileMode("r");

		 public static final FileMode READWRITE_MODE=new FileMode("rw");

		 private String fileModeValue;

		 private FileMode( String fileModeValue){ this.fileModeValue=fileModeValue; }

		 public String getModeValue(){ return fileModeValue; }


	}

	 static boolean IO_EXCEPTION_TESTING=false;

	 private static final String DEBUG_NAME=FileManager.class.getName();

	 private static long writeCount=0;

	 private static long stopOnWriteCount=Long.MAX_VALUE;

	 public static final String JE_SUFFIX=".jdb";

	 public static final String CIF_SUFFIX=".cif";

	 public static final String DEL_SUFFIX=".del";

	 public static final String BAD_SUFFIX=".bad";

	 public static final String LOCK_SUFFIX=".lck";

	 static final String[] DEL_SUFFIXES={DEL_SUFFIX};

	 static final String[] JE_SUFFIXES={JE_SUFFIX};

	 private static final String[] JE_AND_DEL_SUFFIXES={JE_SUFFIX,DEL_SUFFIX};

	 private boolean syncAtFileEnd=true;

	 private EnvironmentImpl envImpl;

	 private long maxFileSize;

	 private File dbEnvHome;

	 private boolean includeDeletedFiles=false;

	 private boolean readOnly;

	 private long currentFileNum;

	 private long nextAvailableLsn;

	 private long lastUsedLsn;

	 private long prevOffset;

	 private boolean forceNewFile;

	 private long savedCurrentFileNum;

	 private long savedNextAvailableLsn;

	 private long savedLastUsedLsn;

	 private long savedPrevOffset;

	 private boolean savedForceNewFile;

	 private LogEndFileDescriptor endOfLog;

	 private Map perFileLastUsedLsn;

	 public FileManager( EnvironmentImpl envImpl, File dbEnvHome, boolean readOnly) throws DatabaseException { this.envImpl=envImpl; this.dbEnvHome=dbEnvHome; this.readOnly=readOnly; DbConfigManager configManager=envImpl.getConfigManager(); maxFileSize=configManager.getLong(EnvironmentParams.LOG_FILE_MAX); this.hook456(configManager); this.hook467(readOnly); this.hook457(configManager); this.hook449(envImpl); if (!dbEnvHome.exists()) { throw new LogException("Environment home " + dbEnvHome + " doesn't exist"); } currentFileNum=0L; nextAvailableLsn=DbLsn.makeLsn(currentFileNum,firstLogEntryOffset()); lastUsedLsn=DbLsn.NULL_LSN; perFileLastUsedLsn=new HashMap(); prevOffset=0L; endOfLog=new LogEndFileDescriptor(); forceNewFile=false; saveLastPosition(); String stopOnWriteProp=System.getProperty("je.debug.stopOnWrite"); if (stopOnWriteProp != null) { stopOnWriteCount=Long.parseLong(stopOnWriteProp); } this.hook452(envImpl); }

	 public void setLastPosition( long nextAvailableLsn, long lastUsedLsn, long prevOffset){ this.lastUsedLsn=lastUsedLsn; perFileLastUsedLsn.put(new Long(DbLsn.getFileNumber(lastUsedLsn)),new Long(lastUsedLsn)); this.nextAvailableLsn=nextAvailableLsn; currentFileNum=DbLsn.getFileNumber(this.nextAvailableLsn); this.prevOffset=prevOffset; saveLastPosition(); }

	 void saveLastPosition(){ savedNextAvailableLsn=nextAvailableLsn; savedLastUsedLsn=lastUsedLsn; savedPrevOffset=prevOffset; savedForceNewFile=forceNewFile; savedCurrentFileNum=currentFileNum; }

	 void restoreLastPosition(){ nextAvailableLsn=savedNextAvailableLsn; lastUsedLsn=savedLastUsedLsn; prevOffset=savedPrevOffset; forceNewFile=savedForceNewFile; currentFileNum=savedCurrentFileNum; }

	 public void setSyncAtFileEnd( boolean sync){ syncAtFileEnd=sync; }

	 public Long getFirstFileNum(){ return getFileNum(true); }

	 public boolean getReadOnly(){ return readOnly; }

	 public Long getLastFileNum(){ return getFileNum(false); }

	 public long getCurrentFileNum(){ return currentFileNum; }

	 public void setIncludeDeletedFiles( boolean includeDeletedFiles){ this.includeDeletedFiles=includeDeletedFiles; }

	 public Long[] getAllFileNumbers(){ String[] names=listFiles(JE_SUFFIXES); Long[] nums=new Long[names.length]; for (int i=0; i < nums.length; i+=1) { nums[i]=getNumFromName(names[i]); } return nums; }

	 public Long getFollowingFileNum( long currentFileNum, boolean forward){ String[] names=listFiles(JE_SUFFIXES); String searchName=getFileName(currentFileNum,JE_SUFFIX); int foundIdx=Arrays.binarySearch(names,searchName); boolean foundTarget=false; if (foundIdx >= 0) { if (forward) { foundIdx++; } else { foundIdx--; } } else { foundIdx=Math.abs(foundIdx + 1); if (!forward) { foundIdx--; } } if (forward && (foundIdx < names.length)) { foundTarget=true; } else if (!forward && (foundIdx > -1)) { foundTarget=true; } if (foundTarget) { return getNumFromName(names[foundIdx]); } else { return null; } }

	 public boolean filesExist(){ String[] names=listFiles(JE_SUFFIXES); return (names.length != 0); }

	 private Long getFileNum( boolean first){ String[] names=listFiles(JE_SUFFIXES); if (names.length == 0) { return null; } else { int index=0; if (!first) { index=names.length - 1; } return getNumFromName(names[index]); } }

	 private Long getNumFromName( String fileName){ String fileNumber=fileName.substring(0,fileName.indexOf(".")); return new Long(Long.parseLong(fileNumber,16)); }

	 String[] listFiles( String[] suffixes){ String[] fileNames=dbEnvHome.list(new JEFileFilter(suffixes)); Arrays.sort(fileNames); return fileNames; }

	 public static String[] listFiles( File envDirFile, String[] suffixes){ String[] fileNames=envDirFile.list(new JEFileFilter(suffixes)); Arrays.sort(fileNames); return fileNames; }

	 String[] getFullFileNames( long fileNum){ if (includeDeletedFiles) { int nSuffixes=JE_AND_DEL_SUFFIXES.length; String[] ret=new String[nSuffixes]; for (int i=0; i < nSuffixes; i++) { ret[i]=getFullFileName(getFileName(fileNum,JE_AND_DEL_SUFFIXES[i])); } return ret; } else { return new String[]{getFullFileName(getFileName(fileNum,JE_SUFFIX))}; } }

	 public String getFullFileName( long fileNum, String suffix){ return getFullFileName(getFileName(fileNum,suffix)); }

	 private String getFullFileName( String fileName){ return dbEnvHome + File.separator + fileName; }

	 public static String getFileName( long fileNum, String suffix){ return (HexFormatter.formatLong(fileNum).substring(10) + suffix); }

	 public void renameFile( long fileNum, String newSuffix) throws DatabaseException, IOException { int repeatNum=0; boolean renamed=false; while (!renamed) { String generation=""; if (repeatNum > 0) { generation="." + repeatNum; } String newName=getFullFileName(getFileName(fileNum,newSuffix) + generation); File targetFile=new File(newName); if (targetFile.exists()) { repeatNum++; } else { String oldFileName=getFullFileNames(fileNum)[0]; this.hook458(fileNum); File oldFile=new File(oldFileName); if (oldFile.renameTo(targetFile)) { renamed=true; } else { throw new LogException("Couldn't rename " + oldFileName + " to "+ newName); } } } }

	 public void deleteFile( long fileNum) throws DatabaseException, IOException { String fileName=getFullFileNames(fileNum)[0]; this.hook459(fileNum); File file=new File(fileName); boolean done=file.delete(); if (!done) { throw new LogException("Couldn't delete " + file); } }

	 FileHandle getFileHandle( long fileNum) throws LogException, DatabaseException { try { Long fileId=new Long(fileNum); FileHandle fileHandle=null; this.hook460(fileNum,fileId,fileHandle); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (FileHandle)r.value; } }

	 private FileHandle makeFileHandle( long fileNum, FileMode mode) throws DatabaseException { String[] fileNames=getFullFileNames(fileNum); RandomAccessFile newFile=null; String fileName=null; try { FileNotFoundException FNFE=null; for (int i=0; i < fileNames.length; i++) { fileName=fileNames[i]; try { newFile=new RandomAccessFile(fileName,mode.getModeValue()); break; } catch ( FileNotFoundException e) { if (FNFE == null) { FNFE=e; } } } if (newFile == null) { throw FNFE; } boolean oldHeaderVersion=false; if (newFile.length() == 0) { if (mode == FileMode.READWRITE_MODE) { long lastLsn=DbLsn.longToLsn((Long)perFileLastUsedLsn.remove(new Long(fileNum - 1))); long headerPrevOffset=0; if (lastLsn != DbLsn.NULL_LSN) { headerPrevOffset=DbLsn.getFileOffset(lastLsn); } FileHeader fileHeader=new FileHeader(fileNum,headerPrevOffset); writeFileHeader(newFile,fileName,fileHeader); } } else { oldHeaderVersion=readAndValidateFileHeader(newFile,fileName,fileNum); } return new FileHandle(newFile,fileName,envImpl,oldHeaderVersion); } catch ( FileNotFoundException e) { throw new LogFileNotFoundException("Couldn't open file " + fileName + ": "+ e.getMessage()); }
catch ( DbChecksumException e) { closeFileInErrorCase(newFile); throw new DbChecksumException(envImpl,"Couldn't open file " + fileName,e); }
catch ( Throwable t) { closeFileInErrorCase(newFile); throw new DatabaseException("Couldn't open file " + fileName + ": "+ t,t); } }

	 private void closeFileInErrorCase( RandomAccessFile file){ try { if (file != null) { file.close(); } } catch ( IOException e) { } }

	 private boolean readAndValidateFileHeader( RandomAccessFile file, String fileName, long fileNum) throws DatabaseException, IOException { LogManager logManager=envImpl.getLogManager(); LogEntry headerEntry=logManager.getLogEntry(DbLsn.makeLsn(fileNum,0),file); FileHeader header=(FileHeader)headerEntry.getMainItem(); return header.validate(fileName,fileNum); }

	 private void writeFileHeader( RandomAccessFile file, String fileName, FileHeader header) throws DatabaseException, IOException { envImpl.checkIfInvalid(); if (envImpl.mayNotWrite()) { return; } int headerSize=header.getLogSize(); int entrySize=headerSize + LogManager.HEADER_BYTES; ByteBuffer headerBuf=envImpl.getLogManager().putIntoBuffer(header,headerSize,0,false,entrySize); if (++writeCount >= stopOnWriteCount) { Runtime.getRuntime().halt(0xff); } int bytesWritten; try { if (RUNRECOVERY_EXCEPTION_TESTING) { generateRunRecoveryException(file,headerBuf,0); } bytesWritten=writeToFile(file,headerBuf,0); } catch ( ClosedChannelException e) { throw new RunRecoveryException(envImpl,"Channel closed, may be due to thread interrupt",e); }
catch ( IOException e) { throw new RunRecoveryException(envImpl,"IOException caught: " + e); } if (bytesWritten != entrySize) { throw new LogException("File " + fileName + " was created with an incomplete header. Only "+ bytesWritten+ " bytes were written."); } }

	 long getFileHeaderPrevOffset( long fileNum) throws IOException, DatabaseException { LogEntry headerEntry=envImpl.getLogManager().getLogEntry(DbLsn.makeLsn(fileNum,0)); FileHeader header=(FileHeader)headerEntry.getMainItem(); return header.getLastEntryInPrevFileOffset(); }

	 long getPrevEntryOffset(){ return prevOffset; }

	 boolean bumpLsn( long size){ saveLastPosition(); boolean flippedFiles=false; if (forceNewFile || (DbLsn.getFileOffset(nextAvailableLsn) + size) > maxFileSize) { forceNewFile=false; currentFileNum++; if (lastUsedLsn != DbLsn.NULL_LSN) { perFileLastUsedLsn.put(new Long(DbLsn.getFileNumber(lastUsedLsn)),new Long(lastUsedLsn)); } prevOffset=0; lastUsedLsn=DbLsn.makeLsn(currentFileNum,firstLogEntryOffset()); flippedFiles=true; } else { if (lastUsedLsn == DbLsn.NULL_LSN) { prevOffset=0; } else { prevOffset=DbLsn.getFileOffset(lastUsedLsn); } lastUsedLsn=nextAvailableLsn; } nextAvailableLsn=DbLsn.makeLsn(DbLsn.getFileNumber(lastUsedLsn),(DbLsn.getFileOffset(lastUsedLsn) + size)); return flippedFiles; }

	 void writeLogBuffer( LogBuffer fullBuffer) throws DatabaseException { envImpl.checkIfInvalid(); if (envImpl.mayNotWrite()) { return; } long firstLsn=fullBuffer.getFirstLsn(); if (firstLsn != DbLsn.NULL_LSN) { RandomAccessFile file=endOfLog.getWritableFile(DbLsn.getFileNumber(firstLsn)); ByteBuffer data=fullBuffer.getDataBuffer(); if (++writeCount >= stopOnWriteCount) { Runtime.getRuntime().halt(0xff); } try { this.hook465(fullBuffer,firstLsn,file); if (IO_EXCEPTION_TESTING) { throw new IOException("generated for testing"); } if (RUNRECOVERY_EXCEPTION_TESTING) { generateRunRecoveryException(file,data,DbLsn.getFileOffset(firstLsn)); } writeToFile(file,data,DbLsn.getFileOffset(firstLsn)); } catch ( ClosedChannelException e) { throw new RunRecoveryException(envImpl,"File closed, may be due to thread interrupt",e); }
catch ( IOException IOE) { abortCommittedTxns(data); this.hook466(fullBuffer,firstLsn,file,data,IOE); } assert EnvironmentImpl.maybeForceYield(); } }

	 private int writeToFile( RandomAccessFile file, ByteBuffer data, long destOffset) throws IOException, DatabaseException { return new FileManager_writeToFile(this,file,data,destOffset).execute(); }

	 void readFromFile( RandomAccessFile file, ByteBuffer readBuffer, long offset) throws IOException { new FileManager_readFromFile(this,file,readBuffer,offset).execute(); }

	 private void abortCommittedTxns( ByteBuffer data){ final byte commitType=LogEntryType.LOG_TXN_COMMIT.getTypeNum(); final byte abortType=LogEntryType.LOG_TXN_ABORT.getTypeNum(); this.hook461(data); while (data.remaining() > 0) { int recStartPos=data.position(); data.position(recStartPos + LogManager.HEADER_ENTRY_TYPE_OFFSET); int typePos=data.position(); byte entryType=data.get(); boolean recomputeChecksum=false; if (entryType == commitType) { data.position(typePos); data.put(abortType); recomputeChecksum=true; } byte version=data.get(); data.position(data.position() + LogManager.PREV_BYTES); int itemSize=LogUtils.readInt(data); int itemDataStartPos=data.position(); if (recomputeChecksum) { Checksum checksum=Adler32.makeChecksum(); data.position(recStartPos); int nChecksumBytes=itemSize + (LogManager.HEADER_BYTES - LogManager.CHECKSUM_BYTES); byte[] checksumBytes=new byte[nChecksumBytes]; System.arraycopy(data.array(),recStartPos + LogManager.CHECKSUM_BYTES,checksumBytes,0,nChecksumBytes); checksum.update(checksumBytes,0,nChecksumBytes); LogUtils.writeUnsignedInt(data,checksum.getValue()); } data.position(itemDataStartPos + itemSize); } data.position(0); }

	 void syncLogEnd() throws DatabaseException { try { endOfLog.force(); } catch ( IOException e) { throw new DatabaseException(e); } }

	 void syncLogEndAndFinishFile() throws DatabaseException, IOException { if (syncAtFileEnd) { syncLogEnd(); } endOfLog.close(); }

	 public void clear() throws IOException, DatabaseException { endOfLog.close(); }

	 public void close() throws IOException, DatabaseException { }

	 private boolean checkEnvHomePermissions( boolean readOnly) throws DatabaseException { boolean envDirIsReadOnly=!dbEnvHome.canWrite(); if (envDirIsReadOnly && !readOnly) { throw new DatabaseException("The Environment directory " + dbEnvHome + " is not writable, but the "+ "Environment was opened for read-write access."); } return envDirIsReadOnly; }

	 public void truncateLog( long fileNum, long offset) throws IOException, DatabaseException { FileHandle handle=makeFileHandle(fileNum,FileMode.READWRITE_MODE); RandomAccessFile file=handle.getFile(); try { file.getChannel().truncate(offset); } finally { file.close(); } if (handle.isOldHeaderVersion()) { forceNewFile=true; } }

	 void forceNewLogFile(){ forceNewFile=true; }

	 public static int firstLogEntryOffset(){ return FileHeader.entrySize() + LogManager.HEADER_BYTES; }

	 public long getNextLsn(){ return nextAvailableLsn; }

	 public long getLastUsedLsn(){ return lastUsedLsn; }

	
 
class  LogEndFileDescriptor {
		 private RandomAccessFile endOfLogRWFile=null;

		 private RandomAccessFile endOfLogSyncFile=null;

		 RandomAccessFile getWritableFile( long fileNumber) throws RunRecoveryException { try { if (endOfLogRWFile == null) { endOfLogRWFile=makeFileHandle(fileNumber,FileMode.READWRITE_MODE).getFile(); endOfLogSyncFile=makeFileHandle(fileNumber,FileMode.READWRITE_MODE).getFile(); } return endOfLogRWFile; } catch ( Exception e) { throw new RunRecoveryException(envImpl,e); } }

		 void force() throws DatabaseException, IOException { RandomAccessFile file=endOfLogSyncFile; if (file != null) { FileChannel channel=file.getChannel(); try { channel.force(false); } catch ( ClosedChannelException e) { throw new RunRecoveryException(envImpl,"Channel closed, may be due to thread interrupt",e); } assert EnvironmentImpl.maybeForceYield(); } }

		 void close() throws IOException { IOException firstException=null; if (endOfLogRWFile != null) { RandomAccessFile file=endOfLogRWFile; endOfLogRWFile=null; try { file.close(); } catch ( IOException e) { firstException=e; } } if (endOfLogSyncFile != null) { RandomAccessFile file=endOfLogSyncFile; endOfLogSyncFile=null; file.close(); } if (firstException != null) { throw firstException; } }


	}

	 static boolean RUNRECOVERY_EXCEPTION_TESTING=false;

	 private static final int RUNRECOVERY_EXCEPTION_MAX=100;

	 private int runRecoveryExceptionCounter=0;

	 private boolean runRecoveryExceptionThrown=false;

	 private Random runRecoveryExceptionRandom=null;

	 private void generateRunRecoveryException( RandomAccessFile file, ByteBuffer data, long destOffset) throws DatabaseException, IOException { if (runRecoveryExceptionThrown) { try { throw new Exception("Write after RunRecoveryException"); } catch ( Exception e) { e.printStackTrace(); } } runRecoveryExceptionCounter+=1; if (runRecoveryExceptionCounter >= RUNRECOVERY_EXCEPTION_MAX) { runRecoveryExceptionCounter=0; } if (runRecoveryExceptionRandom == null) { runRecoveryExceptionRandom=new Random(System.currentTimeMillis()); } if (runRecoveryExceptionCounter == runRecoveryExceptionRandom.nextInt(RUNRECOVERY_EXCEPTION_MAX)) { int len=runRecoveryExceptionRandom.nextInt(data.remaining()); if (len > 0) { byte[] a=new byte[len]; data.get(a,0,len); ByteBuffer buf=ByteBuffer.wrap(a); writeToFile(file,buf,destOffset); } runRecoveryExceptionThrown=true; throw new RunRecoveryException(envImpl,"Randomly generated for testing"); } }

	
@MethodObject static  class  FileManager_writeToFile {
		 FileManager_writeToFile( FileManager _this, RandomAccessFile file, ByteBuffer data, long destOffset){ this._this=_this; this.file=file; this.data=data; this.destOffset=destOffset; }

		 int execute() throws IOException, DatabaseException { totalBytesWritten=0; this.hook455(); this.hook445(); return totalBytesWritten; }

		 protected FileManager _this;

		 protected RandomAccessFile file;

		 protected ByteBuffer data;

		 protected long destOffset;

		 protected int totalBytesWritten;

		 protected FileChannel channel;

		 protected ByteBuffer useData;

		 protected int origLimit;

		 protected int bytesWritten;

		 protected int pos;

		 protected int size;

		 protected void hook445() throws IOException, DatabaseException { }

		 protected void hook455() throws IOException, DatabaseException { }


	}

	
@MethodObject static  class  FileManager_readFromFile {
		 FileManager_readFromFile( FileManager _this, RandomAccessFile file, ByteBuffer readBuffer, long offset){ this._this=_this; this.file=file; this.readBuffer=readBuffer; this.offset=offset; }

		 void execute() throws IOException { this.hook446(); }

		 protected FileManager _this;

		 protected RandomAccessFile file;

		 protected ByteBuffer readBuffer;

		 protected long offset;

		 protected FileChannel channel;

		 protected int readLength;

		 protected long currentPosition;

		 protected int bytesRead1;

		 protected int pos;

		 protected int size;

		 protected int bytesRead2;

		 protected void hook446() throws IOException { }


	}

	 protected void hook449( EnvironmentImpl envImpl) throws DatabaseException { }

	 protected FileHandle hook450( long fileNum, Long fileId, FileHandle fileHandle) throws LogException, DatabaseException { fileHandle=this.hook462(fileNum,fileId,fileHandle); return fileHandle; }

	 protected void hook452( EnvironmentImpl envImpl) throws DatabaseException { }

	 protected void hook453( FileHandle fileHandle) throws LogException, DatabaseException { }

	 protected void hook454( FileHandle fileHandle) throws LogException, DatabaseException { }

	 protected void hook456( DbConfigManager configManager) throws DatabaseException { }

	 protected void hook457( DbConfigManager configManager) throws DatabaseException { }

	 protected void hook458( long fileNum) throws DatabaseException, IOException { }

	 protected void hook459( long fileNum) throws DatabaseException, IOException { }

	 protected void hook460( long fileNum, Long fileId, FileHandle fileHandle) throws LogException, DatabaseException { fileHandle=this.hook463(fileNum,fileId,fileHandle); this.hook453(fileHandle); if (fileHandle.getFile() == null) { this.hook454(fileHandle); } else { throw new ReturnObject(fileHandle); } }

	 protected void hook461( ByteBuffer data){ }

	 protected FileHandle hook462( long fileNum, Long fileId, FileHandle fileHandle) throws LogException, DatabaseException { fileHandle=makeFileHandle(fileNum,FileMode.READ_MODE); this.hook464(fileId,fileHandle); return fileHandle; }

	 protected FileHandle hook463( long fileNum, Long fileId, FileHandle fileHandle) throws LogException, DatabaseException { fileHandle=this.hook450(fileNum,fileId,fileHandle); return fileHandle; }

	 protected void hook464( Long fileId, FileHandle fileHandle) throws LogException, DatabaseException { }

	 protected void hook465( LogBuffer fullBuffer, long firstLsn, RandomAccessFile file) throws DatabaseException, ClosedChannelException, IOException { }

	 protected void hook466( LogBuffer fullBuffer, long firstLsn, RandomAccessFile file, ByteBuffer data, IOException IOE) throws DatabaseException { throw new DatabaseException(IOE); }

	 protected void hook467( boolean readOnly) throws DatabaseException { }


}
