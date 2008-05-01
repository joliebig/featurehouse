package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public  class  INDupDeleteInfo  implements LoggableObject, LogReadable, LogWritable {
	 private long deletedNodeId;

	 private byte[] deletedMainKey;

	 private byte[] deletedDupKey;

	 private DatabaseId dbId;

	 public INDupDeleteInfo( long deletedNodeId, byte[] deletedMainKey, byte[] deletedDupKey, DatabaseId dbId){ this.deletedNodeId=deletedNodeId; this.deletedMainKey=deletedMainKey; this.deletedDupKey=deletedDupKey; this.dbId=dbId; }

	 public INDupDeleteInfo(){ dbId=new DatabaseId(); }

	 public long getDeletedNodeId(){ return deletedNodeId; }

	 public byte[] getDeletedMainKey(){ return deletedMainKey; }

	 public byte[] getDeletedDupKey(){ return deletedDupKey; }

	 public DatabaseId getDatabaseId(){ return dbId; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_IN_DUPDELETE_INFO; }

	 public boolean marshallOutsideWriteLatch(){ return true; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn){ }

	 public int getLogSize(){ return LogUtils.LONG_BYTES + LogUtils.getByteArrayLogSize(deletedMainKey) + LogUtils.getByteArrayLogSize(deletedDupKey)+ dbId.getLogSize(); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,deletedNodeId); LogUtils.writeByteArray(logBuffer,deletedMainKey); LogUtils.writeByteArray(logBuffer,deletedDupKey); dbId.writeToLog(logBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { deletedNodeId=LogUtils.readLong(itemBuffer); deletedMainKey=LogUtils.readByteArray(itemBuffer); deletedDupKey=LogUtils.readByteArray(itemBuffer); dbId.readFromLog(itemBuffer,entryTypeVersion); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<INDupDeleteEntry node=\"").append(deletedNodeId); sb.append("\">"); sb.append(Key.dumpString(deletedMainKey,0)); sb.append(Key.dumpString(deletedDupKey,0)); dbId.dumpLog(sb,verbose); sb.append("</INDupDeleteEntry>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }


}
