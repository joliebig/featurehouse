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
public  class  INDeleteInfo  implements LoggableObject, LogReadable, LogWritable {
	 private long deletedNodeId;

	 private byte[] deletedIdKey;

	 private DatabaseId dbId;

	 public INDeleteInfo( long deletedNodeId, byte[] deletedIdKey, DatabaseId dbId){ this.deletedNodeId=deletedNodeId; this.deletedIdKey=deletedIdKey; this.dbId=dbId; }

	 public INDeleteInfo(){ dbId=new DatabaseId(); }

	 public long getDeletedNodeId(){ return deletedNodeId; }

	 public byte[] getDeletedIdKey(){ return deletedIdKey; }

	 public DatabaseId getDatabaseId(){ return dbId; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_IN_DELETE_INFO; }

	 public boolean marshallOutsideWriteLatch(){ return true; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn){ }

	 public int getLogSize(){ return LogUtils.LONG_BYTES + LogUtils.getByteArrayLogSize(deletedIdKey) + dbId.getLogSize(); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,deletedNodeId); LogUtils.writeByteArray(logBuffer,deletedIdKey); dbId.writeToLog(logBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { deletedNodeId=LogUtils.readLong(itemBuffer); deletedIdKey=LogUtils.readByteArray(itemBuffer); dbId.readFromLog(itemBuffer,entryTypeVersion); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<INDeleteEntry node=\"").append(deletedNodeId); sb.append("\">"); sb.append(Key.dumpString(deletedIdKey,0)); dbId.dumpLog(sb,verbose); sb.append("</INDeleteEntry>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }


}
