package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  DeltaInfo  implements LogWritable, LogReadable {
	 private byte[] key;

	 private long lsn;

	 private byte state;

	 DeltaInfo( byte[] key, long lsn, byte state){ this.key=key; this.lsn=lsn; this.state=state; }

	 DeltaInfo(){ lsn=DbLsn.NULL_LSN; }

	 public int getLogSize(){ return LogUtils.getByteArrayLogSize(key) + LogUtils.getLongLogSize() + 1; }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeByteArray(logBuffer,key); LogUtils.writeLong(logBuffer,lsn); logBuffer.put(state); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { key=LogUtils.readByteArray(itemBuffer); lsn=LogUtils.readLong(itemBuffer); state=itemBuffer.get(); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append(Key.dumpString(key,0)); sb.append(DbLsn.toString(lsn)); IN.dumpDeletedState(sb,state); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }

	 byte[] getKey(){ return key; }

	 byte getState(){ return state; }

	 boolean isKnownDeleted(){ return IN.isStateKnownDeleted(state); }

	 long getLsn(){ return lsn; }


}
