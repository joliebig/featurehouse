package com.sleepycat.je.log.entry; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.tree.Key; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.txn.Txn; 
import de.ovgu.cide.jakutil.*; 
public  class  DeletedDupLNLogEntry  extends LNLogEntry {
	 private byte[] dataAsKey;

	 public DeletedDupLNLogEntry( boolean isTransactional){ super(com.sleepycat.je.tree.LN.class,isTransactional); }

	 public DeletedDupLNLogEntry( LogEntryType entryType, LN ln, DatabaseId dbId, byte[] key, byte[] dataAsKey, long abortLsn, boolean abortKnownDeleted, Txn txn){ super(entryType,ln,dbId,key,abortLsn,abortKnownDeleted,txn); this.dataAsKey=dataAsKey; }

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { super.readEntry(entryBuffer,entrySize,entryTypeVersion,readFullItem); if (readFullItem) { dataAsKey=LogUtils.readByteArray(entryBuffer); } else { dataAsKey=null; } }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ super.dumpEntry(sb,verbose); sb.append(Key.dumpString(dataAsKey,0)); return sb; }

	 public int getLogSize(){ return super.getLogSize() + LogUtils.getByteArrayLogSize(dataAsKey); }

	 public void writeToLog( ByteBuffer destBuffer){ super.writeToLog(destBuffer); LogUtils.writeByteArray(destBuffer,dataAsKey); }

	 public byte[] getDupKey(){ return dataAsKey; }


}
