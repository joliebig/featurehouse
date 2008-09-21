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

	 public void readEntry__wrappee__base( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { super.readEntry(entryBuffer,entrySize,entryTypeVersion,readFullItem); if (readFullItem) { dataAsKey=LogUtils.readByteArray(entryBuffer); } else { dataAsKey=null; } }

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public StringBuffer dumpEntry__wrappee__base( StringBuffer sb, boolean verbose){ super.dumpEntry(sb,verbose); sb.append(Key.dumpString(dataAsKey,0)); return sb; }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return super.getLogSize() + LogUtils.getByteArrayLogSize(dataAsKey); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer destBuffer){ super.writeToLog(destBuffer); LogUtils.writeByteArray(destBuffer,dataAsKey); }

	 public void writeToLog( ByteBuffer destBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupKey__wrappee__base(){ return dataAsKey; }

	 public byte[] getDupKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
