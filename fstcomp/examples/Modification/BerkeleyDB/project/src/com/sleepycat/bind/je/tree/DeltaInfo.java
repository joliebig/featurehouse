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

	 public int getLogSize__wrappee__base(){ return LogUtils.getByteArrayLogSize(key) + LogUtils.getLongLogSize() + 1; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeByteArray(logBuffer,key); LogUtils.writeLong(logBuffer,lsn); logBuffer.put(state); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { key=LogUtils.readByteArray(itemBuffer); lsn=LogUtils.readLong(itemBuffer); state=itemBuffer.get(); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append(Key.dumpString(key,0)); sb.append(DbLsn.toString(lsn)); IN.dumpDeletedState(sb,state); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 byte\[\] getKey__wrappee__base(){ return key; }

	 byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 byte getState__wrappee__base(){ return state; }

	 byte getState(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isKnownDeleted__wrappee__base(){ return IN.isStateKnownDeleted(state); }

	 boolean isKnownDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getLsn__wrappee__base(){ return lsn; }

	 long getLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
