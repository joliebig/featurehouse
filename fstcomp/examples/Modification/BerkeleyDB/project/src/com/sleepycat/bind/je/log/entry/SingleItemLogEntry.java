package com.sleepycat.je.log.entry; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.log.LogReadable; 
import de.ovgu.cide.jakutil.*; 
public  class  SingleItemLogEntry  implements LogEntry {
	 private Class logClass;

	 LogReadable item;

	 public SingleItemLogEntry( Class logClass){ this.logClass=logClass; }

	 public void readEntry__wrappee__base( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { try { item=(LogReadable)logClass.newInstance(); item.readFromLog(entryBuffer,entryTypeVersion); } catch ( IllegalAccessException e) { throw new DatabaseException(e); }
catch ( InstantiationException e) { throw new DatabaseException(e); } }

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public StringBuffer dumpEntry__wrappee__base( StringBuffer sb, boolean verbose){ item.dumpLog(sb,verbose); return sb; }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object getMainItem__wrappee__base(){ return item; }

	 public Object getMainItem(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMainItem__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object clone__wrappee__base() throws CloneNotSupportedException { return super.clone(); }

	 public Object clone() throws CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return item.logEntryIsTransactional(); }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return item.getTransactionId(); }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntry getNewInstance__wrappee__base() throws DatabaseException { try { return (LogEntry)logClass.newInstance(); } catch ( InstantiationException e) { throw new DatabaseException(e); }
catch ( IllegalAccessException e) { throw new DatabaseException(e); } }

	 public LogEntry getNewInstance() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNewInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
