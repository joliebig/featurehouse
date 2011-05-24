package com.sleepycat.je.txn; 
import com.sleepycat.je.log.LogEntryType; 
import de.ovgu.cide.jakutil.*; 
public  class  TxnAbort  extends TxnEnd {
	 public TxnAbort( long id, long lastLsn){ super(id,lastLsn); }

	 public TxnAbort(){ }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_TXN_ABORT; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String getTagName__wrappee__base(){ return "TxnAbort"; }

	 protected String getTagName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTagName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
