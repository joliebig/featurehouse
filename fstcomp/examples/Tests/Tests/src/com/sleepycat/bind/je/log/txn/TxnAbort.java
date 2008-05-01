package com.sleepycat.je.txn; 
import com.sleepycat.je.log.LogEntryType; 
import de.ovgu.cide.jakutil.*; 
public  class  TxnAbort  extends TxnEnd {
	 public TxnAbort( long id, long lastLsn){ super(id,lastLsn); }

	 public TxnAbort(){ }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_TXN_ABORT; }

	 protected String getTagName(){ return "TxnAbort"; }


}
