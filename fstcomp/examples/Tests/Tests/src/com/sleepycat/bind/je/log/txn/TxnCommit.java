package com.sleepycat.je.txn; 
import com.sleepycat.je.log.LogEntryType; 
import de.ovgu.cide.jakutil.*; 
public  class  TxnCommit  extends TxnEnd {
	 public TxnCommit( long id, long lastLsn){ super(id,lastLsn); }

	 public TxnCommit(){ }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_TXN_COMMIT; }

	 protected String getTagName(){ return "TxnCommit"; }


}
