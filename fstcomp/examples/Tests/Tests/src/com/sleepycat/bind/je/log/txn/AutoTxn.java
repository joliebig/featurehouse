package com.sleepycat.je.txn; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.TransactionConfig; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  AutoTxn  extends Txn {
	 public AutoTxn( EnvironmentImpl env, TransactionConfig config) throws DatabaseException { super(env,config); }

	 public void operationEnd( boolean operationOK) throws DatabaseException { if (operationOK) { commit(); } else { abort(false); } }

	 public void operationEnd() throws DatabaseException { operationEnd(true); }

	 public void setHandleLockOwner( boolean operationOK, Database dbHandle, boolean dbIsClosing) throws DatabaseException { if (operationOK) { if (!dbIsClosing) { transferHandleLockToHandle(dbHandle); } unregisterHandle(dbHandle); } }


}
