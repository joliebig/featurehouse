package com.sleepycat.je.txn; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.TransactionConfig; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  AutoTxn  extends Txn {
	 public AutoTxn( EnvironmentImpl env, TransactionConfig config) throws DatabaseException { super(env,config); }

	 public void operationEnd__wrappee__base( boolean operationOK) throws DatabaseException { if (operationOK) { commit(); } else { abort(false); } }

	 public void operationEnd( boolean operationOK) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void operationEnd__wrappee__base() throws DatabaseException { operationEnd(true); }

	 public void operationEnd() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setHandleLockOwner__wrappee__base( boolean operationOK, Database dbHandle, boolean dbIsClosing) throws DatabaseException { if (operationOK) { if (!dbIsClosing) { transferHandleLockToHandle(dbHandle); } unregisterHandle(dbHandle); } }

	 public void setHandleLockOwner( boolean operationOK, Database dbHandle, boolean dbIsClosing) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setHandleLockOwner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
