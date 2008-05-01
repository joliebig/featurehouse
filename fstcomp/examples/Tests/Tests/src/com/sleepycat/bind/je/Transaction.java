package com.sleepycat.je; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.Txn; 
import com.sleepycat.je.utilint.PropUtil; 
import de.ovgu.cide.jakutil.*; 
public  class  Transaction {
	 private Txn txn;

	 private Environment env;

	 private long id;

	 private String name;

	 Transaction( Environment env, Txn txn){ this.env=env; this.txn=txn; this.id=txn.getId(); }

	 public void abort() throws DatabaseException { checkEnv(); env.removeReferringHandle(this); txn.abort(false); txn=null; }

	 public long getId() throws DatabaseException { return id; }

	 public void commit() throws DatabaseException { checkEnv(); env.removeReferringHandle(this); txn.commit(); txn=null; }

	 public void commitSync() throws DatabaseException { doCommit(Txn.TXN_SYNC); }

	 public void commitNoSync() throws DatabaseException { doCommit(Txn.TXN_NOSYNC); }

	 public void commitWriteNoSync() throws DatabaseException { doCommit(Txn.TXN_WRITE_NOSYNC); }

	 private void doCommit( byte commitType) throws DatabaseException { checkEnv(); env.removeReferringHandle(this); txn.commit(commitType); txn=null; }

	 public void setTxnTimeout( long timeOut) throws DatabaseException { checkEnv(); txn.setTxnTimeout(PropUtil.microsToMillis(timeOut)); }

	 public void setLockTimeout( long timeOut) throws DatabaseException { checkEnv(); txn.setLockTimeout(PropUtil.microsToMillis(timeOut)); }

	 public void setName( String name){ this.name=name; }

	 public String getName(){ return name; }

	 public int hashCode(){ return (int)id; }

	 public boolean equals( Object o){ if (o == null) { return false; } if (!(o instanceof Transaction)) { return false; } if (((Transaction)o).id == id) { return true; } return false; }

	 public String toString(){ StringBuffer sb=new StringBuffer(); sb.append("<Transaction id=\""); sb.append(txn.getId()).append("\""); if (name != null) { sb.append(" name=\""); sb.append(name).append("\""); } sb.append(">"); return sb.toString(); }

	 Locker getLocker() throws DatabaseException { if (txn == null) { throw new DatabaseException("Transaction " + id + " has been closed and is no longer"+ " usable."); } else { return txn; } }

	 Txn getTxn(){ return txn; }

	 private void checkEnv() throws RunRecoveryException { env.getEnvironmentImpl().checkIfInvalid(); }


}
