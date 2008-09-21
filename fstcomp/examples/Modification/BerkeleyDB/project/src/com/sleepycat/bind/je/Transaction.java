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

	 public void abort__wrappee__base() throws DatabaseException { checkEnv(); env.removeReferringHandle(this); txn.abort(false); txn=null; }

	 public void abort() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	abort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getId__wrappee__base() throws DatabaseException { return id; }

	 public long getId() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void commit__wrappee__base() throws DatabaseException { checkEnv(); env.removeReferringHandle(this); txn.commit(); txn=null; }

	 public void commit() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	commit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void commitSync__wrappee__base() throws DatabaseException { doCommit(Txn.TXN_SYNC); }

	 public void commitSync() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	commitSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void commitNoSync__wrappee__base() throws DatabaseException { doCommit(Txn.TXN_NOSYNC); }

	 public void commitNoSync() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	commitNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void commitWriteNoSync__wrappee__base() throws DatabaseException { doCommit(Txn.TXN_WRITE_NOSYNC); }

	 public void commitWriteNoSync() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	commitWriteNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void doCommit__wrappee__base( byte commitType) throws DatabaseException { checkEnv(); env.removeReferringHandle(this); txn.commit(commitType); txn=null; }

	 private void doCommit( byte commitType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doCommit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTxnTimeout__wrappee__base( long timeOut) throws DatabaseException { checkEnv(); txn.setTxnTimeout(PropUtil.microsToMillis(timeOut)); }

	 public void setTxnTimeout( long timeOut) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLockTimeout__wrappee__base( long timeOut) throws DatabaseException { checkEnv(); txn.setLockTimeout(PropUtil.microsToMillis(timeOut)); }

	 public void setLockTimeout( long timeOut) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setName__wrappee__base( String name){ this.name=name; }

	 public void setName( String name){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getName__wrappee__base(){ return name; }

	 public String getName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ return (int)id; }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean equals__wrappee__base( Object o){ if (o == null) { return false; } if (!(o instanceof Transaction)) { return false; } if (((Transaction)o).id == id) { return true; } return false; }

	 public boolean equals( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer(); sb.append("<Transaction id=\""); sb.append(txn.getId()).append("\""); if (name != null) { sb.append(" name=\""); sb.append(name).append("\""); } sb.append(">"); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Locker getLocker__wrappee__base() throws DatabaseException { if (txn == null) { throw new DatabaseException("Transaction " + id + " has been closed and is no longer"+ " usable."); } else { return txn; } }

	 Locker getLocker() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Txn getTxn__wrappee__base(){ return txn; }

	 Txn getTxn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void checkEnv__wrappee__base() throws RunRecoveryException { env.getEnvironmentImpl().checkIfInvalid(); }

	 private void checkEnv() throws RunRecoveryException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkEnv__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
