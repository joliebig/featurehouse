package com.sleepycat.je; 
import java.util.Properties; 
import com.sleepycat.je.config.ConfigParam; 
import com.sleepycat.je.config.EnvironmentParams; 
import de.ovgu.cide.jakutil.*; 
public  class  EnvironmentConfig  extends EnvironmentMutableConfig {
	 static EnvironmentConfig DEFAULT=new EnvironmentConfig();

	 private boolean createUP=true;

	 private boolean checkpointUP=true;

	 private boolean allowCreate=false;

	 private boolean txnReadCommitted=false;

	 public EnvironmentConfig(){ super(); }

	 public EnvironmentConfig( Properties properties) throws IllegalArgumentException { super(properties); }

	 public void setAllowCreate__wrappee__base( boolean allowCreate){ this.allowCreate=allowCreate; }

	 public void setAllowCreate( boolean allowCreate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAllowCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAllowCreate__wrappee__base(){ return allowCreate; }

	 public boolean getAllowCreate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAllowCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLockTimeout__wrappee__base( long timeout) throws IllegalArgumentException { setVal(EnvironmentParams.LOCK_TIMEOUT,Long.toString(timeout)); }

	 public void setLockTimeout( long timeout) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLockTimeout__wrappee__base(){ String val=getVal(EnvironmentParams.LOCK_TIMEOUT); long timeout=0; try { timeout=Long.parseLong(val); } catch ( NumberFormatException e) { throw new IllegalArgumentException("Bad value for timeout:" + e.getMessage()); } return timeout; }

	 public long getLockTimeout(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setReadOnly__wrappee__base( boolean readOnly){ setVal(EnvironmentParams.ENV_RDONLY,Boolean.toString(readOnly)); }

	 public void setReadOnly( boolean readOnly){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setReadOnly__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getReadOnly__wrappee__base(){ String val=getVal(EnvironmentParams.ENV_RDONLY); return (Boolean.valueOf(val)).booleanValue(); }

	 public boolean getReadOnly(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReadOnly__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTransactional__wrappee__base( boolean transactional){ setVal(EnvironmentParams.ENV_INIT_TXN,Boolean.toString(transactional)); }

	 public void setTransactional( boolean transactional){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getTransactional__wrappee__base(){ String val=getVal(EnvironmentParams.ENV_INIT_TXN); return (Boolean.valueOf(val)).booleanValue(); }

	 public boolean getTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLocking__wrappee__base( boolean locking){ setVal(EnvironmentParams.ENV_INIT_LOCKING,Boolean.toString(locking)); }

	 public void setLocking( boolean locking){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLocking__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getLocking__wrappee__base(){ String val=getVal(EnvironmentParams.ENV_INIT_LOCKING); return (Boolean.valueOf(val)).booleanValue(); }

	 public boolean getLocking(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLocking__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTxnTimeout__wrappee__base( long timeout) throws IllegalArgumentException { setVal(EnvironmentParams.TXN_TIMEOUT,Long.toString(timeout)); }

	 public void setTxnTimeout( long timeout) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTxnTimeout__wrappee__base(){ String val=getVal(EnvironmentParams.TXN_TIMEOUT); long timeout=0; try { timeout=Long.parseLong(val); } catch ( NumberFormatException e) { throw new IllegalArgumentException("Bad value for timeout:" + e.getMessage()); } return timeout; }

	 public long getTxnTimeout(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTxnSerializableIsolation__wrappee__base( boolean txnSerializableIsolation){ setVal(EnvironmentParams.TXN_SERIALIZABLE_ISOLATION,Boolean.toString(txnSerializableIsolation)); }

	 public void setTxnSerializableIsolation( boolean txnSerializableIsolation){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getTxnSerializableIsolation__wrappee__base(){ String val=getVal(EnvironmentParams.TXN_SERIALIZABLE_ISOLATION); return (Boolean.valueOf(val)).booleanValue(); }

	 public boolean getTxnSerializableIsolation(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setTxnReadCommitted__wrappee__base( boolean txnReadCommitted){ this.txnReadCommitted=txnReadCommitted; }

	 void setTxnReadCommitted( boolean txnReadCommitted){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnReadCommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getTxnReadCommitted__wrappee__base(){ return txnReadCommitted; }

	 boolean getTxnReadCommitted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnReadCommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setConfigParam__wrappee__base( String paramName, String value) throws IllegalArgumentException { ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); if (param == null) { throw new IllegalArgumentException(paramName + " is not a valid BDBJE environment configuration"); } setVal(param,value); }

	 public void setConfigParam( String paramName, String value) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setConfigParam__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setCreateUP__wrappee__base( boolean createUP){ this.createUP=createUP; }

	 void setCreateUP( boolean createUP){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCreateUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getCreateUP__wrappee__base(){ return createUP; }

	 boolean getCreateUP(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCreateUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setCheckpointUP__wrappee__base( boolean checkpointUP){ this.checkpointUP=checkpointUP; }

	 void setCheckpointUP( boolean checkpointUP){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCheckpointUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getCheckpointUP__wrappee__base(){ return checkpointUP; }

	 boolean getCheckpointUP(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCheckpointUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 EnvironmentConfig cloneConfig__wrappee__base(){ try { return (EnvironmentConfig)clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 EnvironmentConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return ("allowCreate=" + allowCreate + "\n"+ super.toString()); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
