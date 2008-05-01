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

	 public void setAllowCreate( boolean allowCreate){ this.allowCreate=allowCreate; }

	 public boolean getAllowCreate(){ return allowCreate; }

	 public void setLockTimeout( long timeout) throws IllegalArgumentException { setVal(EnvironmentParams.LOCK_TIMEOUT,Long.toString(timeout)); }

	 public long getLockTimeout(){ String val=getVal(EnvironmentParams.LOCK_TIMEOUT); long timeout=0; try { timeout=Long.parseLong(val); } catch ( NumberFormatException e) { throw new IllegalArgumentException("Bad value for timeout:" + e.getMessage()); } return timeout; }

	 public void setReadOnly( boolean readOnly){ setVal(EnvironmentParams.ENV_RDONLY,Boolean.toString(readOnly)); }

	 public boolean getReadOnly(){ String val=getVal(EnvironmentParams.ENV_RDONLY); return (Boolean.valueOf(val)).booleanValue(); }

	 public void setTransactional( boolean transactional){ setVal(EnvironmentParams.ENV_INIT_TXN,Boolean.toString(transactional)); }

	 public boolean getTransactional(){ String val=getVal(EnvironmentParams.ENV_INIT_TXN); return (Boolean.valueOf(val)).booleanValue(); }

	 public void setLocking( boolean locking){ setVal(EnvironmentParams.ENV_INIT_LOCKING,Boolean.toString(locking)); }

	 public boolean getLocking(){ String val=getVal(EnvironmentParams.ENV_INIT_LOCKING); return (Boolean.valueOf(val)).booleanValue(); }

	 public void setTxnTimeout( long timeout) throws IllegalArgumentException { setVal(EnvironmentParams.TXN_TIMEOUT,Long.toString(timeout)); }

	 public long getTxnTimeout(){ String val=getVal(EnvironmentParams.TXN_TIMEOUT); long timeout=0; try { timeout=Long.parseLong(val); } catch ( NumberFormatException e) { throw new IllegalArgumentException("Bad value for timeout:" + e.getMessage()); } return timeout; }

	 public void setTxnSerializableIsolation( boolean txnSerializableIsolation){ setVal(EnvironmentParams.TXN_SERIALIZABLE_ISOLATION,Boolean.toString(txnSerializableIsolation)); }

	 public boolean getTxnSerializableIsolation(){ String val=getVal(EnvironmentParams.TXN_SERIALIZABLE_ISOLATION); return (Boolean.valueOf(val)).booleanValue(); }

	 void setTxnReadCommitted( boolean txnReadCommitted){ this.txnReadCommitted=txnReadCommitted; }

	 boolean getTxnReadCommitted(){ return txnReadCommitted; }

	 public void setConfigParam( String paramName, String value) throws IllegalArgumentException { ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); if (param == null) { throw new IllegalArgumentException(paramName + " is not a valid BDBJE environment configuration"); } setVal(param,value); }

	 void setCreateUP( boolean createUP){ this.createUP=createUP; }

	 boolean getCreateUP(){ return createUP; }

	 void setCheckpointUP( boolean checkpointUP){ this.checkpointUP=checkpointUP; }

	 boolean getCheckpointUP(){ return checkpointUP; }

	 EnvironmentConfig cloneConfig(){ try { return (EnvironmentConfig)clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 public String toString(){ return ("allowCreate=" + allowCreate + "\n"+ super.toString()); }


}
