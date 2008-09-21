package com.sleepycat.je; 
import java.util.Enumeration; 
import java.util.Iterator; 
import java.util.Properties; 
import com.sleepycat.je.config.ConfigParam; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  EnvironmentMutableConfig  implements Cloneable {
	 private boolean txnNoSync=false;

	 private boolean txnWriteNoSync=false;

	 protected long cacheSize;

	 private Properties props;

	 private boolean loadPropertyFile=true;

	 private boolean validateParams=true;

	 public EnvironmentMutableConfig(){ props=new Properties(); }

	 EnvironmentMutableConfig( Properties properties) throws IllegalArgumentException { validateProperties(properties); props=new Properties(); props.putAll(properties); }

	 public void setTxnNoSync__wrappee__base( boolean noSync){ txnNoSync=noSync; }

	 public void setTxnNoSync( boolean noSync){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getTxnNoSync__wrappee__base(){ return txnNoSync; }

	 public boolean getTxnNoSync(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTxnWriteNoSync__wrappee__base( boolean writeNoSync){ txnWriteNoSync=writeNoSync; }

	 public void setTxnWriteNoSync( boolean writeNoSync){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnWriteNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getTxnWriteNoSync__wrappee__base(){ return txnWriteNoSync; }

	 public boolean getTxnWriteNoSync(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnWriteNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setCacheSize__wrappee__base( long totalBytes) throws IllegalArgumentException { setVal(EnvironmentParams.MAX_MEMORY,Long.toString(totalBytes)); }

	 public void setCacheSize( long totalBytes) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setCacheSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getCacheSize__wrappee__base(){ return cacheSize; }

	 public long getCacheSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCacheSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setCachePercent__wrappee__base( int percent) throws IllegalArgumentException { setVal(EnvironmentParams.MAX_MEMORY_PERCENT,Integer.toString(percent)); }

	 public void setCachePercent( int percent) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setCachePercent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getCachePercent__wrappee__base(){ String val=getVal(EnvironmentParams.MAX_MEMORY_PERCENT); try { return Integer.parseInt(val); } catch ( NumberFormatException e) { throw new IllegalArgumentException("Cache percent is not a valid integer: " + e.getMessage()); } }

	 public int getCachePercent(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCachePercent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setConfigParam__wrappee__base( String paramName, String value) throws IllegalArgumentException { ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); if (param == null) { throw new IllegalArgumentException(paramName + " is not a valid BDBJE environment configuration"); } if (!param.isMutable()) { throw new IllegalArgumentException(paramName + " is not a mutable BDBJE environment configuration"); } setVal(param,value); }

	 public void setConfigParam( String paramName, String value) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setConfigParam__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getConfigParam__wrappee__base( String paramName) throws IllegalArgumentException { ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); if (param == null) { throw new IllegalArgumentException(paramName + " is not a valid BDBJE environment configuration"); } return getVal(param); }

	 public String getConfigParam( String paramName) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getConfigParam__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String getVal__wrappee__base( ConfigParam param){ String val=props.getProperty(param.getName()); if (val == null) { val=param.getDefault(); } return val; }

	 String getVal( ConfigParam param){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getVal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setVal__wrappee__base( ConfigParam param, String val) throws IllegalArgumentException { if (validateParams) { param.validateValue(val); } props.setProperty(param.getName(),val); }

	 void setVal( ConfigParam param, String val) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setVal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setValidateParams__wrappee__base( boolean validateParams){ this.validateParams=validateParams; }

	 void setValidateParams( boolean validateParams){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setValidateParams__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void validateProperties__wrappee__base( Properties props) throws IllegalArgumentException { Enumeration propNames=props.propertyNames(); while (propNames.hasMoreElements()) { String name=(String)propNames.nextElement(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(name); if (param == null) { throw new IllegalArgumentException(name + " is not a valid BDBJE environment configuration"); } param.validateValue(props.getProperty(name)); } }

	 void validateProperties( Properties props) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateProperties__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void checkImmutablePropsForEquality__wrappee__base( EnvironmentMutableConfig passedConfig) throws IllegalArgumentException { Properties passedProps=passedConfig.props; Iterator iter=EnvironmentParams.SUPPORTED_PARAMS.keySet().iterator(); while (iter.hasNext()) { String paramName=(String)iter.next(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); assert param != null; if (!param.isMutable()) { String paramVal=props.getProperty(paramName); String useParamVal=passedProps.getProperty(paramName); if ((paramVal != null) ? (!paramVal.equals(useParamVal)) : (useParamVal != null)) { throw new IllegalArgumentException(paramName + " is set to " + useParamVal+ " in the config parameter"+ " which is incompatible"+ " with the value of "+ paramVal+ " in the"+ " underlying environment"); } } } }

	 void checkImmutablePropsForEquality( EnvironmentMutableConfig passedConfig) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkImmutablePropsForEquality__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Object clone__wrappee__base() throws CloneNotSupportedException { EnvironmentMutableConfig copy=(EnvironmentMutableConfig)super.clone(); copy.props=(Properties)props.clone(); return copy; }

	 protected Object clone() throws CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 EnvironmentMutableConfig cloneMutableConfig__wrappee__base(){ try { EnvironmentMutableConfig copy=(EnvironmentMutableConfig)clone(); copy.clearImmutableProps(); return copy; } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 EnvironmentMutableConfig cloneMutableConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneMutableConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void copyHandlePropsTo__wrappee__base( EnvironmentMutableConfig other){ other.txnNoSync=txnNoSync; other.txnWriteNoSync=txnWriteNoSync; }

	 void copyHandlePropsTo( EnvironmentMutableConfig other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	copyHandlePropsTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void copyMutablePropsTo__wrappee__base( EnvironmentMutableConfig toConfig){ Properties toProps=toConfig.props; Enumeration propNames=props.propertyNames(); while (propNames.hasMoreElements()) { String paramName=(String)propNames.nextElement(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); assert param != null; if (param.isMutable()) { String newVal=props.getProperty(paramName); toProps.setProperty(paramName,newVal); } } }

	 void copyMutablePropsTo( EnvironmentMutableConfig toConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	copyMutablePropsTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void fillInEnvironmentGeneratedProps__wrappee__base( EnvironmentImpl envImpl){ cacheSize=envImpl.getMemoryBudget().getMaxMemory(); }

	 void fillInEnvironmentGeneratedProps( EnvironmentImpl envImpl){ t.in(Thread.currentThread().getStackTrace()[1].toString());	fillInEnvironmentGeneratedProps__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void clearImmutableProps__wrappee__base(){ Enumeration propNames=props.propertyNames(); while (propNames.hasMoreElements()) { String paramName=(String)propNames.nextElement(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); assert param != null; if (!param.isMutable()) { props.remove(paramName); } } }

	 private void clearImmutableProps(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearImmutableProps__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setLoadPropertyFile__wrappee__base( boolean loadPropertyFile){ this.loadPropertyFile=loadPropertyFile; }

	 void setLoadPropertyFile( boolean loadPropertyFile){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLoadPropertyFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getLoadPropertyFile__wrappee__base(){ return loadPropertyFile; }

	 boolean getLoadPropertyFile(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLoadPropertyFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getNumExplicitlySetParams__wrappee__base(){ return props.size(); }

	 int getNumExplicitlySetParams(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNumExplicitlySetParams__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return props.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
