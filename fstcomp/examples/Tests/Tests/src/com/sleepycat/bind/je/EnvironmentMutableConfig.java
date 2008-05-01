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

	 public void setTxnNoSync( boolean noSync){ txnNoSync=noSync; }

	 public boolean getTxnNoSync(){ return txnNoSync; }

	 public void setTxnWriteNoSync( boolean writeNoSync){ txnWriteNoSync=writeNoSync; }

	 public boolean getTxnWriteNoSync(){ return txnWriteNoSync; }

	 public void setCacheSize( long totalBytes) throws IllegalArgumentException { setVal(EnvironmentParams.MAX_MEMORY,Long.toString(totalBytes)); }

	 public long getCacheSize(){ return cacheSize; }

	 public void setCachePercent( int percent) throws IllegalArgumentException { setVal(EnvironmentParams.MAX_MEMORY_PERCENT,Integer.toString(percent)); }

	 public int getCachePercent(){ String val=getVal(EnvironmentParams.MAX_MEMORY_PERCENT); try { return Integer.parseInt(val); } catch ( NumberFormatException e) { throw new IllegalArgumentException("Cache percent is not a valid integer: " + e.getMessage()); } }

	 public void setConfigParam( String paramName, String value) throws IllegalArgumentException { ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); if (param == null) { throw new IllegalArgumentException(paramName + " is not a valid BDBJE environment configuration"); } if (!param.isMutable()) { throw new IllegalArgumentException(paramName + " is not a mutable BDBJE environment configuration"); } setVal(param,value); }

	 public String getConfigParam( String paramName) throws IllegalArgumentException { ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); if (param == null) { throw new IllegalArgumentException(paramName + " is not a valid BDBJE environment configuration"); } return getVal(param); }

	 String getVal( ConfigParam param){ String val=props.getProperty(param.getName()); if (val == null) { val=param.getDefault(); } return val; }

	 void setVal( ConfigParam param, String val) throws IllegalArgumentException { if (validateParams) { param.validateValue(val); } props.setProperty(param.getName(),val); }

	 void setValidateParams( boolean validateParams){ this.validateParams=validateParams; }

	 void validateProperties( Properties props) throws IllegalArgumentException { Enumeration propNames=props.propertyNames(); while (propNames.hasMoreElements()) { String name=(String)propNames.nextElement(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(name); if (param == null) { throw new IllegalArgumentException(name + " is not a valid BDBJE environment configuration"); } param.validateValue(props.getProperty(name)); } }

	 void checkImmutablePropsForEquality( EnvironmentMutableConfig passedConfig) throws IllegalArgumentException { Properties passedProps=passedConfig.props; Iterator iter=EnvironmentParams.SUPPORTED_PARAMS.keySet().iterator(); while (iter.hasNext()) { String paramName=(String)iter.next(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); assert param != null; if (!param.isMutable()) { String paramVal=props.getProperty(paramName); String useParamVal=passedProps.getProperty(paramName); if ((paramVal != null) ? (!paramVal.equals(useParamVal)) : (useParamVal != null)) { throw new IllegalArgumentException(paramName + " is set to " + useParamVal+ " in the config parameter"+ " which is incompatible"+ " with the value of "+ paramVal+ " in the"+ " underlying environment"); } } } }

	 protected Object clone() throws CloneNotSupportedException { EnvironmentMutableConfig copy=(EnvironmentMutableConfig)super.clone(); copy.props=(Properties)props.clone(); return copy; }

	 EnvironmentMutableConfig cloneMutableConfig(){ try { EnvironmentMutableConfig copy=(EnvironmentMutableConfig)clone(); copy.clearImmutableProps(); return copy; } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 void copyHandlePropsTo( EnvironmentMutableConfig other){ other.txnNoSync=txnNoSync; other.txnWriteNoSync=txnWriteNoSync; }

	 void copyMutablePropsTo( EnvironmentMutableConfig toConfig){ Properties toProps=toConfig.props; Enumeration propNames=props.propertyNames(); while (propNames.hasMoreElements()) { String paramName=(String)propNames.nextElement(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); assert param != null; if (param.isMutable()) { String newVal=props.getProperty(paramName); toProps.setProperty(paramName,newVal); } } }

	 void fillInEnvironmentGeneratedProps( EnvironmentImpl envImpl){ cacheSize=envImpl.getMemoryBudget().getMaxMemory(); }

	 private void clearImmutableProps(){ Enumeration propNames=props.propertyNames(); while (propNames.hasMoreElements()) { String paramName=(String)propNames.nextElement(); ConfigParam param=(ConfigParam)EnvironmentParams.SUPPORTED_PARAMS.get(paramName); assert param != null; if (!param.isMutable()) { props.remove(paramName); } } }

	 void setLoadPropertyFile( boolean loadPropertyFile){ this.loadPropertyFile=loadPropertyFile; }

	 boolean getLoadPropertyFile(){ return loadPropertyFile; }

	 int getNumExplicitlySetParams(){ return props.size(); }

	 public String toString(){ return props.toString(); }


}
