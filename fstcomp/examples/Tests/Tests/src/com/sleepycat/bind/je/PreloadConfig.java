package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  PreloadConfig  implements Cloneable {
	 public static final PreloadConfig DEFAULT=new PreloadConfig();

	 private long maxBytes;

	 private long maxMillisecs;

	 private boolean loadLNs;

	 public PreloadConfig(){ }

	 public void setMaxBytes( long maxBytes){ this.maxBytes=maxBytes; }

	 public long getMaxBytes(){ return maxBytes; }

	 public void setMaxMillisecs( long maxMillisecs){ this.maxMillisecs=maxMillisecs; }

	 public long getMaxMillisecs(){ return maxMillisecs; }

	 public void setLoadLNs( boolean loadLNs){ this.loadLNs=loadLNs; }

	 public boolean getLoadLNs(){ return loadLNs; }

	 DatabaseConfig cloneConfig(){ try { return (DatabaseConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }


}
