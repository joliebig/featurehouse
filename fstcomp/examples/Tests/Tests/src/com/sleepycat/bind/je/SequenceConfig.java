package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  SequenceConfig {
	 public static final SequenceConfig DEFAULT=new SequenceConfig();

	 private int cacheSize=0;

	 private long rangeMin=Long.MIN_VALUE;

	 private long rangeMax=Long.MAX_VALUE;

	 private long initialValue=0L;

	 private boolean allowCreate;

	 private boolean decrement;

	 private boolean exclusiveCreate;

	 private boolean autoCommitNoSync;

	 private boolean wrap;

	 public SequenceConfig(){ }

	 public void setAllowCreate( boolean allowCreate){ this.allowCreate=allowCreate; }

	 public boolean getAllowCreate(){ return allowCreate; }

	 public void setCacheSize( int cacheSize){ this.cacheSize=cacheSize; }

	 public int getCacheSize(){ return cacheSize; }

	 public void setDecrement( boolean decrement){ this.decrement=decrement; }

	 public boolean getDecrement(){ return decrement; }

	 public void setExclusiveCreate( boolean exclusiveCreate){ this.exclusiveCreate=exclusiveCreate; }

	 public boolean getExclusiveCreate(){ return exclusiveCreate; }

	 public void setInitialValue( long initialValue){ this.initialValue=initialValue; }

	 public long getInitialValue(){ return initialValue; }

	 public void setAutoCommitNoSync( boolean autoCommitNoSync){ this.autoCommitNoSync=autoCommitNoSync; }

	 public boolean getAutoCommitNoSync(){ return autoCommitNoSync; }

	 public void setRange( long min, long max){ this.rangeMin=min; this.rangeMax=max; }

	 public long getRangeMin(){ return rangeMin; }

	 public long getRangeMax(){ return rangeMax; }

	 public void setWrap( boolean wrap){ this.wrap=wrap; }

	 public boolean getWrap(){ return wrap; }


}
