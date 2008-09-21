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

	 public void setAllowCreate__wrappee__base( boolean allowCreate){ this.allowCreate=allowCreate; }

	 public void setAllowCreate( boolean allowCreate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAllowCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAllowCreate__wrappee__base(){ return allowCreate; }

	 public boolean getAllowCreate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAllowCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setCacheSize__wrappee__base( int cacheSize){ this.cacheSize=cacheSize; }

	 public void setCacheSize( int cacheSize){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCacheSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getCacheSize__wrappee__base(){ return cacheSize; }

	 public int getCacheSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCacheSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDecrement__wrappee__base( boolean decrement){ this.decrement=decrement; }

	 public void setDecrement( boolean decrement){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDecrement__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getDecrement__wrappee__base(){ return decrement; }

	 public boolean getDecrement(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDecrement__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setExclusiveCreate__wrappee__base( boolean exclusiveCreate){ this.exclusiveCreate=exclusiveCreate; }

	 public void setExclusiveCreate( boolean exclusiveCreate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setExclusiveCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getExclusiveCreate__wrappee__base(){ return exclusiveCreate; }

	 public boolean getExclusiveCreate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getExclusiveCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setInitialValue__wrappee__base( long initialValue){ this.initialValue=initialValue; }

	 public void setInitialValue( long initialValue){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setInitialValue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getInitialValue__wrappee__base(){ return initialValue; }

	 public long getInitialValue(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getInitialValue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setAutoCommitNoSync__wrappee__base( boolean autoCommitNoSync){ this.autoCommitNoSync=autoCommitNoSync; }

	 public void setAutoCommitNoSync( boolean autoCommitNoSync){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAutoCommitNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAutoCommitNoSync__wrappee__base(){ return autoCommitNoSync; }

	 public boolean getAutoCommitNoSync(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAutoCommitNoSync__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setRange__wrappee__base( long min, long max){ this.rangeMin=min; this.rangeMax=max; }

	 public void setRange( long min, long max){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getRangeMin__wrappee__base(){ return rangeMin; }

	 public long getRangeMin(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRangeMin__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getRangeMax__wrappee__base(){ return rangeMax; }

	 public long getRangeMax(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRangeMax__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setWrap__wrappee__base( boolean wrap){ this.wrap=wrap; }

	 public void setWrap( boolean wrap){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setWrap__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getWrap__wrappee__base(){ return wrap; }

	 public boolean getWrap(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWrap__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
