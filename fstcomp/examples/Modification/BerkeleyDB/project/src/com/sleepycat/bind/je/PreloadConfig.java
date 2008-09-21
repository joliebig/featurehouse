package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  PreloadConfig  implements Cloneable {
	 public static final PreloadConfig DEFAULT=new PreloadConfig();

	 private long maxBytes;

	 private long maxMillisecs;

	 private boolean loadLNs;

	 public PreloadConfig(){ }

	 public void setMaxBytes__wrappee__base( long maxBytes){ this.maxBytes=maxBytes; }

	 public void setMaxBytes( long maxBytes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setMaxBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getMaxBytes__wrappee__base(){ return maxBytes; }

	 public long getMaxBytes(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMaxBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setMaxMillisecs__wrappee__base( long maxMillisecs){ this.maxMillisecs=maxMillisecs; }

	 public void setMaxMillisecs( long maxMillisecs){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setMaxMillisecs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getMaxMillisecs__wrappee__base(){ return maxMillisecs; }

	 public long getMaxMillisecs(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMaxMillisecs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLoadLNs__wrappee__base( boolean loadLNs){ this.loadLNs=loadLNs; }

	 public void setLoadLNs( boolean loadLNs){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLoadLNs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getLoadLNs__wrappee__base(){ return loadLNs; }

	 public boolean getLoadLNs(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLoadLNs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 DatabaseConfig cloneConfig__wrappee__base(){ try { return (DatabaseConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 DatabaseConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
