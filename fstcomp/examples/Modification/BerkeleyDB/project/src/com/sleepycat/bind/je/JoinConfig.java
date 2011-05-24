package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  JoinConfig  implements Cloneable {
	 static JoinConfig DEFAULT=new JoinConfig();

	 private boolean noSort;

	 public JoinConfig(){ }

	 public void setNoSort__wrappee__base( boolean noSort){ this.noSort=noSort; }

	 public void setNoSort( boolean noSort){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNoSort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getNoSort__wrappee__base(){ return noSort; }

	 public boolean getNoSort(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoSort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 JoinConfig cloneConfig__wrappee__base(){ try { return (JoinConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 JoinConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
