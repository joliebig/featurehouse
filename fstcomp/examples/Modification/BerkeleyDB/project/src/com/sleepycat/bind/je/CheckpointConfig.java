package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  CheckpointConfig {
	 public final static CheckpointConfig DEFAULT=new CheckpointConfig();

	 private boolean force=false;

	 private boolean minimizeRecoveryTime=false;

	 public CheckpointConfig(){ }

	 public void setForce__wrappee__base( boolean force){ this.force=force; }

	 public void setForce( boolean force){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setForce__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getForce__wrappee__base(){ return force; }

	 public boolean getForce(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getForce__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setMinimizeRecoveryTime__wrappee__base( boolean minimizeRecoveryTime){ this.minimizeRecoveryTime=minimizeRecoveryTime; }

	 public void setMinimizeRecoveryTime( boolean minimizeRecoveryTime){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setMinimizeRecoveryTime__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getMinimizeRecoveryTime__wrappee__base(){ return minimizeRecoveryTime; }

	 public boolean getMinimizeRecoveryTime(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMinimizeRecoveryTime__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
