package com.sleepycat.je; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  RunRecoveryException  extends DatabaseException {
	 private boolean alreadyThrown=false;

	 public RunRecoveryException( EnvironmentImpl env){ super(); invalidate(env); }

	 public RunRecoveryException( EnvironmentImpl env, Throwable t){ super(t); invalidate(env); }

	 public RunRecoveryException( EnvironmentImpl env, String message){ super(message); invalidate(env); }

	 public RunRecoveryException( EnvironmentImpl env, String message, Throwable t){ super(message,t); invalidate(env); }

	 private void invalidate__wrappee__base( EnvironmentImpl env){ if (env != null) { env.invalidate(this); } }

	 private void invalidate( EnvironmentImpl env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	invalidate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setAlreadyThrown__wrappee__base(){ alreadyThrown=true; }

	 public void setAlreadyThrown(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAlreadyThrown__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ if (alreadyThrown) { return "Environment invalid because of previous exception: " + super.toString(); } else { return super.toString(); } }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
