package com.sleepycat.je.utilint; 
import java.util.Collection; 
import java.util.HashSet; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DeadlockException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  DaemonThread  implements DaemonRunner, Runnable {
	 private static final int JOIN_MILLIS=10;

	 private long waitTime;

	 private Object synchronizer=new Object();

	 private Thread thread;

	 private EnvironmentImpl env;

	 protected String name;

	 protected Set workQueue;

	 protected int nWakeupRequests;

	 private volatile boolean shutdownRequest=false;

	 private volatile boolean paused=false;

	 private boolean running=false;

	 protected DaemonThread(){ }

	 public DaemonThread( long waitTime, String name, EnvironmentImpl env){ init(waitTime,name,env); }

	 protected void init__wrappee__base( long waitTime, String name, EnvironmentImpl env){ this.waitTime=waitTime; this.name=name; this.env=env; workQueue=new HashSet(); this.hook856(name,env); }

	 protected void init( long waitTime, String name, EnvironmentImpl env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	init__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Thread getThread__wrappee__base(){ return thread; }

	 public Thread getThread(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getThread__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void runOrPause__wrappee__base( boolean run){ if (run) { paused=false; if (thread != null) { wakeup(); } else { thread=new Thread(this,name); thread.setDaemon(true); thread.start(); } } else { paused=true; } }

	 public void runOrPause( boolean run){ t.in(Thread.currentThread().getStackTrace()[1].toString());	runOrPause__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void requestShutdown__wrappee__base(){ shutdownRequest=true; }

	 public void requestShutdown(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	requestShutdown__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void shutdown__wrappee__base(){ if (thread != null) { shutdownRequest=true; while (thread.isAlive()) {
synchronized (synchronizer) { synchronizer.notifyAll(); } try { thread.join(JOIN_MILLIS); } catch ( InterruptedException e) { } } thread=null; } }

	 public void shutdown(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shutdown__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer(); sb.append("<DaemonThread name=\"").append(name).append("\"/>"); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addToQueue__wrappee__base( Object o) throws DatabaseException { workQueue.add(o); wakeup(); }

	 public void addToQueue( Object o) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addToQueue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getQueueSize__wrappee__base() throws DatabaseException { int count=workQueue.size(); return count; }

	 public int getQueueSize() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getQueueSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addToQueueAlreadyLatched__wrappee__base( Collection c) throws DatabaseException { workQueue.addAll(c); }

	 public void addToQueueAlreadyLatched( Collection c) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addToQueueAlreadyLatched__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void wakeup__wrappee__base(){ if (!paused) {
synchronized (synchronizer) { synchronizer.notifyAll(); } } }

	 public void wakeup(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	wakeup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void run__wrappee__base(){ while (true) { if (shutdownRequest) { break; } try { this.hook858(); boolean nothingToDo=workQueue.size() == 0; this.hook857(); if (nothingToDo) {
synchronized (synchronizer) { if (waitTime == 0) { synchronizer.wait(); } else { synchronizer.wait(waitTime); } } } if (shutdownRequest) { break; } if (paused) {
synchronized (synchronizer) { synchronizer.wait(); } continue; } int numTries=0; int maxRetries=nDeadlockRetries(); do { try { nWakeupRequests++; running=true; onWakeup(); break; } catch ( DeadlockException e) { } finally { running=false; } numTries++; if (shutdownRequest) { break; } } while (numTries <= maxRetries); if (shutdownRequest) { break; } } catch ( InterruptedException IE) { System.err.println("Shutting down " + this + " due to exception: "+ IE); shutdownRequest=true; }
catch ( Exception E) { System.err.println(this + " caught exception: " + E); E.printStackTrace(System.err); if (env.mayNotWrite()) { System.err.println("Exiting"); shutdownRequest=true; } else { System.err.println("Continuing"); } } } }

	 public void run(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	run__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int nDeadlockRetries__wrappee__base() throws DatabaseException { return 0; }

	 protected int nDeadlockRetries() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	nDeadlockRetries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected void onWakeup__wrappee__base() throws DatabaseException ;

	 abstract protected void onWakeup() throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	onWakeup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isShutdownRequested__wrappee__base(){ return shutdownRequest; }

	 protected boolean isShutdownRequested(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isShutdownRequested__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isRunning__wrappee__base(){ return running; }

	 public boolean isRunning(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isRunning__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNWakeupRequests__wrappee__base(){ return nWakeupRequests; }

	 public int getNWakeupRequests(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNWakeupRequests__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook856__wrappee__base( String name, EnvironmentImpl env){ }

	 protected void hook856( String name, EnvironmentImpl env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook856__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook857__wrappee__base() throws InterruptedException, Exception { }

	 protected void hook857() throws InterruptedException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook857__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook858__wrappee__base() throws InterruptedException, Exception { }

	 protected void hook858() throws InterruptedException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook858__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
