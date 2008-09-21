package com.sleepycat.je.txn; 
import java.util.Collections; 
import java.util.Map; 
import java.util.WeakHashMap; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  LockInfo  implements Cloneable {
	 private Locker locker;

	 private LockType lockType;

	 private static boolean deadlockStackTrace=false;

	 private static Map traceExceptionMap=Collections.synchronizedMap(new WeakHashMap());

	
private static  class  StackTraceAtLockTime  extends Exception {

	}

	 public LockInfo( Locker locker, LockType lockType){ this.locker=locker; this.lockType=lockType; if (deadlockStackTrace) { traceExceptionMap.put(this,new StackTraceAtLockTime()); } }

	 static void setDeadlockStackTrace__wrappee__base( boolean enable){ deadlockStackTrace=enable; }

	 static void setDeadlockStackTrace( boolean enable){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDeadlockStackTrace__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getDeadlockStackTrace__wrappee__base(){ return deadlockStackTrace; }

	 public static boolean getDeadlockStackTrace(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDeadlockStackTrace__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setLocker__wrappee__base( Locker locker){ this.locker=locker; }

	 void setLocker( Locker locker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Locker getLocker__wrappee__base(){ return locker; }

	 Locker getLocker(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setLockType__wrappee__base( LockType lockType){ this.lockType=lockType; }

	 void setLockType( LockType lockType){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LockType getLockType__wrappee__base(){ return lockType; }

	 LockType getLockType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object clone__wrappee__base() throws CloneNotSupportedException { return super.clone(); }

	 public Object clone() throws CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dump__wrappee__base(){ System.out.println(this); }

	 public void dump(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer buf=new StringBuffer(500); buf.append("<LockInfo locker=\""); buf.append(locker); buf.append("\" type=\""); buf.append(lockType); buf.append("\"/>"); if (deadlockStackTrace) { Exception traceException=(Exception)traceExceptionMap.get(this); if (traceException != null) { buf.append(" lock taken at: "); buf.append(Tracer.getStackTrace(traceException)); } } return buf.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
