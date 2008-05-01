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

	 static void setDeadlockStackTrace( boolean enable){ deadlockStackTrace=enable; }

	 public static boolean getDeadlockStackTrace(){ return deadlockStackTrace; }

	 public LockInfo( Locker locker, LockType lockType){ this.locker=locker; this.lockType=lockType; if (deadlockStackTrace) { traceExceptionMap.put(this,new StackTraceAtLockTime()); } }

	 void setLocker( Locker locker){ this.locker=locker; }

	 Locker getLocker(){ return locker; }

	 void setLockType( LockType lockType){ this.lockType=lockType; }

	 LockType getLockType(){ return lockType; }

	 public Object clone() throws CloneNotSupportedException { return super.clone(); }

	 public void dump(){ System.out.println(this); }

	 public String toString(){ StringBuffer buf=new StringBuffer(500); buf.append("<LockInfo locker=\""); buf.append(locker); buf.append("\" type=\""); buf.append(lockType); buf.append("\"/>"); if (deadlockStackTrace) { Exception traceException=(Exception)traceExceptionMap.get(this); if (traceException != null) { buf.append(" lock taken at: "); buf.append(Tracer.getStackTrace(traceException)); } } return buf.toString(); }


}
