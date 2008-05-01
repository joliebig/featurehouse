package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  LockNotGrantedException  extends DeadlockException {
	 public LockNotGrantedException(){ super(); }

	 public LockNotGrantedException( Throwable t){ super(t); }

	 public LockNotGrantedException( String message){ super(message); }

	 public LockNotGrantedException( String message, Throwable t){ super(message,t); }


}
