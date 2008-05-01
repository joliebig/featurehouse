package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  DeadlockException  extends DatabaseException {
	 public DeadlockException(){ super(); }

	 public DeadlockException( Throwable t){ super(t); }

	 public DeadlockException( String message){ super(message); }

	 public DeadlockException( String message, Throwable t){ super(message,t); }


}
