package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  LockMode {
	 private String lockModeName;

	 private LockMode( String lockModeName){ this.lockModeName=lockModeName; }

	 public static final LockMode DEFAULT=new LockMode("DEFAULT");

	 public static final LockMode READ_UNCOMMITTED=new LockMode("READ_UNCOMMITTED");

	 public static final LockMode DIRTY_READ=READ_UNCOMMITTED;

	 public static final LockMode READ_COMMITTED=new LockMode("READ_COMMITTED");

	 public static final LockMode RMW=new LockMode("RMW");

	 public String toString(){ return "LockMode." + lockModeName; }


}
