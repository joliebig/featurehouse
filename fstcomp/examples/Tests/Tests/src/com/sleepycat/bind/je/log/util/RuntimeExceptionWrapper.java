package com.sleepycat.util; 
import de.ovgu.cide.jakutil.*; 
public  class  RuntimeExceptionWrapper  extends RuntimeException  implements ExceptionWrapper {
	 private Throwable e;

	 public RuntimeExceptionWrapper( Throwable e){ super(e.getMessage()); this.e=e; }

	 public Throwable getDetail(){ return e; }

	 public Throwable getCause(){ return e; }


}
