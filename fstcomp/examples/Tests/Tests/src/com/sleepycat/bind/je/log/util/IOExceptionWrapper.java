package com.sleepycat.util; 
import java.io.IOException; 
import de.ovgu.cide.jakutil.*; 
public  class  IOExceptionWrapper  extends IOException  implements ExceptionWrapper {
	 private Throwable e;

	 public IOExceptionWrapper( Throwable e){ super(e.getMessage()); this.e=e; }

	 public Throwable getDetail(){ return e; }

	 public Throwable getCause(){ return e; }


}
