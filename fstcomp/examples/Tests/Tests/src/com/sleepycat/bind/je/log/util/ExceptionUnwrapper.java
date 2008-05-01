package com.sleepycat.util; 
import de.ovgu.cide.jakutil.*; 
public  class  ExceptionUnwrapper {
	 public static Exception unwrap( Exception e){ Throwable t=unwrapAny(e); if (t instanceof Exception) { return (Exception)t; } else if (t instanceof Error) { throw (Error)t; } else { throw new IllegalArgumentException("Not Exception or Error: " + t); } }

	 public static Throwable unwrapAny( Throwable e){ while (true) { if (e instanceof ExceptionWrapper) { Throwable e2=((ExceptionWrapper)e).getCause(); if (e2 == null) { return e; } else { e=e2; } } else { return e; } } }


}
