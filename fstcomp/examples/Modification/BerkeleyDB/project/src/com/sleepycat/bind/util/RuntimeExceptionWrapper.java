package com.sleepycat.util; 
import de.ovgu.cide.jakutil.*; 
public  class  RuntimeExceptionWrapper  extends RuntimeException  implements ExceptionWrapper {
	 private Throwable e;

	 public RuntimeExceptionWrapper( Throwable e){ super(e.getMessage()); this.e=e; }

	 public Throwable getDetail__wrappee__base(){ return e; }

	 public Throwable getDetail(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDetail__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Throwable getCause__wrappee__base(){ return e; }

	 public Throwable getCause(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCause__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
