package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  DatabaseNotFoundException  extends DatabaseException {
	 public DatabaseNotFoundException(){ super(); }

	 public DatabaseNotFoundException( Throwable t){ super(t); }

	 public DatabaseNotFoundException( String message){ super(message); }

	 public DatabaseNotFoundException( String message, Throwable t){ super(message,t); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
