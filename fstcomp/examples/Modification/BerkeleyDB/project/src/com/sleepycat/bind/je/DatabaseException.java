package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  DatabaseException  extends Exception {
	 public DatabaseException(){ super(); }

	 public DatabaseException( Throwable t){ super(t); }

	 public DatabaseException( String message){ super(message); }

	 public DatabaseException( String message, Throwable t){ super(message,t); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
