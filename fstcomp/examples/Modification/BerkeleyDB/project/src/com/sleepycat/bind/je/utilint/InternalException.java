package com.sleepycat.je.utilint; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  InternalException  extends DatabaseException {
	 public InternalException(){ super(); }

	 public InternalException( String message){ super(message); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
