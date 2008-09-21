package com.sleepycat.je.tree; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  DuplicateEntryException  extends DatabaseException {
	 public DuplicateEntryException(){ super(); }

	 public DuplicateEntryException( String message){ super(message); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
