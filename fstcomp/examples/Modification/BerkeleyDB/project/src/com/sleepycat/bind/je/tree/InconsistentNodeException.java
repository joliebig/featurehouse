package com.sleepycat.je.tree; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  InconsistentNodeException  extends DatabaseException {
	 public InconsistentNodeException(){ super(); }

	 public InconsistentNodeException( String message){ super(message); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
