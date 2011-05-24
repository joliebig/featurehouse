package com.sleepycat.je.dbi; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  RangeRestartException  extends DatabaseException {
	 public RangeRestartException(){ super(); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
