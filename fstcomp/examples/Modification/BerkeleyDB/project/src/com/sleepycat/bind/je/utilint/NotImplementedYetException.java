package com.sleepycat.je.utilint; 
import de.ovgu.cide.jakutil.*; 
public  class  NotImplementedYetException  extends RuntimeException {
	 public NotImplementedYetException(){ super(); }

	 public NotImplementedYetException( String message){ super(message); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
