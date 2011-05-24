package com.sleepycat.je.log; 
import de.ovgu.cide.jakutil.*; 
public  class  LogFileNotFoundException  extends LogException {
	 public LogFileNotFoundException( String message){ super(message); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
