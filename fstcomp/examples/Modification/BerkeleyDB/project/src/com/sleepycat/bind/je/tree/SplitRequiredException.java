package com.sleepycat.je.tree; 
import de.ovgu.cide.jakutil.*; 
 
class  SplitRequiredException  extends Exception {
	 public SplitRequiredException(){ }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
