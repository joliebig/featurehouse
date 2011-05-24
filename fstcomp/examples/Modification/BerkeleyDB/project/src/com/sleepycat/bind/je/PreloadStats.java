package com.sleepycat.je; 
import java.io.Serializable; 
import de.ovgu.cide.jakutil.*; 
public  class  PreloadStats  implements Serializable {
	 public PreloadStats(){ }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
