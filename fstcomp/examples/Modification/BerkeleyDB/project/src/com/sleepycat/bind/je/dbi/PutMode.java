package com.sleepycat.je.dbi; 
import de.ovgu.cide.jakutil.*; 
public  class  PutMode {
	 public static final PutMode NODUP=new PutMode();

	 public static final PutMode CURRENT=new PutMode();

	 public static final PutMode OVERWRITE=new PutMode();

	 public static final PutMode NOOVERWRITE=new PutMode();

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
