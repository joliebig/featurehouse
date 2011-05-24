package com.sleepycat.je; 
import java.io.Serializable; 
import de.ovgu.cide.jakutil.*; 
public  class  PreloadStatus  implements Serializable {
	 private String statusName;

	 private PreloadStatus( String statusName){ this.statusName=statusName; }

	 public static final PreloadStatus SUCCESS=new PreloadStatus("SUCCESS");

	 public static final PreloadStatus FILLED_CACHE=new PreloadStatus("FILLED_CACHE");

	 public static final PreloadStatus EXCEEDED_TIME=new PreloadStatus("EXCEEDED_TIME");

	 public String toString__wrappee__base(){ return "PreloadStatus." + statusName; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
