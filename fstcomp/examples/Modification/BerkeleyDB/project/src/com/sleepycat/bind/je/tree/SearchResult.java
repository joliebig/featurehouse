package com.sleepycat.je.tree; 
import de.ovgu.cide.jakutil.*; 
public  class  SearchResult {
	 public boolean exactParentFound;

	 public boolean keepSearching;

	 public boolean childNotResident;

	 public IN parent;

	 public int index;

	 public SearchResult(){ exactParentFound=false; keepSearching=true; parent=null; index=-1; childNotResident=false; }

	 public String toString__wrappee__base(){ return "exactParentFound=" + exactParentFound + " keepSearching="+ keepSearching+ " parent="+ ((parent == null) ? "null" : Long.toString(parent.getNodeId()))+ " index="+ index+ " childNotResident="+ childNotResident; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
