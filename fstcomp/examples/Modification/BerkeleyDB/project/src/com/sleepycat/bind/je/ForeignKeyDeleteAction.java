package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  ForeignKeyDeleteAction {
	 private String name;

	 private ForeignKeyDeleteAction( String name){ this.name=name; }

	 public final static ForeignKeyDeleteAction ABORT=new ForeignKeyDeleteAction("ABORT");

	 public final static ForeignKeyDeleteAction CASCADE=new ForeignKeyDeleteAction("CASCADE");

	 public final static ForeignKeyDeleteAction NULLIFY=new ForeignKeyDeleteAction("NULLIFY");

	 public String toString__wrappee__base(){ return "ForeignKeyDeleteAction." + name; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
