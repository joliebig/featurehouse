package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  ForeignKeyDeleteAction {
	 private String name;

	 private ForeignKeyDeleteAction( String name){ this.name=name; }

	 public final static ForeignKeyDeleteAction ABORT=new ForeignKeyDeleteAction("ABORT");

	 public final static ForeignKeyDeleteAction CASCADE=new ForeignKeyDeleteAction("CASCADE");

	 public final static ForeignKeyDeleteAction NULLIFY=new ForeignKeyDeleteAction("NULLIFY");

	 public String toString(){ return "ForeignKeyDeleteAction." + name; }


}
