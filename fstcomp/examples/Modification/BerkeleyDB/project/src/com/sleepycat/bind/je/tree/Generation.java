package com.sleepycat.je.tree; 
import de.ovgu.cide.jakutil.*; 
public final  class  Generation {
	 static private long nextGeneration=0;

	 static long getNextGeneration__wrappee__base(){ return nextGeneration++; }

	 static long getNextGeneration(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextGeneration__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
