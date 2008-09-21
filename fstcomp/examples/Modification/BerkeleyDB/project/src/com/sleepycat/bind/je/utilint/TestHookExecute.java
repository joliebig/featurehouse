package com.sleepycat.je.utilint; 
import de.ovgu.cide.jakutil.*; 
public  class  TestHookExecute {
	 public static boolean doHookIfSet__wrappee__base( TestHook testHook){ if (testHook != null) { testHook.doHook(); } return true; }

	 public static boolean doHookIfSet( TestHook testHook){ t.in(Thread.currentThread().getStackTrace()[1].toString());	doHookIfSet__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
