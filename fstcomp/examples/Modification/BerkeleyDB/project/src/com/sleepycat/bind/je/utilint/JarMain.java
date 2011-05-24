package com.sleepycat.je.utilint; 
import java.lang.reflect.Method; 
import de.ovgu.cide.jakutil.*; 
public  class  JarMain {
	 private static final String USAGE="usage: java <utility> [options...]";

	 private static final String PREFIX="com.sleepycat.je.util.";

	 public static void main__wrappee__base( String[] args){ try { if (args.length < 1) { usage("Missing utility name"); } Class cls=Class.forName(PREFIX + args[0]); Method mainMethod=cls.getMethod("main",new Class[]{String[].class}); String[] mainArgs=new String[args.length - 1]; System.arraycopy(args,1,mainArgs,0,mainArgs.length); mainMethod.invoke(null,new Object[]{mainArgs}); } catch ( Throwable e) { usage(e.toString()); } }

	 public static void main( String[] args){ t.in(Thread.currentThread().getStackTrace()[1].toString());	main__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static void usage__wrappee__base( String msg){ System.err.println(msg); System.err.println(USAGE); System.exit(-1); }

	 private static void usage( String msg){ t.in(Thread.currentThread().getStackTrace()[1].toString());	usage__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
