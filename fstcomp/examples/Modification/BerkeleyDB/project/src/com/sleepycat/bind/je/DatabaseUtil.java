package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
 
class  DatabaseUtil {
	 static void checkForNullParam__wrappee__base( Object param, String name){ if (param == null) { throw new NullPointerException(name + " cannot be null"); } }

	 static void checkForNullParam( Object param, String name){ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkForNullParam__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static void checkForNullDbt__wrappee__base( DatabaseEntry dbt, String name, boolean checkData){ if (dbt == null) { throw new NullPointerException("DatabaseEntry " + name + " cannot be null"); } if (checkData) { if (dbt.getData() == null) { throw new NullPointerException("Data field for DatabaseEntry " + name + " cannot be null"); } } }

	 static void checkForNullDbt( DatabaseEntry dbt, String name, boolean checkData){ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkForNullDbt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static void checkForPartialKey__wrappee__base( DatabaseEntry dbt){ if (dbt.getPartial()) { throw new IllegalArgumentException("A partial key DatabaseEntry is not allowed"); } }

	 static void checkForPartialKey( DatabaseEntry dbt){ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkForPartialKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
