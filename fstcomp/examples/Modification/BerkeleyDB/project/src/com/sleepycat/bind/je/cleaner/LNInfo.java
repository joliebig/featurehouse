package com.sleepycat.je.cleaner; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.tree.LN; 
import de.ovgu.cide.jakutil.*; 
public final  class  LNInfo {
	 private LN ln;

	 private DatabaseId dbId;

	 private byte[] key;

	 private byte[] dupKey;

	 public LNInfo( LN ln, DatabaseId dbId, byte[] key, byte[] dupKey){ this.ln=ln; this.dbId=dbId; this.key=key; this.dupKey=dupKey; }

	 LN getLN__wrappee__base(){ return ln; }

	 LN getLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 DatabaseId getDbId__wrappee__base(){ return dbId; }

	 DatabaseId getDbId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 byte\[\] getKey__wrappee__base(){ return key; }

	 byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 byte\[\] getDupKey__wrappee__base(){ return dupKey; }

	 byte[] getDupKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
