package com.sleepycat.je.tree; 
import com.sleepycat.je.dbi.DatabaseId; 
import de.ovgu.cide.jakutil.*; 
public  class  DBINReference  extends BINReference {
	 private byte[] dupKey;

	 DBINReference( long nodeId, DatabaseId databaseId, byte[] idKey, byte[] dupKey){ super(nodeId,databaseId,idKey); this.dupKey=dupKey; }

	 public byte\[\] getKey__wrappee__base(){ return dupKey; }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getData__wrappee__base(){ return idKey; }

	 public byte[] getData(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return super.toString() + " dupKey=" + Key.dumpString(dupKey,0); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
