package com.sleepycat.je.log.entry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.tree.BINDelta; 
import com.sleepycat.je.tree.IN; 
import de.ovgu.cide.jakutil.*; 
public  class  BINDeltaLogEntry  extends SingleItemLogEntry  implements INContainingEntry {
	 public BINDeltaLogEntry( Class logClass){ super(logClass); }

	 public IN getIN__wrappee__base( EnvironmentImpl env) throws DatabaseException { BINDelta delta=(BINDelta)getMainItem(); return delta.reconstituteBIN(env); }

	 public IN getIN( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDbId__wrappee__base(){ BINDelta delta=(BINDelta)getMainItem(); return delta.getDbId(); }

	 public DatabaseId getDbId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLsnOfIN__wrappee__base( long lastReadLsn){ BINDelta delta=(BINDelta)getMainItem(); return delta.getLastFullLsn(); }

	 public long getLsnOfIN( long lastReadLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLsnOfIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
