package com.sleepycat.je.dbi; 
import java.util.HashMap; 
import java.util.Map; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.PreloadConfig; 
import com.sleepycat.je.tree.ChildReference; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.Node; 
import com.sleepycat.je.tree.WithRootLatched; 
import de.ovgu.cide.jakutil.*; 
 
class  PreloadLSNTreeWalker  extends SortedLSNTreeWalker {
	 private Map lsnINMap=new HashMap();

	
private static  class  INEntry {
		 INEntry( IN in, int index){ this.in=in; this.index=index; }

		 IN in;

		 int index;


	}

	 PreloadLSNTreeWalker( DatabaseImpl db, TreeNodeProcessor callback, PreloadConfig conf) throws DatabaseException { super(db,false,false,db.tree.getRootLsn(),callback); accumulateLNs=conf.getLoadLNs(); }

	
private final  class  PreloadWithRootLatched  implements WithRootLatched {
		 public IN doWork__wrappee__base( ChildReference root) throws DatabaseException { walkInternal(); return null; }

		 public IN doWork( ChildReference root) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public void walk__wrappee__base() throws DatabaseException { WithRootLatched preloadWRL=new PreloadWithRootLatched(); dbImpl.getTree().withRootLatchedExclusive(preloadWRL); }

	 public void walk() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	walk__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected IN getRootIN__wrappee__base( long rootLsn) throws DatabaseException { return dbImpl.getTree().getRootIN(false); }

	 protected IN getRootIN( long rootLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getRootIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void addToLsnINMap__wrappee__base( Long lsn, IN in, int index){ assert in.getDatabase() != null; lsnINMap.put(lsn,new INEntry(in,index)); }

	 protected void addToLsnINMap( Long lsn, IN in, int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addToLsnINMap__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Node fetchLSN__wrappee__base( long lsn) throws DatabaseException { try { INEntry inEntry=(INEntry)lsnINMap.remove(new Long(lsn)); assert (inEntry != null); IN in=inEntry.in; this.hook352(lsn,inEntry,in); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (Node)r.value; } }

	 protected Node fetchLSN( long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	fetchLSN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook352__wrappee__base( long lsn, INEntry inEntry, IN in) throws DatabaseException { int index=inEntry.index; if (in.isEntryKnownDeleted(index) || in.getLsn(index) != lsn) { throw new ReturnObject(null); } throw new ReturnObject(in.fetchTarget(index)); }

	 protected void hook352( long lsn, INEntry inEntry, IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook352__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
