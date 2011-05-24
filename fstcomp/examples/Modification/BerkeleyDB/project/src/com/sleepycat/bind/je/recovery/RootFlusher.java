
package com.sleepycat.je.recovery; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.tree.ChildReference; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.WithRootLatched; 
import de.ovgu.cide.jakutil.*; 
 
class  RootFlusher  implements WithRootLatched {
	 private DatabaseImpl db;

	 private boolean flushed;

	 private boolean stillRoot;

	 private LogManager logManager;

	 private long targetNodeId;

	 RootFlusher( DatabaseImpl db, LogManager logManager, long targetNodeId){ this.db=db; flushed=false; this.logManager=logManager; this.targetNodeId=targetNodeId; stillRoot=false; }

	 public IN doWork__wrappee__base( ChildReference root) throws DatabaseException { if (root == null) { return null; } IN rootIN=(IN)root.fetchTarget(db,null); this.hook599(root,rootIN); return null; }

	 public IN doWork( ChildReference root) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getFlushed__wrappee__base(){ return flushed; }

	 boolean getFlushed(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFlushed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean stillRoot__wrappee__base(){ return stillRoot; }

	 boolean stillRoot(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	stillRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook599__wrappee__base( ChildReference root, IN rootIN) throws DatabaseException { if (rootIN.getNodeId() == targetNodeId) { stillRoot=true; if (rootIN.getDirty()) { long newLsn=rootIN.log(logManager); root.setLsn(newLsn); flushed=true; } } }

	 protected void hook599( ChildReference root, IN rootIN) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook599__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
