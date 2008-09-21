
package com.sleepycat.je.recovery; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.tree.ChildReference; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.Tree; 
import com.sleepycat.je.tree.WithRootLatched; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
 
class  RootUpdater  implements WithRootLatched {
	 private Tree tree;

	 private IN inFromLog;

	 private long lsn=DbLsn.NULL_LSN;

	 private boolean inserted=false;

	 private boolean replaced=false;

	 private long origLsn=DbLsn.NULL_LSN;

	 RootUpdater( Tree tree, IN inFromLog, long lsn){ this.tree=tree; this.inFromLog=inFromLog; this.lsn=lsn; }

	 public IN doWork__wrappee__base( ChildReference root) throws DatabaseException { ChildReference newRoot=tree.makeRootChildReference(inFromLog,new byte[0],lsn); this.hook600(); if (root == null) { tree.setRoot(newRoot,false); inserted=true; } else { origLsn=root.getLsn(); if (DbLsn.compareTo(origLsn,lsn) < 0) { tree.setRoot(newRoot,false); replaced=true; } } return null; }

	 public IN doWork( ChildReference root) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean updateDone__wrappee__base(){ return inserted || replaced; }

	 boolean updateDone(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateDone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getInserted__wrappee__base(){ return inserted; }

	 boolean getInserted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getInserted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getReplaced__wrappee__base(){ return replaced; }

	 boolean getReplaced(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReplaced__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getOriginalLsn__wrappee__base(){ return origLsn; }

	 long getOriginalLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getOriginalLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook600__wrappee__base() throws DatabaseException { }

	 protected void hook600() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook600__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
