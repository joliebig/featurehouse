package com.sleepycat.je.tree; 
import java.util.Iterator; 
import java.util.NoSuchElementException; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public final  class  TreeIterator  implements Iterator {
	 private Tree tree;

	 private BIN nextBin;

	 private int index;

	 public TreeIterator( Tree tree) throws DatabaseException { nextBin=(BIN)tree.getFirstNode(); this.hook755(); index=-1; this.tree=tree; }

	 public boolean hasNext__wrappee__base(){ boolean ret=false; try { this.hook756(); advance(); ret=(nextBin != null) && (index < nextBin.getNEntries()); } catch ( DatabaseException e) { } finally { this.hook757(); } return ret; }

	 public boolean hasNext(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object next__wrappee__base(){ Object ret=null; try { if (nextBin == null) { throw new NoSuchElementException(); } this.hook758(); ret=nextBin.getKey(index); } catch ( DatabaseException e) { } finally { this.hook759(); } return ret; }

	 public Object next(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	next__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void remove__wrappee__base(){ throw new UnsupportedOperationException(); }

	 public void remove(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	remove__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void advance__wrappee__base() throws DatabaseException { while (nextBin != null) { if (++index < nextBin.getNEntries()) { return; } nextBin=tree.getNextBin(nextBin,false); index=-1; } }

	 private void advance() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	advance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook755__wrappee__base() throws DatabaseException { }

	 protected void hook755() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook755__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook756__wrappee__base() throws DatabaseException { }

	 protected void hook756() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook756__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook757__wrappee__base(){ }

	 protected void hook757(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook757__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook758__wrappee__base() throws DatabaseException { }

	 protected void hook758() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook758__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook759__wrappee__base(){ }

	 protected void hook759(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook759__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
