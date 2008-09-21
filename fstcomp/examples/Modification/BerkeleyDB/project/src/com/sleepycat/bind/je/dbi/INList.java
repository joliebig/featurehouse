package com.sleepycat.je.dbi; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Set; 
import java.util.SortedSet; 
import java.util.TreeSet; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.tree.IN; 
import de.ovgu.cide.jakutil.*; 
public  class  INList {
	 private static final String DEBUG_NAME=INList.class.getName();

	 private SortedSet ins=null;

	 private EnvironmentImpl envImpl;

	 INList( EnvironmentImpl envImpl){ this.envImpl=envImpl; ins=new TreeSet(); this.hook338(envImpl); }

	 public INList( INList orig, EnvironmentImpl envImpl) throws DatabaseException { ins=new TreeSet(orig.getINs()); this.hook340(); this.envImpl=envImpl; this.hook339(envImpl); }

	
@MethodObject static  class  INList_add {
		 INList_add( INList _this, IN in){ this._this=_this; this.in=in; }

		 protected INList _this;

		 protected IN in;

		 protected boolean enteredWithLatchHeld;

		 protected boolean addToMajor;

		 void execute__wrappee__base() throws DatabaseException { this.hook344(); addToMajor=true; this.hook343(); }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook343__wrappee__base() throws DatabaseException { this.hook345(); }

		 protected void hook343() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook343__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook344__wrappee__base() throws DatabaseException { }

		 protected void hook344() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook344__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook345__wrappee__base() throws DatabaseException { _this.addAndSetMemory(_this.ins,in); }

		 protected void hook345() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook345__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  INList_addAndSetMemory {
		 INList_addAndSetMemory( INList _this, Set set, IN in){ this._this=_this; this.set=set; this.in=in; }

		 protected INList _this;

		 protected Set set;

		 protected IN in;

		 protected boolean addOk;

		 protected MemoryBudget mb;

		 void execute__wrappee__base(){ addOk=set.add(in); assert addOk : "failed adding in " + in.getNodeId(); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public SortedSet getINs__wrappee__base(){ return ins; }

	 public SortedSet getINs(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getINs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getSize__wrappee__base(){ return ins.size(); }

	 public int getSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void add__wrappee__base( IN in) throws DatabaseException { new INList_add(this,in).execute(); }

	 public void add( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	add__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void addAndSetMemory__wrappee__base( Set set, IN in){ new INList_addAndSetMemory(this,set,in).execute(); }

	 private void addAndSetMemory( Set set, IN in){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addAndSetMemory__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void removeLatchAlreadyHeld__wrappee__base( IN in) throws DatabaseException { boolean removeDone=ins.remove(in); removeDone=this.hook341(in,removeDone); assert removeDone; this.hook346(in); }

	 public void removeLatchAlreadyHeld( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeLatchAlreadyHeld__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void remove__wrappee__base( IN in) throws DatabaseException { removeLatchAlreadyHeld(in); }

	 public void remove( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	remove__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SortedSet tailSet__wrappee__base( IN in) throws DatabaseException { return ins.tailSet(in); }

	 public SortedSet tailSet( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	tailSet__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public IN first__wrappee__base() throws DatabaseException { return (IN)ins.first(); }

	 public IN first() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	first__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Iterator iterator__wrappee__base(){ return ins.iterator(); }

	 public Iterator iterator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	iterator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void clear__wrappee__base() throws DatabaseException { ins.clear(); this.hook342(); }

	 public void clear() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clear__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dump__wrappee__base(){ System.out.println("size=" + getSize()); Iterator iter=ins.iterator(); while (iter.hasNext()) { IN theIN=(IN)iter.next(); System.out.println("db=" + theIN.getDatabase().getId() + " nid=: "+ theIN.getNodeId()+ "/"+ theIN.getLevel()); } }

	 public void dump(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook338__wrappee__base( EnvironmentImpl envImpl){ }

	 protected void hook338( EnvironmentImpl envImpl){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook338__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook339__wrappee__base( EnvironmentImpl envImpl) throws DatabaseException { }

	 protected void hook339( EnvironmentImpl envImpl) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook339__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook340__wrappee__base() throws DatabaseException { }

	 protected void hook340() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook340__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean hook341__wrappee__base( IN in, boolean removeDone) throws DatabaseException { return removeDone; }

	 protected boolean hook341( IN in, boolean removeDone) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook341__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook342__wrappee__base() throws DatabaseException { }

	 protected void hook342() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook342__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook346__wrappee__base( IN in) throws DatabaseException { }

	 protected void hook346( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook346__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
