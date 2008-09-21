package com.sleepycat.je.utilint; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.NoSuchElementException; 
import java.util.Set; 
import de.ovgu.cide.jakutil.*; 
public  class  TinyHashSet {
	 private Set set;

	 private Object single;

	
public static  class  SingleElementIterator  implements Iterator {
		 Object theObject;

		 TinyHashSet theSet;

		 boolean returnedTheObject=false;

		 SingleElementIterator( Object o, TinyHashSet theSet){ theObject=o; this.theSet=theSet; returnedTheObject=(o == null); }

		 public boolean hasNext__wrappee__base(){ return !returnedTheObject; }

		 public boolean hasNext(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public Object next__wrappee__base(){ if (returnedTheObject) { throw new NoSuchElementException(); } returnedTheObject=true; return theObject; }

		 public Object next(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	next__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public void remove__wrappee__base(){ if (theObject == null || !returnedTheObject) { throw new IllegalStateException(); } theSet.remove(theObject); }

		 public void remove(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	remove__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public int size__wrappee__base(){ if (single != null) { return 1; } else if (set != null) { return set.size(); } else { return 0; } }

	 public int size(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	size__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean remove__wrappee__base( Object o){ assert (single == null) || (set == null); if (single != null) { if (single == o || single.equals(o)) { single=null; return true; } else { return false; } } else if (set != null) { return set.remove(o); } else { return false; } }

	 public boolean remove( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	remove__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean add__wrappee__base( Object o){ assert (single == null) || (set == null); if (set != null) { return set.add(o); } else if (single == null) { single=o; return true; } else { set=new HashSet(); set.add(single); single=null; return set.add(o); } }

	 public boolean add( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	add__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Set copy__wrappee__base(){ assert (single == null) || (set == null); if (set != null) { return new HashSet(set); } else { Set ret=new HashSet(); if (single != null) { ret.add(single); } return ret; } }

	 public Set copy(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	copy__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Iterator iterator__wrappee__base(){ assert (single == null) || (set == null); if (set != null) { return set.iterator(); } else { return new SingleElementIterator(single,this); } }

	 public Iterator iterator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	iterator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
