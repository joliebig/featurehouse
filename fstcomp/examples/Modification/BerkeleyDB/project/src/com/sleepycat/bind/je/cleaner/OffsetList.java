package com.sleepycat.je.cleaner; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  OffsetList {
	 static final int SEGMENT_CAPACITY=100;

	 private Segment head;

	 private Segment tail;

	 private int size;

	 public OffsetList(){ head=new Segment(); tail=head; }

	
public static  class  Segment {
		 private int index;

		 private Segment next;

		 private int[] values;

		 public Segment(){ values=new int[SEGMENT_CAPACITY]; }

		 Segment add__wrappee__base( long value){ if (index < values.length) { values[index]=(int)value; index+=1; return this; } else { Segment seg=new Segment(); seg.values[0]=(int)value; seg.index=1; next=seg; return seg; } }

		 Segment add( long value){ t.in(Thread.currentThread().getStackTrace()[1].toString());	add__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 long get__wrappee__base( int i){ return ((long)values[i]) & 0xFFFFFFFF; }

		 long get( int i){ t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 Segment next__wrappee__base(){ return next; }

		 Segment next(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	next__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 void setNext__wrappee__base( Segment next){ this.next=next; }

		 void setNext( Segment next){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 int size__wrappee__base(){ return index; }

		 int size(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	size__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public boolean add__wrappee__base( long value, boolean checkDupOffsets){ if (checkDupOffsets) { assert (!contains(value)) : Tracer.getStackTrace(new Exception("Dup Offset " + Long.toHexString(value))); } Segment oldTail=tail; tail=tail.add(value); size+=1; return tail != oldTail; }

	 public boolean add( long value, boolean checkDupOffsets){ t.in(Thread.currentThread().getStackTrace()[1].toString());	add__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int size__wrappee__base(){ return size; }

	 public int size(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	size__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean merge__wrappee__base( OffsetList other){ boolean oneSegFreed=true; Segment seg=other.head; while (true) { Segment next=seg.next(); if (next != null) { seg.setNext(head); head=seg; seg=next; } else { for (int i=0; i < seg.size(); i+=1) { if (add(seg.get(i),false)) { assert oneSegFreed; oneSegFreed=false; } } break; } } return oneSegFreed; }

	 boolean merge( OffsetList other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	merge__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long\[\] toArray__wrappee__base(){ long[] a=new long[size]; int next=0; segments: for (Segment seg=head; seg != null; seg=seg.next()) { for (int i=0; i < seg.size(); i+=1) { if (next >= a.length) { break segments; } a[next]=seg.get(i); next+=1; } } return a; }

	 public long[] toArray(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean contains__wrappee__base( long offset){ for (Segment seg=head; seg != null; seg=seg.next()) { for (int i=0; i < seg.size(); i+=1) { if (seg.get(i) == offset) { return true; } } } return false; }

	 boolean contains( long offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	contains__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
