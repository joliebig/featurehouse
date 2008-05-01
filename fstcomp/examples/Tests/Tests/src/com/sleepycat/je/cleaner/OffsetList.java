package com.sleepycat.je.cleaner; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  OffsetList {
	 static final int SEGMENT_CAPACITY=100;

	 private Segment head;

	 private Segment tail;

	 private int size;

	 public OffsetList(){ head=new Segment(); tail=head; }

	 public boolean add( long value, boolean checkDupOffsets){ if (checkDupOffsets) { assert (!contains(value)) : Tracer.getStackTrace(new Exception("Dup Offset " + Long.toHexString(value))); } Segment oldTail=tail; tail=tail.add(value); size+=1; return tail != oldTail; }

	 public int size(){ return size; }

	 boolean merge( OffsetList other){ boolean oneSegFreed=true; Segment seg=other.head; while (true) { Segment next=seg.next(); if (next != null) { seg.setNext(head); head=seg; seg=next; } else { for (int i=0; i < seg.size(); i+=1) { if (add(seg.get(i),false)) { assert oneSegFreed; oneSegFreed=false; } } break; } } return oneSegFreed; }

	 public long[] toArray(){ long[] a=new long[size]; int next=0; segments: for (Segment seg=head; seg != null; seg=seg.next()) { for (int i=0; i < seg.size(); i+=1) { if (next >= a.length) { break segments; } a[next]=seg.get(i); next+=1; } } return a; }

	 boolean contains( long offset){ for (Segment seg=head; seg != null; seg=seg.next()) { for (int i=0; i < seg.size(); i+=1) { if (seg.get(i) == offset) { return true; } } } return false; }

	
public static  class  Segment {
		 private int index;

		 private Segment next;

		 private int[] values;

		 public Segment(){ values=new int[SEGMENT_CAPACITY]; }

		 Segment add( long value){ if (index < values.length) { values[index]=(int)value; index+=1; return this; } else { Segment seg=new Segment(); seg.values[0]=(int)value; seg.index=1; next=seg; return seg; } }

		 long get( int i){ return ((long)values[i]) & 0xFFFFFFFF; }

		 Segment next(){ return next; }

		 void setNext( Segment next){ this.next=next; }

		 int size(){ return index; }


	}


}
