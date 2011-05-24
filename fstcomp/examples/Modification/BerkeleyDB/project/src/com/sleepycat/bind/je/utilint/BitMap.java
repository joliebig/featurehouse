package com.sleepycat.je.utilint; 
import java.util.BitSet; 
import java.util.HashMap; 
import java.util.Iterator; 
import java.util.Map; 
import de.ovgu.cide.jakutil.*; 
public  class  BitMap {
	 private static final int SEGMENT_SIZE=16;

	 private static final int SEGMENT_MASK=0xffff;

	 private Map bitSegments;

	 public BitMap(){ bitSegments=new HashMap(); }

	 public void set__wrappee__base( long index) throws IndexOutOfBoundsException { if (index < 0) { throw new IndexOutOfBoundsException(index + " is negative."); } BitSet bitset=getBitSet(index,true); if (bitset == null) { throw new IllegalArgumentException(index + " is out of bounds"); } int useIndex=getIntIndex(index); bitset.set(useIndex); }

	 public void set( long index) throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	set__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean get__wrappee__base( long index) throws IndexOutOfBoundsException { if (index < 0) { throw new IndexOutOfBoundsException(index + " is negative."); } BitSet bitset=getBitSet(index,false); if (bitset == null) { return false; } int useIndex=getIntIndex(index); return bitset.get(useIndex); }

	 public boolean get( long index) throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private BitSet getBitSet__wrappee__base( long index, boolean allowCreate){ Long segmentId=new Long(index >> SEGMENT_SIZE); BitSet bitset=(BitSet)bitSegments.get(segmentId); if (allowCreate) { if (bitset == null) { bitset=new BitSet(); bitSegments.put(segmentId,bitset); } } return bitset; }

	 private BitSet getBitSet( long index, boolean allowCreate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBitSet__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private int getIntIndex__wrappee__base( long index){ return (int)(index & SEGMENT_MASK); }

	 private int getIntIndex( long index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getIntIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getNumSegments__wrappee__base(){ return bitSegments.size(); }

	 int getNumSegments(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNumSegments__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int cardinality__wrappee__base(){ int count=0; Iterator iter=bitSegments.values().iterator(); while (iter.hasNext()) { BitSet b=(BitSet)iter.next(); count+=b.cardinality(); } return count; }

	 int cardinality(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cardinality__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
