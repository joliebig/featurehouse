package com.sleepycat.je; 
import com.sleepycat.je.tree.TreeUtils; 
import de.ovgu.cide.jakutil.*; 
public  class  DatabaseEntry {
	 private byte[] data;

	 private int dlen=0;

	 private int doff=0;

	 private int offset=0;

	 private int size=0;

	 private boolean partial=false;

	 public DatabaseEntry(){ }

	 public DatabaseEntry( byte[] data){ this.data=data; if (data != null) { this.size=data.length; } }

	 public DatabaseEntry( byte[] data, int offset, int size){ this.data=data; this.offset=offset; this.size=size; }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer("<DatabaseEntry"); sb.append(" dlen=").append(dlen); sb.append(" doff=").append(doff); sb.append(" doff=").append(doff); sb.append(" offset=").append(offset); sb.append(" size=").append(size); sb.append(">"); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getData__wrappee__base(){ return data; }

	 public byte[] getData(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setData__wrappee__base( byte[] data){ this.data=data; offset=0; size=(data == null) ? 0 : data.length; }

	 public void setData( byte[] data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setData__wrappee__base( byte[] data, int offset, int size){ this.data=data; this.offset=offset; this.size=size; }

	 public void setData( byte[] data, int offset, int size){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setPartial__wrappee__base( int doff, int dlen, boolean partial){ setPartialOffset(doff); setPartialLength(dlen); setPartial(partial); }

	 public void setPartial( int doff, int dlen, boolean partial){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setPartial__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getPartialLength__wrappee__base(){ return dlen; }

	 public int getPartialLength(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPartialLength__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setPartialLength__wrappee__base( int dlen){ this.dlen=dlen; }

	 public void setPartialLength( int dlen){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setPartialLength__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getPartialOffset__wrappee__base(){ return doff; }

	 public int getPartialOffset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPartialOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setPartialOffset__wrappee__base( int doff){ this.doff=doff; }

	 public void setPartialOffset( int doff){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setPartialOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getPartial__wrappee__base(){ return partial; }

	 public boolean getPartial(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPartial__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setPartial__wrappee__base( boolean partial){ this.partial=partial; }

	 public void setPartial( boolean partial){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setPartial__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getOffset__wrappee__base(){ return offset; }

	 public int getOffset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setOffset__wrappee__base( int offset){ this.offset=offset; }

	 public void setOffset( int offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getSize__wrappee__base(){ return size; }

	 public int getSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setSize__wrappee__base( int size){ this.size=size; }

	 public void setSize( int size){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String dumpData__wrappee__base(){ return TreeUtils.dumpByteArray(data); }

	 String dumpData(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean equals__wrappee__base( Object o){ if (!(o instanceof DatabaseEntry)) { return false; } DatabaseEntry e=(DatabaseEntry)o; if (partial || e.partial) { if (partial != e.partial || dlen != e.dlen || doff != e.doff) { return false; } } if (data == null && e.data == null) { return true; } if (data == null || e.data == null) { return false; } if (size != e.size) { return false; } for (int i=0; i < size; i+=1) { if (data[offset + i] != e.data[e.offset + i]) { return false; } } return true; }

	 public boolean equals( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ int hash=0; if (data != null) { for (int i=0; i < size; i+=1) { hash+=data[offset + i]; } } return hash; }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
