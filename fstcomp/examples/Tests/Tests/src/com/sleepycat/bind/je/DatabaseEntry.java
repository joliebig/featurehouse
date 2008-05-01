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

	 public String toString(){ StringBuffer sb=new StringBuffer("<DatabaseEntry"); sb.append(" dlen=").append(dlen); sb.append(" doff=").append(doff); sb.append(" doff=").append(doff); sb.append(" offset=").append(offset); sb.append(" size=").append(size); sb.append(">"); return sb.toString(); }

	 public DatabaseEntry(){ }

	 public DatabaseEntry( byte[] data){ this.data=data; if (data != null) { this.size=data.length; } }

	 public DatabaseEntry( byte[] data, int offset, int size){ this.data=data; this.offset=offset; this.size=size; }

	 public byte[] getData(){ return data; }

	 public void setData( byte[] data){ this.data=data; offset=0; size=(data == null) ? 0 : data.length; }

	 public void setData( byte[] data, int offset, int size){ this.data=data; this.offset=offset; this.size=size; }

	 public void setPartial( int doff, int dlen, boolean partial){ setPartialOffset(doff); setPartialLength(dlen); setPartial(partial); }

	 public int getPartialLength(){ return dlen; }

	 public void setPartialLength( int dlen){ this.dlen=dlen; }

	 public int getPartialOffset(){ return doff; }

	 public void setPartialOffset( int doff){ this.doff=doff; }

	 public boolean getPartial(){ return partial; }

	 public void setPartial( boolean partial){ this.partial=partial; }

	 public int getOffset(){ return offset; }

	 public void setOffset( int offset){ this.offset=offset; }

	 public int getSize(){ return size; }

	 public void setSize( int size){ this.size=size; }

	 String dumpData(){ return TreeUtils.dumpByteArray(data); }

	 public boolean equals( Object o){ if (!(o instanceof DatabaseEntry)) { return false; } DatabaseEntry e=(DatabaseEntry)o; if (partial || e.partial) { if (partial != e.partial || dlen != e.dlen || doff != e.doff) { return false; } } if (data == null && e.data == null) { return true; } if (data == null || e.data == null) { return false; } if (size != e.size) { return false; } for (int i=0; i < size; i+=1) { if (data[offset + i] != e.data[e.offset + i]) { return false; } } return true; }

	 public int hashCode(){ int hash=0; if (data != null) { for (int i=0; i < size; i+=1) { hash+=data[offset + i]; } } return hash; }


}
