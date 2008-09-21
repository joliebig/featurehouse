package com.sleepycat.je.dbi; 
import java.io.UnsupportedEncodingException; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import de.ovgu.cide.jakutil.*; 
public  class  DatabaseId  implements Comparable, LogWritable, LogReadable {
	 private int id;

	 public DatabaseId( int id){ this.id=id; }

	 public DatabaseId(){ }

	 public int getId__wrappee__base(){ return id; }

	 public int getId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getBytes__wrappee__base() throws DatabaseException { try { return toString().getBytes("UTF-8"); } catch ( UnsupportedEncodingException UEE) { throw new DatabaseException(UEE); } }

	 public byte[] getBytes() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean equals__wrappee__base( Object obj){ if (this == obj) { return true; } if (!(obj instanceof DatabaseId)) { return false; } return ((DatabaseId)obj).id == id; }

	 public boolean equals( Object obj){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ return id; }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return Integer.toString(id); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int compareTo__wrappee__base( Object o){ if (o == null) { throw new NullPointerException(); } DatabaseId argId=(DatabaseId)o; if (id == argId.id) { return 0; } else if (id > argId.id) { return 1; } else { return -1; } }

	 public int compareTo( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return LogUtils.getIntLogSize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeInt(logBuffer,id); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion){ id=LogUtils.readInt(itemBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<dbId id=\""); sb.append(id); sb.append("\"/>"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
