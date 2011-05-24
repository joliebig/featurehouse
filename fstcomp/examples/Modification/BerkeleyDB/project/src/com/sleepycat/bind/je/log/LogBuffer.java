package com.sleepycat.je.log; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
 
class  LogBuffer  implements LogSource {
	 private static final String DEBUG_NAME=LogBuffer.class.getName();

	 private ByteBuffer buffer;

	 private long firstLsn;

	 private long lastLsn;

	 LogBuffer( int capacity, EnvironmentImpl env) throws DatabaseException { this.hook481(capacity); this.hook482(capacity); this.hook479(env); reinit(); }

	 LogBuffer( ByteBuffer buffer, long firstLsn) throws DatabaseException { this.buffer=buffer; this.firstLsn=firstLsn; this.lastLsn=firstLsn; }

	 void reinit__wrappee__base() throws DatabaseException { buffer.clear(); firstLsn=DbLsn.NULL_LSN; lastLsn=DbLsn.NULL_LSN; }

	 void reinit() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	reinit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getFirstLsn__wrappee__base(){ return firstLsn; }

	 long getFirstLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirstLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void registerLsn__wrappee__base( long lsn) throws DatabaseException { if (lastLsn != DbLsn.NULL_LSN) { assert (DbLsn.compareTo(lsn,lastLsn) > 0); } lastLsn=lsn; if (firstLsn == DbLsn.NULL_LSN) { firstLsn=lsn; } }

	 void registerLsn( long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	registerLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean hasRoom__wrappee__base( int numBytes){ return (numBytes <= (buffer.capacity() - buffer.position())); }

	 boolean hasRoom( int numBytes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasRoom__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 ByteBuffer getDataBuffer__wrappee__base(){ return buffer; }

	 ByteBuffer getDataBuffer(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDataBuffer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getCapacity__wrappee__base(){ return buffer.capacity(); }

	 int getCapacity(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCapacity__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean containsLsn__wrappee__base( long lsn) throws DatabaseException { boolean found=false; if ((firstLsn != DbLsn.NULL_LSN) && ((DbLsn.compareTo(firstLsn,lsn) <= 0) && (DbLsn.compareTo(lastLsn,lsn) >= 0))) { found=true; } if (found) { return true; } else { this.hook480(); return false; } }

	 boolean containsLsn( long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	containsLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ByteBuffer getBytes__wrappee__base( long fileOffset){ ByteBuffer copy=null; while (true) { try { copy=buffer.duplicate(); copy.position((int)(fileOffset - DbLsn.getFileOffset(firstLsn))); break; } catch ( IllegalArgumentException IAE) { continue; } } return copy; }

	 public ByteBuffer getBytes( long fileOffset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ByteBuffer getBytes__wrappee__base( long fileOffset, int numBytes){ ByteBuffer copy=getBytes(fileOffset); assert (copy.remaining() >= numBytes) : "copy.remaining=" + copy.remaining() + " numBytes="+ numBytes; return copy; }

	 public ByteBuffer getBytes( long fileOffset, int numBytes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook479__wrappee__base( EnvironmentImpl env) throws DatabaseException { }

	 protected void hook479( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook479__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook480__wrappee__base() throws DatabaseException { }

	 protected void hook480() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook480__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook481__wrappee__base( int capacity) throws DatabaseException { }

	 protected void hook481( int capacity) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook481__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook482__wrappee__base( int capacity) throws DatabaseException { }

	 protected void hook482( int capacity) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook482__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
