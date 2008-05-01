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

	 void reinit() throws DatabaseException { buffer.clear(); firstLsn=DbLsn.NULL_LSN; lastLsn=DbLsn.NULL_LSN; }

	 long getFirstLsn(){ return firstLsn; }

	 void registerLsn( long lsn) throws DatabaseException { if (lastLsn != DbLsn.NULL_LSN) { assert (DbLsn.compareTo(lsn,lastLsn) > 0); } lastLsn=lsn; if (firstLsn == DbLsn.NULL_LSN) { firstLsn=lsn; } }

	 boolean hasRoom( int numBytes){ return (numBytes <= (buffer.capacity() - buffer.position())); }

	 ByteBuffer getDataBuffer(){ return buffer; }

	 int getCapacity(){ return buffer.capacity(); }

	 boolean containsLsn( long lsn) throws DatabaseException { boolean found=false; if ((firstLsn != DbLsn.NULL_LSN) && ((DbLsn.compareTo(firstLsn,lsn) <= 0) && (DbLsn.compareTo(lastLsn,lsn) >= 0))) { found=true; } if (found) { return true; } else { this.hook480(); return false; } }

	 public ByteBuffer getBytes( long fileOffset){ ByteBuffer copy=null; while (true) { try { copy=buffer.duplicate(); copy.position((int)(fileOffset - DbLsn.getFileOffset(firstLsn))); break; } catch ( IllegalArgumentException IAE) { continue; } } return copy; }

	 public ByteBuffer getBytes( long fileOffset, int numBytes){ ByteBuffer copy=getBytes(fileOffset); assert (copy.remaining() >= numBytes) : "copy.remaining=" + copy.remaining() + " numBytes="+ numBytes; return copy; }

	 protected void hook479( EnvironmentImpl env) throws DatabaseException { }

	 protected void hook480() throws DatabaseException { }

	 protected void hook481( int capacity) throws DatabaseException { }

	 protected void hook482( int capacity) throws DatabaseException { }


}
