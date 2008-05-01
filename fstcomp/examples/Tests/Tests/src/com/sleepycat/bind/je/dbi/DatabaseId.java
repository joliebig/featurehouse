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

	 public int getId(){ return id; }

	 public byte[] getBytes() throws DatabaseException { try { return toString().getBytes("UTF-8"); } catch ( UnsupportedEncodingException UEE) { throw new DatabaseException(UEE); } }

	 public boolean equals( Object obj){ if (this == obj) { return true; } if (!(obj instanceof DatabaseId)) { return false; } return ((DatabaseId)obj).id == id; }

	 public int hashCode(){ return id; }

	 public String toString(){ return Integer.toString(id); }

	 public int compareTo( Object o){ if (o == null) { throw new NullPointerException(); } DatabaseId argId=(DatabaseId)o; if (id == argId.id) { return 0; } else if (id > argId.id) { return 1; } else { return -1; } }

	 public int getLogSize(){ return LogUtils.getIntLogSize(); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeInt(logBuffer,id); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion){ id=LogUtils.readInt(itemBuffer); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<dbId id=\""); sb.append(id); sb.append("\"/>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }


}
