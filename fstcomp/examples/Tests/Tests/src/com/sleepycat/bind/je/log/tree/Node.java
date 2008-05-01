package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.INList; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  Node  implements LoggableObject, LogReadable, LogWritable {
	 public synchronized static void setLastNodeId( long id){ if (lastAllocatedId < id) { lastAllocatedId=id; } }

	 private static long lastAllocatedId=0;

	 private static final String BEGIN_TAG="<node>";

	 private static final String END_TAG="</node>";

	 private long nodeId;

	 private Node(){ }

	 protected Node( boolean init){ if (init) { nodeId=getNextNodeId(); } }

	 public static synchronized long getNextNodeId(){ return ++lastAllocatedId; }

	 public static synchronized long getLastId(){ return lastAllocatedId; }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { }

	 public long getNodeId(){ return nodeId; }

	 void setNodeId( long nid){ nodeId=nid; }

	 public void verify( byte[] maxKey) throws DatabaseException { }

	 public boolean containsDuplicates(){ return false; }

	 int getLevel(){ return 0; }

	 boolean matchLNByNodeId( TreeLocation location, long nodeId) throws DatabaseException { throw new DatabaseException("matchLNByNodeId called on non DIN/DBIN"); }

	 abstract void rebuildINList( INList inList) throws DatabaseException ;

	 abstract void accountForSubtreeRemoval( INList inList, UtilizationTracker tracker) throws DatabaseException ;

	 abstract boolean isValidForDelete() throws DatabaseException ;

	 abstract protected boolean isSoughtNode( long nid, boolean updateGeneration) throws DatabaseException ;

	 abstract protected boolean canBeAncestor( boolean targetContainsDuplicates);

	 protected long getMemorySizeIncludedByParent(){ return 0; }

	 public String toString(){ return this.dumpString(0,true); }

	 private String beginTag(){ return BEGIN_TAG; }

	 private String endTag(){ return END_TAG; }

	 public void dump( int nSpaces){ System.out.print(dumpString(nSpaces,true)); }

	 String dumpString( int nSpaces, boolean dumpTags){ StringBuffer self=new StringBuffer(); self.append(TreeUtils.indent(nSpaces)); if (dumpTags) { self.append(beginTag()); } self.append(nodeId); if (dumpTags) { self.append(endTag()); } return self.toString(); }

	 public String shortDescription(){ return "<" + getType() + "/"+ getNodeId(); }

	 public String getType(){ return getClass().getName(); }

	 public abstract LogEntryType getLogType();

	 public boolean marshallOutsideWriteLatch(){ return true; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn) throws DatabaseException { }

	 public int getLogSize(){ return LogUtils.LONG_BYTES; }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,nodeId); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { nodeId=LogUtils.readLong(itemBuffer); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append(BEGIN_TAG); sb.append(nodeId); sb.append(END_TAG); }


}
