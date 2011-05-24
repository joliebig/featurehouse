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
	 private static long lastAllocatedId=0;

	 private static final String BEGIN_TAG="<node>";

	 private static final String END_TAG="</node>";

	 private long nodeId;

	 private Node(){ }

	 protected Node( boolean init){ if (init) { nodeId=getNextNodeId(); } }

	 public synchronized static void setLastNodeId__wrappee__base( long id){ if (lastAllocatedId < id) { lastAllocatedId=id; } }

	 public synchronized static void setLastNodeId( long id){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLastNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static synchronized long getNextNodeId__wrappee__base(){ return ++lastAllocatedId; }

	 public static synchronized long getNextNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static synchronized long getLastId__wrappee__base(){ return lastAllocatedId; }

	 public static synchronized long getLastId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postFetchInit__wrappee__base( DatabaseImpl db, long sourceLsn) throws DatabaseException { }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postFetchInit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getNodeId__wrappee__base(){ return nodeId; }

	 public long getNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setNodeId__wrappee__base( long nid){ nodeId=nid; }

	 void setNodeId( long nid){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void verify__wrappee__base( byte[] maxKey) throws DatabaseException { }

	 public void verify( byte[] maxKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	verify__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean containsDuplicates__wrappee__base(){ return false; }

	 public boolean containsDuplicates(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	containsDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getLevel__wrappee__base(){ return 0; }

	 int getLevel(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLevel__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean matchLNByNodeId__wrappee__base( TreeLocation location, long nodeId) throws DatabaseException { throw new DatabaseException("matchLNByNodeId called on non DIN/DBIN"); }

	 boolean matchLNByNodeId( TreeLocation location, long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	matchLNByNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void rebuildINList__wrappee__base( INList inList) throws DatabaseException ;

	 abstract void rebuildINList( INList inList) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	rebuildINList__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void accountForSubtreeRemoval__wrappee__base( INList inList, UtilizationTracker tracker) throws DatabaseException ;

	 abstract void accountForSubtreeRemoval( INList inList, UtilizationTracker tracker) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	accountForSubtreeRemoval__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract boolean isValidForDelete__wrappee__base() throws DatabaseException ;

	 abstract boolean isValidForDelete() throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isValidForDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected boolean isSoughtNode__wrappee__base( long nid, boolean updateGeneration) throws DatabaseException ;

	 abstract protected boolean isSoughtNode( long nid, boolean updateGeneration) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isSoughtNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected boolean canBeAncestor__wrappee__base( boolean targetContainsDuplicates);

	 abstract protected boolean canBeAncestor( boolean targetContainsDuplicates);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	canBeAncestor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected long getMemorySizeIncludedByParent__wrappee__base(){ return 0; }

	 protected long getMemorySizeIncludedByParent(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMemorySizeIncludedByParent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return this.dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 private String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String endTag__wrappee__base(){ return END_TAG; }

	 private String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dump__wrappee__base( int nSpaces){ System.out.print(dumpString(nSpaces,true)); }

	 public void dump( int nSpaces){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer self=new StringBuffer(); self.append(TreeUtils.indent(nSpaces)); if (dumpTags) { self.append(beginTag()); } self.append(nodeId); if (dumpTags) { self.append(endTag()); } return self.toString(); }

	 String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String shortDescription__wrappee__base(){ return "<" + getType() + "/"+ getNodeId(); }

	 public String shortDescription(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shortDescription__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getType__wrappee__base(){ return getClass().getName(); }

	 public String getType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract LogEntryType getLogType__wrappee__base();

	 public abstract LogEntryType getLogType();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return true; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn) throws DatabaseException { }

	 public void postLogWork( long justLoggedLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return LogUtils.LONG_BYTES; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,nodeId); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { nodeId=LogUtils.readLong(itemBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append(BEGIN_TAG); sb.append(nodeId); sb.append(END_TAG); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
