package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import java.util.Comparator; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public final  class  DBIN  extends BIN  implements LoggableObject {
	 private static final String BEGIN_TAG="<dbin>";

	 private static final String END_TAG="</dbin>";

	 private byte[] dupKey;

	 public DBIN(){ super(); }

	 public DBIN( DatabaseImpl db, byte[] identifierKey, int maxEntriesPerNode, byte[] dupKey, int level){ super(db,identifierKey,maxEntriesPerNode,level); this.dupKey=dupKey; }

	 protected IN createNewInstance__wrappee__base( byte[] identifierKey, int maxEntries, int level){ return new DBIN(getDatabase(),identifierKey,maxEntries,dupKey,level); }

	 protected IN createNewInstance( byte[] identifierKey, int maxEntries, int level){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createNewInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int generateLevel__wrappee__base( DatabaseId dbId, int newLevel){ return newLevel; }

	 protected int generateLevel( DatabaseId dbId, int newLevel){ t.in(Thread.currentThread().getStackTrace()[1].toString());	generateLevel__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final Comparator getKeyComparator__wrappee__base(){ return getDatabase().getDuplicateComparator(); }

	 public final Comparator getKeyComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKeyComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupKey__wrappee__base(){ return dupKey; }

	 public byte[] getDupKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getChildKey__wrappee__base( IN child) throws DatabaseException { return child.getIdentifierKey(); }

	 public byte[] getChildKey( IN child) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getChildKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] selectKey__wrappee__base( byte[] mainTreeKey, byte[] dupTreeKey){ return dupTreeKey; }

	 public byte[] selectKey( byte[] mainTreeKey, byte[] dupTreeKey){ t.in(Thread.currentThread().getStackTrace()[1].toString());	selectKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupTreeKey__wrappee__base(){ return getIdentifierKey(); }

	 public byte[] getDupTreeKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupTreeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getMainTreeKey__wrappee__base(){ return dupKey; }

	 public byte[] getMainTreeKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMainTreeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean containsDuplicates__wrappee__base(){ return true; }

	 public boolean containsDuplicates(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	containsDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LogEntryType getBINDeltaType__wrappee__base(){ return LogEntryType.LOG_DUP_BIN_DELTA; }

	 LogEntryType getBINDeltaType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBINDeltaType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public BINReference createReference__wrappee__base(){ return new DBINReference(getNodeId(),getDatabase().getId(),getIdentifierKey(),dupKey); }

	 public BINReference createReference(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createReference__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean canBeAncestor__wrappee__base( boolean targetContainsDuplicates){ return false; }

	 protected boolean canBeAncestor( boolean targetContainsDuplicates){ t.in(Thread.currentThread().getStackTrace()[1].toString());	canBeAncestor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean hasNonLNChildren__wrappee__base(){ return false; }

	 boolean hasNonLNChildren(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasNonLNChildren__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 BIN getCursorBIN__wrappee__base( CursorImpl cursor){ return cursor.getDupBIN(); }

	 BIN getCursorBIN( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 BIN getCursorBINToBeRemoved__wrappee__base( CursorImpl cursor){ return cursor.getDupBINToBeRemoved(); }

	 BIN getCursorBINToBeRemoved( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorBINToBeRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getCursorIndex__wrappee__base( CursorImpl cursor){ return cursor.getDupIndex(); }

	 int getCursorIndex( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setCursorBIN__wrappee__base( CursorImpl cursor, BIN bin){ cursor.setDupBIN((DBIN)bin); }

	 void setCursorBIN( CursorImpl cursor, BIN bin){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCursorBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setCursorIndex__wrappee__base( CursorImpl cursor, int index){ cursor.setDupIndex(index); }

	 void setCursorIndex( CursorImpl cursor, int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCursorIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean matchLNByNodeId__wrappee__base( TreeLocation location, long nodeId) throws DatabaseException { for (int i=0; i < getNEntries(); i++) { LN ln=(LN)fetchTarget(i); if (ln != null) { if (ln.getNodeId() == nodeId) { location.bin=this; location.index=i; location.lnKey=getKey(i); location.childLsn=getLsn(i); return true; } } } return false; }

	 boolean matchLNByNodeId( TreeLocation location, long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	matchLNByNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void accumulateStats__wrappee__base( TreeWalkerStatsAccumulator acc){ acc.processDBIN(this,new Long(getNodeId()),getLevel()); }

	 void accumulateStats( TreeWalkerStatsAccumulator acc){ t.in(Thread.currentThread().getStackTrace()[1].toString());	accumulateStats__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(TreeUtils.indent(nSpaces)); sb.append(beginTag()); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<dupkey>"); sb.append(dupKey == null ? "" : Key.dumpString(dupKey,0)); sb.append("</dupkey>"); sb.append('\n'); sb.append(super.dumpString(nSpaces,false)); sb.append(TreeUtils.indent(nSpaces)); sb.append(endTag()); return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_DBIN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=super.getLogSize(); size+=LogUtils.getByteArrayLogSize(dupKey); return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ super.writeToLog(logBuffer); LogUtils.writeByteArray(logBuffer,dupKey); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); dupKey=LogUtils.readByteArray(itemBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb){ super.dumpLogAdditional(sb); sb.append(Key.dumpString(dupKey,0)); }

	 protected void dumpLogAdditional( StringBuffer sb){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String shortClassName__wrappee__base(){ return "DBIN"; }

	 public String shortClassName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shortClassName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
