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

	 protected IN createNewInstance( byte[] identifierKey, int maxEntries, int level){ return new DBIN(getDatabase(),identifierKey,maxEntries,dupKey,level); }

	 protected int generateLevel( DatabaseId dbId, int newLevel){ return newLevel; }

	 public final Comparator getKeyComparator(){ return getDatabase().getDuplicateComparator(); }

	 public byte[] getDupKey(){ return dupKey; }

	 public byte[] getChildKey( IN child) throws DatabaseException { return child.getIdentifierKey(); }

	 public byte[] selectKey( byte[] mainTreeKey, byte[] dupTreeKey){ return dupTreeKey; }

	 public byte[] getDupTreeKey(){ return getIdentifierKey(); }

	 public byte[] getMainTreeKey(){ return dupKey; }

	 public boolean containsDuplicates(){ return true; }

	 LogEntryType getBINDeltaType(){ return LogEntryType.LOG_DUP_BIN_DELTA; }

	 public BINReference createReference(){ return new DBINReference(getNodeId(),getDatabase().getId(),getIdentifierKey(),dupKey); }

	 protected boolean canBeAncestor( boolean targetContainsDuplicates){ return false; }

	 boolean hasNonLNChildren(){ return false; }

	 BIN getCursorBIN( CursorImpl cursor){ return cursor.getDupBIN(); }

	 BIN getCursorBINToBeRemoved( CursorImpl cursor){ return cursor.getDupBINToBeRemoved(); }

	 int getCursorIndex( CursorImpl cursor){ return cursor.getDupIndex(); }

	 void setCursorBIN( CursorImpl cursor, BIN bin){ cursor.setDupBIN((DBIN)bin); }

	 void setCursorIndex( CursorImpl cursor, int index){ cursor.setDupIndex(index); }

	 boolean matchLNByNodeId( TreeLocation location, long nodeId) throws DatabaseException { for (int i=0; i < getNEntries(); i++) { LN ln=(LN)fetchTarget(i); if (ln != null) { if (ln.getNodeId() == nodeId) { location.bin=this; location.index=i; location.lnKey=getKey(i); location.childLsn=getLsn(i); return true; } } } return false; }

	 void accumulateStats( TreeWalkerStatsAccumulator acc){ acc.processDBIN(this,new Long(getNodeId()),getLevel()); }

	 public String beginTag(){ return BEGIN_TAG; }

	 public String endTag(){ return END_TAG; }

	 public String dumpString( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(TreeUtils.indent(nSpaces)); sb.append(beginTag()); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<dupkey>"); sb.append(dupKey == null ? "" : Key.dumpString(dupKey,0)); sb.append("</dupkey>"); sb.append('\n'); sb.append(super.dumpString(nSpaces,false)); sb.append(TreeUtils.indent(nSpaces)); sb.append(endTag()); return sb.toString(); }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_DBIN; }

	 public int getLogSize(){ int size=super.getLogSize(); size+=LogUtils.getByteArrayLogSize(dupKey); return size; }

	 public void writeToLog( ByteBuffer logBuffer){ super.writeToLog(logBuffer); LogUtils.writeByteArray(logBuffer,dupKey); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); dupKey=LogUtils.readByteArray(itemBuffer); }

	 protected void dumpLogAdditional( StringBuffer sb){ super.dumpLogAdditional(sb); sb.append(Key.dumpString(dupKey,0)); }

	 public String shortClassName(){ return "DBIN"; }


}
