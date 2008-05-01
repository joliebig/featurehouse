package com.sleepycat.je.tree; 
import java.util.Comparator; 
import java.util.Iterator; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.Cleaner; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.txn.BasicLocker; 
import com.sleepycat.je.txn.LockGrantType; 
import com.sleepycat.je.txn.LockResult; 
import com.sleepycat.je.txn.LockType; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.TinyHashSet; 
import de.ovgu.cide.jakutil.*; 
public  class  BIN  extends IN  implements LoggableObject {
	 private static final String BEGIN_TAG="<bin>";

	 private static final String END_TAG="</bin>";

	 private TinyHashSet cursorSet;

	 private long lastDeltaVersion=DbLsn.NULL_LSN;

	 private int numDeltasSinceLastFull;

	 private boolean prohibitNextDelta;

	 public BIN(){ cursorSet=new TinyHashSet(); numDeltasSinceLastFull=0; prohibitNextDelta=false; }

	 public BIN( DatabaseImpl db, byte[] identifierKey, int maxEntriesPerNode, int level){ super(db,identifierKey,maxEntriesPerNode,level); cursorSet=new TinyHashSet(); numDeltasSinceLastFull=0; prohibitNextDelta=false; }

	 public BINReference createReference(){ return new BINReference(getNodeId(),getDatabase().getId(),getIdentifierKey()); }

	 protected IN createNewInstance( byte[] identifierKey, int maxEntries, int level){ return new BIN(getDatabase(),identifierKey,maxEntries,level); }

	 public byte[] getChildKey( IN child) throws DatabaseException { return child.getDupKey(); }

	 LogEntryType getBINDeltaType(){ return LogEntryType.LOG_BIN_DELTA; }

	 public long getLastDeltaVersion(){ return lastDeltaVersion; }

	 public void setProhibitNextDelta(){ prohibitNextDelta=true; }

	 protected void descendOnParentSearch( SearchResult result, boolean targetContainsDuplicates, boolean targetIsRoot, long targetNodeId, Node child, boolean requireExactMatch) throws DatabaseException { if (child.canBeAncestor(targetContainsDuplicates)) { if (targetContainsDuplicates && targetIsRoot) { long childNid=child.getNodeId(); this.hook603(child); result.keepSearching=false; if (childNid == targetNodeId) { result.exactParentFound=true; } else { result.exactParentFound=false; } if (requireExactMatch && !result.exactParentFound) { result.parent=null; this.hook604(); } else { result.parent=this; } } else { this.hook605(); result.parent=(IN)child; } } else { result.exactParentFound=false; result.keepSearching=false; if (!requireExactMatch && targetContainsDuplicates) { result.parent=this; } else { this.hook606(); result.parent=null; } } }

	 protected boolean canBeAncestor( boolean targetContainsDuplicates){ return targetContainsDuplicates; }

	 boolean isEvictionProhibited(){ return (nCursors() > 0); }

	 boolean hasNonLNChildren(){ for (int i=0; i < getNEntries(); i++) { Node node=getTarget(i); if (node != null) { if (!(node instanceof LN)) { return true; } } } return false; }

	 boolean entryZeroKeyComparesLow(){ return false; }

	 public void setKnownDeleted( int index){ super.setKnownDeleted(index); this.hook610(index); setMigrate(index,false); super.setTarget(index,null); setDirty(true); }

	 public void setKnownDeletedLeaveTarget( int index){ setMigrate(index,false); super.setKnownDeleted(index); setDirty(true); }

	 public void clearKnownDeleted( int index){ super.clearKnownDeleted(index); setDirty(true); }

	 public Set getCursorSet(){ return cursorSet.copy(); }

	 public void addCursor( CursorImpl cursor){ cursorSet.add(cursor); }

	 public void removeCursor( CursorImpl cursor){ cursorSet.remove(cursor); }

	 public int nCursors(){ return cursorSet.size(); }

	 BIN getCursorBIN( CursorImpl cursor){ return cursor.getBIN(); }

	 BIN getCursorBINToBeRemoved( CursorImpl cursor){ return cursor.getBINToBeRemoved(); }

	 int getCursorIndex( CursorImpl cursor){ return cursor.getIndex(); }

	 void setCursorBIN( CursorImpl cursor, BIN bin){ cursor.setBIN(bin); }

	 void setCursorIndex( CursorImpl cursor, int index){ cursor.setIndex(index); }

	 void splitSpecial( IN parent, int parentIndex, int maxEntriesPerNode, byte[] key, boolean leftSide) throws DatabaseException { int index=findEntry(key,true,false); int nEntries=getNEntries(); boolean exact=(index & IN.EXACT_MATCH) != 0; index&=~IN.EXACT_MATCH; if (leftSide && index < 0) { splitInternal(parent,parentIndex,maxEntriesPerNode,1); } else if (!leftSide && !exact && index == (nEntries - 1)) { splitInternal(parent,parentIndex,maxEntriesPerNode,nEntries - 1); } else { split(parent,parentIndex,maxEntriesPerNode); } }

	 void adjustCursors( IN newSibling, int newSiblingLow, int newSiblingHigh){ int adjustmentDelta=(newSiblingHigh - newSiblingLow); Iterator iter=cursorSet.iterator(); while (iter.hasNext()) { CursorImpl cursor=(CursorImpl)iter.next(); if (getCursorBINToBeRemoved(cursor) == this) { continue; } int cIdx=getCursorIndex(cursor); BIN cBin=getCursorBIN(cursor); assert cBin == this : "nodeId=" + getNodeId() + " cursor="+ cursor.dumpToString(true); assert newSibling instanceof BIN; BIN ns=(BIN)newSibling; if (newSiblingLow == 0) { if (cIdx < newSiblingHigh) { setCursorBIN(cursor,ns); iter.remove(); ns.addCursor(cursor); } else { setCursorIndex(cursor,cIdx - adjustmentDelta); } } else { if (cIdx >= newSiblingLow) { setCursorIndex(cursor,cIdx - newSiblingLow); setCursorBIN(cursor,ns); iter.remove(); ns.addCursor(cursor); } } } }

	 void adjustCursorsForInsert( int insertIndex){ if (cursorSet != null) { Iterator iter=cursorSet.iterator(); while (iter.hasNext()) { CursorImpl cursor=(CursorImpl)iter.next(); if (getCursorBINToBeRemoved(cursor) != this) { int cIdx=getCursorIndex(cursor); if (insertIndex <= cIdx) { setCursorIndex(cursor,cIdx + 1); } } } } }

	 void adjustCursorsForMutation( int binIndex, DBIN dupBin, int dupBinIndex, CursorImpl excludeCursor){ if (cursorSet != null) { Iterator iter=cursorSet.iterator(); while (iter.hasNext()) { CursorImpl cursor=(CursorImpl)iter.next(); if (getCursorBINToBeRemoved(cursor) != this && cursor != excludeCursor && cursor.getIndex() == binIndex) { assert cursor.getDupBIN() == null; cursor.addCursor(dupBin); cursor.updateDBin(dupBin,dupBinIndex); } } } }

	 public boolean compress( BINReference binRef, boolean canFetch) throws DatabaseException { boolean ret=false; boolean setNewIdKey=false; boolean anyLocksDenied=false; DatabaseImpl db=getDatabase(); BasicLocker lockingTxn=new BasicLocker(db.getDbEnvironment()); try { for (int i=0; i < getNEntries(); i++) { boolean deleteEntry=false; if (binRef == null || isEntryPendingDeleted(i) || isEntryKnownDeleted(i) || binRef.hasDeletedKey(new Key(getKey(i)))) { Node n=null; if (canFetch) { n=fetchTarget(i); } else { n=getTarget(i); if (n == null) { continue; } } if (n == null) { deleteEntry=true; } else if (isEntryKnownDeleted(i)) { LockResult lockRet=lockingTxn.nonBlockingLock(n.getNodeId(),LockType.READ,db); if (lockRet.getLockGrant() == LockGrantType.DENIED) { anyLocksDenied=true; continue; } deleteEntry=true; } else { if (!n.containsDuplicates()) { LN ln=(LN)n; LockResult lockRet=lockingTxn.nonBlockingLock(ln.getNodeId(),LockType.READ,db); if (lockRet.getLockGrant() == LockGrantType.DENIED) { anyLocksDenied=true; continue; } if (ln.isDeleted()) { deleteEntry=true; } } } if (binRef != null) { binRef.removeDeletedKey(new Key(getKey(i))); } } if (deleteEntry) { boolean entryIsIdentifierKey=Key.compareKeys(getKey(i),getIdentifierKey(),getKeyComparator()) == 0; if (entryIsIdentifierKey) { setNewIdKey=true; } boolean deleteSuccess=deleteEntry(i,true); assert deleteSuccess; i--; } } } finally { if (lockingTxn != null) { lockingTxn.operationEnd(); } } if (anyLocksDenied && binRef != null) { this.hook609(binRef,db); ret=true; } if (getNEntries() != 0 && setNewIdKey) { setIdentifierKey(getKey(0)); } if (getNEntries() == 0) { setGeneration(0); } return ret; }

	 public boolean isCompressible(){ return true; }

	 boolean validateSubtreeBeforeDelete( int index) throws DatabaseException { return true; }

	 boolean isValidForDelete() throws DatabaseException { try { int validIndex=0; int numValidEntries=0; boolean needToLatch=false; this.hook607(validIndex,numValidEntries,needToLatch); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 void accumulateStats( TreeWalkerStatsAccumulator acc){ acc.processBIN(this,new Long(getNodeId()),getLevel()); }

	 public Comparator getKeyComparator(){ return getDatabase().getBtreeComparator(); }

	 public String beginTag(){ return BEGIN_TAG; }

	 public String endTag(){ return END_TAG; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_BIN; }

	 public String shortClassName(){ return "BIN"; }

	 protected long logInternal( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { boolean doDeltaLog=false; long lastFullVersion=getLastFullVersion(); Cleaner cleaner=getDatabase().getDbEnvironment().getCleaner(); cleaner.lazyMigrateLNs(this,proactiveMigration); BINDelta deltaInfo=null; if ((allowDeltas) && (lastFullVersion != DbLsn.NULL_LSN) && !prohibitNextDelta) { deltaInfo=new BINDelta(this); doDeltaLog=doDeltaLog(deltaInfo); } long returnLsn=DbLsn.NULL_LSN; if (doDeltaLog) { lastDeltaVersion=logManager.log(deltaInfo); returnLsn=DbLsn.NULL_LSN; numDeltasSinceLastFull++; } else { returnLsn=super.logInternal(logManager,allowDeltas,isProvisional,proactiveMigration,parent); lastDeltaVersion=DbLsn.NULL_LSN; numDeltasSinceLastFull=0; } prohibitNextDelta=false; return returnLsn; }

	 private boolean doDeltaLog( BINDelta deltaInfo) throws DatabaseException { int maxDiffs=(getNEntries() * getDatabase().getBinDeltaPercent()) / 100; if ((deltaInfo.getNumDeltas() <= maxDiffs) && (numDeltasSinceLastFull < getDatabase().getBinMaxDeltas())) { return true; } else { return false; } }

	 protected void hook603( Node child) throws DatabaseException { }

	 protected void hook604() throws DatabaseException { }

	 protected void hook605() throws DatabaseException { }

	 protected void hook606() throws DatabaseException { }

	 protected void hook607( int validIndex, int numValidEntries, boolean needToLatch) throws DatabaseException { this.hook608(needToLatch); for (int i=0; i < getNEntries(); i++) { if (!isEntryKnownDeleted(i)) { numValidEntries++; validIndex=i; } } if (numValidEntries > 1) { throw new ReturnBoolean(false); } else { if (nCursors() > 0) { throw new ReturnBoolean(false); } if (numValidEntries == 1) { Node child=fetchTarget(validIndex); throw new ReturnBoolean(child != null && child.isValidForDelete()); } else { throw new ReturnBoolean(true); } } }

	 protected void hook608( boolean needToLatch) throws DatabaseException { }

	 protected void hook609( BINReference binRef, DatabaseImpl db) throws DatabaseException { }

	 protected void hook610( int index){ }


}
