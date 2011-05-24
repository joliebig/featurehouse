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

	 public BINReference createReference__wrappee__base(){ return new BINReference(getNodeId(),getDatabase().getId(),getIdentifierKey()); }

	 public BINReference createReference(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createReference__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected IN createNewInstance__wrappee__base( byte[] identifierKey, int maxEntries, int level){ return new BIN(getDatabase(),identifierKey,maxEntries,level); }

	 protected IN createNewInstance( byte[] identifierKey, int maxEntries, int level){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createNewInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getChildKey__wrappee__base( IN child) throws DatabaseException { return child.getDupKey(); }

	 public byte[] getChildKey( IN child) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getChildKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LogEntryType getBINDeltaType__wrappee__base(){ return LogEntryType.LOG_BIN_DELTA; }

	 LogEntryType getBINDeltaType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBINDeltaType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLastDeltaVersion__wrappee__base(){ return lastDeltaVersion; }

	 public long getLastDeltaVersion(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastDeltaVersion__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setProhibitNextDelta__wrappee__base(){ prohibitNextDelta=true; }

	 public void setProhibitNextDelta(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setProhibitNextDelta__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void descendOnParentSearch__wrappee__base( SearchResult result, boolean targetContainsDuplicates, boolean targetIsRoot, long targetNodeId, Node child, boolean requireExactMatch) throws DatabaseException { if (child.canBeAncestor(targetContainsDuplicates)) { if (targetContainsDuplicates && targetIsRoot) { long childNid=child.getNodeId(); this.hook603(child); result.keepSearching=false; if (childNid == targetNodeId) { result.exactParentFound=true; } else { result.exactParentFound=false; } if (requireExactMatch && !result.exactParentFound) { result.parent=null; this.hook604(); } else { result.parent=this; } } else { this.hook605(); result.parent=(IN)child; } } else { result.exactParentFound=false; result.keepSearching=false; if (!requireExactMatch && targetContainsDuplicates) { result.parent=this; } else { this.hook606(); result.parent=null; } } }

	 protected void descendOnParentSearch( SearchResult result, boolean targetContainsDuplicates, boolean targetIsRoot, long targetNodeId, Node child, boolean requireExactMatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	descendOnParentSearch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean canBeAncestor__wrappee__base( boolean targetContainsDuplicates){ return targetContainsDuplicates; }

	 protected boolean canBeAncestor( boolean targetContainsDuplicates){ t.in(Thread.currentThread().getStackTrace()[1].toString());	canBeAncestor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isEvictionProhibited__wrappee__base(){ return (nCursors() > 0); }

	 boolean isEvictionProhibited(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isEvictionProhibited__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean hasNonLNChildren__wrappee__base(){ for (int i=0; i < getNEntries(); i++) { Node node=getTarget(i); if (node != null) { if (!(node instanceof LN)) { return true; } } } return false; }

	 boolean hasNonLNChildren(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasNonLNChildren__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean entryZeroKeyComparesLow__wrappee__base(){ return false; }

	 boolean entryZeroKeyComparesLow(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryZeroKeyComparesLow__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setKnownDeleted__wrappee__base( int index){ super.setKnownDeleted(index); this.hook610(index); setMigrate(index,false); super.setTarget(index,null); setDirty(true); }

	 public void setKnownDeleted( int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setKnownDeletedLeaveTarget__wrappee__base( int index){ setMigrate(index,false); super.setKnownDeleted(index); setDirty(true); }

	 public void setKnownDeletedLeaveTarget( int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setKnownDeletedLeaveTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void clearKnownDeleted__wrappee__base( int index){ super.clearKnownDeleted(index); setDirty(true); }

	 public void clearKnownDeleted( int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Set getCursorSet__wrappee__base(){ return cursorSet.copy(); }

	 public Set getCursorSet(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorSet__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addCursor__wrappee__base( CursorImpl cursor){ cursorSet.add(cursor); }

	 public void addCursor( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void removeCursor__wrappee__base( CursorImpl cursor){ cursorSet.remove(cursor); }

	 public void removeCursor( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int nCursors__wrappee__base(){ return cursorSet.size(); }

	 public int nCursors(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nCursors__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 BIN getCursorBIN__wrappee__base( CursorImpl cursor){ return cursor.getBIN(); }

	 BIN getCursorBIN( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 BIN getCursorBINToBeRemoved__wrappee__base( CursorImpl cursor){ return cursor.getBINToBeRemoved(); }

	 BIN getCursorBINToBeRemoved( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorBINToBeRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getCursorIndex__wrappee__base( CursorImpl cursor){ return cursor.getIndex(); }

	 int getCursorIndex( CursorImpl cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setCursorBIN__wrappee__base( CursorImpl cursor, BIN bin){ cursor.setBIN(bin); }

	 void setCursorBIN( CursorImpl cursor, BIN bin){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCursorBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setCursorIndex__wrappee__base( CursorImpl cursor, int index){ cursor.setIndex(index); }

	 void setCursorIndex( CursorImpl cursor, int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCursorIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void splitSpecial__wrappee__base( IN parent, int parentIndex, int maxEntriesPerNode, byte[] key, boolean leftSide) throws DatabaseException { int index=findEntry(key,true,false); int nEntries=getNEntries(); boolean exact=(index & IN.EXACT_MATCH) != 0; index&=~IN.EXACT_MATCH; if (leftSide && index < 0) { splitInternal(parent,parentIndex,maxEntriesPerNode,1); } else if (!leftSide && !exact && index == (nEntries - 1)) { splitInternal(parent,parentIndex,maxEntriesPerNode,nEntries - 1); } else { split(parent,parentIndex,maxEntriesPerNode); } }

	 void splitSpecial( IN parent, int parentIndex, int maxEntriesPerNode, byte[] key, boolean leftSide) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	splitSpecial__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void adjustCursors__wrappee__base( IN newSibling, int newSiblingLow, int newSiblingHigh){ int adjustmentDelta=(newSiblingHigh - newSiblingLow); Iterator iter=cursorSet.iterator(); while (iter.hasNext()) { CursorImpl cursor=(CursorImpl)iter.next(); if (getCursorBINToBeRemoved(cursor) == this) { continue; } int cIdx=getCursorIndex(cursor); BIN cBin=getCursorBIN(cursor); assert cBin == this : "nodeId=" + getNodeId() + " cursor="+ cursor.dumpToString(true); assert newSibling instanceof BIN; BIN ns=(BIN)newSibling; if (newSiblingLow == 0) { if (cIdx < newSiblingHigh) { setCursorBIN(cursor,ns); iter.remove(); ns.addCursor(cursor); } else { setCursorIndex(cursor,cIdx - adjustmentDelta); } } else { if (cIdx >= newSiblingLow) { setCursorIndex(cursor,cIdx - newSiblingLow); setCursorBIN(cursor,ns); iter.remove(); ns.addCursor(cursor); } } } }

	 void adjustCursors( IN newSibling, int newSiblingLow, int newSiblingHigh){ t.in(Thread.currentThread().getStackTrace()[1].toString());	adjustCursors__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void adjustCursorsForInsert__wrappee__base( int insertIndex){ if (cursorSet != null) { Iterator iter=cursorSet.iterator(); while (iter.hasNext()) { CursorImpl cursor=(CursorImpl)iter.next(); if (getCursorBINToBeRemoved(cursor) != this) { int cIdx=getCursorIndex(cursor); if (insertIndex <= cIdx) { setCursorIndex(cursor,cIdx + 1); } } } } }

	 void adjustCursorsForInsert( int insertIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	adjustCursorsForInsert__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void adjustCursorsForMutation__wrappee__base( int binIndex, DBIN dupBin, int dupBinIndex, CursorImpl excludeCursor){ if (cursorSet != null) { Iterator iter=cursorSet.iterator(); while (iter.hasNext()) { CursorImpl cursor=(CursorImpl)iter.next(); if (getCursorBINToBeRemoved(cursor) != this && cursor != excludeCursor && cursor.getIndex() == binIndex) { assert cursor.getDupBIN() == null; cursor.addCursor(dupBin); cursor.updateDBin(dupBin,dupBinIndex); } } } }

	 void adjustCursorsForMutation( int binIndex, DBIN dupBin, int dupBinIndex, CursorImpl excludeCursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	adjustCursorsForMutation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean compress__wrappee__base( BINReference binRef, boolean canFetch) throws DatabaseException { boolean ret=false; boolean setNewIdKey=false; boolean anyLocksDenied=false; DatabaseImpl db=getDatabase(); BasicLocker lockingTxn=new BasicLocker(db.getDbEnvironment()); try { for (int i=0; i < getNEntries(); i++) { boolean deleteEntry=false; if (binRef == null || isEntryPendingDeleted(i) || isEntryKnownDeleted(i) || binRef.hasDeletedKey(new Key(getKey(i)))) { Node n=null; if (canFetch) { n=fetchTarget(i); } else { n=getTarget(i); if (n == null) { continue; } } if (n == null) { deleteEntry=true; } else if (isEntryKnownDeleted(i)) { LockResult lockRet=lockingTxn.nonBlockingLock(n.getNodeId(),LockType.READ,db); if (lockRet.getLockGrant() == LockGrantType.DENIED) { anyLocksDenied=true; continue; } deleteEntry=true; } else { if (!n.containsDuplicates()) { LN ln=(LN)n; LockResult lockRet=lockingTxn.nonBlockingLock(ln.getNodeId(),LockType.READ,db); if (lockRet.getLockGrant() == LockGrantType.DENIED) { anyLocksDenied=true; continue; } if (ln.isDeleted()) { deleteEntry=true; } } } if (binRef != null) { binRef.removeDeletedKey(new Key(getKey(i))); } } if (deleteEntry) { boolean entryIsIdentifierKey=Key.compareKeys(getKey(i),getIdentifierKey(),getKeyComparator()) == 0; if (entryIsIdentifierKey) { setNewIdKey=true; } boolean deleteSuccess=deleteEntry(i,true); assert deleteSuccess; i--; } } } finally { if (lockingTxn != null) { lockingTxn.operationEnd(); } } if (anyLocksDenied && binRef != null) { this.hook609(binRef,db); ret=true; } if (getNEntries() != 0 && setNewIdKey) { setIdentifierKey(getKey(0)); } if (getNEntries() == 0) { setGeneration(0); } return ret; }

	 public boolean compress( BINReference binRef, boolean canFetch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	compress__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isCompressible__wrappee__base(){ return true; }

	 public boolean isCompressible(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isCompressible__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean validateSubtreeBeforeDelete__wrappee__base( int index) throws DatabaseException { return true; }

	 boolean validateSubtreeBeforeDelete( int index) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateSubtreeBeforeDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isValidForDelete__wrappee__base() throws DatabaseException { try { int validIndex=0; int numValidEntries=0; boolean needToLatch=false; this.hook607(validIndex,numValidEntries,needToLatch); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 boolean isValidForDelete() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	isValidForDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void accumulateStats__wrappee__base( TreeWalkerStatsAccumulator acc){ acc.processBIN(this,new Long(getNodeId()),getLevel()); }

	 void accumulateStats( TreeWalkerStatsAccumulator acc){ t.in(Thread.currentThread().getStackTrace()[1].toString());	accumulateStats__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Comparator getKeyComparator__wrappee__base(){ return getDatabase().getBtreeComparator(); }

	 public Comparator getKeyComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKeyComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_BIN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String shortClassName__wrappee__base(){ return "BIN"; }

	 public String shortClassName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shortClassName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected long logInternal__wrappee__base( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { boolean doDeltaLog=false; long lastFullVersion=getLastFullVersion(); Cleaner cleaner=getDatabase().getDbEnvironment().getCleaner(); cleaner.lazyMigrateLNs(this,proactiveMigration); BINDelta deltaInfo=null; if ((allowDeltas) && (lastFullVersion != DbLsn.NULL_LSN) && !prohibitNextDelta) { deltaInfo=new BINDelta(this); doDeltaLog=doDeltaLog(deltaInfo); } long returnLsn=DbLsn.NULL_LSN; if (doDeltaLog) { lastDeltaVersion=logManager.log(deltaInfo); returnLsn=DbLsn.NULL_LSN; numDeltasSinceLastFull++; } else { returnLsn=super.logInternal(logManager,allowDeltas,isProvisional,proactiveMigration,parent); lastDeltaVersion=DbLsn.NULL_LSN; numDeltasSinceLastFull=0; } prohibitNextDelta=false; return returnLsn; }

	 protected long logInternal( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	logInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean doDeltaLog__wrappee__base( BINDelta deltaInfo) throws DatabaseException { int maxDiffs=(getNEntries() * getDatabase().getBinDeltaPercent()) / 100; if ((deltaInfo.getNumDeltas() <= maxDiffs) && (numDeltasSinceLastFull < getDatabase().getBinMaxDeltas())) { return true; } else { return false; } }

	 private boolean doDeltaLog( BINDelta deltaInfo) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doDeltaLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook603__wrappee__base( Node child) throws DatabaseException { }

	 protected void hook603( Node child) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook603__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook604__wrappee__base() throws DatabaseException { }

	 protected void hook604() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook604__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook605__wrappee__base() throws DatabaseException { }

	 protected void hook605() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook605__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook606__wrappee__base() throws DatabaseException { }

	 protected void hook606() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook606__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook607__wrappee__base( int validIndex, int numValidEntries, boolean needToLatch) throws DatabaseException { this.hook608(needToLatch); for (int i=0; i < getNEntries(); i++) { if (!isEntryKnownDeleted(i)) { numValidEntries++; validIndex=i; } } if (numValidEntries > 1) { throw new ReturnBoolean(false); } else { if (nCursors() > 0) { throw new ReturnBoolean(false); } if (numValidEntries == 1) { Node child=fetchTarget(validIndex); throw new ReturnBoolean(child != null && child.isValidForDelete()); } else { throw new ReturnBoolean(true); } } }

	 protected void hook607( int validIndex, int numValidEntries, boolean needToLatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook607__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook608__wrappee__base( boolean needToLatch) throws DatabaseException { }

	 protected void hook608( boolean needToLatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook608__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook609__wrappee__base( BINReference binRef, DatabaseImpl db) throws DatabaseException { }

	 protected void hook609( BINReference binRef, DatabaseImpl db) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook609__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook610__wrappee__base( int index){ }

	 protected void hook610( int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook610__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
