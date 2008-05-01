package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import java.util.ArrayList; 
import java.util.List; 
import java.util.ListIterator; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.DbTree; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.INList; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.recovery.RecoveryManager; 
import com.sleepycat.je.txn.BasicLocker; 
import com.sleepycat.je.txn.LockGrantType; 
import com.sleepycat.je.txn.LockResult; 
import com.sleepycat.je.txn.LockType; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.WriteLockInfo; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.TestHook; 
import com.sleepycat.je.utilint.TestHookExecute; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public final  class  Tree  implements LogWritable, LogReadable {
	 private static final String TRACE_ROOT_SPLIT="RootSplit:";

	 private static final String TRACE_DUP_ROOT_SPLIT="DupRootSplit:";

	 private static final String TRACE_MUTATE="Mut:";

	 private static final String TRACE_INSERT="Ins:";

	 private static final String TRACE_INSERT_DUPLICATE="InsD:";

	 private DatabaseImpl database;

	 private ChildReference root;

	 private int maxMainTreeEntriesPerNode;

	 private int maxDupTreeEntriesPerNode;

	 private boolean purgeRoot;

	 private TreeStats treeStats;

	 private ThreadLocal treeStatsAccumulatorTL=new ThreadLocal();

	 private static SplitRequiredException splitRequiredException=new SplitRequiredException();

	
public static  class  SearchType {
		 public static final SearchType NORMAL=new SearchType();

		 public static final SearchType LEFT=new SearchType();

		 public static final SearchType RIGHT=new SearchType();

		 private SearchType(){ }


	}

	 private TestHook waitHook;

	 private TestHook searchHook;

	 private TestHook ckptHook;

	 public Tree( DatabaseImpl database) throws DatabaseException { init(database); setDatabase(database); }

	 public Tree() throws DatabaseException { init(null); maxMainTreeEntriesPerNode=0; maxDupTreeEntriesPerNode=0; }

	 private void init( DatabaseImpl database){ treeStats=new TreeStats(); this.root=null; this.database=database; }

	 public void setDatabase( DatabaseImpl database) throws DatabaseException { this.database=database; maxMainTreeEntriesPerNode=database.getNodeMaxEntries(); maxDupTreeEntriesPerNode=database.getNodeMaxDupTreeEntries(); DbConfigManager configManager=database.getDbEnvironment().getConfigManager(); purgeRoot=configManager.getBoolean(EnvironmentParams.COMPRESSOR_PURGE_ROOT); }

	 public DatabaseImpl getDatabase(){ return database; }

	 public void setRoot( ChildReference newRoot, boolean notLatched){ root=newRoot; }

	 public ChildReference makeRootChildReference( Node target, byte[] key, long lsn){ return new RootChildReference(target,key,lsn); }

	 private ChildReference makeRootChildReference(){ return new RootChildReference(); }

	
private  class  RootChildReference  extends ChildReference {
		 private RootChildReference(){ super(); }

		 private RootChildReference( Node target, byte[] key, long lsn){ super(target,key,lsn); }

		 private RootChildReference( Node target, byte[] key, long lsn, byte existingState){ super(target,key,lsn,existingState); }

		 public Node fetchTarget( DatabaseImpl database, IN in) throws DatabaseException { this.hook666(); return super.fetchTarget(database,in); }

		 public void setTarget( Node target){ this.hook667(); super.setTarget(target); }

		 public void clearTarget(){ this.hook668(); super.clearTarget(); }

		 public void setLsn( long lsn){ this.hook669(); super.setLsn(lsn); }

		 protected void hook666() throws DatabaseException { }

		 protected void hook667(){ }

		 protected void hook668(){ }

		 protected void hook669(){ }


	}

	 public long getRootLsn(){ if (root == null) { return DbLsn.NULL_LSN; } else { return root.getLsn(); } }

	 TreeStats getTreeStats(){ return treeStats; }

	 private TreeWalkerStatsAccumulator getTreeStatsAccumulator(){ if (EnvironmentImpl.getThreadLocalReferenceCount() > 0) { return (TreeWalkerStatsAccumulator)treeStatsAccumulatorTL.get(); } else { return null; } }

	 public void setTreeStatsAccumulator( TreeWalkerStatsAccumulator tSA){ treeStatsAccumulatorTL.set(tSA); }

	 public IN withRootLatchedExclusive( WithRootLatched wrl) throws DatabaseException { try { this.hook670(wrl); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (IN)r.value; } }

	 public IN withRootLatchedShared( WithRootLatched wrl) throws DatabaseException { try { this.hook671(wrl); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (IN)r.value; } }

	 public void delete( byte[] idKey, UtilizationTracker tracker) throws DatabaseException, NodeNotEmptyException, CursorsExistException { try { IN subtreeRootIN=null; ArrayList nodeLadder=new ArrayList(); IN rootIN=null; boolean rootNeedsUpdating=false; this.hook672(idKey,tracker,subtreeRootIN,nodeLadder,rootIN,rootNeedsUpdating); if (subtreeRootIN != null) { EnvironmentImpl envImpl=database.getDbEnvironment(); if (rootNeedsUpdating) { DbTree dbTree=envImpl.getDbMapTree(); dbTree.modifyDbRoot(database); this.hook661(); } INList inList=envImpl.getInMemoryINs(); accountForSubtreeRemoval(inList,subtreeRootIN,tracker); } } catch ( ReturnVoid r) { return; } }

	 private IN logTreeRemoval( IN rootIN, UtilizationTracker tracker) throws DatabaseException { IN detachedRootIN=null; if ((rootIN.getNEntries() <= 1) && (rootIN.validateSubtreeBeforeDelete(0))) { root=null; EnvironmentImpl envImpl=database.getDbEnvironment(); LogManager logManager=envImpl.getLogManager(); logManager.log(new INDeleteInfo(rootIN.getNodeId(),rootIN.getIdentifierKey(),database.getId())); detachedRootIN=rootIN; } return detachedRootIN; }

	 private boolean cascadeUpdates( ArrayList nodeLadder, BIN binRoot, int index) throws DatabaseException { ListIterator iter=nodeLadder.listIterator(nodeLadder.size()); EnvironmentImpl envImpl=database.getDbEnvironment(); LogManager logManager=envImpl.getLogManager(); long newLsn=DbLsn.NULL_LSN; SplitInfo info4=null; while (iter.hasPrevious()) { info4=(SplitInfo)iter.previous(); if (newLsn != DbLsn.NULL_LSN) { info4.parent.updateEntry(info4.index,newLsn); } newLsn=info4.parent.log(logManager); } boolean rootNeedsUpdating=false; if (info4 != null) { if (info4.parent.isDbRoot()) { this.hook673(); root.setLsn(newLsn); rootNeedsUpdating=true; } else if ((binRoot != null) && info4.parent.isRoot()) { binRoot.updateEntry(index,newLsn); } else { assert false; } } return rootNeedsUpdating; }

	 public void deleteDup( byte[] idKey, byte[] mainKey, UtilizationTracker tracker) throws DatabaseException, NodeNotEmptyException, CursorsExistException { IN in=search(mainKey,SearchType.NORMAL,-1,null,false); IN deletedSubtreeRoot=null; deletedSubtreeRoot=this.hook674(idKey,mainKey,in,deletedSubtreeRoot); if (deletedSubtreeRoot != null) { EnvironmentImpl envImpl=database.getDbEnvironment(); accountForSubtreeRemoval(envImpl.getInMemoryINs(),deletedSubtreeRoot,tracker); } }

	 private IN deleteDupSubtree( byte[] idKey, BIN bin, int index) throws DatabaseException, NodeNotEmptyException, CursorsExistException { EnvironmentImpl envImpl=database.getDbEnvironment(); boolean dupCountLNLocked=false; DupCountLN dcl=null; BasicLocker locker=new BasicLocker(envImpl); DIN duplicateRoot=(DIN)bin.fetchTarget(index); duplicateRoot.latch(false); ArrayList nodeLadder=new ArrayList(); IN subtreeRootIN=null; try { ChildReference dclRef=duplicateRoot.getDupCountLNRef(); dcl=(DupCountLN)dclRef.fetchTarget(database,duplicateRoot); LockResult lockResult=locker.nonBlockingLock(dcl.getNodeId(),LockType.READ,database); if (lockResult.getLockGrant() == LockGrantType.DENIED) { throw CursorsExistException.CURSORS_EXIST; } else { dupCountLNLocked=true; } searchDeletableSubTree(duplicateRoot,idKey,nodeLadder); LogManager logManager=envImpl.getLogManager(); if (nodeLadder.size() == 0) { if (bin.nCursors() == 0) { boolean deleteOk=bin.deleteEntry(index,true); assert deleteOk; logManager.log(new INDupDeleteInfo(duplicateRoot.getNodeId(),duplicateRoot.getMainTreeKey(),duplicateRoot.getDupTreeKey(),database.getId())); subtreeRootIN=duplicateRoot; this.hook754(bin); } else { throw CursorsExistException.CURSORS_EXIST; } } else { SplitInfo detachPoint=(SplitInfo)nodeLadder.get(nodeLadder.size() - 1); boolean deleteOk=detachPoint.parent.deleteEntry(detachPoint.index,true); assert deleteOk; cascadeUpdates(nodeLadder,bin,index); subtreeRootIN=detachPoint.child; } } finally { this.hook676(nodeLadder); if (dupCountLNLocked) { locker.releaseLock(dcl.getNodeId()); } this.hook675(duplicateRoot); } return subtreeRootIN; }

	 public IN getFirstNode() throws DatabaseException { return search(null,SearchType.LEFT,-1,null,true); }

	 public IN getLastNode() throws DatabaseException { return search(null,SearchType.RIGHT,-1,null,true); }

	 public DBIN getFirstNode( DIN dupRoot) throws DatabaseException { if (dupRoot == null) { throw new IllegalArgumentException("getFirstNode passed null root"); } this.hook677(dupRoot); IN ret=searchSubTree(dupRoot,null,SearchType.LEFT,-1,null,true); return (DBIN)ret; }

	 public DBIN getLastNode( DIN dupRoot) throws DatabaseException { if (dupRoot == null) { throw new IllegalArgumentException("getLastNode passed null root"); } this.hook678(dupRoot); IN ret=searchSubTree(dupRoot,null,SearchType.RIGHT,-1,null,true); return (DBIN)ret; }

	 public SearchResult getParentINForChildIN( IN child, boolean requireExactMatch, boolean updateGeneration) throws DatabaseException { return getParentINForChildIN(child,requireExactMatch,updateGeneration,-1,null); }

	 public SearchResult getParentINForChildIN( IN child, boolean requireExactMatch, boolean updateGeneration, int targetLevel, List trackingList) throws DatabaseException { if (child == null) { throw new IllegalArgumentException("getParentNode passed null"); } this.hook680(child); byte[] mainTreeKey=child.getMainTreeKey(); byte[] dupTreeKey=child.getDupTreeKey(); boolean isRoot=child.isRoot(); this.hook679(child); return getParentINForChildIN(child.getNodeId(),child.containsDuplicates(),isRoot,mainTreeKey,dupTreeKey,requireExactMatch,updateGeneration,targetLevel,trackingList,true); }

	 public SearchResult getParentINForChildIN( long targetNodeId, boolean targetContainsDuplicates, boolean targetIsRoot, byte[] targetMainTreeKey, byte[] targetDupTreeKey, boolean requireExactMatch, boolean updateGeneration, int targetLevel, List trackingList, boolean doFetch) throws DatabaseException { IN rootIN=getRootIN(updateGeneration); SearchResult result=new SearchResult(); if (rootIN != null) { if (trackingList != null) { trackingList.add(new TrackingInfo(root.getLsn(),rootIN.getNodeId())); } IN potentialParent=rootIN; try { while (result.keepSearching) { assert TestHookExecute.doHookIfSet(searchHook); potentialParent.findParent(SearchType.NORMAL,targetNodeId,targetContainsDuplicates,targetIsRoot,targetMainTreeKey,targetDupTreeKey,result,requireExactMatch,updateGeneration,targetLevel,trackingList,doFetch); potentialParent=result.parent; } } catch ( Exception e) { this.hook681(potentialParent); throw new DatabaseException(e); } } return result; }

	 public boolean getParentBINForChildLN( TreeLocation location, byte[] mainKey, byte[] dupKey, LN ln, boolean splitsAllowed, boolean findDeletedEntries, boolean searchDupTree, boolean updateGeneration) throws DatabaseException { try { IN searchResult=null; try { if (splitsAllowed) { searchResult=searchSplitsAllowed(mainKey,-1,updateGeneration); } else { searchResult=search(mainKey,SearchType.NORMAL,-1,null,updateGeneration); } location.bin=(BIN)searchResult; } catch ( Exception e) { StringBuffer msg=new StringBuffer(); if (searchResult != null) { this.hook682(searchResult); msg.append("searchResult=" + searchResult.getClass() + " nodeId="+ searchResult.getNodeId()+ " nEntries="+ searchResult.getNEntries()); } throw new DatabaseException(msg.toString(),e); } if (location.bin == null) { return false; } boolean exactSearch=false; boolean indicateIfExact=true; if (!findDeletedEntries) { exactSearch=true; indicateIfExact=false; } location.index=location.bin.findEntry(mainKey,indicateIfExact,exactSearch); boolean match=false; if (findDeletedEntries) { match=(location.index >= 0 && (location.index & IN.EXACT_MATCH) != 0); location.index&=~IN.EXACT_MATCH; } else { match=(location.index >= 0); } if (match) { if (!location.bin.isEntryKnownDeleted(location.index)) { if (database.getSortedDuplicates()) { Node childNode=location.bin.fetchTarget(location.index); this.hook683(location,mainKey,dupKey,ln,splitsAllowed,findDeletedEntries,searchDupTree,updateGeneration,exactSearch,indicateIfExact,childNode); } } location.childLsn=location.bin.getLsn(location.index); } else { location.lnKey=mainKey; } return match; } catch ( ReturnBoolean r) { return r.value; } }

	 private boolean searchDupTreeByNodeId( TreeLocation location, Node childNode, LN ln, boolean searchDupTree, boolean updateGeneration) throws DatabaseException { if (searchDupTree) { BIN oldBIN=location.bin; if (childNode.matchLNByNodeId(location,ln.getNodeId())) { location.index&=~IN.EXACT_MATCH; this.hook684(oldBIN); location.bin.latch(updateGeneration); return true; } else { return false; } } else { return false; } }

	 private boolean searchDupTreeForDupCountLNParent( TreeLocation location, byte[] mainKey, Node childNode) throws DatabaseException { location.lnKey=mainKey; if (childNode instanceof DIN) { DIN dupRoot=(DIN)childNode; location.childLsn=dupRoot.getDupCountLNRef().getLsn(); return true; } else { return false; } }

	 private boolean searchDupTreeForDBIN( TreeLocation location, byte[] dupKey, DIN dupRoot, LN ln, boolean findDeletedEntries, boolean indicateIfExact, boolean exactSearch, boolean splitsAllowed, boolean updateGeneration) throws DatabaseException { try { assert dupKey != null; this.hook685(location,dupKey,dupRoot,ln,findDeletedEntries,indicateIfExact,exactSearch,splitsAllowed,updateGeneration); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 public BIN getNextBin( BIN bin, boolean traverseWithinDupTree) throws DatabaseException { return getNextBinInternal(traverseWithinDupTree,bin,true); }

	 public BIN getPrevBin( BIN bin, boolean traverseWithinDupTree) throws DatabaseException { return getNextBinInternal(traverseWithinDupTree,bin,false); }

	 private BIN getNextBinInternal( boolean traverseWithinDupTree, BIN bin, boolean forward) throws DatabaseException { try { byte[] idKey=null; if (bin.getNEntries() == 0) { idKey=bin.getIdentifierKey(); } else if (forward) { idKey=bin.getKey(bin.getNEntries() - 1); } else { idKey=bin.getKey(0); } IN next=bin; this.hook687(); IN parent=null; IN nextIN=null; this.hook686(traverseWithinDupTree,forward,idKey,next,parent,nextIN); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (BIN)r.value; } }

	 private void splitRoot() throws DatabaseException { EnvironmentImpl env=database.getDbEnvironment(); LogManager logManager=env.getLogManager(); INList inMemoryINs=env.getInMemoryINs(); IN curRoot=null; curRoot=(IN)root.fetchTarget(database,null); this.hook689(curRoot); long curRootLsn=0; long logLsn=0; IN newRoot=null; this.hook688(logManager,inMemoryINs,curRoot,curRootLsn,logLsn,newRoot); treeStats.nRootSplits++; this.hook662(curRoot,curRootLsn,logLsn,newRoot); }

	 public IN search( byte[] key, SearchType searchType, long nid, BINBoundary binBoundary, boolean updateGeneration) throws DatabaseException { IN rootIN=getRootIN(true); if (rootIN != null) { return searchSubTree(rootIN,key,searchType,nid,binBoundary,updateGeneration); } else { return null; } }

	 public IN searchSplitsAllowed( byte[] key, long nid, boolean updateGeneration) throws DatabaseException { return new Tree_searchSplitsAllowed(this,key,nid,updateGeneration).execute(); }

	 public IN searchSubTree( IN parent, byte[] key, SearchType searchType, long nid, BINBoundary binBoundary, boolean updateGeneration) throws DatabaseException { if (parent == null) { return null; } if ((searchType == SearchType.LEFT || searchType == SearchType.RIGHT) && key != null) { throw new IllegalArgumentException("searchSubTree passed key and left/right search"); } this.hook690(parent); if (parent.getNodeId() == nid) { this.hook691(parent); return null; } if (binBoundary != null) { binBoundary.isLastBin=true; binBoundary.isFirstBin=true; } int index; IN child=null; TreeWalkerStatsAccumulator treeStatsAccumulator=getTreeStatsAccumulator(); try { do { if (treeStatsAccumulator != null) { parent.accumulateStats(treeStatsAccumulator); } if (parent.getNEntries() == 0) { return parent; } else if (searchType == SearchType.NORMAL) { index=parent.findEntry(key,false,false); } else if (searchType == SearchType.LEFT) { index=0; } else if (searchType == SearchType.RIGHT) { index=parent.getNEntries() - 1; } else { throw new IllegalArgumentException("Invalid value of searchType: " + searchType); } assert index >= 0; if (binBoundary != null) { if (index != parent.getNEntries() - 1) { binBoundary.isLastBin=false; } if (index != 0) { binBoundary.isFirstBin=false; } } child=(IN)parent.fetchTarget(index); child.latch(updateGeneration); if (treeStatsAccumulator != null) { child.accumulateStats(treeStatsAccumulator); } if (child.getNodeId() == nid) { this.hook693(child); return parent; } this.hook692(parent); parent=child; } while (!(parent instanceof BIN)); return child; } catch ( Throwable t) { this.hook694(parent,child); if (t instanceof DatabaseException) { throw (DatabaseException)t; } else { throw new DatabaseException(t); } } }

	 public void searchDeletableSubTree( IN parent, byte[] key, ArrayList nodeLadder) throws DatabaseException, NodeNotEmptyException, CursorsExistException { assert (parent != null); assert (key != null); this.hook695(parent); int index; IN child=null; IN lowestMultipleEntryIN=null; do { if (parent.getNEntries() == 0) { break; } if (parent.getNEntries() > 1) { lowestMultipleEntryIN=parent; } index=parent.findEntry(key,false,false); assert index >= 0; child=(IN)parent.fetchTarget(index); child.latch(false); nodeLadder.add(new SplitInfo(parent,child,index)); parent=child; } while (!(parent instanceof BIN)); if ((child != null) && (child instanceof BIN)) { if (child.getNEntries() != 0) { throw NodeNotEmptyException.NODE_NOT_EMPTY; } if (((BIN)child).nCursors() > 0) { throw CursorsExistException.CURSORS_EXIST; } } if (lowestMultipleEntryIN != null) { ListIterator iter=nodeLadder.listIterator(nodeLadder.size()); while (iter.hasPrevious()) { SplitInfo info5=(SplitInfo)iter.previous(); if (info5.parent == lowestMultipleEntryIN) { break; } else { this.hook696(info5); iter.remove(); } } } else { this.hook697(nodeLadder); nodeLadder.clear(); } }

	 private IN searchSubTreeSplitsAllowed( IN parent, byte[] key, long nid, boolean updateGeneration) throws DatabaseException, SplitRequiredException { if (parent != null) { while (true) { try { return searchSubTreeUntilSplit(parent,key,nid,updateGeneration); } catch ( SplitRequiredException e) { if (waitHook != null) { waitHook.doHook(); } forceSplit(parent,key); } } } else { return null; } }

	 private IN searchSubTreeUntilSplit( IN parent, byte[] key, long nid, boolean updateGeneration) throws DatabaseException, SplitRequiredException { try { if (parent == null) { return null; } this.hook699(parent); if (parent.getNodeId() == nid) { this.hook700(parent); return null; } int index=0; IN child=null; this.hook698(parent,key,nid,updateGeneration,index,child); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (IN)r.value; } }

	 private void forceSplit( IN parent, byte[] key) throws DatabaseException, SplitRequiredException { new Tree_forceSplit(this,parent,key).execute(); }

	 public IN getRootIN( boolean updateGeneration) throws DatabaseException { try { this.hook702(); IN rootIN=null; this.hook701(updateGeneration,rootIN); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (IN)r.value; } }

	 public boolean insert( LN ln, byte[] key, boolean allowDuplicates, CursorImpl cursor, LockResult lnLock) throws DatabaseException { try { validateInsertArgs(allowDuplicates); EnvironmentImpl env=database.getDbEnvironment(); LogManager logManager=env.getLogManager(); INList inMemoryINs=env.getInMemoryINs(); BIN bin=null; this.hook703(ln,key,allowDuplicates,cursor,lnLock,env,logManager,inMemoryINs,bin); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 private boolean insertDuplicate( byte[] key, BIN bin, LN newLN, LogManager logManager, INList inMemoryINs, CursorImpl cursor, LockResult lnLock, boolean allowDuplicates) throws DatabaseException { try { EnvironmentImpl env=database.getDbEnvironment(); int index=cursor.getIndex(); boolean successfulInsert=false; DIN dupRoot=null; Node n=bin.fetchTarget(index); long binNid=bin.getNodeId(); if (n instanceof DIN) { DBIN dupBin=null; this.hook704(key,bin,newLN,cursor,lnLock,allowDuplicates,env,index,successfulInsert,dupRoot,n,binNid,dupBin); } else if (n instanceof LN) { if (!allowDuplicates) { return false; } try { lnLock.setAbortLsn(DbLsn.NULL_LSN,true,true); dupRoot=createDuplicateTree(key,logManager,inMemoryINs,newLN,cursor); } finally { if (dupRoot != null) { this.hook705(dupRoot); successfulInsert=true; } else { successfulInsert=false; } } } else { throw new InconsistentNodeException("neither LN or DIN found in BIN"); } return successfulInsert; } catch ( ReturnBoolean r) { return r.value; } }

	 private boolean maybeSplitDuplicateRoot( BIN bin, int index) throws DatabaseException { DIN curRoot=(DIN)bin.fetchTarget(index); if (curRoot.needsSplitting()) { EnvironmentImpl env=database.getDbEnvironment(); LogManager logManager=env.getLogManager(); INList inMemoryINs=env.getInMemoryINs(); byte[] rootIdKey=curRoot.getKey(0); DIN newRoot=new DIN(database,rootIdKey,maxDupTreeEntriesPerNode,curRoot.getDupKey(),curRoot.getDupCountLNRef(),curRoot.getLevel() + 1); this.hook707(newRoot); long curRootLsn=0; long logLsn=0; this.hook706(bin,index,curRoot,logManager,inMemoryINs,rootIdKey,newRoot,curRootLsn,logLsn); this.hook663(curRoot,newRoot,curRootLsn,logLsn); return true; } else { return false; } }

	 private DIN createDuplicateTree( byte[] key, LogManager logManager, INList inMemoryINs, LN newLN, CursorImpl cursor) throws DatabaseException { EnvironmentImpl env=database.getDbEnvironment(); DIN dupRoot=null; DBIN dupBin=null; BIN bin=cursor.getBIN(); int index=cursor.getIndex(); LN existingLN=(LN)bin.fetchTarget(index); boolean existingLNIsDeleted=bin.isEntryKnownDeleted(index) || existingLN.isDeleted(); assert existingLN != null; byte[] existingKey=existingLN.getData(); byte[] newLNKey=newLN.getData(); boolean keysEqual=Key.compareKeys(newLNKey,existingKey,database.getDuplicateComparator()) == 0; if (keysEqual) { return null; } Locker locker=cursor.getLocker(); long nodeId=existingLN.getNodeId(); int startingCount=(locker.createdNode(nodeId) || existingLNIsDeleted || locker.getWriteLockInfo(nodeId).getAbortKnownDeleted()) ? 0 : 1; DupCountLN dupCountLN=new DupCountLN(startingCount); long firstDupCountLNLsn=dupCountLN.logProvisional(env,database.getId(),key,DbLsn.NULL_LSN); dupRoot=new DIN(database,existingKey,maxDupTreeEntriesPerNode,key,new ChildReference(dupCountLN,key,firstDupCountLNLsn),2); this.hook710(dupRoot); dupRoot.setIsRoot(true); dupBin=new DBIN(database,existingKey,maxDupTreeEntriesPerNode,key,1); this.hook709(dupBin); ChildReference newExistingLNRef=new ChildReference(existingLN,existingKey,bin.getLsn(index),bin.getState(index)); boolean insertOk=dupBin.insertEntry(newExistingLNRef); assert insertOk; this.hook708(key,logManager,inMemoryINs,newLN,cursor,env,dupRoot,dupBin,bin,index,existingLN,newLNKey,locker,dupCountLN,firstDupCountLNLsn); return dupRoot; }

	 private void validateInsertArgs( boolean allowDuplicates) throws DatabaseException { if (allowDuplicates && !database.getSortedDuplicates()) { throw new DatabaseException("allowDuplicates passed to insert but database doesn't " + "have allow duplicates set."); } }

	 private BIN findBinForInsert( byte[] key, LogManager logManager, INList inMemoryINs, CursorImpl cursor) throws DatabaseException { BIN bin; bin=cursor.latchBIN(); if (bin != null) { if (!bin.needsSplitting() && bin.isKeyInBounds(key)) { return bin; } else { this.hook712(bin); } } boolean rootLatchIsHeld=false; bin=this.hook711(key,logManager,inMemoryINs,bin,rootLatchIsHeld); if (ckptHook != null) { ckptHook.doHook(); } return bin; }

	 private void accountForSubtreeRemoval( INList inList, IN subtreeRoot, UtilizationTracker tracker) throws DatabaseException { this.hook713(inList,subtreeRoot,tracker); this.hook665(subtreeRoot); }

	 public int getLogSize(){ int size=LogUtils.getBooleanLogSize(); if (root != null) { size+=root.getLogSize(); } return size; }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeBoolean(logBuffer,(root != null)); if (root != null) { root.writeToLog(logBuffer); } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion){ boolean rootExists=LogUtils.readBoolean(itemBuffer); if (rootExists) { root=makeRootChildReference(); root.readFromLog(itemBuffer,entryTypeVersion); } }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<root>"); if (root != null) { root.dumpLog(sb,verbose); } sb.append("</root>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }

	 public void rebuildINList() throws DatabaseException { INList inMemoryList=database.getDbEnvironment().getInMemoryINs(); if (root != null) { this.hook714(inMemoryList); } }

	 public void dump() throws DatabaseException { System.out.println(dumpString(0)); }

	 public String dumpString( int nSpaces) throws DatabaseException { StringBuffer sb=new StringBuffer(); sb.append(TreeUtils.indent(nSpaces)); sb.append("<tree>"); sb.append('\n'); if (root != null) { sb.append(DbLsn.dumpString(root.getLsn(),nSpaces)); sb.append('\n'); IN rootIN=(IN)root.getTarget(); if (rootIN == null) { sb.append("<in/>"); } else { sb.append(rootIN.toString()); } sb.append('\n'); } sb.append(TreeUtils.indent(nSpaces)); sb.append("</tree>"); return sb.toString(); }

	 boolean validateDelete( int index) throws DatabaseException { try { this.hook715(index); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 public void validateINList( IN parent) throws DatabaseException { if (parent == null) { parent=(IN)root.getTarget(); } if (parent != null) { INList inList=database.getDbEnvironment().getInMemoryINs(); if (!inList.getINs().contains(parent)) { throw new DatabaseException("IN " + parent.getNodeId() + " missing from INList"); } for (int i=0; ; i+=1) { try { Node node=parent.getTarget(i); if (i >= parent.getNEntries()) { if (node != null) { throw new DatabaseException("IN " + parent.getNodeId() + " has stray node "+ node.getNodeId()+ " at index "+ i); } byte[] key=parent.getKey(i); if (key != null) { throw new DatabaseException("IN " + parent.getNodeId() + " has stray key "+ key+ " at index "+ i); } } if (node instanceof IN) { validateINList((IN)node); } } catch ( ArrayIndexOutOfBoundsException e) { break; } } } }

	 public void setWaitHook( TestHook hook){ waitHook=hook; }

	 public void setSearchHook( TestHook hook){ searchHook=hook; }

	 public void setCkptHook( TestHook hook){ ckptHook=hook; }

	
static private  class  SplitInfo {
		 IN parent;

		 IN child;

		 int index;

		 SplitInfo( IN parent, IN child, int index){ this.parent=parent; this.child=child; this.index=index; }


	}

	
@MethodObject static  class  Tree_searchSplitsAllowed {
		 Tree_searchSplitsAllowed( Tree _this, byte[] key, long nid, boolean updateGeneration){ this._this=_this; this.key=key; this.nid=nid; this.updateGeneration=updateGeneration; }

		 IN execute() throws DatabaseException { insertTarget=null; while (insertTarget == null) { this.hook717(); rootIN=null; this.hook716(); if (rootIN == null) { break; } try { insertTarget=_this.searchSubTreeSplitsAllowed(rootIN,key,nid,updateGeneration); } catch ( SplitRequiredException e) { continue; } } return insertTarget; }

		 protected Tree _this;

		 protected byte[] key;

		 protected long nid;

		 protected boolean updateGeneration;

		 protected IN insertTarget;

		 protected boolean rootLatched;

		 protected boolean rootLatchedExclusive;

		 protected IN rootIN;

		 protected boolean b;

		 protected EnvironmentImpl env;

		 protected void hook716() throws DatabaseException { while (true) { if (_this.root != null) { rootIN=(IN)_this.root.fetchTarget(_this.database,null); if (rootIN.needsSplitting()) { b=true; this.hook721(); if (b) continue; this.hook720(); env=_this.database.getDbEnvironment(); env.getDbMapTree().modifyDbRoot(_this.database); this.hook719(); rootIN=(IN)_this.root.fetchTarget(_this.database,null); } this.hook718(); } break; } }

		 protected void hook717() throws DatabaseException { }

		 protected void hook718() throws DatabaseException { }

		 protected void hook719() throws DatabaseException { }

		 protected void hook720() throws DatabaseException { }

		 protected void hook721() throws DatabaseException { }


	}

	
@MethodObject static  class  Tree_forceSplit {
		 Tree_forceSplit( Tree _this, IN parent, byte[] key){ this._this=_this; this.parent=parent; this.key=key; }

		 void execute() throws DatabaseException, SplitRequiredException { nodeLadder=new ArrayList(); allLeftSideDescent=true; allRightSideDescent=true;
{ } child=null; origParent=parent; iter=null; this.hook722(); success=false; try { this.hook723(); if (origParent.needsSplitting() || !origParent.isRoot()) { throw _this.splitRequiredException; } do { if (parent.getNEntries() == 0) { break; } else { index=parent.findEntry(key,false,false); if (index != 0) { allLeftSideDescent=false; } if (index != (parent.getNEntries() - 1)) { allRightSideDescent=false; } } assert index >= 0; child=(IN)parent.getTarget(index); if (child == null) { break; } else { this.hook724(); nodeLadder.add(new SplitInfo(parent,child,index)); } parent=child; } while (!(parent instanceof BIN)); startedSplits=false; logManager=_this.database.getDbEnvironment().getLogManager(); iter=nodeLadder.listIterator(nodeLadder.size()); lastParentForSplit=-1; while (iter.hasPrevious()) { info1=(SplitInfo)iter.previous(); child=info1.child; parent=info1.parent; index=info1.index; if (child.needsSplitting()) { maxEntriesPerNode=(child.containsDuplicates() ? _this.maxDupTreeEntriesPerNode : _this.maxMainTreeEntriesPerNode); if (allLeftSideDescent || allRightSideDescent) { child.splitSpecial(parent,index,maxEntriesPerNode,key,allLeftSideDescent); } else { child.split(parent,index,maxEntriesPerNode); } lastParentForSplit=parent.getNodeId(); startedSplits=true; if (parent.isDbRoot()) { this.hook726(); _this.root.setLsn(parent.getLastFullVersion()); parent.setDirty(true); } } else { if (startedSplits) { newLsn=0; if (lastParentForSplit == child.getNodeId()) { newLsn=child.getLastFullVersion(); } else { newLsn=child.log(logManager); } parent.updateEntry(index,newLsn); } } this.hook725(); child=null; iter.remove(); } success=true; } finally { this.hook727(); } }

		 protected Tree _this;

		 protected IN parent;

		 protected byte[] key;

		 protected ArrayList nodeLadder;

		 protected boolean allLeftSideDescent;

		 protected boolean allRightSideDescent;

		 protected int index;

		 protected IN child;

		 protected IN origParent;

		 protected ListIterator iter;

		 protected boolean isRootLatched;

		 protected boolean success;

		 protected boolean startedSplits;

		 protected LogManager logManager;

		 protected long lastParentForSplit;

		 protected SplitInfo info1;

		 protected int maxEntriesPerNode;

		 protected long newLsn;

		 protected SplitInfo info2;

		 protected void hook722() throws DatabaseException, SplitRequiredException { }

		 protected void hook723() throws DatabaseException, SplitRequiredException { }

		 protected void hook724() throws DatabaseException, SplitRequiredException { }

		 protected void hook725() throws DatabaseException, SplitRequiredException { }

		 protected void hook726() throws DatabaseException, SplitRequiredException { }

		 protected void hook727() throws DatabaseException, SplitRequiredException { }


	}

	 protected void hook657( LN ln, EnvironmentImpl env, BIN bin, int index, long newLsn) throws DatabaseException { }

	 protected void hook658( LN ln, EnvironmentImpl env, BIN bin, int index, long newLsn) throws DatabaseException { }

	 protected void hook659( LN newLN, long binNid, DBIN dupBin, long newLsn) throws DatabaseException { }

	 protected void hook660( LN newLN, long binNid, DBIN dupBin, long newLsn) throws DatabaseException { }

	 protected void hook661() throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook662( IN curRoot, long curRootLsn, long logLsn, IN newRoot) throws DatabaseException { }

	 protected void hook663( DIN curRoot, DIN newRoot, long curRootLsn, long logLsn) throws DatabaseException { }

	 protected void hook664( LN newLN, DIN dupRoot, DBIN dupBin, BIN bin, LN existingLN, DupCountLN dupCountLN, long dbinLsn, long dinLsn, long dupCountLsn, long newLsn) throws DatabaseException { }

	 protected void hook665( IN subtreeRoot) throws DatabaseException { }

	 protected void hook670( WithRootLatched wrl) throws DatabaseException { this.hook728(); throw new ReturnObject(wrl.doWork(root)); }

	 protected void hook671( WithRootLatched wrl) throws DatabaseException { this.hook729(); throw new ReturnObject(wrl.doWork(root)); }

	 protected void hook672( byte[] idKey, UtilizationTracker tracker, IN subtreeRootIN, ArrayList nodeLadder, IN rootIN, boolean rootNeedsUpdating) throws DatabaseException, NodeNotEmptyException, CursorsExistException { if (root == null) { throw new ReturnVoid(); } rootIN=(IN)root.fetchTarget(database,null); rootIN.latch(false); searchDeletableSubTree(rootIN,idKey,nodeLadder); if (nodeLadder.size() == 0) { if (purgeRoot) { subtreeRootIN=logTreeRemoval(rootIN,tracker); if (subtreeRootIN != null) { rootNeedsUpdating=true; } } } else { SplitInfo detachPoint=(SplitInfo)nodeLadder.get(nodeLadder.size() - 1); boolean deleteOk=detachPoint.parent.deleteEntry(detachPoint.index,true); assert deleteOk; rootNeedsUpdating=cascadeUpdates(nodeLadder,null,-1); subtreeRootIN=detachPoint.child; } }

	 protected void hook673() throws DatabaseException { }

	 protected IN hook674( byte[] idKey, byte[] mainKey, IN in, IN deletedSubtreeRoot) throws DatabaseException, NodeNotEmptyException, CursorsExistException { this.hook730(in); assert in instanceof BIN; assert in.getNEntries() > 0; int index=in.findEntry(mainKey,false,true); if (index >= 0) { deletedSubtreeRoot=deleteDupSubtree(idKey,(BIN)in,index); } return deletedSubtreeRoot; }

	 protected void hook675( DIN duplicateRoot) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook676( ArrayList nodeLadder) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook677( DIN dupRoot) throws DatabaseException { }

	 protected void hook678( DIN dupRoot) throws DatabaseException { }

	 protected void hook679( IN child) throws DatabaseException { }

	 protected void hook680( IN child) throws DatabaseException { }

	 protected void hook681( IN potentialParent) throws DatabaseException { }

	 protected void hook682( IN searchResult) throws DatabaseException { }

	 protected void hook683( TreeLocation location, byte[] mainKey, byte[] dupKey, LN ln, boolean splitsAllowed, boolean findDeletedEntries, boolean searchDupTree, boolean updateGeneration, boolean exactSearch, boolean indicateIfExact, Node childNode) throws DatabaseException { if (childNode == null) { } else if (ln.containsDuplicates()) { throw new ReturnBoolean(searchDupTreeForDupCountLNParent(location,mainKey,childNode)); } else { if (childNode.containsDuplicates()) { if (dupKey == null) { throw new ReturnBoolean(searchDupTreeByNodeId(location,childNode,ln,searchDupTree,updateGeneration)); } else { throw new ReturnBoolean(searchDupTreeForDBIN(location,dupKey,(DIN)childNode,ln,findDeletedEntries,indicateIfExact,exactSearch,splitsAllowed,updateGeneration)); } } } }

	 protected void hook684( BIN oldBIN) throws DatabaseException { }

	 protected void hook685( TreeLocation location, byte[] dupKey, DIN dupRoot, LN ln, boolean findDeletedEntries, boolean indicateIfExact, boolean exactSearch, boolean splitsAllowed, boolean updateGeneration) throws DatabaseException { if (maybeSplitDuplicateRoot(location.bin,location.index)) { dupRoot=(DIN)location.bin.fetchTarget(location.index); } this.hook731(location); location.lnKey=dupKey; if (splitsAllowed) { try { location.bin=(BIN)searchSubTreeSplitsAllowed(dupRoot,location.lnKey,ln.getNodeId(),updateGeneration); } catch ( SplitRequiredException e) { throw new DatabaseException(e); } } else { location.bin=(BIN)searchSubTree(dupRoot,location.lnKey,SearchType.NORMAL,ln.getNodeId(),null,updateGeneration); } location.index=location.bin.findEntry(location.lnKey,indicateIfExact,exactSearch); boolean match; if (findDeletedEntries) { match=(location.index >= 0 && (location.index & IN.EXACT_MATCH) != 0); location.index&=~IN.EXACT_MATCH; } else { match=(location.index >= 0); } if (match) { location.childLsn=location.bin.getLsn(location.index); throw new ReturnBoolean(true); } else { throw new ReturnBoolean(false); } }

	 protected void hook686( boolean traverseWithinDupTree, boolean forward, byte[] idKey, IN next, IN parent, IN nextIN) throws DatabaseException { while (true) { SearchResult result=null; if (!traverseWithinDupTree) { result=getParentINForChildIN(next,true,true); if (result.exactParentFound) { parent=result.parent; } else { this.hook733(); throw new ReturnObject(null); } } else { if (next.isRoot()) { this.hook734(next); throw new ReturnObject(null); } else { result=getParentINForChildIN(next,true,true); if (result.exactParentFound) { parent=result.parent; } else { throw new ReturnObject(null); } } } this.hook732(); int index=parent.findEntry(idKey,false,false); boolean moreEntriesThisBin=false; if (forward) { index++; if (index < parent.getNEntries()) { moreEntriesThisBin=true; } } else { if (index > 0) { moreEntriesThisBin=true; } index--; } if (moreEntriesThisBin) { nextIN=(IN)parent.fetchTarget(index); this.hook735(nextIN); if (nextIN instanceof BIN) { this.hook736(parent); TreeWalkerStatsAccumulator treeStatsAccumulator=getTreeStatsAccumulator(); if (treeStatsAccumulator != null) { nextIN.accumulateStats(treeStatsAccumulator); } throw new ReturnObject((BIN)nextIN); } else { IN ret=searchSubTree(nextIN,null,(forward ? SearchType.LEFT : SearchType.RIGHT),-1,null,true); this.hook737(parent); if (ret instanceof BIN) { throw new ReturnObject((BIN)ret); } else { throw new InconsistentNodeException("subtree did not have a BIN for leaf"); } } } next=parent; } }

	 protected void hook687() throws DatabaseException { }

	 protected void hook688( LogManager logManager, INList inMemoryINs, IN curRoot, long curRootLsn, long logLsn, IN newRoot) throws DatabaseException { byte[] rootIdKey=curRoot.getKey(0); newRoot=new IN(database,rootIdKey,maxMainTreeEntriesPerNode,curRoot.getLevel() + 1); newRoot.setIsRoot(true); curRoot.setIsRoot(false); try { curRootLsn=curRoot.logProvisional(logManager,newRoot); boolean insertOk=newRoot.insertEntry(new ChildReference(curRoot,rootIdKey,curRootLsn)); assert insertOk; logLsn=newRoot.log(logManager); } catch ( DatabaseException e) { curRoot.setIsRoot(true); throw e; } inMemoryINs.add(newRoot); root.setTarget(newRoot); root.setLsn(logLsn); curRoot.split(newRoot,0,maxMainTreeEntriesPerNode); root.setLsn(newRoot.getLastFullVersion()); }

	 protected void hook689( IN curRoot) throws DatabaseException { }

	 protected void hook690( IN parent) throws DatabaseException { }

	 protected void hook691( IN parent) throws DatabaseException { }

	 protected void hook692( IN parent) throws DatabaseException, Throwable { }

	 protected void hook693( IN child) throws DatabaseException, Throwable { }

	 protected void hook694( IN parent, IN child) throws DatabaseException { }

	 protected void hook695( IN parent) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook696( SplitInfo info5) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook697( ArrayList nodeLadder) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook698( IN parent, byte[] key, long nid, boolean updateGeneration, int index, IN child) throws DatabaseException, SplitRequiredException { do { if (parent.getNEntries() == 0) { throw new ReturnObject(parent); } else { index=parent.findEntry(key,false,false); } assert index >= 0; child=(IN)parent.fetchTarget(index); child.latch(updateGeneration); if (child.needsSplitting()) { this.hook739(parent,child); throw splitRequiredException; } if (child.getNodeId() == nid) { this.hook740(child); throw new ReturnObject(parent); } this.hook738(parent); parent=child; } while (!(parent instanceof BIN)); throw new ReturnObject(parent); }

	 protected void hook699( IN parent) throws DatabaseException, SplitRequiredException { }

	 protected void hook700( IN parent) throws DatabaseException, SplitRequiredException { }

	 protected void hook701( boolean updateGeneration, IN rootIN) throws DatabaseException { if (root != null) { rootIN=(IN)root.fetchTarget(database,null); rootIN.latch(updateGeneration); } throw new ReturnObject(rootIN); }

	 protected void hook702() throws DatabaseException { }

	 protected void hook703( LN ln, byte[] key, boolean allowDuplicates, CursorImpl cursor, LockResult lnLock, EnvironmentImpl env, LogManager logManager, INList inMemoryINs, BIN bin) throws DatabaseException { bin=findBinForInsert(key,logManager,inMemoryINs,cursor); this.hook741(bin); ChildReference newLNRef=new ChildReference(ln,key,DbLsn.NULL_LSN); cursor.setBIN(bin); int index=bin.insertEntry1(newLNRef); if ((index & IN.INSERT_SUCCESS) != 0) { index&=~IN.INSERT_SUCCESS; cursor.updateBin(bin,index); long newLsn=DbLsn.NULL_LSN; try { newLsn=ln.log(env,database.getId(),key,DbLsn.NULL_LSN,cursor.getLocker()); } finally { if (newLsn == DbLsn.NULL_LSN) { bin.setKnownDeleted(index); } } lnLock.setAbortLsn(DbLsn.NULL_LSN,true,true); bin.updateEntry(index,newLsn); this.hook657(ln,env,bin,index,newLsn); throw new ReturnBoolean(true); } else { index&=~IN.EXACT_MATCH; cursor.updateBin(bin,index); LN currentLN=null; boolean isDup=false; Node n=bin.fetchTarget(index); if (n == null || n instanceof LN) { currentLN=(LN)n; } else { isDup=true; } boolean isDeleted=false; LockResult currentLock=null; if (!isDup) { if (currentLN == null) { isDeleted=true; } else { currentLock=cursor.lockLNDeletedAllowed(currentLN,LockType.WRITE); currentLN=currentLock.getLN(); bin=cursor.getBIN(); index=cursor.getIndex(); if (cursor.getDupBIN() != null) { cursor.clearDupBIN(true); isDup=true; } else if (bin.isEntryKnownDeleted(index) || currentLN == null || currentLN.isDeleted()) { isDeleted=true; } } } if (isDeleted) { long abortLsn=bin.getLsn(index); boolean abortKnownDeleted=true; if (currentLN != null && currentLock.getLockGrant() == LockGrantType.EXISTING) { long nodeId=currentLN.getNodeId(); Locker locker=cursor.getLocker(); WriteLockInfo info6=locker.getWriteLockInfo(nodeId); abortLsn=info6.getAbortLsn(); abortKnownDeleted=info6.getAbortKnownDeleted(); } lnLock.setAbortLsn(abortLsn,abortKnownDeleted); long newLsn=ln.log(env,database.getId(),key,DbLsn.NULL_LSN,cursor.getLocker()); bin.updateEntry(index,ln,newLsn,key); bin.clearKnownDeleted(index); bin.clearPendingDeleted(index); this.hook658(ln,env,bin,index,newLsn); throw new ReturnBoolean(true); } else { throw new ReturnBoolean(insertDuplicate(key,bin,ln,logManager,inMemoryINs,cursor,lnLock,allowDuplicates)); } } }

	 protected void hook704( byte[] key, BIN bin, LN newLN, CursorImpl cursor, LockResult lnLock, boolean allowDuplicates, EnvironmentImpl env, int index, boolean successfulInsert, DIN dupRoot, Node n, long binNid, DBIN dupBin) throws DatabaseException { dupRoot=(DIN)n; this.hook744(dupRoot); LockResult dclLockResult=cursor.lockDupCountLN(dupRoot,LockType.WRITE); bin=cursor.getBIN(); index=cursor.getIndex(); if (!allowDuplicates) { DupCountLN dcl=(DupCountLN)dclLockResult.getLN(); if (dcl.getDupCount() > 0) { throw new ReturnBoolean(false); } } maybeSplitDuplicateRoot(bin,index); dupRoot=(DIN)bin.fetchTarget(index); byte[] newLNKey=newLN.getData(); long previousLsn=dupRoot.getLastFullVersion(); try { dupBin=(DBIN)searchSubTreeSplitsAllowed(dupRoot,newLNKey,-1,true); } catch ( SplitRequiredException e) { throw new DatabaseException(e); } long currentLsn=dupRoot.getLastFullVersion(); if (currentLsn != previousLsn) { bin.updateEntry(index,currentLsn); } this.hook743(cursor); bin=null; dupRoot=null; ChildReference newLNRef=new ChildReference(newLN,newLNKey,DbLsn.NULL_LSN); int dupIndex=dupBin.insertEntry1(newLNRef); if ((dupIndex & IN.INSERT_SUCCESS) != 0) { dupIndex&=~IN.INSERT_SUCCESS; cursor.updateDBin(dupBin,dupIndex); long newLsn=DbLsn.NULL_LSN; try { newLsn=newLN.log(env,database.getId(),key,DbLsn.NULL_LSN,cursor.getLocker()); } finally { if (newLsn == DbLsn.NULL_LSN) { dupBin.setKnownDeleted(dupIndex); } } lnLock.setAbortLsn(DbLsn.NULL_LSN,true,true); dupBin.setLsn(dupIndex,newLsn); this.hook659(newLN,binNid,dupBin,newLsn); successfulInsert=true; } else { dupIndex&=~IN.EXACT_MATCH; cursor.updateDBin(dupBin,dupIndex); LN currentLN=(LN)dupBin.fetchTarget(dupIndex); boolean isDeleted=false; LockResult currentLock=null; if (currentLN == null) { isDeleted=true; } else { currentLock=cursor.lockLNDeletedAllowed(currentLN,LockType.WRITE); currentLN=currentLock.getLN(); dupBin=cursor.getDupBIN(); dupIndex=cursor.getDupIndex(); if (dupBin.isEntryKnownDeleted(dupIndex) || currentLN == null || currentLN.isDeleted()) { isDeleted=true; } } if (isDeleted) { long abortLsn=dupBin.getLsn(dupIndex); boolean abortKnownDeleted=true; if (currentLN != null && currentLock.getLockGrant() == LockGrantType.EXISTING) { long nodeId=currentLN.getNodeId(); Locker locker=cursor.getLocker(); WriteLockInfo info7=locker.getWriteLockInfo(nodeId); abortLsn=info7.getAbortLsn(); abortKnownDeleted=info7.getAbortKnownDeleted(); } lnLock.setAbortLsn(abortLsn,abortKnownDeleted); long newLsn=newLN.log(env,database.getId(),key,DbLsn.NULL_LSN,cursor.getLocker()); dupBin.updateEntry(dupIndex,newLN,newLsn,newLNKey); dupBin.clearKnownDeleted(dupIndex); dupBin.clearPendingDeleted(dupIndex); this.hook660(newLN,binNid,dupBin,newLsn); successfulInsert=true; } else { successfulInsert=false; } } this.hook742(dupBin); dupBin=null; if (successfulInsert) { this.hook746(cursor); dupRoot=cursor.getLatchedDupRoot(false); this.hook745(cursor); dupRoot.incrementDuplicateCount(dclLockResult,key,cursor.getLocker(),true); } }

	 protected void hook705( DIN dupRoot) throws DatabaseException { }

	 protected void hook706( BIN bin, int index, DIN curRoot, LogManager logManager, INList inMemoryINs, byte[] rootIdKey, DIN newRoot, long curRootLsn, long logLsn) throws DatabaseException { newRoot.setIsRoot(true); curRoot.setDupCountLN(null); curRoot.setIsRoot(false); try { curRootLsn=curRoot.logProvisional(logManager,newRoot); boolean insertOk=newRoot.insertEntry(new ChildReference(curRoot,rootIdKey,bin.getLsn(index))); assert insertOk; logLsn=newRoot.log(logManager); } catch ( DatabaseException e) { curRoot.setIsRoot(true); throw e; } inMemoryINs.add(newRoot); bin.updateEntry(index,newRoot,logLsn); curRoot.split(newRoot,0,maxDupTreeEntriesPerNode); }

	 protected void hook707( DIN newRoot) throws DatabaseException { }

	 protected void hook708( byte[] key, LogManager logManager, INList inMemoryINs, LN newLN, CursorImpl cursor, EnvironmentImpl env, DIN dupRoot, DBIN dupBin, BIN bin, int index, LN existingLN, byte[] newLNKey, Locker locker, DupCountLN dupCountLN, long firstDupCountLNLsn) throws DatabaseException { long dbinLsn=dupBin.logProvisional(logManager,dupRoot); inMemoryINs.add(dupBin); dupRoot.setEntry(0,dupBin,dupBin.getKey(0),dbinLsn,dupBin.getState(0)); long dinLsn=dupRoot.log(logManager); inMemoryINs.add(dupRoot); LockResult lockResult=locker.lock(dupCountLN.getNodeId(),LockType.WRITE,false,database); lockResult.setAbortLsn(firstDupCountLNLsn,false); dupCountLN.setDupCount(2); long dupCountLsn=dupCountLN.log(env,database.getId(),key,firstDupCountLNLsn,locker); dupRoot.updateDupCountLNRef(dupCountLsn); long newLsn=newLN.log(env,database.getId(),key,DbLsn.NULL_LSN,locker); int dupIndex=dupBin.insertEntry1(new ChildReference(newLN,newLNKey,newLsn)); dupIndex&=~IN.INSERT_SUCCESS; cursor.updateDBin(dupBin,dupIndex); bin.adjustCursorsForMutation(index,dupBin,dupIndex ^ 1,cursor); this.hook747(dupBin); bin.updateEntry(index,dupRoot,dinLsn); bin.setMigrate(index,false); this.hook664(newLN,dupRoot,dupBin,bin,existingLN,dupCountLN,dbinLsn,dinLsn,dupCountLsn,newLsn); }

	 protected void hook709( DBIN dupBin) throws DatabaseException { }

	 protected void hook710( DIN dupRoot) throws DatabaseException { }

	 protected BIN hook711( byte[] key, LogManager logManager, INList inMemoryINs, BIN bin, boolean rootLatchIsHeld) throws DatabaseException { long logLsn; while (true) { rootLatchIsHeld=true; this.hook748(); if (root == null) { this.hook751(); if (root != null) { this.hook752(); rootLatchIsHeld=false; continue; } bin=new BIN(database,key,maxMainTreeEntriesPerNode,1); this.hook750(bin); logLsn=bin.logProvisional(logManager,null); IN rootIN=new IN(database,key,maxMainTreeEntriesPerNode,2); rootIN.setIsRoot(true); boolean insertOk=rootIN.insertEntry(new ChildReference(bin,key,logLsn)); assert insertOk; logLsn=rootIN.log(logManager); rootIN.setDirty(true); root=new ChildReference(rootIN,new byte[0],logLsn); inMemoryINs.add(bin); inMemoryINs.add(rootIN); this.hook749(); rootLatchIsHeld=false; break; } else { this.hook753(); rootLatchIsHeld=false; IN in=searchSplitsAllowed(key,-1,true); if (in == null) { continue; } else { bin=(BIN)in; break; } } } return bin; }

	 protected void hook712( BIN bin) throws DatabaseException { }

	 protected void hook713( INList inList, IN subtreeRoot, UtilizationTracker tracker) throws DatabaseException { subtreeRoot.accountForSubtreeRemoval(inList,tracker); }

	 protected void hook714( INList inMemoryList) throws DatabaseException { Node rootIN=root.getTarget(); if (rootIN != null) { rootIN.rebuildINList(inMemoryList); } }

	 protected void hook715( int index) throws DatabaseException { IN rootIN=(IN)root.fetchTarget(database,null); throw new ReturnBoolean(rootIN.validateSubtreeBeforeDelete(index)); }

	 protected void hook728() throws DatabaseException { }

	 protected void hook729() throws DatabaseException { }

	 protected void hook730( IN in) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }

	 protected void hook731( TreeLocation location) throws DatabaseException { }

	 protected void hook732() throws DatabaseException { }

	 protected void hook733() throws DatabaseException { }

	 protected void hook734( IN next) throws DatabaseException { }

	 protected void hook735( IN nextIN) throws DatabaseException { }

	 protected void hook736( IN parent) throws DatabaseException { }

	 protected void hook737( IN parent) throws DatabaseException { }

	 protected void hook738( IN parent) throws DatabaseException, SplitRequiredException { }

	 protected void hook739( IN parent, IN child) throws DatabaseException, SplitRequiredException { }

	 protected void hook740( IN child) throws DatabaseException, SplitRequiredException { }

	 protected void hook741( BIN bin) throws DatabaseException { }

	 protected void hook742( DBIN dupBin) throws DatabaseException { }

	 protected void hook743( CursorImpl cursor) throws DatabaseException { }

	 protected void hook744( DIN dupRoot) throws DatabaseException { }

	 protected void hook745( CursorImpl cursor) throws DatabaseException { }

	 protected void hook746( CursorImpl cursor) throws DatabaseException { }

	 protected void hook747( DBIN dupBin) throws DatabaseException { }

	 protected void hook748() throws DatabaseException { }

	 protected void hook749() throws DatabaseException { }

	 protected void hook750( BIN bin) throws DatabaseException { }

	 protected void hook751() throws DatabaseException { }

	 protected void hook752() throws DatabaseException { }

	 protected void hook753() throws DatabaseException { }

	 protected void hook754( BIN bin) throws DatabaseException, NodeNotEmptyException, CursorsExistException { }


}
