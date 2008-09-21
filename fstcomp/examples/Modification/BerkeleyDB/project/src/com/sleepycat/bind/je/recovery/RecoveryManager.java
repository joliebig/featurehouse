package com.sleepycat.je.recovery; 
import java.io.IOException; 
import java.util.ArrayList; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.List; 
import java.util.Map; 
import java.util.Set; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.CheckpointConfig; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.TransactionConfig; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.DbTree; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.CheckpointFileReader; 
import com.sleepycat.je.log.FileManager; 
import com.sleepycat.je.log.INFileReader; 
import com.sleepycat.je.log.LNFileReader; 
import com.sleepycat.je.log.LastFileReader; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.ChildReference; 
import com.sleepycat.je.tree.DIN; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.Key; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.tree.Node; 
import com.sleepycat.je.tree.SearchResult; 
import com.sleepycat.je.tree.TrackingInfo; 
import com.sleepycat.je.tree.Tree; 
import com.sleepycat.je.tree.TreeLocation; 
import com.sleepycat.je.tree.WithRootLatched; 
import com.sleepycat.je.txn.LockType; 
import com.sleepycat.je.txn.Txn; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  RecoveryManager {
	 private static final String TRACE_DUP_ROOT_REPLACE="DupRootRecover:";

	 private static final String TRACE_LN_REDO="LNRedo:";

	 private static final String TRACE_LN_UNDO="LNUndo";

	 private static final String TRACE_IN_REPLACE="INRecover:";

	 private static final String TRACE_ROOT_REPLACE="RootRecover:";

	 private static final String TRACE_IN_DEL_REPLAY="INDelReplay:";

	 private static final String TRACE_IN_DUPDEL_REPLAY="INDupDelReplay:";

	 private static final String TRACE_ROOT_DELETE="RootDelete:";

	 private static final int CLEAR_INCREMENT=50;

	 private EnvironmentImpl env;

	 private int readBufferSize;

	 private RecoveryInfo info;

	 private Set committedTxnIds;

	 private Set abortedTxnIds;

	 private Map preparedTxns;

	 private Set inListRebuildDbIds;

	 private Level detailedTraceLevel;

	 private Map fileSummaryLsns;

	 private int inListClearCounter;

	 public RecoveryManager( EnvironmentImpl env) throws DatabaseException { this.env=env; DbConfigManager cm=env.getConfigManager(); readBufferSize=cm.getInt(EnvironmentParams.LOG_ITERATOR_READ_SIZE); committedTxnIds=new HashSet(); abortedTxnIds=new HashSet(); preparedTxns=new HashMap(); inListRebuildDbIds=new HashSet(); fileSummaryLsns=new HashMap(); this.hook578(env); }

	
private static  class  TxnNodeId {
		 long nodeId;

		 long txnId;

		 TxnNodeId( long nodeId, long txnId){ this.nodeId=nodeId; this.txnId=txnId; }

		 public boolean equals__wrappee__base( Object obj){ if (this == obj) { return true; } if (!(obj instanceof TxnNodeId)) { return false; } return ((((TxnNodeId)obj).txnId == txnId) && (((TxnNodeId)obj).nodeId == nodeId)); }

		 public boolean equals( Object obj){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public int hashCode__wrappee__base(){ return (int)(txnId + nodeId); }

		 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public String toString__wrappee__base(){ return "txnId=" + txnId + "/nodeId="+ nodeId; }

		 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
private static  class  RootDeleter  implements WithRootLatched {
		 Tree tree;

		 RootDeleter( Tree tree){ this.tree=tree; }

		 public IN doWork__wrappee__base( ChildReference root) throws DatabaseException { tree.setRoot(null,false); return null; }

		 public IN doWork( ChildReference root) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  RecoveryManager_trace {
		 RecoveryManager_trace( Level level, DatabaseImpl database, String debugType, boolean success, Node node, long logLsn, IN parent, boolean found, boolean replaced, boolean inserted, long replacedLsn, long abortLsn, int index){ this.level=level; this.database=database; this.debugType=debugType; this.success=success; this.node=node; this.logLsn=logLsn; this.parent=parent; this.found=found; this.replaced=replaced; this.inserted=inserted; this.replacedLsn=replacedLsn; this.abortLsn=abortLsn; this.index=index; }

		 protected Level level;

		 protected DatabaseImpl database;

		 protected String debugType;

		 protected boolean success;

		 protected Node node;

		 protected long logLsn;

		 protected IN parent;

		 protected boolean found;

		 protected boolean replaced;

		 protected boolean inserted;

		 protected long replacedLsn;

		 protected long abortLsn;

		 protected int index;

		 protected Logger logger;

		 protected Level useLevel;

		 protected StringBuffer sb;

		 void execute__wrappee__base(){ }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public RecoveryInfo recover__wrappee__base( boolean readOnly) throws DatabaseException { info=new RecoveryInfo(); try { FileManager fileManager=env.getFileManager(); DbConfigManager configManager=env.getConfigManager(); boolean forceCheckpoint=configManager.getBoolean(EnvironmentParams.ENV_RECOVERY_FORCE_CHECKPOINT); if (fileManager.filesExist()) { findEndOfLog(readOnly); this.hook559(); findLastCheckpoint(); env.getLogManager().setLastLsnAtRecovery(fileManager.getLastUsedLsn()); this.hook558(); env.readMapTreeFromLog(info.useRootLsn); buildTree(); } else { this.hook556(); this.hook560(); env.logMapTreeRoot(); forceCheckpoint=true; } if (preparedTxns.size() > 0) { this.hook573(); preparedTxns=null; } if (DbInternal.getCreateUP(env.getConfigManager().getEnvironmentConfig())) { env.getUtilizationProfile().populateCache(); } if (!readOnly && (env.getLogManager().getLastLsnAtRecovery() != info.checkpointEndLsn || forceCheckpoint)) { CheckpointConfig config=new CheckpointConfig(); config.setForce(true); config.setMinimizeRecoveryTime(true); env.invokeCheckpoint(config,false,"recovery"); } } catch ( IOException e) { this.hook575(e); throw new RecoveryException(env,"Couldn't recover: " + e.getMessage(),e); } finally { Tracer.trace(Level.CONFIG,env,"Recovery finished: " + info); } return info; }

	 public RecoveryInfo recover( boolean readOnly) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	recover__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void findEndOfLog__wrappee__base( boolean readOnly) throws IOException, DatabaseException { LastFileReader reader=new LastFileReader(env,readBufferSize); while (reader.readNextEntry()) { LogEntryType type=reader.getEntryType(); if (LogEntryType.LOG_CKPT_END.equals(type)) { info.checkpointEndLsn=reader.getLastLsn(); info.partialCheckpointStartLsn=DbLsn.NULL_LSN; } else if (LogEntryType.LOG_CKPT_START.equals(type)) { if (info.partialCheckpointStartLsn == DbLsn.NULL_LSN) { info.partialCheckpointStartLsn=reader.getLastLsn(); } } } assert (reader.getLastValidLsn() != reader.getEndOfLog()) : "lastUsed=" + DbLsn.getNoFormatString(reader.getLastValidLsn()) + " end="+ DbLsn.getNoFormatString(reader.getEndOfLog()); if (!readOnly) { reader.setEndOfFile(); } info.lastUsedLsn=reader.getLastValidLsn(); info.nextAvailableLsn=reader.getEndOfLog(); info.nRepeatIteratorReads+=reader.getNRepeatIteratorReads(); env.getFileManager().setLastPosition(info.nextAvailableLsn,info.lastUsedLsn,reader.getPrevOffset()); }

	 private void findEndOfLog( boolean readOnly) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	findEndOfLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void findLastCheckpoint__wrappee__base() throws IOException, DatabaseException { if (info.checkpointEndLsn == DbLsn.NULL_LSN) { CheckpointFileReader searcher=new CheckpointFileReader(env,readBufferSize,false,info.lastUsedLsn,DbLsn.NULL_LSN,info.nextAvailableLsn); while (searcher.readNextEntry()) { if (searcher.isCheckpointEnd()) { info.checkpointEndLsn=searcher.getLastLsn(); break; } else if (searcher.isCheckpointStart()) { info.partialCheckpointStartLsn=searcher.getLastLsn(); } else if (searcher.isRoot()) { if (info.useRootLsn == DbLsn.NULL_LSN) { info.useRootLsn=searcher.getLastLsn(); } } } info.nRepeatIteratorReads+=searcher.getNRepeatIteratorReads(); } if (info.checkpointEndLsn == DbLsn.NULL_LSN) { info.checkpointStartLsn=DbLsn.NULL_LSN; info.firstActiveLsn=DbLsn.NULL_LSN; } else { CheckpointEnd checkpointEnd=(CheckpointEnd)(env.getLogManager().get(info.checkpointEndLsn)); info.checkpointEnd=checkpointEnd; info.checkpointStartLsn=checkpointEnd.getCheckpointStartLsn(); info.firstActiveLsn=checkpointEnd.getFirstActiveLsn(); if (checkpointEnd.getRootLsn() != DbLsn.NULL_LSN) { info.useRootLsn=checkpointEnd.getRootLsn(); } env.getCheckpointer().setCheckpointId(checkpointEnd.getId()); env.getCheckpointer().setFirstActiveLsn(checkpointEnd.getFirstActiveLsn()); } if (info.useRootLsn == DbLsn.NULL_LSN) { throw new RecoveryException(env,"This environment's log file has no root. Since the root " + "is the first entry written into a log at environment " + "creation, this should only happen if the initial creation "+ "of the environment was never checkpointed or synced. "+ "Please move aside the existing log files to allow the "+ "creation of a new environment"); } }

	 private void findLastCheckpoint() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	findLastCheckpoint__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void buildTree__wrappee__base() throws IOException, DatabaseException { inListClearCounter=0; this.hook572(); long start=System.currentTimeMillis(); readINsAndTrackIds(info.checkpointStartLsn); long end=System.currentTimeMillis(); this.hook571(start,end); start=System.currentTimeMillis(); info.numOtherINs+=readINs(info.checkpointStartLsn,true,LogEntryType.LOG_BIN_DELTA,null,null,true); end=System.currentTimeMillis(); this.hook570(start,end); start=System.currentTimeMillis(); Set mapLNSet=new HashSet(); mapLNSet.add(LogEntryType.LOG_MAPLN_TRANSACTIONAL); mapLNSet.add(LogEntryType.LOG_TXN_COMMIT); mapLNSet.add(LogEntryType.LOG_TXN_ABORT); mapLNSet.add(LogEntryType.LOG_TXN_PREPARE); undoLNs(info,mapLNSet); end=System.currentTimeMillis(); this.hook569(start,end); start=System.currentTimeMillis(); mapLNSet.add(LogEntryType.LOG_MAPLN); redoLNs(info,mapLNSet); end=System.currentTimeMillis(); this.hook568(start,end); start=System.currentTimeMillis(); info.numOtherINs+=readINs(info.checkpointStartLsn,false,LogEntryType.LOG_IN,LogEntryType.LOG_BIN,LogEntryType.LOG_IN_DELETE_INFO,false); end=System.currentTimeMillis(); this.hook567(start,end); start=System.currentTimeMillis(); info.numBinDeltas=readINs(info.checkpointStartLsn,false,LogEntryType.LOG_BIN_DELTA,null,null,true); end=System.currentTimeMillis(); this.hook566(start,end); start=System.currentTimeMillis(); info.numDuplicateINs+=readINs(info.checkpointStartLsn,false,LogEntryType.LOG_DIN,LogEntryType.LOG_DBIN,LogEntryType.LOG_IN_DUPDELETE_INFO,true); end=System.currentTimeMillis(); this.hook565(start,end); start=System.currentTimeMillis(); info.numBinDeltas+=readINs(info.checkpointStartLsn,false,LogEntryType.LOG_DUP_BIN_DELTA,null,null,true); end=System.currentTimeMillis(); this.hook564(start,end); rebuildINList(); this.hook596(); this.hook563(); start=System.currentTimeMillis(); Set lnSet=new HashSet(); lnSet.add(LogEntryType.LOG_LN_TRANSACTIONAL); lnSet.add(LogEntryType.LOG_NAMELN_TRANSACTIONAL); lnSet.add(LogEntryType.LOG_DEL_DUPLN_TRANSACTIONAL); lnSet.add(LogEntryType.LOG_DUPCOUNTLN_TRANSACTIONAL); undoLNs(info,lnSet); end=System.currentTimeMillis(); this.hook562(start,end); start=System.currentTimeMillis(); lnSet.add(LogEntryType.LOG_LN); lnSet.add(LogEntryType.LOG_NAMELN); lnSet.add(LogEntryType.LOG_DEL_DUPLN); lnSet.add(LogEntryType.LOG_DUPCOUNTLN); lnSet.add(LogEntryType.LOG_FILESUMMARYLN); redoLNs(info,lnSet); end=System.currentTimeMillis(); this.hook561(start,end); }

	 private void buildTree() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	buildTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void readINsAndTrackIds__wrappee__base( long rollForwardLsn) throws IOException, DatabaseException { INFileReader reader=new INFileReader(env,readBufferSize,rollForwardLsn,info.nextAvailableLsn,true,false,info.partialCheckpointStartLsn,fileSummaryLsns); reader.addTargetType(LogEntryType.LOG_IN); reader.addTargetType(LogEntryType.LOG_BIN); reader.addTargetType(LogEntryType.LOG_IN_DELETE_INFO); this.hook593(reader); try { info.numMapINs=0; DbTree dbMapTree=env.getDbMapTree(); while (reader.readNextEntry()) { DatabaseId dbId=reader.getDatabaseId(); if (dbId.equals(DbTree.ID_DB_ID)) { DatabaseImpl db=dbMapTree.getDb(dbId); replayOneIN(reader,db,false); info.numMapINs++; } } info.useMaxNodeId=reader.getMaxNodeId(); info.useMaxDbId=reader.getMaxDbId(); info.useMaxTxnId=reader.getMaxTxnId(); if (info.checkpointEnd != null) { if (info.useMaxNodeId < info.checkpointEnd.getLastNodeId()) { info.useMaxNodeId=info.checkpointEnd.getLastNodeId(); } if (info.useMaxDbId < info.checkpointEnd.getLastDbId()) { info.useMaxDbId=info.checkpointEnd.getLastDbId(); } if (info.useMaxTxnId < info.checkpointEnd.getLastTxnId()) { info.useMaxTxnId=info.checkpointEnd.getLastTxnId(); } } Node.setLastNodeId(info.useMaxNodeId); env.getDbMapTree().setLastDbId(info.useMaxDbId); env.getTxnManager().setLastTxnId(info.useMaxTxnId); info.nRepeatIteratorReads+=reader.getNRepeatIteratorReads(); } catch ( Exception e) { traceAndThrowException(reader.getLastLsn(),"readMapIns",e); } }

	 private void readINsAndTrackIds( long rollForwardLsn) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readINsAndTrackIds__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private int readINs__wrappee__base( long rollForwardLsn, boolean mapDbOnly, LogEntryType inType1, LogEntryType inType2, LogEntryType inType3, boolean requireExactMatch) throws IOException, DatabaseException { INFileReader reader=new INFileReader(env,readBufferSize,rollForwardLsn,info.nextAvailableLsn,false,mapDbOnly,info.partialCheckpointStartLsn,fileSummaryLsns); if (inType1 != null) { reader.addTargetType(inType1); } if (inType2 != null) { reader.addTargetType(inType2); } if (inType3 != null) { reader.addTargetType(inType3); } int numINsSeen=0; try { DbTree dbMapTree=env.getDbMapTree(); while (reader.readNextEntry()) { DatabaseId dbId=reader.getDatabaseId(); boolean isMapDb=dbId.equals(DbTree.ID_DB_ID); boolean isTarget=false; if (mapDbOnly && isMapDb) { isTarget=true; } else if (!mapDbOnly && !isMapDb) { isTarget=true; } if (isTarget) { DatabaseImpl db=dbMapTree.getDb(dbId); if (db == null) { } else { replayOneIN(reader,db,requireExactMatch); numINsSeen++; inListRebuildDbIds.add(dbId); } } } info.nRepeatIteratorReads+=reader.getNRepeatIteratorReads(); return numINsSeen; } catch ( Exception e) { traceAndThrowException(reader.getLastLsn(),"readNonMapIns",e); return 0; } }

	 private int readINs( long rollForwardLsn, boolean mapDbOnly, LogEntryType inType1, LogEntryType inType2, LogEntryType inType3, boolean requireExactMatch) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readINs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void replayOneIN__wrappee__base( INFileReader reader, DatabaseImpl db, boolean requireExactMatch) throws DatabaseException { if (reader.isDeleteInfo()) { replayINDelete(db,reader.getDeletedNodeId(),false,reader.getDeletedIdKey(),null,reader.getLastLsn()); } else if (reader.isDupDeleteInfo()) { replayINDelete(db,reader.getDupDeletedNodeId(),true,reader.getDupDeletedMainKey(),reader.getDupDeletedDupKey(),reader.getLastLsn()); } else { IN in=reader.getIN(); long inLsn=reader.getLsnOfIN(); in.postRecoveryInit(db,inLsn); this.hook585(in); replaceOrInsert(db,in,reader.getLastLsn(),inLsn,requireExactMatch); } if ((++inListClearCounter % CLEAR_INCREMENT) == 0) { env.getInMemoryINs().clear(); } }

	 private void replayOneIN( INFileReader reader, DatabaseImpl db, boolean requireExactMatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	replayOneIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void undoLNs__wrappee__base( RecoveryInfo info, Set lnTypes) throws IOException, DatabaseException { long firstActiveLsn=info.firstActiveLsn; long lastUsedLsn=info.lastUsedLsn; long endOfFileLsn=info.nextAvailableLsn; LNFileReader reader=new LNFileReader(env,readBufferSize,lastUsedLsn,false,endOfFileLsn,firstActiveLsn,null); Iterator iter=lnTypes.iterator(); while (iter.hasNext()) { LogEntryType lnType=(LogEntryType)iter.next(); reader.addTargetType(lnType); } Map countedFileSummaries=new HashMap(); Set countedAbortLsnNodes=new HashSet(); DbTree dbMapTree=env.getDbMapTree(); TreeLocation location=new TreeLocation(); try { while (reader.readNextEntry()) { if (reader.isLN()) { Long txnId=reader.getTxnId(); if (txnId != null && !committedTxnIds.contains(txnId)) { this.hook597(); LN ln=reader.getLN(); long logLsn=reader.getLastLsn(); long abortLsn=reader.getAbortLsn(); boolean abortKnownDeleted=reader.getAbortKnownDeleted(); DatabaseId dbId=reader.getDatabaseId(); DatabaseImpl db=dbMapTree.getDb(dbId); if (db != null) { ln.postFetchInit(db,logLsn); this.hook586(info,reader,location,ln,logLsn,abortLsn,abortKnownDeleted,db); TxnNodeId txnNodeId=new TxnNodeId(reader.getNodeId(),txnId.longValue()); undoUtilizationInfo(ln,logLsn,abortLsn,abortKnownDeleted,txnNodeId,countedFileSummaries,countedAbortLsnNodes); inListRebuildDbIds.add(dbId); } } } else if (reader.isPrepare()) { long prepareId=reader.getTxnPrepareId(); Long prepareIdL=new Long(prepareId); if (!committedTxnIds.contains(prepareIdL) && !abortedTxnIds.contains(prepareIdL)) { TransactionConfig txnConf=new TransactionConfig(); Txn preparedTxn=new Txn(env,txnConf,prepareId); preparedTxn.setLockTimeout(0); preparedTxns.put(prepareIdL,preparedTxn); env.getTxnManager().registerXATxn(reader.getTxnPrepareXid(),preparedTxn,true); this.hook574(reader); } } else if (reader.isAbort()) { abortedTxnIds.add(new Long(reader.getTxnAbortId())); } else { committedTxnIds.add(new Long(reader.getTxnCommitId())); } } info.nRepeatIteratorReads+=reader.getNRepeatIteratorReads(); } catch ( Exception e) { traceAndThrowException(reader.getLastLsn(),"undoLNs",e); } }

	 private void undoLNs( RecoveryInfo info, Set lnTypes) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	undoLNs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void redoLNs__wrappee__base( RecoveryInfo info, Set lnTypes) throws IOException, DatabaseException { long endOfFileLsn=info.nextAvailableLsn; long rollForwardLsn=info.checkpointStartLsn; LNFileReader reader=new LNFileReader(env,readBufferSize,rollForwardLsn,true,DbLsn.NULL_LSN,endOfFileLsn,null); Iterator iter=lnTypes.iterator(); while (iter.hasNext()) { LogEntryType lnType=(LogEntryType)iter.next(); reader.addTargetType(lnType); } Set countedAbortLsnNodes=new HashSet(); DbTree dbMapTree=env.getDbMapTree(); TreeLocation location=new TreeLocation(); try { while (reader.readNextEntry()) { if (reader.isLN()) { Long txnId=reader.getTxnId(); boolean processThisLN=false; boolean lnIsCommitted=false; boolean lnIsPrepared=false; Txn preparedTxn=null; if (txnId == null) { processThisLN=true; } else { lnIsCommitted=committedTxnIds.contains(txnId); if (!lnIsCommitted) { preparedTxn=(Txn)preparedTxns.get(txnId); lnIsPrepared=preparedTxn != null; } if (lnIsCommitted || lnIsPrepared) { processThisLN=true; } } if (processThisLN) { this.hook598(); LN ln=reader.getLN(); DatabaseId dbId=reader.getDatabaseId(); DatabaseImpl db=dbMapTree.getDb(dbId); long logLsn=reader.getLastLsn(); long treeLsn=DbLsn.NULL_LSN; if (db != null) { ln.postFetchInit(db,logLsn); if (preparedTxn != null) { preparedTxn.addLogInfo(logLsn); preparedTxn.lock(ln.getNodeId(),LockType.WRITE,false,db); preparedTxn.setPrepared(true); } treeLsn=redo(db,location,ln,reader.getKey(),reader.getDupTreeKey(),logLsn,info); inListRebuildDbIds.add(dbId); } TxnNodeId txnNodeId=null; if (txnId != null) { txnNodeId=new TxnNodeId(reader.getNodeId(),txnId.longValue()); } redoUtilizationInfo(logLsn,treeLsn,reader.getAbortLsn(),reader.getAbortKnownDeleted(),ln,txnNodeId,countedAbortLsnNodes); } } } info.nRepeatIteratorReads+=reader.getNRepeatIteratorReads(); } catch ( Exception e) { traceAndThrowException(reader.getLastLsn(),"redoLns",e); } }

	 private void redoLNs( RecoveryInfo info, Set lnTypes) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	redoLNs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void rebuildINList__wrappee__base() throws DatabaseException { env.getInMemoryINs().clear(); env.getDbMapTree().rebuildINListMapDb(); Iterator iter=inListRebuildDbIds.iterator(); while (iter.hasNext()) { DatabaseId dbId=(DatabaseId)iter.next(); if (!dbId.equals(DbTree.ID_DB_ID)) { DatabaseImpl db=env.getDbMapTree().getDb(dbId); if (db != null) { db.getTree().rebuildINList(); } } } }

	 private void rebuildINList() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	rebuildINList__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void replaceOrInsert__wrappee__base( DatabaseImpl db, IN inFromLog, long logLsn, long inLsn, boolean requireExactMatch) throws DatabaseException { List trackingList=null; try { if (inFromLog.isRoot()) { if (inFromLog.containsDuplicates()) { replaceOrInsertDuplicateRoot(db,(DIN)inFromLog,logLsn); } else { replaceOrInsertRoot(db,inFromLog,logLsn); } } else { trackingList=new ArrayList(); replaceOrInsertChild(db,inFromLog,logLsn,inLsn,trackingList,requireExactMatch); } } catch ( Exception e) { String trace=printTrackList(trackingList); this.hook576(db,logLsn,e,trace); throw new DatabaseException("lsnFromLog=" + DbLsn.getNoFormatString(logLsn),e); } finally { this.hook587(inFromLog,logLsn); } }

	 private void replaceOrInsert( DatabaseImpl db, IN inFromLog, long logLsn, long inLsn, boolean requireExactMatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	replaceOrInsert__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String printTrackList__wrappee__base( List trackingList){ if (trackingList != null) { StringBuffer sb=new StringBuffer(); Iterator iter=trackingList.iterator(); sb.append("Trace list:"); sb.append('\n'); while (iter.hasNext()) { sb.append((TrackingInfo)iter.next()); sb.append('\n'); } return sb.toString(); } else { return null; } }

	 private String printTrackList( List trackingList){ t.in(Thread.currentThread().getStackTrace()[1].toString());	printTrackList__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void replayINDelete__wrappee__base( DatabaseImpl db, long nodeId, boolean containsDuplicates, byte[] mainKey, byte[] dupKey, long logLsn) throws DatabaseException { boolean found=false; boolean deleted=false; Tree tree=db.getTree(); SearchResult result=new SearchResult(); try { result=db.getTree().getParentINForChildIN(nodeId,containsDuplicates,false,mainKey,dupKey,false,false,-1,null,true); if (result.parent == null) { tree.withRootLatchedExclusive(new RootDeleter(tree)); DbTree dbTree=db.getDbEnvironment().getDbMapTree(); dbTree.modifyDbRoot(db); this.hook557(db); deleted=true; } else if (result.exactParentFound) { found=true; deleted=result.parent.deleteEntry(result.index,false); } } finally { this.hook588(result); this.hook579(nodeId,containsDuplicates,logLsn,found,deleted,result); } }

	 private void replayINDelete( DatabaseImpl db, long nodeId, boolean containsDuplicates, byte[] mainKey, byte[] dupKey, long logLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	replayINDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void replaceOrInsertRoot__wrappee__base( DatabaseImpl db, IN inFromLog, long lsn) throws DatabaseException { boolean success=true; Tree tree=db.getTree(); RootUpdater rootUpdater=new RootUpdater(tree,inFromLog,lsn); try { tree.withRootLatchedExclusive(rootUpdater); if (rootUpdater.updateDone()) { EnvironmentImpl env=db.getDbEnvironment(); env.getDbMapTree().modifyDbRoot(db); } } catch ( Exception e) { success=false; throw new DatabaseException("lsnFromLog=" + DbLsn.getNoFormatString(lsn),e); } finally { this.hook580(db,inFromLog,lsn,success,rootUpdater); } }

	 private void replaceOrInsertRoot( DatabaseImpl db, IN inFromLog, long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	replaceOrInsertRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void replaceOrInsertDuplicateRoot__wrappee__base( DatabaseImpl db, DIN inFromLog, long lsn) throws DatabaseException { boolean found=true; boolean inserted=false; boolean replaced=false; long origLsn=DbLsn.NULL_LSN; byte[] mainTreeKey=inFromLog.getMainTreeKey(); IN parent=null; int index=-1; boolean success=false; try { parent=db.getTree().searchSplitsAllowed(mainTreeKey,-1,true); assert parent instanceof BIN; ChildReference newRef=new ChildReference(inFromLog,mainTreeKey,lsn); index=parent.insertEntry1(newRef); if ((index >= 0 && (index & IN.EXACT_MATCH) != 0)) { index&=~IN.EXACT_MATCH; if (parent.isEntryKnownDeleted(index)) { parent.setEntry(index,inFromLog,mainTreeKey,lsn,(byte)0); replaced=true; } else { origLsn=parent.getLsn(index); if (DbLsn.compareTo(origLsn,lsn) < 0) { parent.setEntry(index,inFromLog,mainTreeKey,lsn,parent.getState(index)); replaced=true; } } } else { found=false; } success=true; } finally { this.hook589(parent); this.hook581(db,inFromLog,lsn,found,inserted,replaced,origLsn,parent,index,success); } }

	 private void replaceOrInsertDuplicateRoot( DatabaseImpl db, DIN inFromLog, long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	replaceOrInsertDuplicateRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void replaceOrInsertChild__wrappee__base( DatabaseImpl db, IN inFromLog, long logLsn, long inLsn, List trackingList, boolean requireExactMatch) throws DatabaseException { boolean inserted=false; boolean replaced=false; long origLsn=DbLsn.NULL_LSN; boolean success=false; SearchResult result=new SearchResult(); try { result=db.getTree().getParentINForChildIN(inFromLog,requireExactMatch,false,-1,trackingList); if (result.parent == null) { return; } if (result.index >= 0) { if (result.parent.getLsn(result.index) == logLsn) { } else { if (result.exactParentFound) { origLsn=result.parent.getLsn(result.index); if (DbLsn.compareTo(origLsn,logLsn) < 0) { result.parent.updateEntry(result.index,inFromLog,inLsn); replaced=true; } } } } success=true; } finally { this.hook590(result); this.hook582(db,inFromLog,logLsn,inserted,replaced,origLsn,success,result); } }

	 private void replaceOrInsertChild( DatabaseImpl db, IN inFromLog, long logLsn, long inLsn, List trackingList, boolean requireExactMatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	replaceOrInsertChild__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private long redo__wrappee__base( DatabaseImpl db, TreeLocation location, LN lnFromLog, byte[] mainKey, byte[] dupKey, long logLsn, RecoveryInfo info) throws DatabaseException { boolean found=false; boolean replaced=false; boolean inserted=false; boolean success=false; try { location.reset(); found=db.getTree().getParentBINForChildLN(location,mainKey,dupKey,lnFromLog,true,false,true,true); if (!found && (location.bin == null)) { success=true; return DbLsn.NULL_LSN; } if (lnFromLog.containsDuplicates()) { if (found) { DIN duplicateRoot=(DIN)location.bin.fetchTarget(location.index); if (DbLsn.compareTo(logLsn,location.childLsn) >= 0) { duplicateRoot.updateDupCountLNRefAndNullTarget(logLsn); } } } else { if (found) { info.lnFound++; if (DbLsn.compareTo(logLsn,location.childLsn) > 0) { info.lnReplaced++; replaced=true; location.bin.updateEntry(location.index,null,logLsn); } if (DbLsn.compareTo(logLsn,location.childLsn) >= 0 && lnFromLog.isDeleted()) { location.bin.setKnownDeletedLeaveTarget(location.index); byte[] deletedKey=location.bin.containsDuplicates() ? dupKey : mainKey; this.hook594(db,location,deletedKey); } } else { info.lnNotFound++; if (!lnFromLog.isDeleted()) { info.lnInserted++; inserted=true; boolean insertOk=insertRecovery(db,location,logLsn); assert insertOk; } } } success=true; return found ? location.childLsn : DbLsn.NULL_LSN; } finally { this.hook591(location); this.hook583(db,location,lnFromLog,logLsn,found,replaced,inserted,success); } }

	 private long redo( DatabaseImpl db, TreeLocation location, LN lnFromLog, byte[] mainKey, byte[] dupKey, long logLsn, RecoveryInfo info) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	redo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void undo__wrappee__base( Level traceLevel, DatabaseImpl db, TreeLocation location, LN lnFromLog, byte[] mainKey, byte[] dupKey, long logLsn, long abortLsn, boolean abortKnownDeleted, RecoveryInfo info, boolean splitsAllowed) throws DatabaseException { boolean found=false; boolean replaced=false; boolean success=false; hook584(traceLevel,db,location,lnFromLog,mainKey,dupKey,logLsn,abortLsn,abortKnownDeleted,info,splitsAllowed,found,replaced,success); }

	 public static void undo( Level traceLevel, DatabaseImpl db, TreeLocation location, LN lnFromLog, byte[] mainKey, byte[] dupKey, long logLsn, long abortLsn, boolean abortKnownDeleted, RecoveryInfo info, boolean splitsAllowed) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	undo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static boolean insertRecovery__wrappee__base( DatabaseImpl db, TreeLocation location, long logLsn) throws DatabaseException { ChildReference newLNRef=new ChildReference(null,location.lnKey,logLsn); BIN parentBIN=location.bin; int entryIndex=parentBIN.insertEntry1(newLNRef); if ((entryIndex & IN.INSERT_SUCCESS) == 0) { entryIndex&=~IN.EXACT_MATCH; boolean canOverwrite=false; if (parentBIN.isEntryKnownDeleted(entryIndex)) { canOverwrite=true; } else { LN currentLN=(LN)parentBIN.fetchTarget(entryIndex); if (currentLN == null || currentLN.isDeleted()) { canOverwrite=true; } parentBIN.updateEntry(entryIndex,null); } if (canOverwrite) { parentBIN.updateEntry(entryIndex,null,logLsn,location.lnKey); parentBIN.clearKnownDeleted(entryIndex); location.index=entryIndex; return true; } else { return false; } } location.index=entryIndex & ~IN.INSERT_SUCCESS; return true; }

	 private static boolean insertRecovery( DatabaseImpl db, TreeLocation location, long logLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	insertRecovery__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void redoUtilizationInfo__wrappee__base( long logLsn, long treeLsn, long abortLsn, boolean abortKnownDeleted, LN ln, TxnNodeId txnNodeId, Set countedAbortLsnNodes){ UtilizationTracker tracker=env.getUtilizationTracker(); if (ln.isDeleted()) { Long logFileNum=new Long(DbLsn.getFileNumber(logLsn)); long fileSummaryLsn=DbLsn.longToLsn((Long)fileSummaryLsns.get(logFileNum)); int cmpFsLsnToLogLsn=(fileSummaryLsn != DbLsn.NULL_LSN) ? DbLsn.compareTo(fileSummaryLsn,logLsn) : -1; if (cmpFsLsnToLogLsn < 0) { tracker.countObsoleteNode(logLsn,null); } } if (treeLsn != DbLsn.NULL_LSN) { int cmpLogLsnToTreeLsn=DbLsn.compareTo(logLsn,treeLsn); if (cmpLogLsnToTreeLsn != 0) { long newLsn=(cmpLogLsnToTreeLsn < 0) ? treeLsn : logLsn; long oldLsn=(cmpLogLsnToTreeLsn > 0) ? treeLsn : logLsn; Long oldLsnFile=new Long(DbLsn.getFileNumber(oldLsn)); long oldFsLsn=DbLsn.longToLsn((Long)fileSummaryLsns.get(oldLsnFile)); int cmpOldFsLsnToNewLsn=(oldFsLsn != DbLsn.NULL_LSN) ? DbLsn.compareTo(oldFsLsn,newLsn) : -1; if (cmpOldFsLsnToNewLsn < 0) { tracker.countObsoleteNode(oldLsn,null); } } if (cmpLogLsnToTreeLsn <= 0 && abortLsn != DbLsn.NULL_LSN && !abortKnownDeleted && !countedAbortLsnNodes.contains(txnNodeId)) { Long abortFileNum=new Long(DbLsn.getFileNumber(abortLsn)); long abortFsLsn=DbLsn.longToLsn((Long)fileSummaryLsns.get(abortFileNum)); int cmpAbortFsLsnToLogLsn=(abortFsLsn != DbLsn.NULL_LSN) ? DbLsn.compareTo(abortFsLsn,logLsn) : -1; if (cmpAbortFsLsnToLogLsn < 0) { tracker.countObsoleteNodeInexact(abortLsn,null); countedAbortLsnNodes.add(txnNodeId); } } } }

	 private void redoUtilizationInfo( long logLsn, long treeLsn, long abortLsn, boolean abortKnownDeleted, LN ln, TxnNodeId txnNodeId, Set countedAbortLsnNodes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	redoUtilizationInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void undoUtilizationInfo__wrappee__base( LN ln, long logLsn, long abortLsn, boolean abortKnownDeleted, TxnNodeId txnNodeId, Map countedFileSummaries, Set countedAbortLsnNodes){ UtilizationTracker tracker=env.getUtilizationTracker(); Long logFileNum=new Long(DbLsn.getFileNumber(logLsn)); long fileSummaryLsn=DbLsn.longToLsn((Long)fileSummaryLsns.get(logFileNum)); int cmpFsLsnToLogLsn=(fileSummaryLsn != DbLsn.NULL_LSN) ? DbLsn.compareTo(fileSummaryLsn,logLsn) : -1; if (cmpFsLsnToLogLsn < 0) { tracker.countObsoleteNode(logLsn,null); } if (cmpFsLsnToLogLsn > 0) { Long countedFile=(Long)countedFileSummaries.get(txnNodeId); if (countedFile == null || countedFile.longValue() > logFileNum.longValue()) { if (!ln.isDeleted()) { tracker.countObsoleteNode(logLsn,null); } countedFileSummaries.put(txnNodeId,logFileNum); } } }

	 private void undoUtilizationInfo( LN ln, long logLsn, long abortLsn, boolean abortKnownDeleted, TxnNodeId txnNodeId, Map countedFileSummaries, Set countedAbortLsnNodes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	undoUtilizationInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String passStartHeader__wrappee__base( int passNum){ return "Recovery Pass " + passNum + " start: "; }

	 private String passStartHeader( int passNum){ t.in(Thread.currentThread().getStackTrace()[1].toString());	passStartHeader__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String passEndHeader__wrappee__base( int passNum, long start, long end){ return "Recovery Pass " + passNum + " end ("+ (end - start)+ "): "; }

	 private String passEndHeader( int passNum, long start, long end){ t.in(Thread.currentThread().getStackTrace()[1].toString());	passEndHeader__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static void trace__wrappee__base( Level level, DatabaseImpl database, String debugType, boolean success, Node node, long logLsn, IN parent, boolean found, boolean replaced, boolean inserted, long replacedLsn, long abortLsn, int index){ new RecoveryManager_trace(level,database,debugType,success,node,logLsn,parent,found,replaced,inserted,replacedLsn,abortLsn,index).execute(); }

	 private static void trace( Level level, DatabaseImpl database, String debugType, boolean success, Node node, long logLsn, IN parent, boolean found, boolean replaced, boolean inserted, long replacedLsn, long abortLsn, int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	trace__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void traceAndThrowException__wrappee__base( long badLsn, String method, Exception origException) throws DatabaseException { String badLsnString=DbLsn.getNoFormatString(badLsn); this.hook577(method,origException,badLsnString); throw new DatabaseException("last LSN=" + badLsnString,origException); }

	 private void traceAndThrowException( long badLsn, String method, Exception origException) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	traceAndThrowException__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook556__wrappee__base() throws DatabaseException, IOException { }

	 protected void hook556() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook556__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook557__wrappee__base( DatabaseImpl db) throws DatabaseException { }

	 protected void hook557( DatabaseImpl db) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook557__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook558__wrappee__base() throws DatabaseException, IOException { }

	 protected void hook558() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook558__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook559__wrappee__base() throws DatabaseException, IOException { }

	 protected void hook559() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook559__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook560__wrappee__base() throws DatabaseException, IOException { }

	 protected void hook560() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook560__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook561__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook561( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook561__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook562__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook562( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook562__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook563__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook563() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook563__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook564__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook564( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook564__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook565__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook565( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook565__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook566__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook566( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook566__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook567__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook567( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook567__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook568__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook568( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook568__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook569__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook569( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook569__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook570__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook570( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook570__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook571__wrappee__base( long start, long end) throws IOException, DatabaseException { }

	 protected void hook571( long start, long end) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook571__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook572__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook572() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook572__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook573__wrappee__base() throws DatabaseException, IOException { }

	 protected void hook573() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook573__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook574__wrappee__base( LNFileReader reader) throws IOException, DatabaseException, Exception { }

	 protected void hook574( LNFileReader reader) throws IOException, DatabaseException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook574__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook575__wrappee__base( IOException e) throws DatabaseException { }

	 protected void hook575( IOException e) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook575__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook576__wrappee__base( DatabaseImpl db, long logLsn, Exception e, String trace) throws DatabaseException { }

	 protected void hook576( DatabaseImpl db, long logLsn, Exception e, String trace) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook576__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook577__wrappee__base( String method, Exception origException, String badLsnString) throws DatabaseException { }

	 protected void hook577( String method, Exception origException, String badLsnString) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook577__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook578__wrappee__base( EnvironmentImpl env) throws DatabaseException { }

	 protected void hook578( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook578__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook579__wrappee__base( long nodeId, boolean containsDuplicates, long logLsn, boolean found, boolean deleted, SearchResult result) throws DatabaseException { }

	 protected void hook579( long nodeId, boolean containsDuplicates, long logLsn, boolean found, boolean deleted, SearchResult result) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook579__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook580__wrappee__base( DatabaseImpl db, IN inFromLog, long lsn, boolean success, RootUpdater rootUpdater) throws DatabaseException { }

	 protected void hook580( DatabaseImpl db, IN inFromLog, long lsn, boolean success, RootUpdater rootUpdater) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook580__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook581__wrappee__base( DatabaseImpl db, DIN inFromLog, long lsn, boolean found, boolean inserted, boolean replaced, long origLsn, IN parent, int index, boolean success) throws DatabaseException { }

	 protected void hook581( DatabaseImpl db, DIN inFromLog, long lsn, boolean found, boolean inserted, boolean replaced, long origLsn, IN parent, int index, boolean success) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook581__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook582__wrappee__base( DatabaseImpl db, IN inFromLog, long logLsn, boolean inserted, boolean replaced, long origLsn, boolean success, SearchResult result) throws DatabaseException { }

	 protected void hook582( DatabaseImpl db, IN inFromLog, long logLsn, boolean inserted, boolean replaced, long origLsn, boolean success, SearchResult result) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook582__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook583__wrappee__base( DatabaseImpl db, TreeLocation location, LN lnFromLog, long logLsn, boolean found, boolean replaced, boolean inserted, boolean success) throws DatabaseException { }

	 protected void hook583( DatabaseImpl db, TreeLocation location, LN lnFromLog, long logLsn, boolean found, boolean replaced, boolean inserted, boolean success) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook583__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected static void hook584__wrappee__base( Level traceLevel, DatabaseImpl db, TreeLocation location, LN lnFromLog, byte[] mainKey, byte[] dupKey, long logLsn, long abortLsn, boolean abortKnownDeleted, RecoveryInfo info, boolean splitsAllowed, boolean found, boolean replaced, boolean success) throws DatabaseException { location.reset(); found=db.getTree().getParentBINForChildLN(location,mainKey,dupKey,lnFromLog,splitsAllowed,true,false,true); if (lnFromLog.containsDuplicates()) { if (found) { DIN duplicateRoot=(DIN)location.bin.fetchTarget(location.index); replaced=hook592(location,logLsn,abortLsn,replaced,duplicateRoot); } } else { if (found) { if (info != null) { info.lnFound++; } boolean updateEntry=DbLsn.compareTo(logLsn,location.childLsn) == 0; if (updateEntry) { if (abortLsn == DbLsn.NULL_LSN) { location.bin.setKnownDeletedLeaveTarget(location.index); byte[] deletedKey=location.bin.containsDuplicates() ? dupKey : mainKey; hook595(db,location,deletedKey); } else { if (info != null) { info.lnReplaced++; } replaced=true; location.bin.updateEntry(location.index,null,abortLsn); if (abortKnownDeleted) { location.bin.setKnownDeleted(location.index); } else { location.bin.clearKnownDeleted(location.index); } } location.bin.clearPendingDeleted(location.index); } } else { if (info != null) { info.lnNotFound++; } } } success=true; }

	 protected static void hook584( Level traceLevel, DatabaseImpl db, TreeLocation location, LN lnFromLog, byte[] mainKey, byte[] dupKey, long logLsn, long abortLsn, boolean abortKnownDeleted, RecoveryInfo info, boolean splitsAllowed, boolean found, boolean replaced, boolean success) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook584__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook585__wrappee__base( IN in) throws DatabaseException { }

	 protected void hook585( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook585__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook586__wrappee__base( RecoveryInfo info, LNFileReader reader, TreeLocation location, LN ln, long logLsn, long abortLsn, boolean abortKnownDeleted, DatabaseImpl db) throws IOException, DatabaseException, Exception { undo(detailedTraceLevel,db,location,ln,reader.getKey(),reader.getDupTreeKey(),logLsn,abortLsn,abortKnownDeleted,info,true); }

	 protected void hook586( RecoveryInfo info, LNFileReader reader, TreeLocation location, LN ln, long logLsn, long abortLsn, boolean abortKnownDeleted, DatabaseImpl db) throws IOException, DatabaseException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook586__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook587__wrappee__base( IN inFromLog, long logLsn) throws DatabaseException { }

	 protected void hook587( IN inFromLog, long logLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook587__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook588__wrappee__base( SearchResult result) throws DatabaseException { }

	 protected void hook588( SearchResult result) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook588__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook589__wrappee__base( IN parent) throws DatabaseException { }

	 protected void hook589( IN parent) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook589__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook590__wrappee__base( SearchResult result) throws DatabaseException { }

	 protected void hook590( SearchResult result) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook590__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook591__wrappee__base( TreeLocation location) throws DatabaseException { }

	 protected void hook591( TreeLocation location) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook591__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected static boolean hook592__wrappee__base( TreeLocation location, long logLsn, long abortLsn, boolean replaced, DIN duplicateRoot) throws DatabaseException { if (DbLsn.compareTo(logLsn,location.childLsn) == 0) { duplicateRoot.updateDupCountLNRefAndNullTarget(abortLsn); replaced=true; } return replaced; }

	 protected static boolean hook592( TreeLocation location, long logLsn, long abortLsn, boolean replaced, DIN duplicateRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook592__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook593__wrappee__base( INFileReader reader) throws IOException, DatabaseException { }

	 protected void hook593( INFileReader reader) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook593__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook594__wrappee__base( DatabaseImpl db, TreeLocation location, byte[] deletedKey) throws DatabaseException { }

	 protected void hook594( DatabaseImpl db, TreeLocation location, byte[] deletedKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook594__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected static void hook595__wrappee__base( DatabaseImpl db, TreeLocation location, byte[] deletedKey) throws DatabaseException { }

	 protected static void hook595( DatabaseImpl db, TreeLocation location, byte[] deletedKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook595__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook596__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook596() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook596__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook597__wrappee__base() throws IOException, DatabaseException, Exception { }

	 protected void hook597() throws IOException, DatabaseException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook597__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook598__wrappee__base() throws IOException, DatabaseException, Exception { }

	 protected void hook598() throws IOException, DatabaseException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook598__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
