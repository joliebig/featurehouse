package com.sleepycat.je.cleaner; 
import java.io.IOException; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.Set; 
import java.util.SortedMap; 
import java.util.TreeMap; 
import java.util.logging.Level; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbTree; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.CleanerFileReader; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.ChildReference; 
import com.sleepycat.je.tree.DIN; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.tree.SearchResult; 
import com.sleepycat.je.tree.Tree; 
import com.sleepycat.je.tree.TreeLocation; 
import com.sleepycat.je.tree.WithRootLatched; 
import com.sleepycat.je.txn.BasicLocker; 
import com.sleepycat.je.txn.LockGrantType; 
import com.sleepycat.je.txn.LockResult; 
import com.sleepycat.je.txn.LockType; 
import com.sleepycat.je.utilint.DaemonThread; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
 
class  FileProcessor  extends DaemonThread {
	 private static final int PROCESS_PENDING_EVERY_N_LNS=100;

	 private static final boolean PROHIBIT_DELTAS_WHEN_FETCHING=false;

	 private static final boolean DEBUG_TRACING=false;

	 private EnvironmentImpl env;

	 private Cleaner cleaner;

	 private FileSelector fileSelector;

	 private UtilizationProfile profile;

	 FileProcessor( String name, EnvironmentImpl env, Cleaner cleaner, UtilizationProfile profile, FileSelector fileSelector){ super(0,name,env); this.env=env; this.cleaner=cleaner; this.fileSelector=fileSelector; this.profile=profile; }

	
private static  class  RootDoWork  implements WithRootLatched {
		 private DatabaseImpl db;

		 private IN inClone;

		 private long lsn;

		 RootDoWork( DatabaseImpl db, IN inClone, long lsn){ this.db=db; this.inClone=inClone; this.lsn=lsn; }

		 public IN doWork__wrappee__base( ChildReference root) throws DatabaseException { if (root == null || root.fetchTarget(db,null).getNodeId() != inClone.getNodeId()) { return null; } if (DbLsn.compareTo(root.getLsn(),lsn) <= 0) { IN rootIN=(IN)root.fetchTarget(db,null); rootIN.latch(Cleaner.UPDATE_GENERATION); return rootIN; } else { return null; } }

		 public IN doWork( ChildReference root) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  FileProcessor_processFile {
		 FileProcessor_processFile( FileProcessor _this, Long fileNum){ this._this=_this; this.fileNum=fileNum; }

		 protected FileProcessor _this;

		 protected Long fileNum;

		 protected PackedOffsets obsoleteOffsets;

		 protected TrackedFileSummary tfs;

		 protected PackedOffsets.Iterator obsoleteIter;

		 protected long nextObsolete;

		 protected int readBufferSize;

		 protected int lookAheadCacheSize;

		 protected int adjustMem;

		 protected MemoryBudget budget;

		 protected LookAheadCache lookAheadCache;

		 protected Set checkPendingDbSet;

		 protected Map dbCache;

		 protected CleanerFileReader reader;

		 protected DbTree dbMapTree;

		 protected TreeLocation location;

		 protected int nProcessedLNs;

		 protected long lsn;

		 protected long fileOffset;

		 protected boolean isLN;

		 protected boolean isIN;

		 protected boolean isRoot;

		 protected boolean isObsolete;

		 protected DatabaseId dbId1;

		 protected LN targetLN;

		 protected DatabaseId dbId2;

		 protected byte[] key;

		 protected byte[] dupKey;

		 protected Long aLsn;

		 protected LNInfo aLninfo;

		 protected Object p;

		 protected IN targetIN;

		 protected DatabaseId dbId3;

		 protected DatabaseImpl db3;

		 protected DatabaseId dbId;

		 protected DatabaseImpl db;

		 boolean execute__wrappee__base() throws DatabaseException, IOException { obsoleteOffsets=new PackedOffsets(); tfs=_this.profile.getObsoleteDetail(fileNum,obsoleteOffsets,true); obsoleteIter=obsoleteOffsets.iterator(); nextObsolete=-1; readBufferSize=_this.cleaner.readBufferSize; this.hook128(); this.hook161(); this.hook119(); this.hook127(); this.hook154(); dbCache=new HashMap(); try { reader=new CleanerFileReader(_this.env,readBufferSize,DbLsn.NULL_LSN,fileNum); this.hook137(); dbMapTree=_this.env.getDbMapTree(); location=new TreeLocation(); nProcessedLNs=0; while (reader.readNextEntry()) { this.hook146(); lsn=reader.getLastLsn(); fileOffset=DbLsn.getFileOffset(lsn); isLN=reader.isLN(); isIN=reader.isIN(); isRoot=reader.isRoot(); isObsolete=false; if (_this.env.isClosing()) { return false; } while (nextObsolete < fileOffset && obsoleteIter.hasNext()) { nextObsolete=obsoleteIter.next(); } if (nextObsolete == fileOffset) { isObsolete=true; } if (!isObsolete && !isLN && !isIN&& !isRoot) { isObsolete=true; } if (!isObsolete && isLN && reader.getLN().isDeleted()) { isObsolete=true; } if (!isObsolete && tfs != null && tfs.containsObsoleteOffset(fileOffset)) { isObsolete=true; } if (isObsolete) { this.hook147(); this.hook156(); continue; } this.hook120(); if (isLN) { targetLN=reader.getLN(); dbId2=reader.getDatabaseId(); key=reader.getKey(); dupKey=reader.getDupTreeKey(); aLsn=new Long(DbLsn.getFileOffset(lsn)); aLninfo=new LNInfo(targetLN,dbId2,key,dupKey); this.hook130(); nProcessedLNs+=1; if (nProcessedLNs % _this.PROCESS_PENDING_EVERY_N_LNS == 0) { _this.cleaner.processPending(); } } else if (isIN) { targetIN=reader.getIN(); dbId3=reader.getDatabaseId(); db3=dbMapTree.getDb(dbId3,_this.cleaner.lockTimeout,dbCache); targetIN.setDatabase(db3); _this.processIN(targetIN,db3,lsn); } else if (isRoot) { _this.env.rewriteMapTreeRoot(lsn); } else { assert false; } } this.hook129(); this.hook155(); this.hook145(); } finally { this.hook162(); if (tfs != null) { tfs.setAllowFlush(true); } } return true; }

		 boolean execute() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook119__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook119() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook119__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook120__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook120() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook120__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook127__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook127() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook127__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook128__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook128() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook128__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook129__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook129() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook129__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook130__wrappee__base() throws DatabaseException, IOException { p=null; this.hook131(); _this.processLN(fileNum,location,aLsn,aLninfo,p,dbCache); }

		 protected void hook130() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook130__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook131__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook131() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook131__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook137__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook137() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook137__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook145__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook145() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook145__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook146__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook146() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook146__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook147__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook147() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook147__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook154__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook154() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook154__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook155__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook155() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook155__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook156__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook156() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook156__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook161__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook161() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook161__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook162__wrappee__base() throws DatabaseException, IOException { }

		 protected void hook162() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook162__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  FileProcessor_processLN {
		 FileProcessor_processLN( FileProcessor _this, Long fileNum, TreeLocation location, Long offset, LNInfo info, Object lookAheadCachep, Map dbCache){ this._this=_this; this.fileNum=fileNum; this.location=location; this.offset=offset; this.info=info; this.lookAheadCachep=lookAheadCachep; this.dbCache=dbCache; }

		 protected FileProcessor _this;

		 protected Long fileNum;

		 protected TreeLocation location;

		 protected Long offset;

		 protected LNInfo info;

		 protected Object lookAheadCachep;

		 protected Map dbCache;

		 protected LookAheadCache lookAheadCache;

		 protected LN ln;

		 protected byte[] key;

		 protected byte[] dupKey;

		 protected long logLsn;

		 protected DatabaseImpl db;

		 protected boolean processedHere;

		 protected boolean obsolete;

		 protected boolean completed;

		 protected BIN bin;

		 protected DIN parentDIN;

		 protected boolean b;

		 protected Tree tree;

		 protected boolean parentFound;

		 protected int index;

		 protected boolean isDupCountLN;

		 protected long treeLsn;

		 protected ChildReference dclRef;

		 protected long lsn;

		 protected Long myOffset;

		 protected LNInfo myInfo;

		 void execute__wrappee__base() throws DatabaseException { this.hook132(); ln=info.getLN(); key=info.getKey(); dupKey=info.getDupKey(); logLsn=DbLsn.makeLsn(fileNum.longValue(),offset.longValue()); db=_this.env.getDbMapTree().getDb(info.getDbId(),_this.cleaner.lockTimeout,dbCache); processedHere=true; obsolete=false; completed=false; bin=null; parentDIN=null; try { b=db == null; this.hook157(); if (b) { this.hook158(); this.hook148(); obsolete=true; completed=true; return; } tree=db.getTree(); assert tree != null; parentFound=tree.getParentBINForChildLN(location,key,dupKey,ln,false,true,false,Cleaner.UPDATE_GENERATION); bin=location.bin; index=location.index; if (!parentFound) { this.hook149(); obsolete=true; completed=true; return; } if (bin.isEntryKnownDeleted(index)) { this.hook150(); obsolete=true; completed=true; return; } isDupCountLN=ln.containsDuplicates();
{ } if (isDupCountLN) { parentDIN=(DIN)bin.fetchTarget(index); parentDIN.latch(Cleaner.UPDATE_GENERATION); dclRef=parentDIN.getDupCountLNRef(); treeLsn=dclRef.getLsn(); } else { treeLsn=bin.getLsn(index); } processedHere=false; _this.processFoundLN(info,logLsn,treeLsn,bin,index,parentDIN); completed=true; this.hook133(); return; } finally { this.hook135(); this.hook126(); } }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook126__wrappee__base() throws DatabaseException { }

		 protected void hook126() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook126__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook132__wrappee__base() throws DatabaseException { }

		 protected void hook132() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook132__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook133__wrappee__base() throws DatabaseException { }

		 protected void hook133() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook133__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook135__wrappee__base() throws DatabaseException { }

		 protected void hook135() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook135__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook148__wrappee__base() throws DatabaseException { }

		 protected void hook148() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook148__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook149__wrappee__base() throws DatabaseException { }

		 protected void hook149() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook149__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook150__wrappee__base() throws DatabaseException { }

		 protected void hook150() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook150__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook157__wrappee__base() throws DatabaseException { }

		 protected void hook157() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook157__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook158__wrappee__base() throws DatabaseException { }

		 protected void hook158() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook158__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public void clearEnv__wrappee__base(){ env=null; cleaner=null; fileSelector=null; profile=null; }

	 public void clearEnv(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearEnv__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int nDeadlockRetries__wrappee__base() throws DatabaseException { return cleaner.nDeadlockRetries; }

	 protected int nDeadlockRetries() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	nDeadlockRetries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addToQueue__wrappee__base( Object o) throws DatabaseException { throw new DatabaseException("Cleaner.addToQueue should never be called."); }

	 public void addToQueue( Object o) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addToQueue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void onWakeup__wrappee__base() throws DatabaseException { doClean(true,true,false); }

	 public void onWakeup() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	onWakeup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized int doClean__wrappee__base( boolean invokedFromDaemon, boolean cleanMultipleFiles, boolean forceCleaning) throws DatabaseException { if (env.isClosed()) { return 0; } int nOriginalLogFiles=profile.getNumberOfFiles(); int nFilesCleaned=0; while (true) { if (nFilesCleaned >= nOriginalLogFiles) { break; } if (env.isClosing()) { break; } cleaner.processPending(); cleaner.deleteSafeToDeleteFiles(); boolean needLowUtilizationSet=cleaner.clusterResident || cleaner.clusterAll; Long fileNum=fileSelector.selectFileForCleaning(profile,forceCleaning,needLowUtilizationSet,cleaner.maxBatchFiles); cleaner.updateReadOnlyFileCollections(); if (fileNum == null) { break; } this.hook138(); boolean finished=false; long fileNumValue=fileNum.longValue(); int runId=++cleaner.nCleanerRuns; try { String traceMsg="CleanerRun " + runId + " on file 0x"+ Long.toHexString(fileNumValue); traceMsg=this.hook139(traceMsg); this.hook121(traceMsg); if (DEBUG_TRACING) { System.out.println("\n" + traceMsg); } if (processFile(fileNum)) { fileSelector.addCleanedFile(fileNum); nFilesCleaned+=1; this.hook140(); finished=true; } } catch ( IOException IOE) { this.hook122(IOE); throw new DatabaseException(IOE); } finally { if (!finished) { fileSelector.putBackFileForCleaning(fileNum); } String traceMsg="CleanerRun " + runId + " on file 0x"+ Long.toHexString(fileNumValue)+ " invokedFromDaemon="+ invokedFromDaemon+ " finished="+ finished; traceMsg=this.hook141(traceMsg); this.hook123(traceMsg); if (DEBUG_TRACING) { System.out.println("\n" + traceMsg); } } if (!cleanMultipleFiles) { break; } } return nFilesCleaned; }

	 public synchronized int doClean( boolean invokedFromDaemon, boolean cleanMultipleFiles, boolean forceCleaning) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doClean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean processFile__wrappee__base( Long fileNum) throws DatabaseException, IOException { return new FileProcessor_processFile(this,fileNum).execute(); }

	 private boolean processFile( Long fileNum) throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void processLN__wrappee__base( Long fileNum, TreeLocation location, Long offset, LNInfo info, Object lookAheadCachep, Map dbCache) throws DatabaseException { new FileProcessor_processLN(this,fileNum,location,offset,info,lookAheadCachep,dbCache).execute(); }

	 private void processLN( Long fileNum, TreeLocation location, Long offset, LNInfo info, Object lookAheadCachep, Map dbCache) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void processFoundLN__wrappee__base( LNInfo info, long logLsn, long treeLsn, BIN bin, int index, DIN parentDIN) throws DatabaseException { LN ln=info.getLN(); byte[] key=info.getKey(); byte[] dupKey=info.getDupKey(); DatabaseImpl db=bin.getDatabase(); boolean isDupCountLN=parentDIN != null; boolean obsolete=false; boolean migrated=false; boolean lockDenied=false; boolean completed=false; long nodeId=ln.getNodeId(); BasicLocker locker=null; try { Tree tree=db.getTree(); assert tree != null; if (treeLsn != logLsn) { locker=new BasicLocker(env); LockResult lockRet=locker.nonBlockingLock(nodeId,LockType.READ,db); if (lockRet.getLockGrant() == LockGrantType.DENIED) { this.hook142(); lockDenied=true; } else { this.hook143(); obsolete=true; } } if (!obsolete && !lockDenied) { if (isDupCountLN) { ChildReference dclRef=parentDIN.getDupCountLNRef(); dclRef.setMigrate(true); parentDIN.setDirty(true); if (treeLsn == logLsn && dclRef.getTarget() == null) { ln.postFetchInit(db,logLsn); parentDIN.updateDupCountLN(ln); } } else { bin.setMigrate(index,true); bin.setDirty(true); if (treeLsn == logLsn && bin.getTarget(index) == null) { ln.postFetchInit(db,logLsn); bin.updateEntry(index,ln); } if (PROHIBIT_DELTAS_WHEN_FETCHING && bin.getGeneration() == 0) { bin.setProhibitNextDelta(); } bin.setGeneration(); } this.hook144(); migrated=true; } completed=true; } finally { if (locker != null) { locker.operationEnd(); } if (completed && lockDenied) { fileSelector.addPendingLN(ln,db.getId(),key,dupKey); } this.hook124(logLsn,ln,obsolete,migrated,completed); } }

	 private void processFoundLN( LNInfo info, long logLsn, long treeLsn, BIN bin, int index, DIN parentDIN) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processFoundLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void processIN__wrappee__base( IN inClone, DatabaseImpl db, long lsn) throws DatabaseException { try { boolean obsolete=false; boolean dirtied=false; boolean completed=false; this.hook125(inClone,db,lsn,obsolete,dirtied,completed); } catch ( ReturnVoid r) { return; } }

	 private void processIN( IN inClone, DatabaseImpl db, long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private IN findINInTree__wrappee__base( Tree tree, DatabaseImpl db, IN inClone, long lsn) throws DatabaseException { try { if (inClone.isDbRoot()) { IN rootIN=isRoot(tree,db,inClone,lsn); if (rootIN == null) { return null; } else { return rootIN; } } inClone.latch(Cleaner.UPDATE_GENERATION); SearchResult result=null; this.hook134(tree,db,inClone,lsn,result); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (IN)r.value; } }

	 private IN findINInTree( Tree tree, DatabaseImpl db, IN inClone, long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	findINInTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private IN isRoot__wrappee__base( Tree tree, DatabaseImpl db, IN inClone, long lsn) throws DatabaseException { RootDoWork rdw=new RootDoWork(db,inClone,lsn); return tree.withRootLatchedShared(rdw); }

	 private IN isRoot( Tree tree, DatabaseImpl db, IN inClone, long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	isRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer(); sb.append("<Cleaner name=\"").append(name).append("\"/>"); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook121__wrappee__base( String traceMsg) throws DatabaseException, IOException { }

	 protected void hook121( String traceMsg) throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook121__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook122__wrappee__base( IOException IOE) throws DatabaseException { }

	 protected void hook122( IOException IOE) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook122__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook123__wrappee__base( String traceMsg) throws DatabaseException { }

	 protected void hook123( String traceMsg) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook123__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook124__wrappee__base( long logLsn, LN ln, boolean obsolete, boolean migrated, boolean completed) throws DatabaseException { }

	 protected void hook124( long logLsn, LN ln, boolean obsolete, boolean migrated, boolean completed) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook124__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook125__wrappee__base( IN inClone, DatabaseImpl db, long lsn, boolean obsolete, boolean dirtied, boolean completed) throws DatabaseException { boolean b=db == null; b=this.hook159(db,b); if (b) { this.hook160(db); this.hook151(); obsolete=true; completed=true; throw new ReturnVoid(); } Tree tree=db.getTree(); assert tree != null; IN inInTree=findINInTree(tree,db,inClone,lsn); if (inInTree == null) { this.hook152(); obsolete=true; } else { this.hook153(); inInTree.setDirty(true); inInTree.setProhibitNextDelta(); this.hook136(inInTree); dirtied=true; } completed=true; }

	 protected void hook125( IN inClone, DatabaseImpl db, long lsn, boolean obsolete, boolean dirtied, boolean completed) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook125__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook134__wrappee__base( Tree tree, DatabaseImpl db, IN inClone, long lsn, SearchResult result) throws DatabaseException { result=tree.getParentINForChildIN(inClone,true,Cleaner.UPDATE_GENERATION,inClone.getLevel(),null); if (!result.exactParentFound) { throw new ReturnObject(null); } int compareVal=DbLsn.compareTo(result.parent.getLsn(result.index),lsn); if (compareVal > 0) { throw new ReturnObject(null); } else { IN in; if (compareVal == 0) { in=(IN)result.parent.getTarget(result.index); if (in == null) { in=inClone; in.postFetchInit(db,lsn); result.parent.updateEntry(result.index,in); } } else { in=(IN)result.parent.fetchTarget(result.index); } in.latch(Cleaner.UPDATE_GENERATION); throw new ReturnObject(in); } }

	 protected void hook134( Tree tree, DatabaseImpl db, IN inClone, long lsn, SearchResult result) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook134__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook136__wrappee__base( IN inInTree) throws DatabaseException { }

	 protected void hook136( IN inInTree) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook136__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook138__wrappee__base() throws DatabaseException { }

	 protected void hook138() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook138__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String hook139__wrappee__base( String traceMsg) throws DatabaseException, IOException { return traceMsg; }

	 protected String hook139( String traceMsg) throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook139__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook140__wrappee__base() throws DatabaseException, IOException { }

	 protected void hook140() throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook140__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String hook141__wrappee__base( String traceMsg) throws DatabaseException { return traceMsg; }

	 protected String hook141( String traceMsg) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook141__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook142__wrappee__base() throws DatabaseException { }

	 protected void hook142() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook142__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook143__wrappee__base() throws DatabaseException { }

	 protected void hook143() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook143__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook144__wrappee__base() throws DatabaseException { }

	 protected void hook144() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook144__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook151__wrappee__base() throws DatabaseException { }

	 protected void hook151() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook151__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook152__wrappee__base() throws DatabaseException { }

	 protected void hook152() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook152__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook153__wrappee__base() throws DatabaseException { }

	 protected void hook153() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook153__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean hook159__wrappee__base( DatabaseImpl db, boolean b) throws DatabaseException { return b; }

	 protected boolean hook159( DatabaseImpl db, boolean b) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook159__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook160__wrappee__base( DatabaseImpl db) throws DatabaseException { }

	 protected void hook160( DatabaseImpl db) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook160__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
