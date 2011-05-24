package com.sleepycat.je.dbi; 
import java.io.PrintStream; 
import java.nio.ByteBuffer; 
import java.util.Collections; 
import java.util.Comparator; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.Set; 
import com.sleepycat.je.Cursor; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseConfig; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.LockMode; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.je.PreloadConfig; 
import com.sleepycat.je.PreloadStats; 
import com.sleepycat.je.PreloadStatus; 
import com.sleepycat.je.SecondaryDatabase; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.SortedLSNTreeWalker.TreeNodeProcessor; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.tree.ChildReference; 
import com.sleepycat.je.tree.Node; 
import com.sleepycat.je.tree.Tree; 
import com.sleepycat.je.tree.TreeUtils; 
import com.sleepycat.je.tree.TreeWalkerStatsAccumulator; 
import com.sleepycat.je.tree.WithRootLatched; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.ThreadLocker; 
import com.sleepycat.je.utilint.CmdUtil; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.TestHook; 
import de.ovgu.cide.jakutil.*; 
public  class  DatabaseImpl  implements LogWritable, LogReadable, Cloneable {
	 private DatabaseId id;

	 Tree tree;

	 private EnvironmentImpl envImpl;

	 private boolean duplicatesAllowed;

	 private boolean transactional;

	 private Set referringHandles;

	 private long eofNodeId;

	 private Comparator btreeComparator=null;

	 private Comparator duplicateComparator=null;

	 private String btreeComparatorName="";

	 private String duplicateComparatorName="";

	 private int binDeltaPercent;

	 private int binMaxDeltas;

	 private int maxMainTreeEntriesPerNode;

	 private int maxDupTreeEntriesPerNode;

	 private String debugDatabaseName;

	 private TestHook pendingDeletedHook;

	 public DatabaseImpl( String dbName, DatabaseId id, EnvironmentImpl envImpl, DatabaseConfig dbConfig) throws DatabaseException { this.id=id; this.envImpl=envImpl; this.btreeComparator=dbConfig.getBtreeComparator(); this.duplicateComparator=dbConfig.getDuplicateComparator(); duplicatesAllowed=dbConfig.getSortedDuplicates(); transactional=dbConfig.getTransactional(); maxMainTreeEntriesPerNode=dbConfig.getNodeMaxEntries(); maxDupTreeEntriesPerNode=dbConfig.getNodeMaxDupTreeEntries(); initDefaultSettings(); this.hook288(); tree=new Tree(this); referringHandles=Collections.synchronizedSet(new HashSet()); eofNodeId=Node.getNextNodeId(); debugDatabaseName=dbName; }

	 public DatabaseImpl() throws DatabaseException { id=new DatabaseId(); envImpl=null; this.hook289(); tree=new Tree(); referringHandles=Collections.synchronizedSet(new HashSet()); eofNodeId=Node.getNextNodeId(); }

	
private static  class  ObsoleteProcessor  implements TreeNodeProcessor {
		 private UtilizationTracker tracker;

		 ObsoleteProcessor( UtilizationTracker tracker){ this.tracker=tracker; }

		 public void processLSN__wrappee__base( long childLsn, LogEntryType childType){ assert childLsn != DbLsn.NULL_LSN; tracker.countObsoleteNodeInexact(childLsn,childType); }

		 public void processLSN( long childLsn, LogEntryType childType){ t.in(Thread.currentThread().getStackTrace()[1].toString());	processLSN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
private static  class  LNCounter  implements TreeNodeProcessor {
		 private long counter;

		 public void processLSN__wrappee__base( long childLsn, LogEntryType childType){ assert childLsn != DbLsn.NULL_LSN; if (childType.equals(LogEntryType.LOG_LN_TRANSACTIONAL) || childType.equals(LogEntryType.LOG_LN)) { counter++; } }

		 public void processLSN( long childLsn, LogEntryType childType){ t.in(Thread.currentThread().getStackTrace()[1].toString());	processLSN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 long getCount__wrappee__base(){ return counter; }

		 long getCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
private static  class  HaltPreloadException  extends RuntimeException {
		 private PreloadStatus status;

		 HaltPreloadException( PreloadStatus status){ super(status.toString()); this.status=status; }

		 PreloadStatus getStatus__wrappee__base(){ return status; }

		 PreloadStatus getStatus(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getStatus__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 static final HaltPreloadException timeExceededPreloadException=new HaltPreloadException(PreloadStatus.EXCEEDED_TIME);

	 static final HaltPreloadException memoryExceededPreloadException=new HaltPreloadException(PreloadStatus.FILLED_CACHE);

	
@MethodObject static  class  DatabaseImpl_preload {
		 DatabaseImpl_preload( DatabaseImpl _this, PreloadConfig config){ this._this=_this; this.config=config; }

		 protected DatabaseImpl _this;

		 protected PreloadConfig config;

		 protected long maxBytes;

		 protected long maxMillisecs;

		 protected long targetTime;

		 protected long cacheBudget;

		 protected PreloadStats ret;

		 protected PreloadProcessor callback;

		 protected SortedLSNTreeWalker walker;

		 PreloadStats execute__wrappee__base() throws DatabaseException { maxBytes=config.getMaxBytes(); maxMillisecs=config.getMaxMillisecs(); targetTime=Long.MAX_VALUE; if (maxMillisecs > 0) { targetTime=System.currentTimeMillis() + maxMillisecs; } this.hook290(); ret=new PreloadStats(); callback=new PreloadProcessor(_this.envImpl,maxBytes,targetTime,ret); walker=new PreloadLSNTreeWalker(_this,callback,config); this.hook287(); return ret; }

		 PreloadStats execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook287__wrappee__base() throws DatabaseException { walker.walk(); }

		 protected void hook287() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook287__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook290__wrappee__base() throws DatabaseException { }

		 protected void hook290() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook290__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public void setDebugDatabaseName__wrappee__base( String debugName){ debugDatabaseName=debugName; }

	 public void setDebugDatabaseName( String debugName){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDebugDatabaseName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getDebugName__wrappee__base(){ return debugDatabaseName; }

	 public String getDebugName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDebugName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setPendingDeletedHook__wrappee__base( TestHook hook){ pendingDeletedHook=hook; }

	 public void setPendingDeletedHook( TestHook hook){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setPendingDeletedHook__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void initDefaultSettings__wrappee__base() throws DatabaseException { DbConfigManager configMgr=envImpl.getConfigManager(); binDeltaPercent=configMgr.getInt(EnvironmentParams.BIN_DELTA_PERCENT); binMaxDeltas=configMgr.getInt(EnvironmentParams.BIN_MAX_DELTAS); if (maxMainTreeEntriesPerNode == 0) { maxMainTreeEntriesPerNode=configMgr.getInt(EnvironmentParams.NODE_MAX); } if (maxDupTreeEntriesPerNode == 0) { maxDupTreeEntriesPerNode=configMgr.getInt(EnvironmentParams.NODE_MAX_DUPTREE); } }

	 private void initDefaultSettings() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initDefaultSettings__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object clone__wrappee__base() throws CloneNotSupportedException { return super.clone(); }

	 public Object clone() throws CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Tree getTree__wrappee__base(){ return tree; }

	 public Tree getTree(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setTree__wrappee__base( Tree tree){ this.tree=tree; }

	 void setTree( Tree tree){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getId__wrappee__base(){ return id; }

	 public DatabaseId getId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setId__wrappee__base( DatabaseId id){ this.id=id; }

	 void setId( DatabaseId id){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getEofNodeId__wrappee__base(){ return eofNodeId; }

	 public long getEofNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEofNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return transactional; }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTransactional__wrappee__base( boolean transactional){ this.transactional=transactional; }

	 public void setTransactional( boolean transactional){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getSortedDuplicates__wrappee__base(){ return duplicatesAllowed; }

	 public boolean getSortedDuplicates(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNodeMaxEntries__wrappee__base(){ return maxMainTreeEntriesPerNode; }

	 public int getNodeMaxEntries(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeMaxEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNodeMaxDupTreeEntries__wrappee__base(){ return maxDupTreeEntriesPerNode; }

	 public int getNodeMaxDupTreeEntries(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeMaxDupTreeEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDuplicateComparator__wrappee__base( Comparator duplicateComparator){ this.duplicateComparator=duplicateComparator; }

	 public void setDuplicateComparator( Comparator duplicateComparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDuplicateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setBtreeComparator__wrappee__base( Comparator btreeComparator){ this.btreeComparator=btreeComparator; }

	 public void setBtreeComparator( Comparator btreeComparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Comparator getBtreeComparator__wrappee__base(){ return btreeComparator; }

	 public Comparator getBtreeComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Comparator getDuplicateComparator__wrappee__base(){ return duplicateComparator; }

	 public Comparator getDuplicateComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDuplicateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setEnvironmentImpl__wrappee__base( EnvironmentImpl envImpl) throws DatabaseException { this.envImpl=envImpl; initDefaultSettings(); tree.setDatabase(this); }

	 public void setEnvironmentImpl( EnvironmentImpl envImpl) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setEnvironmentImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public EnvironmentImpl getDbEnvironment__wrappee__base(){ return envImpl; }

	 public EnvironmentImpl getDbEnvironment(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbEnvironment__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean hasOpenHandles__wrappee__base(){ return referringHandles.size() > 0; }

	 public boolean hasOpenHandles(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasOpenHandles__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addReferringHandle__wrappee__base( Database db){ referringHandles.add(db); }

	 public void addReferringHandle( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addReferringHandle__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void removeReferringHandle__wrappee__base( Database db){ referringHandles.remove(db); }

	 public void removeReferringHandle( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeReferringHandle__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized int getReferringHandleCount__wrappee__base(){ return referringHandles.size(); }

	 synchronized int getReferringHandleCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReferringHandleCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Database findPrimaryDatabase__wrappee__base() throws DatabaseException { for (Iterator i=referringHandles.iterator(); i.hasNext(); ) { Object obj=i.next(); if (obj instanceof SecondaryDatabase) { return ((SecondaryDatabase)obj).getPrimaryDatabase(); } } return null; }

	 public Database findPrimaryDatabase() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	findPrimaryDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getName__wrappee__base() throws DatabaseException { return envImpl.getDbMapTree().getDbName(id); }

	 public String getName() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long countRecords__wrappee__base() throws DatabaseException { LNCounter lnCounter=new LNCounter(); SortedLSNTreeWalker walker=new SortedLSNTreeWalker(this,false,false,tree.getRootLsn(),lnCounter); walker.walk(); return lnCounter.getCount(); }

	 long countRecords() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	countRecords__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean walkDatabaseTree__wrappee__base( TreeWalkerStatsAccumulator statsAcc, PrintStream out, boolean verbose) throws DatabaseException { boolean ok=true; Locker locker=new ThreadLocker(envImpl); Cursor cursor=null; CursorImpl impl=null; try { EnvironmentImpl.incThreadLocalReferenceCount(); cursor=DbInternal.newCursor(this,locker,null); impl=DbInternal.getCursorImpl(cursor); tree.setTreeStatsAccumulator(statsAcc); impl.setTreeStatsAccumulator(statsAcc); DatabaseEntry foundData=new DatabaseEntry(); DatabaseEntry key=new DatabaseEntry(); OperationStatus status=DbInternal.position(cursor,key,foundData,LockMode.READ_UNCOMMITTED,true); while (status == OperationStatus.SUCCESS) { try { status=DbInternal.retrieveNext(cursor,key,foundData,LockMode.READ_UNCOMMITTED,GetMode.NEXT); } catch ( DatabaseException DBE) { ok=false; if (DbInternal.advanceCursor(cursor,key,foundData)) { if (verbose) { out.println("Error encountered (continuing):"); out.println(DBE); printErrorRecord(out,key,foundData); } } else { throw DBE; } } } } finally { if (impl != null) { impl.setTreeStatsAccumulator(null); } tree.setTreeStatsAccumulator(null); EnvironmentImpl.decThreadLocalReferenceCount(); if (cursor != null) { cursor.close(); } } return ok; }

	 private boolean walkDatabaseTree( TreeWalkerStatsAccumulator statsAcc, PrintStream out, boolean verbose) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	walkDatabaseTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void printErrorRecord__wrappee__base( PrintStream out, DatabaseEntry key, DatabaseEntry data){ byte[] bytes=key.getData(); StringBuffer sb=new StringBuffer("Error Key "); if (bytes == null) { sb.append("UNKNOWN"); } else { CmdUtil.formatEntry(sb,bytes,false); sb.append(' '); CmdUtil.formatEntry(sb,bytes,true); } out.println(sb); bytes=data.getData(); sb=new StringBuffer("Error Data "); if (bytes == null) { sb.append("UNKNOWN"); } else { CmdUtil.formatEntry(sb,bytes,false); sb.append(' '); CmdUtil.formatEntry(sb,bytes,true); } out.println(sb); }

	 private void printErrorRecord( PrintStream out, DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	printErrorRecord__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public PreloadStats preload__wrappee__base( PreloadConfig config) throws DatabaseException { return new DatabaseImpl_preload(this,config).execute(); }

	 public PreloadStats preload( PreloadConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	preload__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces){ StringBuffer sb=new StringBuffer(); sb.append(TreeUtils.indent(nSpaces)); sb.append("<database id=\""); sb.append(id.toString()); sb.append("\""); if (btreeComparator != null) { sb.append(" btc=\""); sb.append(serializeComparator(btreeComparator)); sb.append("\""); } if (duplicateComparator != null) { sb.append(" dupc=\""); sb.append(serializeComparator(duplicateComparator)); sb.append("\""); } sb.append("/>"); return sb.toString(); }

	 public String dumpString( int nSpaces){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return id.getLogSize() + tree.getLogSize() + LogUtils.getBooleanLogSize()+ LogUtils.getStringLogSize(serializeComparator(btreeComparator))+ LogUtils.getStringLogSize(serializeComparator(duplicateComparator))+ (LogUtils.getIntLogSize() * 2); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ id.writeToLog(logBuffer); tree.writeToLog(logBuffer); LogUtils.writeBoolean(logBuffer,duplicatesAllowed); LogUtils.writeString(logBuffer,serializeComparator(btreeComparator)); LogUtils.writeString(logBuffer,serializeComparator(duplicateComparator)); LogUtils.writeInt(logBuffer,maxMainTreeEntriesPerNode); LogUtils.writeInt(logBuffer,maxDupTreeEntriesPerNode); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { id.readFromLog(itemBuffer,entryTypeVersion); tree.readFromLog(itemBuffer,entryTypeVersion); duplicatesAllowed=LogUtils.readBoolean(itemBuffer); btreeComparatorName=LogUtils.readString(itemBuffer); duplicateComparatorName=LogUtils.readString(itemBuffer); try { if (!EnvironmentImpl.getNoComparators()) { if (btreeComparatorName.length() != 0) { Class btreeComparatorClass=Class.forName(btreeComparatorName); btreeComparator=instantiateComparator(btreeComparatorClass,"Btree"); } if (duplicateComparatorName.length() != 0) { Class duplicateComparatorClass=Class.forName(duplicateComparatorName); duplicateComparator=instantiateComparator(duplicateComparatorClass,"Duplicate"); } } } catch ( ClassNotFoundException CNFE) { throw new LogException("couldn't instantiate class comparator",CNFE); } if (entryTypeVersion >= 1) { maxMainTreeEntriesPerNode=LogUtils.readInt(itemBuffer); maxDupTreeEntriesPerNode=LogUtils.readInt(itemBuffer); } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<database>"); id.dumpLog(sb,verbose); tree.dumpLog(sb,verbose); sb.append("<dupsort v=\"").append(duplicatesAllowed); sb.append("\"/>"); sb.append("<btcf name=\""); sb.append(btreeComparatorName); sb.append("\"/>"); sb.append("<dupcf name=\""); sb.append(duplicateComparatorName); sb.append("\"/>"); sb.append("</database>"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String serializeComparator__wrappee__base( Comparator comparator){ if (comparator != null) { return comparator.getClass().getName(); } else { return ""; } }

	 public static String serializeComparator( Comparator comparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	serializeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Comparator instantiateComparator__wrappee__base( Class comparator, String comparatorType) throws LogException { if (comparator == null) { return null; } try { return (Comparator)comparator.newInstance(); } catch ( InstantiationException IE) { throw new LogException("Exception while trying to load " + comparatorType + " Comparator class: "+ IE); }
catch ( IllegalAccessException IAE) { throw new LogException("Exception while trying to load " + comparatorType + " Comparator class: "+ IAE); } }

	 public static Comparator instantiateComparator( Class comparator, String comparatorType) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	instantiateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getBinDeltaPercent__wrappee__base(){ return binDeltaPercent; }

	 public int getBinDeltaPercent(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBinDeltaPercent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getBinMaxDeltas__wrappee__base(){ return binMaxDeltas; }

	 public int getBinMaxDeltas(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBinMaxDeltas__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook288__wrappee__base() throws DatabaseException { }

	 protected void hook288() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook288__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook289__wrappee__base() throws DatabaseException { }

	 protected void hook289() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook289__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
