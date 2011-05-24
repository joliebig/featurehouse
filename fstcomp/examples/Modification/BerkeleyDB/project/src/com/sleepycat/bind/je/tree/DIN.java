package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import java.util.Comparator; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.Cleaner; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.txn.LockResult; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
public final  class  DIN  extends IN {
	 private static final String BEGIN_TAG="<din>";

	 private static final String END_TAG="</din>";

	 private byte[] dupKey;

	 private ChildReference dupCountLNRef;

	 public DIN(){ super(); dupCountLNRef=new ChildReference(); init(null,Key.EMPTY_KEY,0,0); }

	 public DIN( DatabaseImpl db, byte[] identifierKey, int capacity, byte[] dupKey, ChildReference dupCountLNRef, int level){ super(db,identifierKey,capacity,level); this.dupKey=dupKey; this.dupCountLNRef=dupCountLNRef; }

	
@MethodObject static  class  DIN_updateDupCountLN {
		 DIN_updateDupCountLN( DIN _this, Node target){ this._this=_this; this.target=target; }

		 protected DIN _this;

		 protected Node target;

		 protected long oldSize;

		 protected long newSize;

		 void execute__wrappee__base(){ _this.dupCountLNRef.setTarget(target); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  DIN_updateDupCountLNRefAndNullTarget {
		 DIN_updateDupCountLNRefAndNullTarget( DIN _this, long newLsn){ this._this=_this; this.newLsn=newLsn; }

		 protected DIN _this;

		 protected long newLsn;

		 protected long oldSize;

		 protected long newSize;

		 void execute__wrappee__base(){ _this.setDirty(true); this.hook614(); _this.dupCountLNRef.setTarget(null); _this.dupCountLNRef.setLsn(newLsn); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook614__wrappee__base(){ }

		 protected void hook614(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook614__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 protected int generateLevel__wrappee__base( DatabaseId dbId, int newLevel){ return newLevel; }

	 protected int generateLevel( DatabaseId dbId, int newLevel){ t.in(Thread.currentThread().getStackTrace()[1].toString());	generateLevel__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected IN createNewInstance__wrappee__base( byte[] identifierKey, int maxEntries, int level){ return new DIN(getDatabase(),identifierKey,maxEntries,dupKey,dupCountLNRef,level); }

	 protected IN createNewInstance( byte[] identifierKey, int maxEntries, int level){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createNewInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

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

	 public ChildReference getDupCountLNRef__wrappee__base(){ return dupCountLNRef; }

	 public ChildReference getDupCountLNRef(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupCountLNRef__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DupCountLN getDupCountLN__wrappee__base() throws DatabaseException { return (DupCountLN)dupCountLNRef.fetchTarget(getDatabase(),this); }

	 public DupCountLN getDupCountLN() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupCountLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setDupCountLN__wrappee__base( ChildReference dupCountLNRef){ this.dupCountLNRef=dupCountLNRef; }

	 void setDupCountLN( ChildReference dupCountLNRef){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDupCountLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateDupCountLN__wrappee__base( Node target){ new DIN_updateDupCountLN(this,target).execute(); }

	 public void updateDupCountLN( Node target){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateDupCountLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateDupCountLNRefAndNullTarget__wrappee__base( long newLsn){ new DIN_updateDupCountLNRefAndNullTarget(this,newLsn).execute(); }

	 public void updateDupCountLNRefAndNullTarget( long newLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateDupCountLNRefAndNullTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateDupCountLNRef__wrappee__base( long newLsn){ setDirty(true); dupCountLNRef.setLsn(newLsn); }

	 public void updateDupCountLNRef( long newLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateDupCountLNRef__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean containsDuplicates__wrappee__base(){ return true; }

	 public boolean containsDuplicates(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	containsDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isDbRoot__wrappee__base(){ return false; }

	 public boolean isDbRoot(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDbRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final Comparator getKeyComparator__wrappee__base(){ return getDatabase().getDuplicateComparator(); }

	 public final Comparator getKeyComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKeyComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void incrementDuplicateCount__wrappee__base( LockResult lockResult, byte[] key, Locker locker, boolean increment) throws DatabaseException { long oldLsn=dupCountLNRef.getLsn(); lockResult.setAbortLsn(oldLsn,dupCountLNRef.isKnownDeleted()); DupCountLN dupCountLN=getDupCountLN(); if (increment) { dupCountLN.incDupCount(); } else { dupCountLN.decDupCount(); assert dupCountLN.getDupCount() >= 0; } DatabaseImpl db=getDatabase(); long newCountLSN=dupCountLN.log(db.getDbEnvironment(),db.getId(),key,oldLsn,locker); updateDupCountLNRef(newCountLSN); }

	 public void incrementDuplicateCount( LockResult lockResult, byte[] key, Locker locker, boolean increment) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	incrementDuplicateCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean matchLNByNodeId__wrappee__base( TreeLocation location, long nodeId) throws DatabaseException { for (int i=0; i < getNEntries(); i++) { Node n=fetchTarget(i); if (n != null) { boolean ret=n.matchLNByNodeId(location,nodeId); if (ret) { return true; } } } return false; }

	 boolean matchLNByNodeId( TreeLocation location, long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	matchLNByNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void accumulateStats__wrappee__base( TreeWalkerStatsAccumulator acc){ acc.processDIN(this,new Long(getNodeId()),getLevel()); }

	 void accumulateStats( TreeWalkerStatsAccumulator acc){ t.in(Thread.currentThread().getStackTrace()[1].toString());	accumulateStats__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_DIN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected long logInternal__wrappee__base( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { if (dupCountLNRef != null) { Cleaner cleaner=getDatabase().getDbEnvironment().getCleaner(); cleaner.lazyMigrateDupCountLN(this,dupCountLNRef,proactiveMigration); } return super.logInternal(logManager,allowDeltas,isProvisional,proactiveMigration,parent); }

	 protected long logInternal( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	logInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=super.getLogSize(); size+=LogUtils.getByteArrayLogSize(dupKey); size+=LogUtils.getBooleanLogSize(); if (dupCountLNRef != null) { size+=dupCountLNRef.getLogSize(); } return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ super.writeToLog(logBuffer); LogUtils.writeByteArray(logBuffer,dupKey); boolean dupCountLNRefExists=(dupCountLNRef != null); LogUtils.writeBoolean(logBuffer,dupCountLNRefExists); if (dupCountLNRefExists) { dupCountLNRef.writeToLog(logBuffer); } }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); dupKey=LogUtils.readByteArray(itemBuffer); boolean dupCountLNRefExists=LogUtils.readBoolean(itemBuffer); if (dupCountLNRefExists) { dupCountLNRef.readFromLog(itemBuffer,entryTypeVersion); } else { dupCountLNRef=null; } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb){ super.dumpLogAdditional(sb); sb.append(Key.dumpString(dupKey,0)); if (dupCountLNRef != null) { dupCountLNRef.dumpLog(sb,true); } }

	 protected void dumpLogAdditional( StringBuffer sb){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(beginTag()); sb.append('\n'); } sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<dupkey>"); sb.append(dupKey == null ? "" : Key.dumpString(dupKey,0)); sb.append("</dupkey>"); sb.append('\n'); if (dupCountLNRef == null) { sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<dupCountLN/>"); } else { sb.append(dupCountLNRef.dumpString(nSpaces + 4,true)); } sb.append('\n'); sb.append(super.dumpString(nSpaces,false)); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(endTag()); } return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String shortClassName__wrappee__base(){ return "DIN"; }

	 public String shortClassName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shortClassName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
