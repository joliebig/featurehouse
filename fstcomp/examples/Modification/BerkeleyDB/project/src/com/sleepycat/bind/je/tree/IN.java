package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import java.util.ArrayList; 
import java.util.Comparator; 
import java.util.List; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.DbTree; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.INList; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogFileNotFoundException; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.log.entry.INLogEntry; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  IN  extends Node  implements Comparable, LoggableObject, LogReadable {
	 private static final String BEGIN_TAG="<in>";

	 private static final String END_TAG="</in>";

	 private static final String TRACE_SPLIT="Split:";

	 private static final String TRACE_DELETE="Delete:";

	 private static final byte KNOWN_DELETED_BIT=0x1;

	 private static final byte CLEAR_KNOWN_DELETED_BIT=~0x1;

	 private static final byte DIRTY_BIT=0x2;

	 private static final byte CLEAR_DIRTY_BIT=~0x2;

	 private static final byte MIGRATE_BIT=0x4;

	 private static final byte CLEAR_MIGRATE_BIT=~0x4;

	 private static final byte PENDING_DELETED_BIT=0x8;

	 private static final byte CLEAR_PENDING_DELETED_BIT=~0x8;

	 private static final int BYTES_PER_LSN_ENTRY=4;

	 private static final int MAX_FILE_OFFSET=0xfffffe;

	 private static final int THREE_BYTE_NEGATIVE_ONE=0xffffff;

	 private static final int GROWTH_INCREMENT=5;

	 public static final int DBMAP_LEVEL=0x20000;

	 public static final int MAIN_LEVEL=0x10000;

	 public static final int LEVEL_MASK=0x0ffff;

	 public static final int MIN_LEVEL=-1;

	 public static final int MAX_LEVEL=Integer.MAX_VALUE;

	 public static final int BIN_LEVEL=MAIN_LEVEL | 1;

	 private long generation;

	 private boolean dirty;

	 private int nEntries;

	 private byte[] identifierKey;

	 private Node[] entryTargets;

	 private byte[][] entryKeyVals;

	 private long baseFileNumber;

	 private byte[] entryLsnByteArray;

	 private long[] entryLsnLongArray;

	 private byte[] entryStates;

	 private DatabaseImpl databaseImpl;

	 private boolean isRoot;

	 private int level;

	 private long inMemorySize;

	 private long lastFullVersion=DbLsn.NULL_LSN;

	 private List provisionalObsolete;

	 public static final int EXACT_MATCH=(1 << 16);

	 public static final int INSERT_SUCCESS=(1 << 17);

	 public static int ACCUMULATED_LIMIT=1000;

	 public IN(){ super(false); init(null,Key.EMPTY_KEY,0,0); }

	 public IN( DatabaseImpl db, byte[] identifierKey, int capacity, int level){ super(true); init(db,identifierKey,capacity,generateLevel(db.getId(),level)); }

	
@MethodObject static  class  IN_setLsn {
		 IN_setLsn( IN _this, int idx, long lsn){ this._this=_this; this.idx=idx; this.lsn=lsn; }

		 protected IN _this;

		 protected int idx;

		 protected long lsn;

		 protected int oldSize;

		 void execute__wrappee__base(){ _this.setLsnElement(idx,lsn); this.hook639(); _this.entryStates[idx]|=_this.DIRTY_BIT; }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook639__wrappee__base(){ }

		 protected void hook639(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook639__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_setEntry {
		 IN_setEntry( IN _this, int idx, Node target, byte[] keyVal, long lsn, byte state){ this._this=_this; this.idx=idx; this.target=target; this.keyVal=keyVal; this.lsn=lsn; this.state=state; }

		 protected IN _this;

		 protected int idx;

		 protected Node target;

		 protected byte[] keyVal;

		 protected long lsn;

		 protected byte state;

		 protected long oldSize;

		 protected int newNEntries;

		 protected long newSize;

		 void execute__wrappee__base(){ newNEntries=idx + 1; if (newNEntries > _this.nEntries) { _this.nEntries=newNEntries; this.hook641(); } _this.entryTargets[idx]=target; _this.entryKeyVals[idx]=keyVal; _this.setLsnElement(idx,lsn); _this.entryStates[idx]=state; this.hook640(); _this.setDirty(true); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook640__wrappee__base(){ }

		 protected void hook640(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook640__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook641__wrappee__base(){ }

		 protected void hook641(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook641__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_updateEntry {
		 IN_updateEntry( IN _this, int idx, Node node){ this._this=_this; this.idx=idx; this.node=node; }

		 protected IN _this;

		 protected int idx;

		 protected Node node;

		 protected long oldSize;

		 protected long newSize;

		 void execute__wrappee__base(){ _this.setTarget(idx,node); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_updateEntry2 {
		 IN_updateEntry2( IN _this, int idx, Node node, long lsn){ this._this=_this; this.idx=idx; this.node=node; this.lsn=lsn; }

		 protected IN _this;

		 protected int idx;

		 protected Node node;

		 protected long lsn;

		 protected long oldSize;

		 protected long newSize;

		 void execute__wrappee__base(){ _this.setLsn(idx,lsn); _this.setTarget(idx,node); this.hook642(); _this.setDirty(true); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook642__wrappee__base(){ }

		 protected void hook642(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook642__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_updateEntry3 {
		 IN_updateEntry3( IN _this, int idx, Node node, long lsn, byte[] key){ this._this=_this; this.idx=idx; this.node=node; this.lsn=lsn; this.key=key; }

		 protected IN _this;

		 protected int idx;

		 protected Node node;

		 protected long lsn;

		 protected byte[] key;

		 protected long oldSize;

		 protected long newSize;

		 void execute__wrappee__base(){ _this.setLsn(idx,lsn); _this.setTarget(idx,node); _this.setKey(idx,key); this.hook643(); _this.setDirty(true); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook643__wrappee__base(){ }

		 protected void hook643(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook643__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_updateEntryCompareKey {
		 IN_updateEntryCompareKey( IN _this, int idx, Node node, long lsn, byte[] key){ this._this=_this; this.idx=idx; this.node=node; this.lsn=lsn; this.key=key; }

		 protected IN _this;

		 protected int idx;

		 protected Node node;

		 protected long lsn;

		 protected byte[] key;

		 protected long oldSize;

		 protected byte[] existingKey;

		 protected int s;

		 protected long newSize;

		 void execute__wrappee__base(){ _this.setLsn(idx,lsn); _this.setTarget(idx,node); existingKey=_this.getKey(idx); s=Key.compareKeys(key,existingKey,_this.getKeyComparator()); if (s < 0) { _this.setKey(idx,key); } this.hook644(); _this.setDirty(true); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook644__wrappee__base(){ }

		 protected void hook644(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook644__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_insertEntry1 {
		 IN_insertEntry1( IN _this, ChildReference entry){ this._this=_this; this.entry=entry; }

		 protected IN _this;

		 protected ChildReference entry;

		 protected byte[] key;

		 protected int index;

		 protected int oldSize;

		 int execute__wrappee__base() throws DatabaseException { if (_this.nEntries >= _this.entryTargets.length) { _this.compress(null,true); } if (_this.nEntries < _this.entryTargets.length) { key=entry.getKey(); index=_this.findEntry(key,true,false); if (index >= 0 && (index & _this.EXACT_MATCH) != 0) { return index; } else { index++; } if (index < _this.nEntries) { this.hook647(); _this.shiftEntriesRight(index); this.hook646(); } _this.entryTargets[index]=entry.getTarget(); _this.entryKeyVals[index]=entry.getKey(); _this.setLsnElement(index,entry.getLsn()); _this.entryStates[index]=entry.getState(); _this.nEntries++; _this.adjustCursorsForInsert(index); this.hook645(); _this.setDirty(true); return (index | _this.INSERT_SUCCESS); } else { throw new InconsistentNodeException("Node " + _this.getNodeId() + " should have been split before calling insertEntry"); } }

		 int execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook645__wrappee__base() throws DatabaseException { }

		 protected void hook645() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook645__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook646__wrappee__base() throws DatabaseException { }

		 protected void hook646() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook646__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook647__wrappee__base() throws DatabaseException { }

		 protected void hook647() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook647__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_deleteEntry {
		 IN_deleteEntry( IN _this, int index, boolean maybeValidate){ this._this=_this; this.index=index; this.maybeValidate=maybeValidate; }

		 protected IN _this;

		 protected int index;

		 protected boolean maybeValidate;

		 protected int oldLSNArraySize;

		 boolean execute__wrappee__base() throws DatabaseException { if (_this.nEntries == 0) { return false; } assert maybeValidate ? _this.validateSubtreeBeforeDelete(index) : true; if (index < _this.nEntries) { this.hook649(); for (int i=index; i < _this.nEntries - 1; i++) { _this.setEntryInternal(i + 1,i); } _this.clearEntry(_this.nEntries - 1); this.hook648(); _this.nEntries--; _this.setDirty(true); _this.setProhibitNextDelta(); this.hook616(); return true; } else { return false; } }

		 boolean execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook616__wrappee__base() throws DatabaseException { }

		 protected void hook616() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook616__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook648__wrappee__base() throws DatabaseException { }

		 protected void hook648() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook648__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook649__wrappee__base() throws DatabaseException { }

		 protected void hook649() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook649__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_validateSubtreeBeforeDelete {
		 IN_validateSubtreeBeforeDelete( IN _this, int index){ this._this=_this; this.index=index; }

		 protected IN _this;

		 protected int index;

		 protected boolean needToLatch;

		 protected Node child;

		 boolean execute__wrappee__base() throws DatabaseException { try { this.hook628(); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

		 boolean execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook628__wrappee__base() throws DatabaseException { this.hook629(); if (index >= _this.nEntries) { throw new ReturnBoolean(true); } else { child=_this.fetchTarget(index); throw new ReturnBoolean(child != null && child.isValidForDelete()); } }

		 protected void hook628() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook628__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook629__wrappee__base() throws DatabaseException { }

		 protected void hook629() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook629__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_splitInternal {
		 IN_splitInternal( IN _this, IN parent, int childIndex, int maxEntries, int splitIndex){ this._this=_this; this.parent=parent; this.childIndex=childIndex; this.maxEntries=maxEntries; this.splitIndex=splitIndex; }

		 protected IN _this;

		 protected IN parent;

		 protected int childIndex;

		 protected int maxEntries;

		 protected int splitIndex;

		 protected int idKeyIndex;

		 protected int low;

		 protected int high;

		 protected IN newSibling;

		 protected byte[] newIdKey;

		 protected long parentLsn;

		 protected long oldMemorySize;

		 protected int toIdx;

		 protected boolean deletedEntrySeen;

		 protected BINReference binRef;

		 protected byte[] thisKey;

		 protected int newSiblingNEntries;

		 protected EnvironmentImpl env;

		 protected LogManager logManager;

		 protected INList inMemoryINs;

		 protected long newSiblingLsn;

		 protected long myNewLsn;

		 protected boolean insertOk1;

		 protected boolean insertOk2;

		 protected long newSize;

		 void execute__wrappee__base() throws DatabaseException { if (_this.identifierKey == null) { throw new InconsistentNodeException("idkey is null"); } idKeyIndex=_this.findEntry(_this.identifierKey,false,false); if (splitIndex < 0) { splitIndex=_this.nEntries / 2; }
{ } newSibling=null; if (idKeyIndex < splitIndex) { low=splitIndex; high=_this.nEntries; } else { low=0; high=splitIndex; } newIdKey=_this.entryKeyVals[low]; parentLsn=DbLsn.NULL_LSN; newSibling=_this.createNewInstance(newIdKey,maxEntries,_this.level); this.hook631(); oldMemorySize=_this.inMemorySize; this.hook630(); }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook617__wrappee__base() throws DatabaseException { }

		 protected void hook617() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook617__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook630__wrappee__base() throws DatabaseException { toIdx=0; deletedEntrySeen=false; binRef=null; for (int i=low; i < high; i++) { thisKey=_this.entryKeyVals[i]; if (_this.isEntryPendingDeleted(i)) { if (!deletedEntrySeen) { deletedEntrySeen=true; binRef=new BINReference(newSibling.getNodeId(),_this.databaseImpl.getId(),newIdKey); } binRef.addDeletedKey(new Key(thisKey)); } newSibling.setEntry(toIdx++,_this.entryTargets[i],thisKey,_this.getLsn(i),_this.entryStates[i]); _this.clearEntry(i); } this.hook636(); newSiblingNEntries=(high - low); if (low == 0) { _this.shiftEntriesLeft(newSiblingNEntries); } newSibling.nEntries=toIdx; _this.nEntries-=newSiblingNEntries; _this.setDirty(true); _this.adjustCursors(newSibling,low,high); env=_this.databaseImpl.getDbEnvironment(); logManager=env.getLogManager(); inMemoryINs=env.getInMemoryINs(); newSiblingLsn=newSibling.logProvisional(logManager,parent); myNewLsn=_this.logProvisional(logManager,parent); if (low == 0) { if (childIndex == 0) { parent.updateEntryCompareKey(childIndex,newSibling,newSiblingLsn,newIdKey); } else { parent.updateEntry(childIndex,newSibling,newSiblingLsn); } insertOk1=parent.insertEntry(new ChildReference(_this,_this.entryKeyVals[0],myNewLsn)); assert insertOk1; } else { if (childIndex == 0) { parent.updateEntryCompareKey(childIndex,_this,myNewLsn,_this.entryKeyVals[0]); } else { parent.updateEntry(childIndex,_this,myNewLsn); } insertOk2=parent.insertEntry(new ChildReference(newSibling,newIdKey,newSiblingLsn)); assert insertOk2; } parentLsn=parent.log(logManager); if (parent.isRoot()) { parent.setDirty(true); } this.hook650(); inMemoryINs.add(newSibling); this.hook617(); }

		 protected void hook630() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook630__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook631__wrappee__base() throws DatabaseException { }

		 protected void hook631() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook631__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook636__wrappee__base() throws DatabaseException { }

		 protected void hook636() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook636__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook650__wrappee__base() throws DatabaseException { }

		 protected void hook650() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook650__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_verify {
		 IN_verify( IN _this, byte[] maxKey){ this._this=_this; this.maxKey=maxKey; }

		 protected IN _this;

		 protected byte[] maxKey;

		 protected boolean unlatchThis;

		 protected Comparator userCompareToFcn;

		 protected byte[] key1;

		 protected byte[] key2;

		 protected int s;

		 protected boolean inconsistent;

		 void execute__wrappee__base() throws DatabaseException { try { this.hook632(); userCompareToFcn=(_this.databaseImpl == null ? null : _this.getKeyComparator()); key1=null; for (int i=1; i < _this.nEntries; i++) { key1=_this.entryKeyVals[i]; key2=_this.entryKeyVals[i - 1]; s=Key.compareKeys(key1,key2,userCompareToFcn); if (s <= 0) { throw new InconsistentNodeException("IN " + _this.getNodeId() + " key "+ (i - 1)+ " ("+ Key.dumpString(key2,0)+ ") and "+ i+ " ("+ Key.dumpString(key1,0)+ ") are out of order"); } } inconsistent=false; if (maxKey != null && key1 != null) { if (Key.compareKeys(key1,maxKey,userCompareToFcn) >= 0) { inconsistent=true; } } if (inconsistent) { throw new InconsistentNodeException("IN " + _this.getNodeId() + " has entry larger than next entry in parent."); } } catch ( DatabaseException DE) { DE.printStackTrace(System.out); } finally { this.hook633(); } }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook632__wrappee__base() throws DatabaseException { }

		 protected void hook632() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook632__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook633__wrappee__base() throws DatabaseException { }

		 protected void hook633() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook633__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_isValidForDelete {
		 IN_isValidForDelete( IN _this){ this._this=_this; }

		 protected IN _this;

		 protected boolean needToLatch;

		 protected Node child;

		 boolean execute__wrappee__base() throws DatabaseException { try { this.hook634(); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

		 boolean execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook634__wrappee__base() throws DatabaseException { this.hook635(); if (_this.nEntries > 1) { throw new ReturnBoolean(false); } else if (_this.nEntries == 1) { child=_this.fetchTarget(0); throw new ReturnBoolean(child != null && child.isValidForDelete()); } else { throw new ReturnBoolean(true); } }

		 protected void hook634() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook634__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook635__wrappee__base() throws DatabaseException { }

		 protected void hook635() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook635__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_trackProvisionalObsolete {
		 IN_trackProvisionalObsolete( IN _this, IN child, long obsoleteLsn1, long obsoleteLsn2){ this._this=_this; this.child=child; this.obsoleteLsn1=obsoleteLsn1; this.obsoleteLsn2=obsoleteLsn2; }

		 protected IN _this;

		 protected IN child;

		 protected long obsoleteLsn1;

		 protected long obsoleteLsn2;

		 protected int memDelta;

		 protected int childMemDelta;

		 void execute__wrappee__base(){ memDelta=0; if (child.provisionalObsolete != null) { this.hook652(); if (_this.provisionalObsolete != null) { _this.provisionalObsolete.addAll(child.provisionalObsolete); } else { _this.provisionalObsolete=child.provisionalObsolete; } child.provisionalObsolete=null; this.hook651(); } if (obsoleteLsn1 != DbLsn.NULL_LSN || obsoleteLsn2 != DbLsn.NULL_LSN) { if (_this.provisionalObsolete == null) { _this.provisionalObsolete=new ArrayList(); } if (obsoleteLsn1 != DbLsn.NULL_LSN) { _this.provisionalObsolete.add(new Long(obsoleteLsn1)); this.hook653(); } if (obsoleteLsn2 != DbLsn.NULL_LSN) { _this.provisionalObsolete.add(new Long(obsoleteLsn2)); this.hook654(); } } }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook651__wrappee__base(){ }

		 protected void hook651(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook651__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook652__wrappee__base(){ }

		 protected void hook652(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook652__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook653__wrappee__base(){ }

		 protected void hook653(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook653__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook654__wrappee__base(){ }

		 protected void hook654(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook654__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  IN_flushProvisionalObsolete {
		 IN_flushProvisionalObsolete( IN _this, LogManager logManager){ this._this=_this; this.logManager=logManager; }

		 protected IN _this;

		 protected LogManager logManager;

		 protected int memDelta;

		 void execute__wrappee__base() throws DatabaseException { if (_this.provisionalObsolete != null) { this.hook656(); logManager.countObsoleteINs(_this.provisionalObsolete); _this.provisionalObsolete=null; this.hook655(); } }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook655__wrappee__base() throws DatabaseException { }

		 protected void hook655() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook655__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook656__wrappee__base() throws DatabaseException { }

		 protected void hook656() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook656__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 protected void init__wrappee__base( DatabaseImpl db, byte[] identifierKey, int initialCapacity, int level){ setDatabase(db); EnvironmentImpl env=(databaseImpl == null) ? null : databaseImpl.getDbEnvironment(); this.hook618(env); generation=0; dirty=false; nEntries=0; this.identifierKey=identifierKey; entryTargets=new Node[initialCapacity]; entryKeyVals=new byte[initialCapacity][]; baseFileNumber=-1; entryLsnByteArray=new byte[initialCapacity << 2]; entryLsnLongArray=null; entryStates=new byte[initialCapacity]; isRoot=false; this.level=level; }

	 protected void init( DatabaseImpl db, byte[] identifierKey, int initialCapacity, int level){ t.in(Thread.currentThread().getStackTrace()[1].toString());	init__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private long getEqualityKey__wrappee__base(){ int hash=System.identityHashCode(this); long hash2=(((long)hash) << 32) | hash; return hash2 ^ getNodeId(); }

	 private long getEqualityKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEqualityKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean equals__wrappee__base( Object obj){ if (!(obj instanceof IN)) { return false; } IN in=(IN)obj; return (this.getEqualityKey() == in.getEqualityKey()); }

	 public boolean equals( Object obj){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ return (int)getEqualityKey(); }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int compareTo__wrappee__base( Object o){ if (o == null) { throw new NullPointerException(); } IN argIN=(IN)o; long argEqualityKey=argIN.getEqualityKey(); long myEqualityKey=getEqualityKey(); if (myEqualityKey < argEqualityKey) { return -1; } else if (myEqualityKey > argEqualityKey) { return 1; } else { return 0; } }

	 public int compareTo( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected IN createNewInstance__wrappee__base( byte[] identifierKey, int maxEntries, int level){ return new IN(databaseImpl,identifierKey,maxEntries,level); }

	 protected IN createNewInstance( byte[] identifierKey, int maxEntries, int level){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createNewInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postFetchInit__wrappee__base( DatabaseImpl db, long sourceLsn) throws DatabaseException { setDatabase(db); setLastFullLsn(sourceLsn); EnvironmentImpl env=db.getDbEnvironment(); this.hook637(); env.getInMemoryINs().add(this); }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postFetchInit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postRecoveryInit__wrappee__base( DatabaseImpl db, long sourceLsn){ setDatabase(db); setLastFullLsn(sourceLsn); }

	 public void postRecoveryInit( DatabaseImpl db, long sourceLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	postRecoveryInit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setLastFullLsn__wrappee__base( long lsn){ lastFullVersion=lsn; }

	 void setLastFullLsn( long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLastFullLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLastFullVersion__wrappee__base(){ return lastFullVersion; }

	 public long getLastFullVersion(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastFullVersion__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void latch__wrappee__base( boolean updateGeneration) throws DatabaseException { if (updateGeneration) { setGeneration(); } }

	 public void latch( boolean updateGeneration) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	latch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean latchNoWait__wrappee__base( boolean updateGeneration) throws DatabaseException { try { this.hook619(updateGeneration); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 public boolean latchNoWait( boolean updateGeneration) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	latchNoWait__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getGeneration__wrappee__base(){ return generation; }

	 public long getGeneration(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getGeneration__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setGeneration__wrappee__base(){ generation=Generation.getNextGeneration(); }

	 public void setGeneration(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setGeneration__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setGeneration__wrappee__base( long newGeneration){ generation=newGeneration; }

	 public void setGeneration( long newGeneration){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setGeneration__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLevel__wrappee__base(){ return level; }

	 public int getLevel(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLevel__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int generateLevel__wrappee__base( DatabaseId dbId, int newLevel){ if (dbId.equals(DbTree.ID_DB_ID)) { return newLevel | DBMAP_LEVEL; } else { return newLevel | MAIN_LEVEL; } }

	 protected int generateLevel( DatabaseId dbId, int newLevel){ t.in(Thread.currentThread().getStackTrace()[1].toString());	generateLevel__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getDirty__wrappee__base(){ return dirty; }

	 public boolean getDirty(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDirty__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDirty__wrappee__base( boolean dirty){ this.dirty=dirty; }

	 public void setDirty( boolean dirty){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDirty__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isRoot__wrappee__base(){ return isRoot; }

	 public boolean isRoot(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isDbRoot__wrappee__base(){ return isRoot; }

	 public boolean isDbRoot(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDbRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setIsRoot__wrappee__base( boolean isRoot){ this.isRoot=isRoot; setDirty(true); }

	 void setIsRoot( boolean isRoot){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setIsRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getIdentifierKey__wrappee__base(){ return identifierKey; }

	 public byte[] getIdentifierKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getIdentifierKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setIdentifierKey__wrappee__base( byte[] key){ identifierKey=key; setDirty(true); }

	 void setIdentifierKey( byte[] key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setIdentifierKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getChildKey__wrappee__base( IN child) throws DatabaseException { return child.getIdentifierKey(); }

	 public byte[] getChildKey( IN child) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getChildKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] selectKey__wrappee__base( byte[] mainTreeKey, byte[] dupTreeKey){ return mainTreeKey; }

	 public byte[] selectKey( byte[] mainTreeKey, byte[] dupTreeKey){ t.in(Thread.currentThread().getStackTrace()[1].toString());	selectKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupKey__wrappee__base() throws DatabaseException { throw new DatabaseException(shortClassName() + ".getDupKey() called"); }

	 public byte[] getDupKey() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupTreeKey__wrappee__base(){ return null; }

	 public byte[] getDupTreeKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupTreeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getMainTreeKey__wrappee__base(){ return getIdentifierKey(); }

	 public byte[] getMainTreeKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMainTreeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseImpl getDatabase__wrappee__base(){ return databaseImpl; }

	 public DatabaseImpl getDatabase(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDatabase__wrappee__base( DatabaseImpl db){ databaseImpl=db; }

	 public void setDatabase( DatabaseImpl db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDatabaseId__wrappee__base(){ return databaseImpl.getId(); }

	 public DatabaseId getDatabaseId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void setEntryInternal__wrappee__base( int from, int to){ entryTargets[to]=entryTargets[from]; entryKeyVals[to]=entryKeyVals[from]; entryStates[to]=entryStates[from]; if (entryLsnLongArray == null) { int fromOff=from << 2; int toOff=to << 2; entryLsnByteArray[toOff++]=entryLsnByteArray[fromOff++]; entryLsnByteArray[toOff++]=entryLsnByteArray[fromOff++]; entryLsnByteArray[toOff++]=entryLsnByteArray[fromOff++]; entryLsnByteArray[toOff]=entryLsnByteArray[fromOff]; } else { entryLsnLongArray[to]=entryLsnLongArray[from]; } }

	 private void setEntryInternal( int from, int to){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setEntryInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void clearEntry__wrappee__base( int idx){ entryTargets[idx]=null; entryKeyVals[idx]=null; setLsnElement(idx,DbLsn.NULL_LSN); entryStates[idx]=0; }

	 private void clearEntry( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base( int idx){ return entryKeyVals[idx]; }

	 public byte[] getKey( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void setKey__wrappee__base( int idx, byte[] keyVal){ entryKeyVals[idx]=keyVal; entryStates[idx]|=DIRTY_BIT; }

	 private void setKey( int idx, byte[] keyVal){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getMigrate__wrappee__base( int idx){ return (entryStates[idx] & MIGRATE_BIT) != 0; }

	 public boolean getMigrate( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMigrate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setMigrate__wrappee__base( int idx, boolean migrate){ if (migrate) { entryStates[idx]|=MIGRATE_BIT; } else { entryStates[idx]&=CLEAR_MIGRATE_BIT; } }

	 public void setMigrate( int idx, boolean migrate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setMigrate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte getState__wrappee__base( int idx){ return entryStates[idx]; }

	 public byte getState( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Node getTarget__wrappee__base( int idx){ return entryTargets[idx]; }

	 public Node getTarget( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setTarget__wrappee__base( int idx, Node target){ entryTargets[idx]=target; }

	 void setTarget( int idx, Node target){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLsn__wrappee__base( int idx){ if (entryLsnLongArray == null) { int offset=idx << 2; int fileOffset=getFileOffset(offset); if (fileOffset == -1) { return DbLsn.NULL_LSN; } else { return DbLsn.makeLsn((long)(baseFileNumber + getFileNumberOffset(offset)),fileOffset); } } else { return entryLsnLongArray[idx]; } }

	 public long getLsn( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLsn__wrappee__base( int idx, long lsn){ new IN_setLsn(this,idx,lsn).execute(); }

	 public void setLsn( int idx, long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long\[\] getEntryLsnLongArray__wrappee__base(){ return entryLsnLongArray; }

	 long[] getEntryLsnLongArray(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEntryLsnLongArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 byte\[\] getEntryLsnByteArray__wrappee__base(){ return entryLsnByteArray; }

	 byte[] getEntryLsnByteArray(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEntryLsnByteArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void initEntryLsn__wrappee__base( int capacity){ entryLsnLongArray=null; entryLsnByteArray=new byte[capacity << 2]; baseFileNumber=-1; }

	 void initEntryLsn( int capacity){ t.in(Thread.currentThread().getStackTrace()[1].toString());	initEntryLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setLsnElement__wrappee__base( int idx, long value){ int offset=idx << 2; if (entryLsnLongArray != null) { entryLsnLongArray[idx]=value; return; } if (value == DbLsn.NULL_LSN) { setFileNumberOffset(offset,(byte)0); setFileOffset(offset,-1); return; } long thisFileNumber=DbLsn.getFileNumber(value); if (baseFileNumber == -1) { baseFileNumber=thisFileNumber; setFileNumberOffset(offset,(byte)0); } else { if (thisFileNumber < baseFileNumber) { if (!adjustFileNumbers(thisFileNumber)) { mutateToLongArray(idx,value); return; } baseFileNumber=thisFileNumber; } long fileNumberDifference=thisFileNumber - baseFileNumber; if (fileNumberDifference > Byte.MAX_VALUE) { mutateToLongArray(idx,value); return; } setFileNumberOffset(offset,(byte)(thisFileNumber - baseFileNumber)); } int fileOffset=(int)DbLsn.getFileOffset(value); if (fileOffset > MAX_FILE_OFFSET) { mutateToLongArray(idx,value); return; } setFileOffset(offset,fileOffset); }

	 void setLsnElement( int idx, long value){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLsnElement__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void mutateToLongArray__wrappee__base( int idx, long value){ int nElts=entryLsnByteArray.length >> 2; long[] newArr=new long[nElts]; for (int i=0; i < nElts; i++) { newArr[i]=getLsn(i); } newArr[idx]=value; entryLsnLongArray=newArr; entryLsnByteArray=null; }

	 private void mutateToLongArray( int idx, long value){ t.in(Thread.currentThread().getStackTrace()[1].toString());	mutateToLongArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean adjustFileNumbers__wrappee__base( long newBaseFileNumber){ long oldBaseFileNumber=baseFileNumber; for (int i=0; i < entryLsnByteArray.length; i+=BYTES_PER_LSN_ENTRY) { if (getFileOffset(i) == -1) { continue; } long curEntryFileNumber=oldBaseFileNumber + getFileNumberOffset(i); long newCurEntryFileNumberOffset=(curEntryFileNumber - newBaseFileNumber); if (newCurEntryFileNumberOffset > Byte.MAX_VALUE) { long undoOffset=oldBaseFileNumber - newBaseFileNumber; for (int j=i - BYTES_PER_LSN_ENTRY; j >= 0; j-=BYTES_PER_LSN_ENTRY) { if (getFileOffset(j) == -1) { continue; } setFileNumberOffset(j,(byte)(getFileNumberOffset(j) - undoOffset)); } return false; } setFileNumberOffset(i,(byte)newCurEntryFileNumberOffset); } return true; }

	 private boolean adjustFileNumbers( long newBaseFileNumber){ t.in(Thread.currentThread().getStackTrace()[1].toString());	adjustFileNumbers__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void setFileNumberOffset__wrappee__base( int offset, byte fileNumberOffset){ entryLsnByteArray[offset]=fileNumberOffset; }

	 private void setFileNumberOffset( int offset, byte fileNumberOffset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setFileNumberOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private byte getFileNumberOffset__wrappee__base( int offset){ return entryLsnByteArray[offset]; }

	 private byte getFileNumberOffset( int offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileNumberOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void setFileOffset__wrappee__base( int offset, int fileOffset){ put3ByteInt(offset + 1,fileOffset); }

	 private void setFileOffset( int offset, int fileOffset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setFileOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private int getFileOffset__wrappee__base( int offset){ return get3ByteInt(offset + 1); }

	 private int getFileOffset( int offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void put3ByteInt__wrappee__base( int offset, int value){ entryLsnByteArray[offset++]=(byte)(value >>> 0); entryLsnByteArray[offset++]=(byte)(value >>> 8); entryLsnByteArray[offset]=(byte)(value >>> 16); }

	 private void put3ByteInt( int offset, int value){ t.in(Thread.currentThread().getStackTrace()[1].toString());	put3ByteInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private int get3ByteInt__wrappee__base( int offset){ int ret=(entryLsnByteArray[offset++] & 0xFF) << 0; ret+=(entryLsnByteArray[offset++] & 0xFF) << 8; ret+=(entryLsnByteArray[offset] & 0xFF) << 16; if (ret == THREE_BYTE_NEGATIVE_ONE) { ret=-1; } return ret; }

	 private int get3ByteInt( int offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	get3ByteInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isEntryPendingDeleted__wrappee__base( int idx){ return ((entryStates[idx] & PENDING_DELETED_BIT) != 0); }

	 public boolean isEntryPendingDeleted( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isEntryPendingDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setPendingDeleted__wrappee__base( int idx){ entryStates[idx]|=PENDING_DELETED_BIT; entryStates[idx]|=DIRTY_BIT; }

	 public void setPendingDeleted( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setPendingDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void clearPendingDeleted__wrappee__base( int idx){ entryStates[idx]&=CLEAR_PENDING_DELETED_BIT; entryStates[idx]|=DIRTY_BIT; }

	 public void clearPendingDeleted( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearPendingDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isEntryKnownDeleted__wrappee__base( int idx){ return ((entryStates[idx] & KNOWN_DELETED_BIT) != 0); }

	 public boolean isEntryKnownDeleted( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isEntryKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setKnownDeleted__wrappee__base( int idx){ entryStates[idx]|=KNOWN_DELETED_BIT; entryStates[idx]|=DIRTY_BIT; }

	 void setKnownDeleted( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void clearKnownDeleted__wrappee__base( int idx){ entryStates[idx]&=CLEAR_KNOWN_DELETED_BIT; entryStates[idx]|=DIRTY_BIT; }

	 void clearKnownDeleted( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isDirty__wrappee__base( int idx){ return ((entryStates[idx] & DIRTY_BIT) != 0); }

	 boolean isDirty( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDirty__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNEntries__wrappee__base(){ return nEntries; }

	 public int getNEntries(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static boolean isStateKnownDeleted__wrappee__base( byte state){ return ((state & KNOWN_DELETED_BIT) != 0); }

	 static boolean isStateKnownDeleted( byte state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isStateKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static boolean isStatePendingDeleted__wrappee__base( byte state){ return ((state & KNOWN_DELETED_BIT) != 0); }

	 static boolean isStatePendingDeleted( byte state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isStatePendingDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getMaxEntries__wrappee__base(){ return entryTargets.length; }

	 int getMaxEntries(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMaxEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Node fetchTarget__wrappee__base( int idx) throws DatabaseException { if (entryTargets[idx] == null) { long lsn=getLsn(idx); if (lsn == DbLsn.NULL_LSN) { if (!isEntryKnownDeleted(idx)) { throw new DatabaseException(makeFetchErrorMsg("NULL_LSN without KnownDeleted",this,lsn,entryStates[idx])); } } else { try { EnvironmentImpl env=databaseImpl.getDbEnvironment(); Node node=(Node)env.getLogManager().get(lsn); node.postFetchInit(databaseImpl,lsn); entryTargets[idx]=node; this.hook638(node); } catch ( LogFileNotFoundException LNFE) { if (!isEntryKnownDeleted(idx) && !isEntryPendingDeleted(idx)) { throw new DatabaseException(makeFetchErrorMsg(LNFE.toString(),this,lsn,entryStates[idx])); } }
catch ( Exception e) { throw new DatabaseException(makeFetchErrorMsg(e.toString(),this,lsn,entryStates[idx]),e); } } } return entryTargets[idx]; }

	 public Node fetchTarget( int idx) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	fetchTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static String makeFetchErrorMsg__wrappee__base( String msg, IN in, long lsn, byte state){ StringBuffer sb=new StringBuffer(); sb.append("fetchTarget of "); if (lsn == DbLsn.NULL_LSN) { sb.append("null lsn"); } else { sb.append(DbLsn.getNoFormatString(lsn)); } if (in != null) { sb.append(" IN=").append(in.getNodeId()); } sb.append(" state=").append(state); sb.append(" ").append(msg); return sb.toString(); }

	 static String makeFetchErrorMsg( String msg, IN in, long lsn, byte state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeFetchErrorMsg__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setEntry__wrappee__base( int idx, Node target, byte[] keyVal, long lsn, byte state){ new IN_setEntry(this,idx,target,keyVal,lsn,state).execute(); }

	 public void setEntry( int idx, Node target, byte[] keyVal, long lsn, byte state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateEntry__wrappee__base( int idx, Node node){ new IN_updateEntry(this,idx,node).execute(); }

	 public void updateEntry( int idx, Node node){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateEntry__wrappee__base( int idx, Node node, long lsn){ new IN_updateEntry2(this,idx,node,lsn).execute(); }

	 public void updateEntry( int idx, Node node, long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateEntry__wrappee__base( int idx, Node node, long lsn, byte[] key){ new IN_updateEntry3(this,idx,node,lsn,key).execute(); }

	 public void updateEntry( int idx, Node node, long lsn, byte[] key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateEntry__wrappee__base( int idx, long lsn){ setLsn(idx,lsn); setDirty(true); }

	 public void updateEntry( int idx, long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateEntry__wrappee__base( int idx, long lsn, byte state){ setLsn(idx,lsn); entryStates[idx]=state; setDirty(true); }

	 public void updateEntry( int idx, long lsn, byte state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateEntry__wrappee__base( int idx, long lsn, long oldLNSize, long newLNSize){ setLsn(idx,lsn); setDirty(true); }

	 public void updateEntry( int idx, long lsn, long oldLNSize, long newLNSize){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void updateEntryCompareKey__wrappee__base( int idx, Node node, long lsn, byte[] key){ new IN_updateEntryCompareKey(this,idx,node,lsn,key).execute(); }

	 private void updateEntryCompareKey( int idx, Node node, long lsn, byte[] key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateEntryCompareKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isKeyInBounds__wrappee__base( byte[] keyVal){ if (nEntries < 2) { return false; } Comparator userCompareToFcn=getKeyComparator(); int cmp; byte[] myKey; myKey=entryKeyVals[0]; cmp=Key.compareKeys(keyVal,myKey,userCompareToFcn); if (cmp < 0) { return false; } myKey=entryKeyVals[nEntries - 1]; cmp=Key.compareKeys(keyVal,myKey,userCompareToFcn); if (cmp > 0) { return false; } return true; }

	 public boolean isKeyInBounds( byte[] keyVal){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isKeyInBounds__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int findEntry__wrappee__base( byte[] key, boolean indicateIfDuplicate, boolean exact){ int high=nEntries - 1; int low=0; int middle=0; Comparator userCompareToFcn=getKeyComparator(); boolean entryZeroSpecialCompare=entryZeroKeyComparesLow() && !exact && !indicateIfDuplicate; assert nEntries >= 0; while (low <= high) { middle=(high + low) / 2; int s; byte[] middleKey=null; if (middle == 0 && entryZeroSpecialCompare) { s=1; } else { middleKey=entryKeyVals[middle]; s=Key.compareKeys(key,middleKey,userCompareToFcn); } if (s < 0) { high=middle - 1; } else if (s > 0) { low=middle + 1; } else { int ret; if (indicateIfDuplicate) { ret=middle | EXACT_MATCH; } else { ret=middle; } if ((ret >= 0) && exact && isEntryKnownDeleted(ret & 0xffff)) { return -1; } else { return ret; } } } if (exact) { return -1; } else { return high; } }

	 public int findEntry( byte[] key, boolean indicateIfDuplicate, boolean exact){ t.in(Thread.currentThread().getStackTrace()[1].toString());	findEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean insertEntry__wrappee__base( ChildReference entry) throws DatabaseException { return (insertEntry1(entry) & INSERT_SUCCESS) != 0; }

	 public boolean insertEntry( ChildReference entry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	insertEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int insertEntry1__wrappee__base( ChildReference entry) throws DatabaseException { return new IN_insertEntry1(this,entry).execute(); }

	 public int insertEntry1( ChildReference entry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	insertEntry1__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean deleteEntry__wrappee__base( byte[] key, boolean maybeValidate) throws DatabaseException { if (nEntries == 0) { return false; } int index=findEntry(key,false,true); if (index < 0) { return false; } return deleteEntry(index,maybeValidate); }

	 boolean deleteEntry( byte[] key, boolean maybeValidate) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	deleteEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean deleteEntry__wrappee__base( int index, boolean maybeValidate) throws DatabaseException { return new IN_deleteEntry(this,index,maybeValidate).execute(); }

	 public boolean deleteEntry( int index, boolean maybeValidate) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	deleteEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setProhibitNextDelta__wrappee__base(){ }

	 public void setProhibitNextDelta(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setProhibitNextDelta__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean compress__wrappee__base( BINReference binRef, boolean canFetch) throws DatabaseException { return false; }

	 public boolean compress( BINReference binRef, boolean canFetch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	compress__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isCompressible__wrappee__base(){ return false; }

	 public boolean isCompressible(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isCompressible__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean validateSubtreeBeforeDelete__wrappee__base( int index) throws DatabaseException { return new IN_validateSubtreeBeforeDelete(this,index).execute(); }

	 boolean validateSubtreeBeforeDelete( int index) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateSubtreeBeforeDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean needsSplitting__wrappee__base(){ if ((entryTargets.length - nEntries) < 1) { return true; } else { return false; } }

	 public boolean needsSplitting(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	needsSplitting__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean entryZeroKeyComparesLow__wrappee__base(){ return true; }

	 boolean entryZeroKeyComparesLow(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryZeroKeyComparesLow__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void split__wrappee__base( IN parent, int childIndex, int maxEntries) throws DatabaseException { splitInternal(parent,childIndex,maxEntries,-1); }

	 void split( IN parent, int childIndex, int maxEntries) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	split__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void splitInternal__wrappee__base( IN parent, int childIndex, int maxEntries, int splitIndex) throws DatabaseException { new IN_splitInternal(this,parent,childIndex,maxEntries,splitIndex).execute(); }

	 protected void splitInternal( IN parent, int childIndex, int maxEntries, int splitIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	splitInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void splitSpecial__wrappee__base( IN parent, int parentIndex, int maxEntriesPerNode, byte[] key, boolean leftSide) throws DatabaseException { int index=findEntry(key,false,false); if (leftSide && index == 0) { splitInternal(parent,parentIndex,maxEntriesPerNode,1); } else if (!leftSide && index == (nEntries - 1)) { splitInternal(parent,parentIndex,maxEntriesPerNode,nEntries - 1); } else { split(parent,parentIndex,maxEntriesPerNode); } }

	 void splitSpecial( IN parent, int parentIndex, int maxEntriesPerNode, byte[] key, boolean leftSide) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	splitSpecial__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void adjustCursors__wrappee__base( IN newSibling, int newSiblingLow, int newSiblingHigh){ }

	 void adjustCursors( IN newSibling, int newSiblingLow, int newSiblingHigh){ t.in(Thread.currentThread().getStackTrace()[1].toString());	adjustCursors__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void adjustCursorsForInsert__wrappee__base( int insertIndex){ }

	 void adjustCursorsForInsert( int insertIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	adjustCursorsForInsert__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Comparator getKeyComparator__wrappee__base(){ return databaseImpl.getBtreeComparator(); }

	 public Comparator getKeyComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKeyComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void shiftEntriesRight__wrappee__base( int index){ for (int i=nEntries; i > index; i--) { setEntryInternal(i - 1,i); } clearEntry(index); setDirty(true); }

	 private void shiftEntriesRight( int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shiftEntriesRight__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void shiftEntriesLeft__wrappee__base( int byHowMuch){ for (int i=0; i < nEntries - byHowMuch; i++) { setEntryInternal(i + byHowMuch,i); } for (int i=nEntries - byHowMuch; i < nEntries; i++) { clearEntry(i); } setDirty(true); }

	 private void shiftEntriesLeft( int byHowMuch){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shiftEntriesLeft__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void verify__wrappee__base( byte[] maxKey) throws DatabaseException { new IN_verify(this,maxKey).execute(); }

	 public void verify( byte[] maxKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	verify__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void rebuildINList__wrappee__base( INList inList) throws DatabaseException { inList.add(this); for (int i=0; i < nEntries; i++) { Node n=getTarget(i); if (n != null) { n.rebuildINList(inList); } } }

	 void rebuildINList( INList inList) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	rebuildINList__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void accountForSubtreeRemoval__wrappee__base( INList inList, UtilizationTracker tracker) throws DatabaseException { if (nEntries > 1) { throw new DatabaseException("Found non-deletable IN " + getNodeId() + " while flushing INList. nEntries = "+ nEntries); } inList.removeLatchAlreadyHeld(this); if (lastFullVersion != DbLsn.NULL_LSN) { tracker.countObsoleteNode(lastFullVersion,getLogType()); } for (int i=0; i < nEntries; i++) { Node n=fetchTarget(i); if (n != null) { n.accountForSubtreeRemoval(inList,tracker); } } }

	 void accountForSubtreeRemoval( INList inList, UtilizationTracker tracker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	accountForSubtreeRemoval__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isValidForDelete__wrappee__base() throws DatabaseException { return new IN_isValidForDelete(this).execute(); }

	 boolean isValidForDelete() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	isValidForDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void findParent__wrappee__base( Tree.SearchType searchType, long targetNodeId, boolean targetContainsDuplicates, boolean targetIsRoot, byte[] targetMainTreeKey, byte[] targetDupTreeKey, SearchResult result, boolean requireExactMatch, boolean updateGeneration, int targetLevel, List trackingList, boolean doFetch) throws DatabaseException { if (getNodeId() == targetNodeId) { this.hook620(); result.exactParentFound=false; result.keepSearching=false; result.parent=null; return; } if (getNEntries() == 0) { result.keepSearching=false; result.exactParentFound=false; if (requireExactMatch) { this.hook621(); result.parent=null; } else { result.parent=this; result.index=-1; } return; } else { if (searchType == Tree.SearchType.NORMAL) { result.index=findEntry(selectKey(targetMainTreeKey,targetDupTreeKey),false,false); } else if (searchType == Tree.SearchType.LEFT) { result.index=0; } else if (searchType == Tree.SearchType.RIGHT) { result.index=nEntries - 1; } else { throw new IllegalArgumentException("Invalid value of searchType: " + searchType); } if (result.index < 0) { result.keepSearching=false; result.exactParentFound=false; if (requireExactMatch) { this.hook622(); result.parent=null; } else { result.parent=this; } return; } Node child=null; boolean isDeleted=false; if (isEntryKnownDeleted(result.index)) { isDeleted=true; } else if (doFetch) { child=fetchTarget(result.index); if (child == null) { isDeleted=true; } } else { child=getTarget(result.index); } if (isDeleted) { result.exactParentFound=false; result.keepSearching=false; if (requireExactMatch) { result.parent=null; this.hook623(); } else { result.parent=this; } return; } if (targetLevel >= 0 && level == targetLevel + 1) { result.exactParentFound=true; result.parent=this; result.keepSearching=false; return; } if (child == null) { assert !doFetch; result.keepSearching=false; result.exactParentFound=false; result.parent=this; result.childNotResident=true; return; } long childLsn=getLsn(result.index); if (child.isSoughtNode(targetNodeId,updateGeneration)) { result.exactParentFound=true; result.parent=this; result.keepSearching=false; return; } else { descendOnParentSearch(result,targetContainsDuplicates,targetIsRoot,targetNodeId,child,requireExactMatch); if (trackingList != null) { if ((result.parent != this) && (result.parent != null)) { trackingList.add(new TrackingInfo(childLsn,child.getNodeId())); } } return; } } }

	 void findParent( Tree.SearchType searchType, long targetNodeId, boolean targetContainsDuplicates, boolean targetIsRoot, byte[] targetMainTreeKey, byte[] targetDupTreeKey, SearchResult result, boolean requireExactMatch, boolean updateGeneration, int targetLevel, List trackingList, boolean doFetch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	findParent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void descendOnParentSearch__wrappee__base( SearchResult result, boolean targetContainsDuplicates, boolean targetIsRoot, long targetNodeId, Node child, boolean requireExactMatch) throws DatabaseException { if (child.canBeAncestor(targetContainsDuplicates)) { this.hook624(); result.parent=(IN)child; } else { this.hook625(child); result.exactParentFound=false; result.keepSearching=false; if (requireExactMatch) { this.hook626(); result.parent=null; } else { result.parent=this; } } }

	 protected void descendOnParentSearch( SearchResult result, boolean targetContainsDuplicates, boolean targetIsRoot, long targetNodeId, Node child, boolean requireExactMatch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	descendOnParentSearch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isSoughtNode__wrappee__base( long nid, boolean updateGeneration) throws DatabaseException { latch(updateGeneration); if (getNodeId() == nid) { this.hook627(); return true; } else { return false; } }

	 protected boolean isSoughtNode( long nid, boolean updateGeneration) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	isSoughtNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean canBeAncestor__wrappee__base( boolean targetContainsDuplicates){ return true; }

	 protected boolean canBeAncestor( boolean targetContainsDuplicates){ t.in(Thread.currentThread().getStackTrace()[1].toString());	canBeAncestor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean hasNonLNChildren__wrappee__base(){ return hasResidentChildren(); }

	 boolean hasNonLNChildren(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasNonLNChildren__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean hasResidentChildren__wrappee__base(){ for (int i=0; i < getNEntries(); i++) { if (getTarget(i) != null) { return true; } } return false; }

	 private boolean hasResidentChildren(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasResidentChildren__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void accumulateStats__wrappee__base( TreeWalkerStatsAccumulator acc){ acc.processIN(this,new Long(getNodeId()),getLevel()); }

	 void accumulateStats( TreeWalkerStatsAccumulator acc){ t.in(Thread.currentThread().getStackTrace()[1].toString());	accumulateStats__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long log__wrappee__base( LogManager logManager) throws DatabaseException { return logInternal(logManager,false,false,false,null); }

	 public long log( LogManager logManager) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	log__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long log__wrappee__base( LogManager logManager, boolean allowDeltas, boolean proactiveMigration) throws DatabaseException { return logInternal(logManager,allowDeltas,false,proactiveMigration,null); }

	 public long log( LogManager logManager, boolean allowDeltas, boolean proactiveMigration) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	log__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long logProvisional__wrappee__base( LogManager logManager, IN parent) throws DatabaseException { return logInternal(logManager,false,true,false,parent); }

	 public long logProvisional( LogManager logManager, IN parent) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	logProvisional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long log__wrappee__base( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { return logInternal(logManager,allowDeltas,isProvisional,proactiveMigration,parent); }

	 public long log( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	log__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected long logInternal__wrappee__base( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { long lsn=logManager.log(new INLogEntry(this),isProvisional,isProvisional ? DbLsn.NULL_LSN : lastFullVersion); if (isProvisional) { if (parent != null) { parent.trackProvisionalObsolete(this,lastFullVersion,DbLsn.NULL_LSN); } } else { flushProvisionalObsolete(logManager); } setLastFullLsn(lsn); setDirty(false); return lsn; }

	 protected long logInternal( LogManager logManager, boolean allowDeltas, boolean isProvisional, boolean proactiveMigration, IN parent) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	logInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void trackProvisionalObsolete__wrappee__base( IN child, long obsoleteLsn1, long obsoleteLsn2){ new IN_trackProvisionalObsolete(this,child,obsoleteLsn1,obsoleteLsn2).execute(); }

	 void trackProvisionalObsolete( IN child, long obsoleteLsn1, long obsoleteLsn2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	trackProvisionalObsolete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void flushProvisionalObsolete__wrappee__base( LogManager logManager) throws DatabaseException { new IN_flushProvisionalObsolete(this,logManager).execute(); }

	 void flushProvisionalObsolete( LogManager logManager) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	flushProvisionalObsolete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_IN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=super.getLogSize(); size+=LogUtils.getByteArrayLogSize(identifierKey); size+=LogUtils.getBooleanLogSize(); size+=LogUtils.INT_BYTES; size+=LogUtils.INT_BYTES; size+=LogUtils.INT_BYTES; size+=LogUtils.getBooleanLogSize(); boolean compactLsnsRep=(entryLsnLongArray == null); if (compactLsnsRep) { size+=LogUtils.INT_BYTES; } for (int i=0; i < nEntries; i++) { size+=LogUtils.getByteArrayLogSize(entryKeyVals[i]) + (compactLsnsRep ? LogUtils.INT_BYTES : LogUtils.getLongLogSize()) + 1; } return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ super.writeToLog(logBuffer); LogUtils.writeByteArray(logBuffer,identifierKey); LogUtils.writeBoolean(logBuffer,isRoot); LogUtils.writeInt(logBuffer,nEntries); LogUtils.writeInt(logBuffer,level); LogUtils.writeInt(logBuffer,entryTargets.length); boolean compactLsnsRep=(entryLsnLongArray == null); LogUtils.writeBoolean(logBuffer,compactLsnsRep); if (compactLsnsRep) { LogUtils.writeInt(logBuffer,(int)baseFileNumber); } for (int i=0; i < nEntries; i++) { LogUtils.writeByteArray(logBuffer,entryKeyVals[i]); assert !(getLsn(i) == DbLsn.NULL_LSN && (entryStates[i] & KNOWN_DELETED_BIT) == 0); if (compactLsnsRep) { int offset=i << 2; int fileOffset=getFileOffset(offset); logBuffer.put(getFileNumberOffset(offset)); logBuffer.put((byte)((fileOffset >>> 0) & 0xff)); logBuffer.put((byte)((fileOffset >>> 8) & 0xff)); logBuffer.put((byte)((fileOffset >>> 16) & 0xff)); } else { LogUtils.writeLong(logBuffer,entryLsnLongArray[i]); } logBuffer.put(entryStates[i]); entryStates[i]&=CLEAR_DIRTY_BIT; } }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); identifierKey=LogUtils.readByteArray(itemBuffer); isRoot=LogUtils.readBoolean(itemBuffer); nEntries=LogUtils.readInt(itemBuffer); level=LogUtils.readInt(itemBuffer); int length=LogUtils.readInt(itemBuffer); entryTargets=new Node[length]; entryKeyVals=new byte[length][]; baseFileNumber=-1; long storedBaseFileNumber=-1; entryLsnByteArray=new byte[length << 2]; entryLsnLongArray=null; entryStates=new byte[length]; boolean compactLsnsRep=false; if (entryTypeVersion > 1) { compactLsnsRep=LogUtils.readBoolean(itemBuffer); if (compactLsnsRep) { baseFileNumber=LogUtils.readInt(itemBuffer) & 0xffffffff; storedBaseFileNumber=baseFileNumber; } } for (int i=0; i < nEntries; i++) { entryKeyVals[i]=LogUtils.readByteArray(itemBuffer); long lsn; if (compactLsnsRep) { byte fileNumberOffset=itemBuffer.get(); int fileOffset=(itemBuffer.get() & 0xff); fileOffset|=((itemBuffer.get() & 0xff) << 8); fileOffset|=((itemBuffer.get() & 0xff) << 16); if (fileOffset == THREE_BYTE_NEGATIVE_ONE) { lsn=DbLsn.NULL_LSN; } else { lsn=DbLsn.makeLsn(storedBaseFileNumber + fileNumberOffset,fileOffset); } } else { lsn=LogUtils.readLong(itemBuffer); } setLsnElement(i,lsn); byte entryState=itemBuffer.get(); entryState&=CLEAR_DIRTY_BIT; entryState&=CLEAR_MIGRATE_BIT; if (lsn == DbLsn.NULL_LSN) { entryState|=KNOWN_DELETED_BIT; } entryStates[i]=entryState; } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append(beginTag()); super.dumpLog(sb,verbose); sb.append(Key.dumpString(identifierKey,0)); sb.append("<isRoot val=\""); sb.append(isRoot); sb.append("\"/>"); sb.append("<level val=\""); sb.append(Integer.toHexString(level)); sb.append("\"/>"); sb.append("<entries numEntries=\""); sb.append(nEntries); sb.append("\" length=\""); sb.append(entryTargets.length); boolean compactLsnsRep=(entryLsnLongArray == null); if (compactLsnsRep) { sb.append("\" baseFileNumber=\""); sb.append(baseFileNumber); } sb.append("\">"); if (verbose) { for (int i=0; i < nEntries; i++) { sb.append("<ref knownDeleted=\"").append(isEntryKnownDeleted(i)); sb.append("\" pendingDeleted=\"").append(isEntryPendingDeleted(i)); sb.append("\">"); sb.append(Key.dumpString(entryKeyVals[i],0)); sb.append(DbLsn.toString(getLsn(i))); sb.append("</ref>"); } } sb.append("</entries>"); dumpLogAdditional(sb); sb.append(endTag()); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb){ }

	 protected void dumpLogAdditional( StringBuffer sb){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void dumpKeys__wrappee__base() throws DatabaseException { for (int i=0; i < nEntries; i++) { System.out.println(Key.dumpString(entryKeyVals[i],0)); } }

	 void dumpKeys() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpKeys__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(beginTag()); sb.append('\n'); } sb.append(super.dumpString(nSpaces + 2,true)); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<idkey>"); sb.append(identifierKey == null ? "" : Key.dumpString(identifierKey,0)); sb.append("</idkey>"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<dirty val=\"").append(dirty).append("\"/>"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<generation val=\"").append(generation).append("\"/>"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<level val=\""); sb.append(Integer.toHexString(level)).append("\"/>"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<isRoot val=\"").append(isRoot).append("\"/>"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<entries nEntries=\""); sb.append(nEntries); sb.append("\">"); sb.append('\n'); for (int i=0; i < nEntries; i++) { sb.append(TreeUtils.indent(nSpaces + 4)); sb.append("<entry id=\"" + i + "\">"); sb.append('\n'); if (getLsn(i) == DbLsn.NULL_LSN) { sb.append(TreeUtils.indent(nSpaces + 6)); sb.append("<lsn/>"); } else { sb.append(DbLsn.dumpString(getLsn(i),nSpaces + 6)); } sb.append('\n'); if (entryKeyVals[i] == null) { sb.append(TreeUtils.indent(nSpaces + 6)); sb.append("<key/>"); } else { sb.append(Key.dumpString(entryKeyVals[i],(nSpaces + 6))); } sb.append('\n'); if (entryTargets[i] == null) { sb.append(TreeUtils.indent(nSpaces + 6)); sb.append("<target/>"); } else { sb.append(entryTargets[i].dumpString(nSpaces + 6,true)); } sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 6)); dumpDeletedState(sb,getState(i)); sb.append("<dirty val=\"").append(isDirty(i)).append("\"/>"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces + 4)); sb.append("</entry>"); sb.append('\n'); } sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("</entries>"); sb.append('\n'); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(endTag()); } return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static void dumpDeletedState__wrappee__base( StringBuffer sb, byte state){ sb.append("<knownDeleted val=\""); sb.append(isStateKnownDeleted(state)).append("\"/>"); sb.append("<pendingDeleted val=\""); sb.append(isStatePendingDeleted(state)).append("\"/>"); }

	 static void dumpDeletedState( StringBuffer sb, byte state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpDeletedState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String shortClassName__wrappee__base(){ return "IN"; }

	 public String shortClassName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shortClassName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook618__wrappee__base( EnvironmentImpl env){ }

	 protected void hook618( EnvironmentImpl env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook618__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook619__wrappee__base( boolean updateGeneration) throws DatabaseException { if (updateGeneration) { setGeneration(); } throw new ReturnBoolean(true); }

	 protected void hook619( boolean updateGeneration) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook619__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook620__wrappee__base() throws DatabaseException { }

	 protected void hook620() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook620__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook621__wrappee__base() throws DatabaseException { }

	 protected void hook621() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook621__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook622__wrappee__base() throws DatabaseException { }

	 protected void hook622() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook622__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook623__wrappee__base() throws DatabaseException { }

	 protected void hook623() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook623__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook624__wrappee__base() throws DatabaseException { }

	 protected void hook624() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook624__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook625__wrappee__base( Node child) throws DatabaseException { }

	 protected void hook625( Node child) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook625__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook626__wrappee__base() throws DatabaseException { }

	 protected void hook626() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook626__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook627__wrappee__base() throws DatabaseException { }

	 protected void hook627() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook627__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook637__wrappee__base() throws DatabaseException { }

	 protected void hook637() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook637__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook638__wrappee__base( Node node) throws DatabaseException, LogFileNotFoundException, Exception { }

	 protected void hook638( Node node) throws DatabaseException, LogFileNotFoundException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook638__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
