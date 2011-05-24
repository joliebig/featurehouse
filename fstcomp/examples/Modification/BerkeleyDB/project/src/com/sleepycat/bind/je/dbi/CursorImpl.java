package com.sleepycat.je.dbi; 
import java.util.Comparator; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.je.RunRecoveryException; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.BINBoundary; 
import com.sleepycat.je.tree.DBIN; 
import com.sleepycat.je.tree.DIN; 
import com.sleepycat.je.tree.DupCountLN; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.Key; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.tree.Node; 
import com.sleepycat.je.tree.Tree; 
import com.sleepycat.je.tree.TreeWalkerStatsAccumulator; 
import com.sleepycat.je.txn.BasicLocker; 
import com.sleepycat.je.txn.LockGrantType; 
import com.sleepycat.je.txn.LockResult; 
import com.sleepycat.je.txn.LockType; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.ThreadLocker; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.TestHook; 
import com.sleepycat.je.utilint.TestHookExecute; 
import de.ovgu.cide.jakutil.*; 
public  class  CursorImpl  implements Cloneable {
	 private static final byte CURSOR_NOT_INITIALIZED=1;

	 private static final byte CURSOR_INITIALIZED=2;

	 private static final byte CURSOR_CLOSED=3;

	 private static final String TRACE_DELETE="Delete";

	 private static final String TRACE_MOD="Mod:";

	 volatile private BIN bin;

	 volatile private int index;

	 volatile private DBIN dupBin;

	 volatile private int dupIndex;

	 volatile private BIN binToBeRemoved;

	 volatile private DBIN dupBinToBeRemoved;

	 private BIN targetBin;

	 private int targetIndex;

	 private byte[] dupKey;

	 private DatabaseImpl database;

	 private Locker locker;

	 private CursorImpl lockerPrev;

	 private CursorImpl lockerNext;

	 private boolean retainNonTxnLocks;

	 private byte status;

	 private TestHook testHook;

	 private boolean nonCloning=false;

	 private int thisId;

	 private static long lastAllocatedId=0;

	 private ThreadLocal treeStatsAccumulatorTL=new ThreadLocal();

	
public static  class  SearchMode {
		 public static final SearchMode SET=new SearchMode(true,false,"SET");

		 public static final SearchMode BOTH=new SearchMode(true,true,"BOTH");

		 public static final SearchMode SET_RANGE=new SearchMode(false,false,"SET_RANGE");

		 public static final SearchMode BOTH_RANGE=new SearchMode(false,true,"BOTH_RANGE");

		 private boolean exactSearch;

		 private boolean dataSearch;

		 private String name;

		 private SearchMode( boolean exactSearch, boolean dataSearch, String name){ this.exactSearch=exactSearch; this.dataSearch=dataSearch; this.name="SearchMode." + name; }

		 public final boolean isExactSearch__wrappee__base(){ return exactSearch; }

		 public final boolean isExactSearch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isExactSearch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public final boolean isDataSearch__wrappee__base(){ return dataSearch; }

		 public final boolean isDataSearch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDataSearch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public String toString__wrappee__base(){ return name; }

		 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
public static  class  KeyChangeStatus {
		 public OperationStatus status;

		 public boolean keyChange;

		 public KeyChangeStatus( OperationStatus status, boolean keyChange){ this.status=status; this.keyChange=keyChange; }


	}

	 public CursorImpl( DatabaseImpl database, Locker locker) throws DatabaseException { this(database,locker,true); }

	 public CursorImpl( DatabaseImpl database, Locker locker, boolean retainNonTxnLocks) throws DatabaseException { thisId=(int)getNextCursorId(); bin=null; index=-1; dupBin=null; dupIndex=-1; assert !(retainNonTxnLocks && (locker instanceof ThreadLocker)); assert !(!retainNonTxnLocks && locker.getClass() == BasicLocker.class); this.retainNonTxnLocks=retainNonTxnLocks; this.database=database; this.locker=locker; this.locker.registerCursor(this); status=CURSOR_NOT_INITIALIZED; }

	 public static final int FOUND=0x1;

	 public static final int EXACT_KEY=0x2;

	 public static final int EXACT_DATA=0x4;

	 public static final int FOUND_LAST=0x8;

	
@MethodObject static  class  CursorImpl_latchBIN {
		
CursorImpl_latchBIN(CursorImpl _this){
this._this=_this;
}

		
protected CursorImpl _this;

		
protected BIN waitingOn;

		
BIN execute__wrappee__base() throws DatabaseException {
try {
this.hook244();
throw ReturnHack.returnObject;
} catch (ReturnObject r) {
return (BIN)r.value;
}
}

		 BIN execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook244__wrappee__base() throws DatabaseException {
this.hook245();
}

		 protected void hook244() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook244__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook245__wrappee__base() throws DatabaseException {
throw new ReturnObject(_this.bin);
}

		 protected void hook245() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook245__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  CursorImpl_latchDBIN {
		
CursorImpl_latchDBIN(CursorImpl _this){
this._this=_this;
}

		
protected CursorImpl _this;

		
protected BIN waitingOn;

		
DBIN execute__wrappee__base() throws DatabaseException {
try {
this.hook246();
throw ReturnHack.returnObject;
} catch (ReturnObject r) {
return (DBIN)r.value;
}
}

		 DBIN execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook246__wrappee__base() throws DatabaseException {
this.hook247();
}

		 protected void hook246() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook246__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook247__wrappee__base() throws DatabaseException {
throw new ReturnObject(_this.dupBin);
}

		 protected void hook247() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook247__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  CursorImpl_lockNextKeyForInsert {
		
CursorImpl_lockNextKeyForInsert(CursorImpl _this,DatabaseEntry key,DatabaseEntry data){
this._this=_this;
this.key=key;
this.data=data;
}

		
protected CursorImpl _this;

		
protected DatabaseEntry key;

		
protected DatabaseEntry data;

		
protected DatabaseEntry tempKey;

		
protected DatabaseEntry tempData;

		
protected boolean lockedNextKey;

		
protected SearchMode searchMode;

		
protected boolean latched;

		
protected int searchResult;

		
protected OperationStatus status;

		
void execute__wrappee__base() throws DatabaseException {
tempKey=new DatabaseEntry(key.getData(),key.getOffset(),key.getSize());
tempData=new DatabaseEntry(data.getData(),data.getOffset(),data.getSize());
tempKey.setPartial(0,0,true);
tempData.setPartial(0,0,true);
lockedNextKey=false;
searchMode=_this.database.getSortedDuplicates() ? SearchMode.BOTH_RANGE : SearchMode.SET_RANGE;
this.hook248();
if (!lockedNextKey) {
_this.lockEofNode(LockType.RANGE_INSERT);
}
}

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook248__wrappee__base() throws DatabaseException {
searchResult=_this.searchAndPosition(tempKey,tempData,searchMode,LockType.RANGE_INSERT);
if ((searchResult & _this.FOUND) != 0 && (searchResult & _this.FOUND_LAST) == 0) {
{
}
if ((searchResult & _this.EXACT_KEY) != 0) { status=_this.getNext(tempKey,tempData,LockType.RANGE_INSERT,true,true);
} else { status=_this.getNextNoDup(tempKey,tempData,LockType.RANGE_INSERT,true,true);
}
if (status == OperationStatus.SUCCESS) { lockedNextKey=true;
}
this.hook249();
}
}

		 protected void hook248() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook248__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook249__wrappee__base() throws DatabaseException {
}

		 protected void hook249() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook249__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  CursorImpl_getNextDuplicate {
		
CursorImpl_getNextDuplicate(CursorImpl _this,DatabaseEntry foundKey,DatabaseEntry foundData,LockType lockType,boolean forward,boolean alreadyLatched){
this._this=_this;
this.foundKey=foundKey;
this.foundData=foundData;
this.lockType=lockType;
this.forward=forward;
this.alreadyLatched=alreadyLatched;
}

		
protected CursorImpl _this;

		
protected DatabaseEntry foundKey;

		
protected DatabaseEntry foundData;

		
protected LockType lockType;

		
protected boolean forward;

		
protected boolean alreadyLatched;

		
protected OperationStatus ret;

		
protected TreeWalkerStatsAccumulator treeStatsAccumulator;

		
protected DIN duplicateRoot;

		
protected DupCountLN dcl;

		
protected DBIN newDupBin;

		
OperationStatus execute__wrappee__base() throws DatabaseException {
try {
assert _this.assertCursorState(true) : _this.dumpToString(true);
this.hook250();
try { while (_this.dupBin != null) { this.hook251(); this.hook279(); if ((forward && ++_this.dupIndex < _this.dupBin.getNEntries()) || (!forward && --_this.dupIndex > -1)) { ret=OperationStatus.SUCCESS; if (foundKey != null) { ret=_this.getCurrentAlreadyLatched(foundKey,foundData,lockType,forward); } else { this.hook252(); } if (ret == OperationStatus.SUCCESS) { _this.incrementLNCount(); return ret; } else { this.hook253(); if (_this.dupBinToBeRemoved != null) { _this.flushDBINToBeRemoved(); } continue; } } else { if (_this.dupBinToBeRemoved != null) { _this.flushDBINToBeRemoved(); } _this.dupBinToBeRemoved=_this.dupBin; _this.dupBin=null; this.hook255(); this.hook275(); this.hook254();
{ } if (forward) { newDupBin=(DBIN)_this.database.getTree().getNextBin(_this.dupBinToBeRemoved,true); } else { newDupBin=(DBIN)_this.database.getTree().getPrevBin(_this.dupBinToBeRemoved,true); } if (newDupBin == null) { return OperationStatus.NOTFOUND; } else { if (forward) { _this.dupIndex=-1; } else { _this.dupIndex=newDupBin.getNEntries(); } _this.addCursor(newDupBin); _this.dupBin=newDupBin; this.hook256(); } } }
} finally { this.hook257(); if (_this.dupBinToBeRemoved != null) { _this.flushDBINToBeRemoved(); }
}
return OperationStatus.NOTFOUND;
} catch (ReturnObject r) {
return (OperationStatus)r.value;
}
}

		 OperationStatus execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook250__wrappee__base() throws DatabaseException {
}

		 protected void hook250() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook250__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook251__wrappee__base() throws DatabaseException {
}

		 protected void hook251() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook251__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook252__wrappee__base() throws DatabaseException {
}

		 protected void hook252() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook252__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook253__wrappee__base() throws DatabaseException {
}

		 protected void hook253() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook253__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook254__wrappee__base() throws DatabaseException {
}

		 protected void hook254() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook254__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook255__wrappee__base() throws DatabaseException {
}

		 protected void hook255() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook255__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook256__wrappee__base() throws DatabaseException {
}

		 protected void hook256() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook256__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook257__wrappee__base() throws DatabaseException {
}

		 protected void hook257() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook257__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook275__wrappee__base() throws DatabaseException {
}

		 protected void hook275() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook275__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook279__wrappee__base() throws DatabaseException {
}

		 protected void hook279() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook279__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  CursorImpl_fetchCurrent {
		
CursorImpl_fetchCurrent(CursorImpl _this,DatabaseEntry foundKey,DatabaseEntry foundData,LockType lockType,boolean first){
this._this=_this;
this.foundKey=foundKey;
this.foundData=foundData;
this.lockType=lockType;
this.first=first;
}

		
protected CursorImpl _this;

		
protected DatabaseEntry foundKey;

		
protected DatabaseEntry foundData;

		
protected LockType lockType;

		
protected boolean first;

		
protected TreeWalkerStatsAccumulator treeStatsAccumulator;

		
protected boolean duplicateFetch;

		
protected Node n;

		
protected EnvironmentImpl envImpl;

		
protected DIN duplicateRoot;

		
protected LN ln;

		
protected LockResult lockResult;

		
protected byte[] lnData;

		
OperationStatus execute__wrappee__base() throws DatabaseException {
try {
treeStatsAccumulator=_this.getTreeStatsAccumulator();
duplicateFetch=_this.setTargetBin();
if (_this.targetBin == null) { return OperationStatus.NOTFOUND;
}
this.hook259();
n=null;
if (_this.targetIndex < 0 || _this.targetIndex >= _this.targetBin.getNEntries() || _this.targetBin.isEntryKnownDeleted(_this.targetIndex)) {
} else { if (_this.targetBin.isEntryPendingDeleted(_this.targetIndex)) { this.hook280(); } this.hook260();
}
if (n == null) { if (treeStatsAccumulator != null) { treeStatsAccumulator.incrementDeletedLNCount(); } this.hook261(); return OperationStatus.KEYEMPTY;
}
_this.addCursor(_this.targetBin);
if (n.containsDuplicates()) { assert !duplicateFetch; duplicateRoot=(DIN)n; this.hook262(); if (_this.positionFirstOrLast(first,duplicateRoot)) { this.hook263(); } else { return OperationStatus.NOTFOUND; }
}
ln=(LN)n;
assert TestHookExecute.doHookIfSet(_this.testHook);
lockResult=_this.lockLN(ln,lockType);
this.hook258();
throw ReturnHack.returnObject;
} catch (ReturnObject r) {
return (OperationStatus)r.value;
}
}

		 OperationStatus execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook258__wrappee__base() throws DatabaseException {
ln=lockResult.getLN();
lnData=(ln != null) ? ln.getData() : null;
if (ln == null || lnData == null) {
if (treeStatsAccumulator != null) { treeStatsAccumulator.incrementDeletedLNCount();
}
throw new ReturnObject(OperationStatus.KEYEMPTY);
}
duplicateFetch=_this.setTargetBin();
if (duplicateFetch) {
if (foundData != null) { _this.setDbt(foundData,_this.targetBin.getKey(_this.targetIndex));
}
if (foundKey != null) { _this.setDbt(foundKey,_this.targetBin.getDupKey());
}
} else {
if (foundData != null) { _this.setDbt(foundData,lnData);
}
if (foundKey != null) { _this.setDbt(foundKey,_this.targetBin.getKey(_this.targetIndex));
}
}
throw new ReturnObject(OperationStatus.SUCCESS);
}

		 protected void hook258() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook258__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook259__wrappee__base() throws DatabaseException {
}

		 protected void hook259() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook259__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook260__wrappee__base() throws DatabaseException {
n=_this.targetBin.fetchTarget(_this.targetIndex);
}

		 protected void hook260() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook260__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook261__wrappee__base() throws DatabaseException {
}

		 protected void hook261() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook261__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook262__wrappee__base() throws DatabaseException {
}

		 protected void hook262() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook262__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook263__wrappee__base() throws DatabaseException {
throw new ReturnObject(_this.fetchCurrent(foundKey,foundData,lockType,first));
}

		 protected void hook263() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook263__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		
protected void hook280__wrappee__base() throws DatabaseException {
}

		 protected void hook280() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook280__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 private static long getNextCursorId__wrappee__base(){ return ++lastAllocatedId; }

	 private static long getNextCursorId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextCursorId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ return thisId; }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private TreeWalkerStatsAccumulator getTreeStatsAccumulator__wrappee__base(){ if (EnvironmentImpl.getThreadLocalReferenceCount() > 0) { return (TreeWalkerStatsAccumulator)treeStatsAccumulatorTL.get(); } else { return null; } }

	 private TreeWalkerStatsAccumulator getTreeStatsAccumulator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTreeStatsAccumulator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void incrementLNCount__wrappee__base(){ TreeWalkerStatsAccumulator treeStatsAccumulator=getTreeStatsAccumulator(); if (treeStatsAccumulator != null) { treeStatsAccumulator.incrementLNCount(); } }

	 public void incrementLNCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	incrementLNCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setNonCloning__wrappee__base( boolean nonCloning){ this.nonCloning=nonCloning; }

	 public void setNonCloning( boolean nonCloning){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNonCloning__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public CursorImpl cloneCursor__wrappee__base( boolean addCursor) throws DatabaseException { return cloneCursor(addCursor,null); }

	 public CursorImpl cloneCursor( boolean addCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public CursorImpl cloneCursor__wrappee__base( boolean addCursor, CursorImpl usePosition) throws DatabaseException { CursorImpl ret=null; if (nonCloning) { ret=this; } else { try { this.hook206(); ret=(CursorImpl)super.clone(); if (!retainNonTxnLocks) { ret.locker=locker.newNonTxnLocker(); } ret.locker.registerCursor(ret); if (usePosition != null && usePosition.status == CURSOR_INITIALIZED) { ret.bin=usePosition.bin; ret.index=usePosition.index; ret.dupBin=usePosition.dupBin; ret.dupIndex=usePosition.dupIndex; } if (addCursor) { ret.addCursor(); } } catch ( CloneNotSupportedException cannotOccur) { return null; } finally { this.hook207(); } } return ret; }

	 public CursorImpl cloneCursor( boolean addCursor, CursorImpl usePosition) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getIndex__wrappee__base(){ return index; }

	 public int getIndex(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setIndex__wrappee__base( int idx){ index=idx; }

	 public void setIndex( int idx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public BIN getBIN__wrappee__base(){ return bin; }

	 public BIN getBIN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setBIN__wrappee__base( BIN newBin){ bin=newBin; }

	 public void setBIN( BIN newBin){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public BIN getBINToBeRemoved__wrappee__base(){ return binToBeRemoved; }

	 public BIN getBINToBeRemoved(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBINToBeRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getDupIndex__wrappee__base(){ return dupIndex; }

	 public int getDupIndex(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDupIndex__wrappee__base( int dupIdx){ dupIndex=dupIdx; }

	 public void setDupIndex( int dupIdx){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDupIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DBIN getDupBIN__wrappee__base(){ return dupBin; }

	 public DBIN getDupBIN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDupBIN__wrappee__base( DBIN newDupBin){ dupBin=newDupBin; }

	 public void setDupBIN( DBIN newDupBin){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDupBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DBIN getDupBINToBeRemoved__wrappee__base(){ return dupBinToBeRemoved; }

	 public DBIN getDupBINToBeRemoved(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupBINToBeRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTreeStatsAccumulator__wrappee__base( TreeWalkerStatsAccumulator tSA){ treeStatsAccumulatorTL.set(tSA); }

	 public void setTreeStatsAccumulator( TreeWalkerStatsAccumulator tSA){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTreeStatsAccumulator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean setTargetBin__wrappee__base(){ targetBin=null; targetIndex=0; boolean isDup=(dupBin != null); dupKey=null; if (isDup) { targetBin=dupBin; targetIndex=dupIndex; dupKey=dupBin.getDupKey(); } else { targetBin=bin; targetIndex=index; } return isDup; }

	 private boolean setTargetBin(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTargetBin__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean advanceCursor__wrappee__base( DatabaseEntry key, DatabaseEntry data){ BIN oldBin=bin; BIN oldDupBin=dupBin; int oldIndex=index; int oldDupIndex=dupIndex; key.setData(null); data.setData(null); try { getNext(key,data,LockType.NONE,true,false); } catch ( DatabaseException ignored) { } if (bin != oldBin || dupBin != oldDupBin || index != oldIndex || dupIndex != oldDupIndex) { if (key.getData() == null && bin != null && index > 0) { setDbt(key,bin.getKey(index)); } if (data.getData() == null && dupBin != null && dupIndex > 0) { setDbt(data,dupBin.getKey(dupIndex)); } return true; } else { return false; } }

	 public boolean advanceCursor( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	advanceCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public BIN latchBIN__wrappee__base() throws DatabaseException { return new CursorImpl_latchBIN(this).execute(); }

	 public BIN latchBIN() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	latchBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DBIN latchDBIN__wrappee__base() throws DatabaseException { return new CursorImpl_latchDBIN(this).execute(); }

	 public DBIN latchDBIN() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	latchDBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Locker getLocker__wrappee__base(){ return locker; }

	 public Locker getLocker(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addCursor__wrappee__base( BIN bin){ if (bin != null) { this.hook208(bin); bin.addCursor(this); } }

	 public void addCursor( BIN bin){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addCursor__wrappee__base(){ if (dupBin != null) { addCursor(dupBin); } if (bin != null) { addCursor(bin); } }

	 public void addCursor(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateBin__wrappee__base( BIN bin, int index) throws DatabaseException { removeCursorDBIN(); setDupIndex(-1); setDupBIN(null); setIndex(index); setBIN(bin); addCursor(bin); }

	 public void updateBin( BIN bin, int index) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	updateBin__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void updateDBin__wrappee__base( DBIN dupBin, int dupIndex){ setDupIndex(dupIndex); setDupBIN(dupBin); addCursor(dupBin); }

	 public void updateDBin( DBIN dupBin, int dupIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateDBin__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void removeCursor__wrappee__base() throws DatabaseException { removeCursorBIN(); removeCursorDBIN(); }

	 private void removeCursor() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void removeCursorBIN__wrappee__base() throws DatabaseException { BIN abin=latchBIN(); if (abin != null) { abin.removeCursor(this); this.hook209(abin); } }

	 private void removeCursorBIN() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeCursorBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void removeCursorDBIN__wrappee__base() throws DatabaseException { DBIN abin=latchDBIN(); if (abin != null) { abin.removeCursor(this); this.hook210(abin); } }

	 private void removeCursorDBIN() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeCursorDBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void clearDupBIN__wrappee__base( boolean alreadyLatched) throws DatabaseException { if (dupBin != null) { if (alreadyLatched) { dupBin.removeCursor(this); this.hook211(); } else { removeCursorDBIN(); } dupBin=null; dupIndex=-1; } }

	 public void clearDupBIN( boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clearDupBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpTree__wrappee__base() throws DatabaseException { database.getTree().dump(); }

	 public void dumpTree() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isClosed__wrappee__base(){ return (status == CURSOR_CLOSED); }

	 public boolean isClosed(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isClosed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isNotInitialized__wrappee__base(){ return (status == CURSOR_NOT_INITIALIZED); }

	 public boolean isNotInitialized(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isNotInitialized__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void reset__wrappee__base() throws DatabaseException { removeCursor(); if (!retainNonTxnLocks) { locker.releaseNonTxnLocks(); } bin=null; index=-1; dupBin=null; dupIndex=-1; status=CURSOR_NOT_INITIALIZED; }

	 public void reset() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void close__wrappee__base() throws DatabaseException { assert assertCursorState(false) : dumpToString(true); removeCursor(); locker.unRegisterCursor(this); if (!retainNonTxnLocks) { locker.releaseNonTxnLocks(); } status=CURSOR_CLOSED; }

	 public void close() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int count__wrappee__base( LockType lockType) throws DatabaseException { try { assert assertCursorState(true) : dumpToString(true); if (!database.getSortedDuplicates()) { return 1; } if (bin == null) { return 0; } this.hook212(lockType); throw ReturnHack.returnInt; } catch ( ReturnInt r) { return r.value; } }

	 public int count( LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	count__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus delete__wrappee__base() throws DatabaseException { assert assertCursorState(true) : dumpToString(true); boolean isDup=setTargetBin(); if (targetBin == null) { return OperationStatus.KEYEMPTY; } if (targetBin.isEntryKnownDeleted(targetIndex)) { this.hook214(); return OperationStatus.KEYEMPTY; } LN ln=(LN)targetBin.fetchTarget(targetIndex); if (ln == null) { this.hook215(); return OperationStatus.KEYEMPTY; } LockResult lockResult=lockLN(ln,LockType.WRITE); ln=lockResult.getLN(); if (ln == null) { this.hook216(); return OperationStatus.KEYEMPTY; } LockResult dclLockResult=null; DIN dupRoot=null; this.hook213(isDup,ln,lockResult,dclLockResult,dupRoot); return OperationStatus.SUCCESS; }

	 public OperationStatus delete() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	delete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public CursorImpl dup__wrappee__base( boolean samePosition) throws DatabaseException { assert assertCursorState(false) : dumpToString(true); CursorImpl ret=cloneCursor(samePosition); if (!samePosition) { ret.bin=null; ret.index=-1; ret.dupBin=null; ret.dupIndex=-1; ret.status=CURSOR_NOT_INITIALIZED; } return ret; }

	 public CursorImpl dup( boolean samePosition) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void lockNextKeyForInsert__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { new CursorImpl_lockNextKeyForInsert(this,key,data).execute(); }

	 public void lockNextKeyForInsert( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockNextKeyForInsert__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putLN__wrappee__base( byte[] key, LN ln, boolean allowDuplicates) throws DatabaseException { assert assertCursorState(false) : dumpToString(true); this.hook217(); LockResult lockResult=locker.lock(ln.getNodeId(),LockType.WRITE,false,database); if (database.getTree().insert(ln,key,allowDuplicates,this,lockResult)) { status=CURSOR_INITIALIZED; return OperationStatus.SUCCESS; } else { locker.releaseLock(ln.getNodeId()); return OperationStatus.KEYEXIST; } }

	 public OperationStatus putLN( byte[] key, LN ln, boolean allowDuplicates) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus put__wrappee__base( DatabaseEntry key, DatabaseEntry data, DatabaseEntry foundData) throws DatabaseException { assert assertCursorState(false) : dumpToString(true); OperationStatus result=putLN(Key.makeKey(key),new LN(data),database.getSortedDuplicates()); if (result == OperationStatus.KEYEXIST) { status=CURSOR_INITIALIZED; result=putCurrent(data,null,foundData); } return result; }

	 public OperationStatus put( DatabaseEntry key, DatabaseEntry data, DatabaseEntry foundData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	put__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoOverwrite__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { assert assertCursorState(false) : dumpToString(true); return putLN(Key.makeKey(key),new LN(data),false); }

	 public OperationStatus putNoOverwrite( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoOverwrite__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoDupData__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { assert assertCursorState(false) : dumpToString(true); if (!database.getSortedDuplicates()) { throw new DatabaseException("putNoDupData() called, but database is not configured " + "for duplicate data."); } return putLN(Key.makeKey(key),new LN(data),true); }

	 public OperationStatus putNoDupData( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoDupData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putCurrent__wrappee__base( DatabaseEntry data, DatabaseEntry foundKey, DatabaseEntry foundData) throws DatabaseException { try { assert assertCursorState(true) : dumpToString(true); if (foundKey != null) { foundKey.setData(null); } if (foundData != null) { foundData.setData(null); } if (bin == null) { return OperationStatus.KEYEMPTY; } this.hook219(); boolean isDup=setTargetBin(); this.hook218(data,foundKey,foundData,isDup); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (OperationStatus)r.value; } }

	 public OperationStatus putCurrent( DatabaseEntry data, DatabaseEntry foundKey, DatabaseEntry foundData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getCurrent__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType) throws DatabaseException { assert assertCursorState(true) : dumpToString(true); if (bin == null) { return OperationStatus.KEYEMPTY; } this.hook220(); return getCurrentAlreadyLatched(foundKey,foundData,lockType,true); }

	 public OperationStatus getCurrent( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getCurrentAlreadyLatched__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean first) throws DatabaseException { try { assert assertCursorState(true) : dumpToString(true); this.hook221(foundKey,foundData,lockType,first); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (OperationStatus)r.value; } }

	 public OperationStatus getCurrentAlreadyLatched( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrentAlreadyLatched__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LN getCurrentLN__wrappee__base( LockType lockType) throws DatabaseException { assert assertCursorState(true) : dumpToString(true); if (bin == null) { return null; } else { this.hook222(); return getCurrentLNAlreadyLatched(lockType); } }

	 public LN getCurrentLN( LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrentLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LN getCurrentLNAlreadyLatched__wrappee__base( LockType lockType) throws DatabaseException { try { this.hook223(lockType); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (LN)r.value; } }

	 public LN getCurrentLNAlreadyLatched( LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrentLNAlreadyLatched__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNext__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { return getNextWithKeyChangeStatus(foundKey,foundData,lockType,forward,alreadyLatched).status; }

	 public OperationStatus getNext( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public KeyChangeStatus getNextWithKeyChangeStatus__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { assert assertCursorState(true) : dumpToString(true); this.hook224(alreadyLatched); KeyChangeStatus result=new KeyChangeStatus(OperationStatus.NOTFOUND,true); try { while (bin != null) { if (dupBin != null) { this.hook277(); if (getNextDuplicate(foundKey,foundData,lockType,forward,alreadyLatched) == OperationStatus.SUCCESS) { result.status=OperationStatus.SUCCESS; result.keyChange=false; break; } else { removeCursorDBIN(); alreadyLatched=this.hook226(alreadyLatched); dupBin=null; dupIndex=-1; continue; } } alreadyLatched=this.hook225(alreadyLatched); this.hook276(); if ((forward && ++index < bin.getNEntries()) || (!forward && --index > -1)) { OperationStatus ret=getCurrentAlreadyLatched(foundKey,foundData,lockType,forward); if (ret == OperationStatus.SUCCESS) { incrementLNCount(); result.status=OperationStatus.SUCCESS; break; } else { this.hook227(); if (binToBeRemoved != null) { flushBINToBeRemoved(); } continue; } } else { if (binToBeRemoved != null) { this.hook229(); flushBINToBeRemoved(); this.hook228(); } binToBeRemoved=bin; bin=null; BIN newBin; assert TestHookExecute.doHookIfSet(testHook); if (forward) { newBin=database.getTree().getNextBin(binToBeRemoved,false); } else { newBin=database.getTree().getPrevBin(binToBeRemoved,false); } if (newBin == null) { result.status=OperationStatus.NOTFOUND; break; } else { if (forward) { index=-1; } else { index=newBin.getNEntries(); } addCursor(newBin); bin=newBin; this.hook230(alreadyLatched); } } } } finally { this.hook231(); if (binToBeRemoved != null) { flushBINToBeRemoved(); } } return result; }

	 public KeyChangeStatus getNextWithKeyChangeStatus( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextWithKeyChangeStatus__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void flushBINToBeRemoved__wrappee__base() throws DatabaseException { binToBeRemoved.removeCursor(this); this.hook232(); binToBeRemoved=null; }

	 private void flushBINToBeRemoved() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	flushBINToBeRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextNoDup__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { assert assertCursorState(true) : dumpToString(true); if (dupBin != null) { clearDupBIN(alreadyLatched); alreadyLatched=false; } return getNext(foundKey,foundData,lockType,forward,alreadyLatched); }

	 public OperationStatus getNextNoDup( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getFirstDuplicate__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType) throws DatabaseException { assert assertCursorState(true) : dumpToString(true); if (dupBin != null) { removeCursorDBIN(); dupBin=null; dupIndex=-1; } return getCurrent(foundKey,foundData,lockType); }

	 public OperationStatus getFirstDuplicate( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirstDuplicate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextDuplicate__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { return new CursorImpl_getNextDuplicate(this,foundKey,foundData,lockType,forward,alreadyLatched).execute(); }

	 public OperationStatus getNextDuplicate( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean forward, boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextDuplicate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void flushDBINToBeRemoved__wrappee__base() throws DatabaseException { dupBinToBeRemoved.removeCursor(this); this.hook233(); dupBinToBeRemoved=null; }

	 private void flushDBINToBeRemoved() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	flushDBINToBeRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean positionFirstOrLast__wrappee__base( boolean first, DIN duplicateRoot) throws DatabaseException { try { assert assertCursorState(false) : dumpToString(true); IN in=null; boolean found=false; this.hook234(first,duplicateRoot,in,found); throw ReturnHack.returnBoolean; } catch ( ReturnBoolean r) { return r.value; } }

	 public boolean positionFirstOrLast( boolean first, DIN duplicateRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	positionFirstOrLast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int searchAndPosition__wrappee__base( DatabaseEntry matchKey, DatabaseEntry matchData, SearchMode searchMode, LockType lockType) throws DatabaseException { try { assert assertCursorState(false) : dumpToString(true); removeCursor(); bin=null; boolean foundSomething=false; boolean foundExactKey=false; boolean foundExactData=false; boolean foundLast=false; boolean exactSearch=searchMode.isExactSearch(); BINBoundary binBoundary=new BINBoundary(); this.hook235(matchKey,matchData,searchMode,lockType,foundSomething,foundExactKey,foundExactData,foundLast,exactSearch,binBoundary); throw ReturnHack.returnInt; } catch ( ReturnInt r) { return r.value; } }

	 public int searchAndPosition( DatabaseEntry matchKey, DatabaseEntry matchData, SearchMode searchMode, LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	searchAndPosition__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private int searchAndPositionBoth__wrappee__base( boolean containsDuplicates, Node n, DatabaseEntry matchData, boolean exactSearch, LockType lockType, long oldLsn) throws DatabaseException { assert assertCursorState(false) : dumpToString(true); boolean found=false; boolean exact=false; assert (matchData != null); byte[] data=Key.makeKey(matchData); if (containsDuplicates) { DIN duplicateRoot=(DIN)n; this.hook236(duplicateRoot); dupBin=(DBIN)database.getTree().searchSubTree(duplicateRoot,data,Tree.SearchType.NORMAL,-1,null,true); if (dupBin != null) { addCursor(dupBin); dupIndex=dupBin.findEntry(data,true,exactSearch); if (dupIndex >= 0) { if ((dupIndex & IN.EXACT_MATCH) != 0) { exact=true; } dupIndex&=~IN.EXACT_MATCH; found=true; } else { dupIndex=-1; found=!exactSearch; } } } else { LN ln=(LN)n; LockResult lockResult=lockLN(ln,lockType); ln=lockResult.getLN(); if (ln == null) { found=!exactSearch; } else { dupBin=null; dupIndex=-1; int cmp=Key.compareKeys(ln.getData(),data,database.getDuplicateComparator()); if (cmp == 0 || (cmp <= 0 && !exactSearch)) { if (cmp == 0) { exact=true; } found=true; } else { index--; found=!exactSearch; } } } return (found ? FOUND : 0) | (exact ? EXACT_DATA : 0); }

	 private int searchAndPositionBoth( boolean containsDuplicates, Node n, DatabaseEntry matchData, boolean exactSearch, LockType lockType, long oldLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	searchAndPositionBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus fetchCurrent__wrappee__base( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean first) throws DatabaseException { return new CursorImpl_fetchCurrent(this,foundKey,foundData,lockType,first).execute(); }

	 private OperationStatus fetchCurrent( DatabaseEntry foundKey, DatabaseEntry foundData, LockType lockType, boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	fetchCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private LockResult lockLN__wrappee__base( LN ln, LockType lockType) throws DatabaseException { LockResult lockResult=lockLNDeletedAllowed(ln,lockType); ln=lockResult.getLN(); if (ln != null) { setTargetBin(); if (targetBin.isEntryKnownDeleted(targetIndex) || ln.isDeleted()) { revertLock(ln.getNodeId(),lockResult.getLockGrant()); lockResult.setLN(null); } } return lockResult; }

	 private LockResult lockLN( LN ln, LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockResult lockLNDeletedAllowed__wrappee__base( LN ln, LockType lockType) throws DatabaseException { LockResult lockResult; if (lockType == LockType.NONE) { lockResult=new LockResult(LockGrantType.NONE_NEEDED,null); lockResult.setLN(ln); return lockResult; } if (locker.getDefaultNoWait()) { lockResult=locker.lock(ln.getNodeId(),lockType,true,database); } else { lockResult=locker.nonBlockingLock(ln.getNodeId(),lockType,database); } if (lockResult.getLockGrant() != LockGrantType.DENIED) { lockResult.setLN(ln); return lockResult; } while (true) { long nodeId=ln.getNodeId(); this.hook238(); lockResult=locker.lock(nodeId,lockType,false,database); this.hook237(); setTargetBin(); ln=(LN)targetBin.fetchTarget(targetIndex); if (ln != null && nodeId != ln.getNodeId()) { revertLock(nodeId,lockResult.getLockGrant()); continue; } else { lockResult.setLN(ln); return lockResult; } } }

	 public LockResult lockLNDeletedAllowed( LN ln, LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockLNDeletedAllowed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockResult lockDupCountLN__wrappee__base( DIN dupRoot, LockType lockType) throws DatabaseException { DupCountLN ln=dupRoot.getDupCountLN(); LockResult lockResult; if (locker.getDefaultNoWait()) { lockResult=locker.lock(ln.getNodeId(),lockType,true,database); } else { lockResult=locker.nonBlockingLock(ln.getNodeId(),lockType,database); } if (lockResult.getLockGrant() == LockGrantType.DENIED) { this.hook241(dupRoot); lockResult=locker.lock(ln.getNodeId(),lockType,false,database); this.hook240(); dupRoot=(DIN)bin.fetchTarget(index); this.hook239(dupRoot); ln=dupRoot.getDupCountLN(); } lockResult.setLN(ln); return lockResult; }

	 public LockResult lockDupCountLN( DIN dupRoot, LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockDupCountLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DIN getLatchedDupRoot__wrappee__base( boolean isDBINLatched) throws DatabaseException { assert bin != null; this.hook243(); assert index >= 0; DIN dupRoot=(DIN)bin.fetchTarget(index); this.hook242(isDBINLatched,dupRoot); return dupRoot; }

	 public DIN getLatchedDupRoot( boolean isDBINLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLatchedDupRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void setDbt__wrappee__base( DatabaseEntry data, byte[] bytes){ if (bytes != null) { boolean partial=data.getPartial(); int off=partial ? data.getPartialOffset() : 0; int len=partial ? data.getPartialLength() : bytes.length; if (off + len > bytes.length) { len=(off > bytes.length) ? 0 : bytes.length - off; } byte[] newdata=null; if (len == 0) { newdata=LogUtils.ZERO_LENGTH_BYTE_ARRAY; } else { newdata=new byte[len]; System.arraycopy(bytes,off,newdata,0,len); } data.setData(newdata); data.setOffset(0); data.setSize(len); } else { data.setData(null); data.setOffset(0); data.setSize(0); } }

	 private void setDbt( DatabaseEntry data, byte[] bytes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDbt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean assertCursorState__wrappee__base( boolean mustBeInitialized){ try { checkCursorState(mustBeInitialized); return true; } catch ( DatabaseException e) { return false; } }

	 private boolean assertCursorState( boolean mustBeInitialized){ t.in(Thread.currentThread().getStackTrace()[1].toString());	assertCursorState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void checkCursorState__wrappee__base( boolean mustBeInitialized) throws DatabaseException { if (status == CURSOR_INITIALIZED) { this.hook278(); return; } else if (status == CURSOR_NOT_INITIALIZED) { if (mustBeInitialized) { throw new DatabaseException("Cursor Not Initialized."); } } else if (status == CURSOR_CLOSED) { throw new DatabaseException("Cursor has been closed."); } else { throw new DatabaseException("Unknown cursor status: " + status); } }

	 public void checkCursorState( boolean mustBeInitialized) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkCursorState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void revertLock__wrappee__base( LN ln, LockResult lockResult) throws DatabaseException { revertLock(ln.getNodeId(),lockResult.getLockGrant()); }

	 private void revertLock( LN ln, LockResult lockResult) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	revertLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void revertLock__wrappee__base( long nodeId, LockGrantType lockStatus) throws DatabaseException { if ((lockStatus == LockGrantType.NEW) || (lockStatus == LockGrantType.WAIT_NEW)) { locker.releaseLock(nodeId); } else if ((lockStatus == LockGrantType.PROMOTION) || (lockStatus == LockGrantType.WAIT_PROMOTION)) { locker.demoteLock(nodeId); } }

	 private void revertLock( long nodeId, LockGrantType lockStatus) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	revertLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void lockEofNode__wrappee__base( LockType lockType) throws DatabaseException { locker.lock(database.getEofNodeId(),lockType,false,database); }

	 public void lockEofNode( LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockEofNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void checkEnv__wrappee__base() throws RunRecoveryException { database.getDbEnvironment().checkIfInvalid(); }

	 public void checkEnv() throws RunRecoveryException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkEnv__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public CursorImpl getLockerPrev__wrappee__base(){ return lockerPrev; }

	 public CursorImpl getLockerPrev(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockerPrev__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public CursorImpl getLockerNext__wrappee__base(){ return lockerNext; }

	 public CursorImpl getLockerNext(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockerNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLockerPrev__wrappee__base( CursorImpl p){ lockerPrev=p; }

	 public void setLockerPrev( CursorImpl p){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockerPrev__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLockerNext__wrappee__base( CursorImpl n){ lockerNext=n; }

	 public void setLockerNext( CursorImpl n){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockerNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dump__wrappee__base( boolean verbose){ System.out.println(dumpToString(verbose)); }

	 public void dump( boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dump__wrappee__base(){ System.out.println(dumpToString(true)); }

	 public void dump(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String statusToString__wrappee__base( byte status){
switch (status) {
case CURSOR_NOT_INITIALIZED: return "CURSOR_NOT_INITIALIZED";
case CURSOR_INITIALIZED: return "CURSOR_INITIALIZED";
case CURSOR_CLOSED: return "CURSOR_CLOSED";
default :
return "UNKNOWN (" + Byte.toString(status) + ")";
}
}

	 private String statusToString( byte status){ t.in(Thread.currentThread().getStackTrace()[1].toString());	statusToString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
public String dumpToString__wrappee__base(boolean verbose){
StringBuffer sb=new StringBuffer();
sb.append("<Cursor idx=\"").append(index).append("\"");
if (dupBin != null) {
sb.append(" dupIdx=\"").append(dupIndex).append("\"");
}
sb.append(" status=\"").append(statusToString(status)).append("\"");
sb.append(">\n");
if (verbose) {
sb.append((bin == null) ? "" : bin.dumpString(2,true));
sb.append((dupBin == null) ? "" : dupBin.dumpString(2,true));
}
sb.append("\n</Cursor>");
return sb.toString();
}

	 public String dumpToString(boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpToString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
public void setTestHook__wrappee__base(TestHook hook){
testHook=hook;
}

	 public void setTestHook(TestHook hook){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTestHook__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook204__wrappee__base(LN ln,long oldLsn,long newLsn) throws DatabaseException {
}

	 protected void hook204(LN ln,long oldLsn,long newLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook204__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook205__wrappee__base(LN ln,long oldLsn,long newLsn) throws DatabaseException {
}

	 protected void hook205(LN ln,long oldLsn,long newLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook205__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook206__wrappee__base() throws DatabaseException, CloneNotSupportedException {
}

	 protected void hook206() throws DatabaseException, CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook206__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook207__wrappee__base() throws DatabaseException {
}

	 protected void hook207() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook207__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook208__wrappee__base(BIN bin){
}

	 protected void hook208(BIN bin){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook208__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook209__wrappee__base(BIN abin) throws DatabaseException {
}

	 protected void hook209(BIN abin) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook209__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook210__wrappee__base(DBIN abin) throws DatabaseException {
}

	 protected void hook210(DBIN abin) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook210__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook211__wrappee__base() throws DatabaseException {
}

	 protected void hook211() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook211__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook212__wrappee__base(LockType lockType) throws DatabaseException {
if (bin.getNEntries() <= index) {
throw new ReturnInt(0);
}
Node n=bin.fetchTarget(index);
if (n != null && n.containsDuplicates()) {
DIN dupRoot=(DIN)n;
this.hook265(dupRoot);
DupCountLN dupCountLN=(DupCountLN)dupRoot.getDupCountLNRef().fetchTarget(database,dupRoot);
this.hook264(dupRoot);
if (lockType != LockType.NONE) {
locker.lock(dupCountLN.getNodeId(),lockType,false,database);
}
throw new ReturnInt(dupCountLN.getDupCount());
} else {
throw new ReturnInt(1);
}
}

	 protected void hook212(LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook212__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook213__wrappee__base(boolean isDup,LN ln,LockResult lockResult,LockResult dclLockResult,DIN dupRoot) throws DatabaseException {
isDup=(dupBin != null);
if (isDup) {
dupRoot=getLatchedDupRoot(true);
dclLockResult=lockDupCountLN(dupRoot,LockType.WRITE);
dupRoot=(DIN)bin.getTarget(index);
this.hook267();
}
setTargetBin();
long oldLsn=targetBin.getLsn(targetIndex);
byte[] lnKey=targetBin.getKey(targetIndex);
lockResult.setAbortLsn(oldLsn,targetBin.isEntryKnownDeleted(targetIndex));
long oldLNSize=0;
oldLNSize=this.hook284(ln,oldLNSize);
long newLsn=ln.delete(database,lnKey,dupKey,oldLsn,locker);
long newLNSize=0;
newLNSize=this.hook283(ln,newLNSize);
targetBin.updateEntry(targetIndex,newLsn,oldLNSize,newLNSize);
targetBin.setPendingDeleted(targetIndex);
this.hook266();
if (isDup) {
dupRoot.incrementDuplicateCount(dclLockResult,dupKey,locker,false);
this.hook268(dupRoot);
dupRoot=null;
this.hook281(lnKey);
} else {
this.hook282(lnKey);
}
this.hook204(ln,oldLsn,newLsn);
}

	 protected void hook213(boolean isDup,LN ln,LockResult lockResult,LockResult dclLockResult,DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook213__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook214__wrappee__base() throws DatabaseException {
}

	 protected void hook214() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook214__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook215__wrappee__base() throws DatabaseException {
}

	 protected void hook215() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook215__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook216__wrappee__base() throws DatabaseException {
}

	 protected void hook216() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook216__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook217__wrappee__base() throws DatabaseException {
}

	 protected void hook217() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook217__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook218__wrappee__base(DatabaseEntry data,DatabaseEntry foundKey,DatabaseEntry foundData,boolean isDup) throws DatabaseException {
LN ln=(LN)targetBin.fetchTarget(targetIndex);
byte[] lnKey=targetBin.getKey(targetIndex);
Comparator userComparisonFcn=targetBin.getKeyComparator();
if (targetBin.isEntryKnownDeleted(targetIndex) || ln == null) {
this.hook270();
throw new ReturnObject(OperationStatus.NOTFOUND);
}
LockResult lockResult=lockLN(ln,LockType.WRITE);
ln=lockResult.getLN();
if (ln == null) {
this.hook271();
throw new ReturnObject(OperationStatus.NOTFOUND);
}
byte[] foundDataBytes;
byte[] foundKeyBytes;
isDup=setTargetBin();
if (isDup) {
foundDataBytes=lnKey;
foundKeyBytes=targetBin.getDupKey();
} else {
foundDataBytes=ln.getData();
foundKeyBytes=lnKey;
}
byte[] newData;
if (data.getPartial()) {
int dlen=data.getPartialLength();
int doff=data.getPartialOffset();
int origlen=(foundDataBytes != null) ? foundDataBytes.length : 0;
int oldlen=(doff + dlen > origlen) ? doff + dlen : origlen;
int len=oldlen - dlen + data.getSize();
if (len == 0) {
newData=LogUtils.ZERO_LENGTH_BYTE_ARRAY;
} else {
newData=new byte[len];
}
int pos=0;
int slicelen=(doff < origlen) ? doff : origlen;
if (slicelen > 0) System.arraycopy(foundDataBytes,0,newData,pos,slicelen);
pos+=doff;
slicelen=data.getSize();
System.arraycopy(data.getData(),data.getOffset(),newData,pos,slicelen);
pos+=slicelen;
slicelen=origlen - (doff + dlen);
if (slicelen > 0) System.arraycopy(foundDataBytes,doff + dlen,newData,pos,slicelen);
} else {
int len=data.getSize();
if (len == 0) {
newData=LogUtils.ZERO_LENGTH_BYTE_ARRAY;
} else {
newData=new byte[len];
}
System.arraycopy(data.getData(),data.getOffset(),newData,0,len);
}
if (database.getSortedDuplicates()) {
boolean keysEqual=false;
if (foundDataBytes != null) {
keysEqual=Key.compareKeys(foundDataBytes,newData,userComparisonFcn) == 0;
}
if (!keysEqual) {
revertLock(ln,lockResult);
throw new DatabaseException("Can't replace a duplicate with different data.");
}
}
if (foundData != null) {
setDbt(foundData,foundDataBytes);
}
if (foundKey != null) {
setDbt(foundKey,foundKeyBytes);
}
long oldLsn=targetBin.getLsn(targetIndex);
lockResult.setAbortLsn(oldLsn,targetBin.isEntryKnownDeleted(targetIndex));
long oldLNSize=0;
oldLNSize=this.hook286(ln,oldLNSize);
byte[] newKey=(isDup ? targetBin.getDupKey() : lnKey);
long newLsn=ln.modify(newData,database,newKey,oldLsn,locker);
long newLNSize=0;
newLNSize=this.hook285(ln,newLNSize);
targetBin.updateEntry(targetIndex,newLsn,oldLNSize,newLNSize);
this.hook269();
this.hook205(ln,oldLsn,newLsn);
status=CURSOR_INITIALIZED;
throw new ReturnObject(OperationStatus.SUCCESS);
}

	 protected void hook218(DatabaseEntry data,DatabaseEntry foundKey,DatabaseEntry foundData,boolean isDup) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook218__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook219__wrappee__base() throws DatabaseException {
}

	 protected void hook219() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook219__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook220__wrappee__base() throws DatabaseException {
}

	 protected void hook220() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook220__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook221__wrappee__base(DatabaseEntry foundKey,DatabaseEntry foundData,LockType lockType,boolean first) throws DatabaseException {
throw new ReturnObject(fetchCurrent(foundKey,foundData,lockType,first));
}

	 protected void hook221(DatabaseEntry foundKey,DatabaseEntry foundData,LockType lockType,boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook221__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook222__wrappee__base() throws DatabaseException {
}

	 protected void hook222() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook222__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook223__wrappee__base(LockType lockType) throws DatabaseException {
assert assertCursorState(true) : dumpToString(true);
this.hook272();
if (bin == null) {
throw new ReturnObject(null);
}
LN ln=null;
if (!bin.isEntryKnownDeleted(index)) {
ln=(LN)bin.fetchTarget(index);
}
if (ln == null) {
this.hook273();
throw new ReturnObject(null);
}
addCursor(bin);
LockResult lockResult=lockLN(ln,lockType);
ln=lockResult.getLN();
throw new ReturnObject(ln);
}

	 protected void hook223(LockType lockType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook223__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook224__wrappee__base(boolean alreadyLatched) throws DatabaseException {
}

	 protected void hook224(boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook224__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected boolean hook225__wrappee__base(boolean alreadyLatched) throws DatabaseException {
return alreadyLatched;
}

	 protected boolean hook225(boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook225__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected boolean hook226__wrappee__base(boolean alreadyLatched) throws DatabaseException {
return alreadyLatched;
}

	 protected boolean hook226(boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook226__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook227__wrappee__base() throws DatabaseException {
}

	 protected void hook227() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook227__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook228__wrappee__base() throws DatabaseException {
}

	 protected void hook228() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook228__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook229__wrappee__base() throws DatabaseException {
}

	 protected void hook229() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook229__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook230__wrappee__base(boolean alreadyLatched) throws DatabaseException {
}

	 protected void hook230(boolean alreadyLatched) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook230__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook231__wrappee__base() throws DatabaseException {
}

	 protected void hook231() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook231__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook232__wrappee__base() throws DatabaseException {
}

	 protected void hook232() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook232__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook233__wrappee__base() throws DatabaseException {
}

	 protected void hook233() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook233__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook234__wrappee__base(boolean first,DIN duplicateRoot,IN in,boolean found) throws DatabaseException {
if (duplicateRoot == null) {
removeCursorBIN();
if (first) {
in=database.getTree().getFirstNode();
} else {
in=database.getTree().getLastNode();
}
if (in != null) {
assert (in instanceof BIN);
dupBin=null;
dupIndex=-1;
bin=(BIN)in;
index=(first ? 0 : (bin.getNEntries() - 1));
addCursor(bin);
TreeWalkerStatsAccumulator treeStatsAccumulator=getTreeStatsAccumulator();
if (bin.getNEntries() == 0) { found=true;
} else { Node n=null; if (!in.isEntryKnownDeleted(index)) { n=in.fetchTarget(index); } if (n != null && n.containsDuplicates()) { DIN dupRoot=(DIN)n; this.hook274(in,dupRoot); in=null; found=positionFirstOrLast(first,dupRoot); } else { if (treeStatsAccumulator != null) { if (n == null || ((LN)n).isDeleted()) { treeStatsAccumulator.incrementDeletedLNCount(); } else { treeStatsAccumulator.incrementLNCount(); } } found=true; }
}
}
} else {
removeCursorDBIN();
if (first) {
in=database.getTree().getFirstNode(duplicateRoot);
} else {
in=database.getTree().getLastNode(duplicateRoot);
}
if (in != null) {
assert (in instanceof DBIN);
dupBin=(DBIN)in;
dupIndex=(first ? 0 : (dupBin.getNEntries() - 1));
addCursor(dupBin);
found=true;
}
}
status=CURSOR_INITIALIZED;
throw new ReturnBoolean(found);
}

	 protected void hook234(boolean first,DIN duplicateRoot,IN in,boolean found) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook234__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook235__wrappee__base(DatabaseEntry matchKey,DatabaseEntry matchData,SearchMode searchMode,LockType lockType,boolean foundSomething,boolean foundExactKey,boolean foundExactData,boolean foundLast,boolean exactSearch,BINBoundary binBoundary) throws DatabaseException {
byte[] key=Key.makeKey(matchKey);
bin=(BIN)database.getTree().search(key,Tree.SearchType.NORMAL,-1,binBoundary,true);
if (bin != null) {
addCursor(bin);
index=bin.findEntry(key,true,exactSearch);
foundSomething=!exactSearch;
dupBin=null;
dupIndex=-1;
boolean containsDuplicates=false;
if (index >= 0) {
if ((index & IN.EXACT_MATCH) != 0) { foundExactKey=true; index&=~IN.EXACT_MATCH;
}
Node n=null;
if (!bin.isEntryKnownDeleted(index)) { n=bin.fetchTarget(index);
}
if (n != null) { containsDuplicates=n.containsDuplicates(); if (searchMode.isDataSearch()) { if (foundExactKey) { int searchResult=searchAndPositionBoth(containsDuplicates,n,matchData,exactSearch,lockType,bin.getLsn(index)); foundSomething=(searchResult & FOUND) != 0; foundExactData=(searchResult & EXACT_DATA) != 0; } } else { foundSomething=true; if (!containsDuplicates && exactSearch) { LN ln=(LN)n; LockResult lockResult=lockLN(ln,lockType); ln=lockResult.getLN(); if (ln == null) { foundSomething=false; } } }
}
foundLast=(searchMode == SearchMode.SET_RANGE && foundSomething && !containsDuplicates && binBoundary.isLastBin && index == bin.getNEntries() - 1);
}
}
status=CURSOR_INITIALIZED;
throw new ReturnInt((foundSomething ? FOUND : 0) | (foundExactKey ? EXACT_KEY : 0) | (foundExactData ? EXACT_DATA : 0)| (foundLast ? FOUND_LAST : 0));
}

	 protected void hook235(DatabaseEntry matchKey,DatabaseEntry matchData,SearchMode searchMode,LockType lockType,boolean foundSomething,boolean foundExactKey,boolean foundExactData,boolean foundLast,boolean exactSearch,BINBoundary binBoundary) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook235__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook236__wrappee__base(DIN duplicateRoot) throws DatabaseException {
}

	 protected void hook236(DIN duplicateRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook236__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook237__wrappee__base() throws DatabaseException {
}

	 protected void hook237() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook237__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook238__wrappee__base() throws DatabaseException {
}

	 protected void hook238() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook238__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook239__wrappee__base(DIN dupRoot) throws DatabaseException {
}

	 protected void hook239(DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook239__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook240__wrappee__base() throws DatabaseException {
}

	 protected void hook240() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook240__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook241__wrappee__base(DIN dupRoot) throws DatabaseException {
}

	 protected void hook241(DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook241__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook242__wrappee__base(boolean isDBINLatched,DIN dupRoot) throws DatabaseException {
}

	 protected void hook242(boolean isDBINLatched,DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook242__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook243__wrappee__base() throws DatabaseException {
}

	 protected void hook243() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook243__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook264__wrappee__base(DIN dupRoot) throws DatabaseException {
}

	 protected void hook264(DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook264__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook265__wrappee__base(DIN dupRoot) throws DatabaseException {
}

	 protected void hook265(DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook265__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook266__wrappee__base() throws DatabaseException {
}

	 protected void hook266() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook266__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook267__wrappee__base() throws DatabaseException {
}

	 protected void hook267() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook267__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook268__wrappee__base(DIN dupRoot) throws DatabaseException {
}

	 protected void hook268(DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook268__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook269__wrappee__base() throws DatabaseException {
}

	 protected void hook269() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook269__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook270__wrappee__base() throws DatabaseException {
}

	 protected void hook270() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook270__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook271__wrappee__base() throws DatabaseException {
}

	 protected void hook271() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook271__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook272__wrappee__base() throws DatabaseException {
}

	 protected void hook272() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook272__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook273__wrappee__base() throws DatabaseException {
}

	 protected void hook273() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook273__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook274__wrappee__base(IN in,DIN dupRoot) throws DatabaseException {
}

	 protected void hook274(IN in,DIN dupRoot) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook274__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook276__wrappee__base() throws DatabaseException {
}

	 protected void hook276() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook276__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook277__wrappee__base() throws DatabaseException {
}

	 protected void hook277() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook277__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook278__wrappee__base() throws DatabaseException {
}

	 protected void hook278() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook278__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook281__wrappee__base(byte[] lnKey) throws DatabaseException {
}

	 protected void hook281(byte[] lnKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook281__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected void hook282__wrappee__base(byte[] lnKey) throws DatabaseException {
}

	 protected void hook282(byte[] lnKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook282__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected long hook283__wrappee__base(LN ln,long newLNSize) throws DatabaseException {
return newLNSize;
}

	 protected long hook283(LN ln,long newLNSize) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook283__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected long hook284__wrappee__base(LN ln,long oldLNSize) throws DatabaseException {
return oldLNSize;
}

	 protected long hook284(LN ln,long oldLNSize) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook284__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected long hook285__wrappee__base(LN ln,long newLNSize) throws DatabaseException {
return newLNSize;
}

	 protected long hook285(LN ln,long newLNSize) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook285__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	
protected long hook286__wrappee__base(LN ln,long oldLNSize) throws DatabaseException {
return oldLNSize;
}

	 protected long hook286(LN ln,long oldLNSize) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook286__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
