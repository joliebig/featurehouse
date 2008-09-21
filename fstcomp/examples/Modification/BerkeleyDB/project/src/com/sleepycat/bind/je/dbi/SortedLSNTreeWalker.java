package com.sleepycat.je.dbi; 
import java.util.Arrays; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.OffsetList; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.DBIN; 
import com.sleepycat.je.tree.DIN; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.Node; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  SortedLSNTreeWalker {
	
public  interface  TreeNodeProcessor {
		 void processLSN( long childLSN, LogEntryType childType);


	}

	 protected DatabaseImpl dbImpl;

	 private EnvironmentImpl envImpl;

	 private long rootLsn;

	 private boolean dups;

	 private boolean removeINsFromINList;

	 private boolean setDbState;

	 private long[] currentLSNs;

	 private int currentLSNIdx=0;

	 private OffsetList accumulatedLSNFileNumbers;

	 private OffsetList accumulatedLSNFileOffsets;

	 private TreeNodeProcessor callback;

	 protected boolean accumulateLNs=false;

	 public SortedLSNTreeWalker( DatabaseImpl dbImpl, boolean removeINsFromINList, boolean setDbState, long rootLsn, TreeNodeProcessor callback) throws DatabaseException { this.dbImpl=dbImpl; this.envImpl=dbImpl.getDbEnvironment(); if (envImpl == null) { throw new DatabaseException("environmentImpl is null for target db " + dbImpl.getDebugName()); } this.dups=dbImpl.getSortedDuplicates(); this.removeINsFromINList=removeINsFromINList; this.setDbState=setDbState; this.rootLsn=rootLsn; this.callback=callback; currentLSNs=new long[0]; currentLSNIdx=0; }

	
@MethodObject static  class  SortedLSNTreeWalker_extractINsForDb {
		 SortedLSNTreeWalker_extractINsForDb( SortedLSNTreeWalker _this, INList inList){ this._this=_this; this.inList=inList; }

		 protected SortedLSNTreeWalker _this;

		 protected INList inList;

		 protected boolean foundSome;

		 protected Set foundSet;

		 protected long memoryChange;

		 protected MemoryBudget mb;

		 protected Iterator iter;

		 protected IN thisIN;

		 protected Iterator iter1;

		 protected IN thisIN1;

		 boolean execute__wrappee__base() throws DatabaseException { foundSome=false; foundSet=new HashSet(); this.hook360(); this.hook356(); try { this.hook357(); iter=inList.iterator(); while (iter.hasNext()) { thisIN=(IN)iter.next(); if (thisIN.getDatabase() == _this.dbImpl) { foundSome=true; if (_this.removeINsFromINList) { iter.remove(); this.hook361(); } foundSet.add(thisIN); } } } catch ( DatabaseException e) { this.hook362(); throw e; } finally { this.hook358(); } if (foundSome) { iter1=foundSet.iterator(); while (iter1.hasNext()) { thisIN1=(IN)iter1.next(); _this.accumulateLSNs(thisIN1); } } foundSet=null; return foundSome; }

		 boolean execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook356__wrappee__base() throws DatabaseException { }

		 protected void hook356() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook356__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook357__wrappee__base() throws DatabaseException { }

		 protected void hook357() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook357__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook358__wrappee__base() throws DatabaseException { }

		 protected void hook358() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook358__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook360__wrappee__base() throws DatabaseException { }

		 protected void hook360() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook360__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook361__wrappee__base() throws DatabaseException { }

		 protected void hook361() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook361__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook362__wrappee__base() throws DatabaseException { }

		 protected void hook362() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook362__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 private boolean extractINsForDb__wrappee__base( INList inList) throws DatabaseException { return new SortedLSNTreeWalker_extractINsForDb(this,inList).execute(); }

	 private boolean extractINsForDb( INList inList) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	extractINsForDb__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void walk__wrappee__base() throws DatabaseException { walkInternal(); }

	 public void walk() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	walk__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void walkInternal__wrappee__base() throws DatabaseException { INList inList=envImpl.getInMemoryINs(); IN root=null; if (!extractINsForDb(inList)) { if (rootLsn == DbLsn.NULL_LSN) { return; } root=getRootIN(rootLsn); accumulateLSNs(root); releaseRootIN(root); } this.hook359(); while (true) { maybeGetMoreINs(); if (currentLSNs != null && currentLSNIdx < currentLSNs.length) { fetchAndProcessLSN(currentLSNs[currentLSNIdx++]); } else { break; } } }

	 protected void walkInternal() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	walkInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void maybeGetMoreINs__wrappee__base(){ if ((currentLSNs != null && currentLSNIdx >= currentLSNs.length)) { if (accumulatedLSNFileNumbers == null || accumulatedLSNFileNumbers.size() == 0) { currentLSNs=null; currentLSNIdx=Integer.MAX_VALUE; return; } long[] tempFileNumbers=accumulatedLSNFileNumbers.toArray(); long[] tempFileOffsets=accumulatedLSNFileOffsets.toArray(); int nLSNs=tempFileNumbers.length; currentLSNIdx=0; currentLSNs=new long[nLSNs]; for (int i=0; i < nLSNs; i++) { currentLSNs[i]=DbLsn.makeLsn(tempFileNumbers[i],tempFileOffsets[i]); } Arrays.sort(currentLSNs); accumulatedLSNFileNumbers=null; accumulatedLSNFileOffsets=null; } }

	 private void maybeGetMoreINs(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	maybeGetMoreINs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void accumulateLSNs__wrappee__base( IN in) throws DatabaseException { boolean accumulate=true; if (!accumulateLNs) { if ((!dups && (in instanceof BIN)) || (in instanceof DBIN)) { accumulate=false; } } for (int i=0; i < in.getNEntries(); i++) { if (in.isEntryPendingDeleted(i) || in.isEntryKnownDeleted(i)) { continue; } long lsn=in.getLsn(i); Node node=in.getTarget(i); if (accumulate && (node == null)) { if (accumulatedLSNFileNumbers == null) { accumulatedLSNFileNumbers=new OffsetList(); accumulatedLSNFileOffsets=new OffsetList(); } accumulatedLSNFileNumbers.add(DbLsn.getFileNumber(lsn),false); accumulatedLSNFileOffsets.add(DbLsn.getFileOffset(lsn),false); addToLsnINMap(new Long(lsn),in,i); } else { callback.processLSN(lsn,(node == null) ? LogEntryType.LOG_LN : node.getLogType()); } } if (in instanceof DIN) { if (in.isRoot()) { DIN din=(DIN)in; callback.processLSN(din.getDupCountLNRef().getLsn(),LogEntryType.LOG_DUPCOUNTLN); } } }

	 private void accumulateLSNs( IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	accumulateLSNs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void fetchAndProcessLSN__wrappee__base( long lsn) throws DatabaseException { Node node=fetchLSN(lsn); if (node != null) { callback.processLSN(lsn,node.getLogType()); if (node instanceof IN) { accumulateLSNs((IN)node); } } }

	 private void fetchAndProcessLSN( long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	fetchAndProcessLSN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected IN getRootIN__wrappee__base( long rootLsn) throws DatabaseException { return (IN)envImpl.getLogManager().get(rootLsn); }

	 protected IN getRootIN( long rootLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getRootIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void releaseRootIN__wrappee__base( IN ignore) throws DatabaseException { }

	 protected void releaseRootIN( IN ignore) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseRootIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void addToLsnINMap__wrappee__base( Long lsn, IN in, int index){ }

	 protected void addToLsnINMap( Long lsn, IN in, int index){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addToLsnINMap__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Node fetchLSN__wrappee__base( long lsn) throws DatabaseException { return (Node)envImpl.getLogManager().get(lsn); }

	 protected Node fetchLSN( long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	fetchLSN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook359__wrappee__base() throws DatabaseException { }

	 protected void hook359() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook359__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
