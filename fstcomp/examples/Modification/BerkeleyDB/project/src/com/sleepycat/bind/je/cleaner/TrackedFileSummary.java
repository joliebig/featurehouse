package com.sleepycat.je.cleaner; 
import com.sleepycat.je.dbi.MemoryBudget; 
import de.ovgu.cide.jakutil.*; 
public  class  TrackedFileSummary  extends FileSummary {
	 private UtilizationTracker tracker;

	 private long fileNum;

	 private OffsetList obsoleteOffsets;

	 private boolean trackDetail;

	 private boolean allowFlush=true;

	 TrackedFileSummary( UtilizationTracker tracker, long fileNum, boolean trackDetail){ this.tracker=tracker; this.fileNum=fileNum; this.trackDetail=trackDetail; }

	
@MethodObject static  class  TrackedFileSummary_trackObsolete {
		 TrackedFileSummary_trackObsolete( TrackedFileSummary _this, long offset){ this._this=_this; this.offset=offset; }

		 protected TrackedFileSummary _this;

		 protected long offset;

		 protected int adjustMem;

		 void execute__wrappee__base(){ if (!_this.trackDetail) { return; } this.hook170(); if (_this.obsoleteOffsets == null) { _this.obsoleteOffsets=new OffsetList(); this.hook171(); } if (_this.obsoleteOffsets.add(offset,_this.tracker.getEnvironment().isOpen())) { this.hook172(); } }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook170__wrappee__base(){ }

		 protected void hook170(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook170__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook171__wrappee__base(){ }

		 protected void hook171(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook171__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook172__wrappee__base(){ }

		 protected void hook172(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook172__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public boolean getAllowFlush__wrappee__base(){ return allowFlush; }

	 public boolean getAllowFlush(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAllowFlush__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setAllowFlush__wrappee__base( boolean allowFlush){ this.allowFlush=allowFlush; }

	 void setAllowFlush( boolean allowFlush){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAllowFlush__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getFileNumber__wrappee__base(){ return fileNum; }

	 public long getFileNumber(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void reset__wrappee__base(){ obsoleteOffsets=null; tracker.resetFile(this); this.hook168(); super.reset(); }

	 public void reset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void trackObsolete__wrappee__base( long offset){ new TrackedFileSummary_trackObsolete(this,offset).execute(); }

	 void trackObsolete( long offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	trackObsolete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void addTrackedSummary__wrappee__base( TrackedFileSummary other){ add(other); if (other.obsoleteOffsets != null) { if (obsoleteOffsets != null) { if (obsoleteOffsets.merge(other.obsoleteOffsets)) { this.hook169(); } } else { obsoleteOffsets=other.obsoleteOffsets; } } }

	 void addTrackedSummary( TrackedFileSummary other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addTrackedSummary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long\[\] getObsoleteOffsets__wrappee__base(){ if (obsoleteOffsets != null) { return obsoleteOffsets.toArray(); } else { return null; } }

	 public long[] getObsoleteOffsets(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getObsoleteOffsets__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean containsObsoleteOffset__wrappee__base( long offset){ if (obsoleteOffsets != null) { return obsoleteOffsets.contains(offset); } else { return false; } }

	 boolean containsObsoleteOffset( long offset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	containsObsoleteOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook168__wrappee__base(){ }

	 protected void hook168(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook168__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook169__wrappee__base(){ }

	 protected void hook169(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook169__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
