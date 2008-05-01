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

	 public boolean getAllowFlush(){ return allowFlush; }

	 void setAllowFlush( boolean allowFlush){ this.allowFlush=allowFlush; }

	 public long getFileNumber(){ return fileNum; }

	 public void reset(){ obsoleteOffsets=null; tracker.resetFile(this); this.hook168(); super.reset(); }

	 void trackObsolete( long offset){ new TrackedFileSummary_trackObsolete(this,offset).execute(); }

	 void addTrackedSummary( TrackedFileSummary other){ add(other); if (other.obsoleteOffsets != null) { if (obsoleteOffsets != null) { if (obsoleteOffsets.merge(other.obsoleteOffsets)) { this.hook169(); } } else { obsoleteOffsets=other.obsoleteOffsets; } } }

	 public long[] getObsoleteOffsets(){ if (obsoleteOffsets != null) { return obsoleteOffsets.toArray(); } else { return null; } }

	 boolean containsObsoleteOffset( long offset){ if (obsoleteOffsets != null) { return obsoleteOffsets.contains(offset); } else { return false; } }

	
@MethodObject static  class  TrackedFileSummary_trackObsolete {
		 TrackedFileSummary_trackObsolete( TrackedFileSummary _this, long offset){ this._this=_this; this.offset=offset; }

		 void execute(){ if (!_this.trackDetail) { return; } this.hook170(); if (_this.obsoleteOffsets == null) { _this.obsoleteOffsets=new OffsetList(); this.hook171(); } if (_this.obsoleteOffsets.add(offset,_this.tracker.getEnvironment().isOpen())) { this.hook172(); } }

		 protected TrackedFileSummary _this;

		 protected long offset;

		 protected int adjustMem;

		 protected void hook170(){ }

		 protected void hook171(){ }

		 protected void hook172(){ }


	}

	 protected void hook168(){ }

	 protected void hook169(){ }


}
