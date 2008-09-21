package com.sleepycat.je.dbi; 
import com.sleepycat.je.PreloadStats; 
import com.sleepycat.je.dbi.SortedLSNTreeWalker.TreeNodeProcessor; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
 
class  PreloadProcessor  implements TreeNodeProcessor {
	 private EnvironmentImpl envImpl;

	 private long maxBytes;

	 private long targetTime;

	 PreloadProcessor( EnvironmentImpl envImpl, long maxBytes, long targetTime, PreloadStats stats){ this.envImpl=envImpl; this.maxBytes=maxBytes; this.targetTime=targetTime; this.hook353(stats); }

	 public void processLSN__wrappee__base( long childLsn, LogEntryType childType){ assert childLsn != DbLsn.NULL_LSN; if (System.currentTimeMillis() > targetTime) { throw DatabaseImpl.timeExceededPreloadException; } this.hook355(); this.hook354(childType); }

	 public void processLSN( long childLsn, LogEntryType childType){ t.in(Thread.currentThread().getStackTrace()[1].toString());	processLSN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook353__wrappee__base( PreloadStats stats){ }

	 protected void hook353( PreloadStats stats){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook353__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook354__wrappee__base( LogEntryType childType){ }

	 protected void hook354( LogEntryType childType){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook354__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook355__wrappee__base(){ }

	 protected void hook355(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook355__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
