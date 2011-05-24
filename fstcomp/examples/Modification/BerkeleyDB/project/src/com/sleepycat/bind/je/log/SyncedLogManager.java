package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.List; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.TrackedFileSummary; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  SyncedLogManager  extends LogManager {
	 public SyncedLogManager( EnvironmentImpl envImpl, boolean readOnly) throws DatabaseException { super(envImpl,readOnly); }

	 protected LogResult logItem__wrappee__base( LoggableObject item, boolean isProvisional, boolean flushRequired, boolean forceNewLogFile, long oldNodeLsn, boolean marshallOutsideLatch, ByteBuffer marshalledBuffer, UtilizationTracker tracker) throws IOException, DatabaseException { try { this.hook511(item,isProvisional,flushRequired,forceNewLogFile,oldNodeLsn,marshallOutsideLatch,marshalledBuffer,tracker); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (LogResult)r.value; } }

	 protected LogResult logItem( LoggableObject item, boolean isProvisional, boolean flushRequired, boolean forceNewLogFile, long oldNodeLsn, boolean marshallOutsideLatch, ByteBuffer marshalledBuffer, UtilizationTracker tracker) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	logItem__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void flushInternal__wrappee__base() throws LogException, DatabaseException { try { this.hook512(); } catch ( IOException e) { throw new LogException(e.getMessage()); } }

	 protected void flushInternal() throws LogException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	flushInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public TrackedFileSummary getUnflushableTrackedSummary__wrappee__base( long file) throws DatabaseException { try { this.hook513(file); throw ReturnHack.returnObject; } catch ( ReturnObject r) { return (TrackedFileSummary)r.value; } }

	 public TrackedFileSummary getUnflushableTrackedSummary( long file) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getUnflushableTrackedSummary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void countObsoleteNode__wrappee__base( long lsn, LogEntryType type) throws DatabaseException { UtilizationTracker tracker=envImpl.getUtilizationTracker(); this.hook514(lsn,type,tracker); }

	 public void countObsoleteNode( long lsn, LogEntryType type) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	countObsoleteNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void countObsoleteNodes__wrappee__base( TrackedFileSummary[] summaries) throws DatabaseException { UtilizationTracker tracker=envImpl.getUtilizationTracker(); this.hook515(summaries,tracker); }

	 public void countObsoleteNodes( TrackedFileSummary[] summaries) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	countObsoleteNodes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void countObsoleteINs__wrappee__base( List lsnList) throws DatabaseException { countObsoleteINsInternal(lsnList); }

	 public void countObsoleteINs( List lsnList) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	countObsoleteINs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook511__wrappee__base( LoggableObject item, boolean isProvisional, boolean flushRequired, boolean forceNewLogFile, long oldNodeLsn, boolean marshallOutsideLatch, ByteBuffer marshalledBuffer, UtilizationTracker tracker) throws IOException, DatabaseException { throw new ReturnObject(logInternal(item,isProvisional,flushRequired,forceNewLogFile,oldNodeLsn,marshallOutsideLatch,marshalledBuffer,tracker)); }

	 protected void hook511( LoggableObject item, boolean isProvisional, boolean flushRequired, boolean forceNewLogFile, long oldNodeLsn, boolean marshallOutsideLatch, ByteBuffer marshalledBuffer, UtilizationTracker tracker) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook511__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook512__wrappee__base() throws LogException, DatabaseException, IOException { logBufferPool.writeBufferToFile(0); }

	 protected void hook512() throws LogException, DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook512__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook513__wrappee__base( long file) throws DatabaseException { throw new ReturnObject(getUnflushableTrackedSummaryInternal(file)); }

	 protected void hook513( long file) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook513__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook514__wrappee__base( long lsn, LogEntryType type, UtilizationTracker tracker) throws DatabaseException { countObsoleteNodeInternal(tracker,lsn,type); }

	 protected void hook514( long lsn, LogEntryType type, UtilizationTracker tracker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook514__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook515__wrappee__base( TrackedFileSummary[] summaries, UtilizationTracker tracker) throws DatabaseException { countObsoleteNodesInternal(tracker,summaries); }

	 protected void hook515( TrackedFileSummary[] summaries, UtilizationTracker tracker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook515__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
