package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import java.util.ArrayList; 
import java.util.List; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  BINDelta  implements LoggableObject, LogReadable {
	 private DatabaseId dbId;

	 private long lastFullLsn;

	 private List deltas;

	 private LogEntryType logEntryType;

	 public BINDelta( BIN bin){ lastFullLsn=bin.getLastFullVersion(); dbId=bin.getDatabaseId(); deltas=new ArrayList(); logEntryType=bin.getBINDeltaType(); for (int i=0; i < bin.getNEntries(); i++) { if (bin.isDirty(i)) { deltas.add(new DeltaInfo(bin.getKey(i),bin.getLsn(i),bin.getState(i))); } } }

	 public BINDelta(){ dbId=new DatabaseId(); lastFullLsn=DbLsn.NULL_LSN; deltas=new ArrayList(); }

	 int getNumDeltas__wrappee__base(){ return deltas.size(); }

	 int getNumDeltas(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNumDeltas__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDbId__wrappee__base(){ return dbId; }

	 public DatabaseId getDbId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLastFullLsn__wrappee__base(){ return lastFullLsn; }

	 public long getLastFullLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastFullLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public BIN reconstituteBIN__wrappee__base( EnvironmentImpl env) throws DatabaseException { BIN fullBIN=(BIN)env.getLogManager().get(lastFullLsn); DatabaseImpl db=env.getDbMapTree().getDb(dbId); fullBIN.setDatabase(db); fullBIN.setLastFullLsn(lastFullLsn); this.hook612(fullBIN); for (int i=0; i < deltas.size(); i++) { DeltaInfo info=(DeltaInfo)deltas.get(i); int foundIndex=fullBIN.findEntry(info.getKey(),true,false); if (foundIndex >= 0 && (foundIndex & IN.EXACT_MATCH) != 0) { foundIndex&=~IN.EXACT_MATCH; if (info.isKnownDeleted()) { fullBIN.setKnownDeleted(foundIndex); } else { fullBIN.updateEntry(foundIndex,info.getLsn(),info.getState()); } } else { if (!info.isKnownDeleted()) { ChildReference entry=new ChildReference(null,info.getKey(),info.getLsn(),info.getState()); boolean insertOk=fullBIN.insertEntry(entry); assert insertOk; } } } fullBIN.setGeneration(0); this.hook611(fullBIN); return fullBIN; }

	 public BIN reconstituteBIN( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	reconstituteBIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return logEntryType; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return true; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn){ }

	 public void postLogWork( long justLoggedLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { dbId.readFromLog(itemBuffer,entryTypeVersion); lastFullLsn=LogUtils.readLong(itemBuffer); int numDeltas=LogUtils.readInt(itemBuffer); for (int i=0; i < numDeltas; i++) { DeltaInfo info=new DeltaInfo(); info.readFromLog(itemBuffer,entryTypeVersion); deltas.add(info); } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=dbId.getLogSize() + LogUtils.LONG_BYTES + LogUtils.INT_BYTES; for (int i=0; i < deltas.size(); i++) { DeltaInfo info=(DeltaInfo)deltas.get(i); size+=info.getLogSize(); } return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ dbId.writeToLog(logBuffer); LogUtils.writeLong(logBuffer,lastFullLsn); LogUtils.writeInt(logBuffer,deltas.size()); for (int i=0; i < deltas.size(); i++) { DeltaInfo info=(DeltaInfo)deltas.get(i); info.writeToLog(logBuffer); } }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ dbId.dumpLog(sb,verbose); sb.append("<lastFullLsn>"); sb.append(DbLsn.toString(lastFullLsn)); sb.append("</lastFullLsn>"); sb.append("<deltas size=\"").append(deltas.size()).append("\"/>"); for (int i=0; i < deltas.size(); i++) { DeltaInfo info=(DeltaInfo)deltas.get(i); info.dumpLog(sb,verbose); } }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook611__wrappee__base( BIN fullBIN) throws DatabaseException { }

	 protected void hook611( BIN fullBIN) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook611__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook612__wrappee__base( BIN fullBIN) throws DatabaseException { }

	 protected void hook612( BIN fullBIN) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook612__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
