package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogUtils; 
import de.ovgu.cide.jakutil.*; 
public final  class  MapLN  extends LN {
	 private static final String BEGIN_TAG="<mapLN>";

	 private static final String END_TAG="</mapLN>";

	 private DatabaseImpl databaseImpl;

	 private boolean deleted;

	 public MapLN( DatabaseImpl db){ super(new byte[0]); databaseImpl=db; deleted=false; }

	 public MapLN() throws DatabaseException { super(); databaseImpl=new DatabaseImpl(); }

	 public boolean isDeleted__wrappee__base(){ return deleted; }

	 public boolean isDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void makeDeleted__wrappee__base(){ deleted=true; databaseImpl.getTree().setRoot(null,true); }

	 void makeDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseImpl getDatabase__wrappee__base(){ return databaseImpl; }

	 public DatabaseImpl getDatabase(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postFetchInit__wrappee__base( DatabaseImpl db, long sourceLsn) throws DatabaseException { databaseImpl.setEnvironmentImpl(db.getDbEnvironment()); }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postFetchInit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(super.dumpString(nSpaces,dumpTags)); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces)); sb.append("<deleted val=\"").append(Boolean.toString(deleted)); sb.append("\">"); sb.append('\n'); sb.append(databaseImpl.dumpString(nSpaces)); return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected LogEntryType getTransactionalLogType__wrappee__base(){ return LogEntryType.LOG_MAPLN_TRANSACTIONAL; }

	 protected LogEntryType getTransactionalLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionalLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_MAPLN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return super.getLogSize() + databaseImpl.getLogSize() + LogUtils.getBooleanLogSize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ super.writeToLog(logBuffer); databaseImpl.writeToLog(logBuffer); LogUtils.writeBoolean(logBuffer,deleted); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); databaseImpl.readFromLog(itemBuffer,entryTypeVersion); deleted=LogUtils.readBoolean(itemBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb, boolean verbose){ databaseImpl.dumpLog(sb,true); }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
