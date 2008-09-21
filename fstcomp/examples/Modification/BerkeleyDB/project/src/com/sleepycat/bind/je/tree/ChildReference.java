package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.LogFileNotFoundException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  ChildReference  implements LogWritable, LogReadable {
	 private Node target;

	 private long lsn;

	 private byte[] key;

	 private byte state;

	 private static final byte KNOWN_DELETED_BIT=0x1;

	 private static final byte DIRTY_BIT=0x2;

	 private static final byte CLEAR_DIRTY_BIT=~0x2;

	 private static final byte MIGRATE_BIT=0x4;

	 private static final byte CLEAR_MIGRATE_BIT=~0x4;

	 private static final byte PENDING_DELETED_BIT=0x8;

	 ChildReference(){ init(null,Key.EMPTY_KEY,DbLsn.NULL_LSN,0); }

	 public ChildReference( Node target, byte[] key, long lsn){ init(target,key,lsn,DIRTY_BIT); }

	 public ChildReference( Node target, byte[] key, long lsn, byte existingState){ init(target,key,lsn,existingState | DIRTY_BIT); }

	 private void init__wrappee__base( Node target, byte[] key, long lsn, int state){ this.target=target; this.key=key; this.lsn=lsn; this.state=(byte)state; }

	 private void init( Node target, byte[] key, long lsn, int state){ t.in(Thread.currentThread().getStackTrace()[1].toString());	init__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base(){ return key; }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setKey__wrappee__base( byte[] key){ this.key=key; state|=DIRTY_BIT; }

	 public void setKey( byte[] key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Node fetchTarget__wrappee__base( DatabaseImpl database, IN in) throws DatabaseException { if (target == null) { if (lsn == DbLsn.NULL_LSN) { if (!isKnownDeleted()) { throw new DatabaseException(IN.makeFetchErrorMsg("NULL_LSN without KnownDeleted",in,lsn,state)); } } else { try { EnvironmentImpl env=database.getDbEnvironment(); Node node=(Node)env.getLogManager().get(lsn); node.postFetchInit(database,lsn); target=node; this.hook613(in); } catch ( LogFileNotFoundException LNFE) { if (!isKnownDeleted() && !isPendingDeleted()) { throw new DatabaseException(IN.makeFetchErrorMsg(LNFE.toString(),in,lsn,state),LNFE); } }
catch ( Exception e) { throw new DatabaseException(IN.makeFetchErrorMsg(e.toString(),in,lsn,state),e); } } } return target; }

	 public Node fetchTarget( DatabaseImpl database, IN in) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	fetchTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 byte getState__wrappee__base(){ return state; }

	 byte getState(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Node getTarget__wrappee__base(){ return target; }

	 public Node getTarget(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTarget__wrappee__base( Node target){ this.target=target; }

	 public void setTarget( Node target){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void clearTarget__wrappee__base(){ this.target=null; }

	 public void clearTarget(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearTarget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLsn__wrappee__base(){ return lsn; }

	 public long getLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setLsn__wrappee__base( long lsn){ this.lsn=lsn; state|=DIRTY_BIT; }

	 public void setLsn( long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean isPendingDeleted__wrappee__base(){ return ((state & PENDING_DELETED_BIT) != 0); }

	 private boolean isPendingDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isPendingDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isKnownDeleted__wrappee__base(){ return ((state & KNOWN_DELETED_BIT) != 0); }

	 public boolean isKnownDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean isDirty__wrappee__base(){ return ((state & DIRTY_BIT) != 0); }

	 private boolean isDirty(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDirty__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getMigrate__wrappee__base(){ return (state & MIGRATE_BIT) != 0; }

	 public boolean getMigrate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMigrate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setMigrate__wrappee__base( boolean migrate){ if (migrate) { state|=MIGRATE_BIT; } else { state&=CLEAR_MIGRATE_BIT; } }

	 public void setMigrate( boolean migrate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setMigrate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return LogUtils.getByteArrayLogSize(key) + LogUtils.getLongLogSize() + 1; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeByteArray(logBuffer,key); assert lsn != DbLsn.NULL_LSN; LogUtils.writeLong(logBuffer,lsn); logBuffer.put(state); state&=CLEAR_DIRTY_BIT; }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion){ key=LogUtils.readByteArray(itemBuffer); lsn=LogUtils.readLong(itemBuffer); state=itemBuffer.get(); state&=CLEAR_DIRTY_BIT; }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<ref knownDeleted=\"").append(isKnownDeleted()); sb.append("\" pendingDeleted=\"").append(isPendingDeleted()); sb.append("\">"); sb.append(Key.dumpString(key,0)); sb.append(DbLsn.toString(lsn)); sb.append("</ref>"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String dumpString__wrappee__base( int nspaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); if (lsn == DbLsn.NULL_LSN) { sb.append(TreeUtils.indent(nspaces)); sb.append("<lsn/>"); } else { sb.append(DbLsn.dumpString(lsn,nspaces)); } sb.append('\n'); if (key == null) { sb.append(TreeUtils.indent(nspaces)); sb.append("<key/>"); } else { sb.append(Key.dumpString(key,nspaces)); } sb.append('\n'); if (target == null) { sb.append(TreeUtils.indent(nspaces)); sb.append("<target/>"); } else { sb.append(target.dumpString(nspaces,true)); } sb.append('\n'); sb.append(TreeUtils.indent(nspaces)); sb.append("<knownDeleted val=\""); sb.append(isKnownDeleted()).append("\"/>"); sb.append("<pendingDeleted val=\""); sb.append(isPendingDeleted()).append("\"/>"); sb.append("<dirty val=\"").append(isDirty()).append("\"/>"); return sb.toString(); }

	 String dumpString( int nspaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,false); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook613__wrappee__base( IN in) throws DatabaseException, LogFileNotFoundException, Exception { }

	 protected void hook613( IN in) throws DatabaseException, LogFileNotFoundException, Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook613__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
