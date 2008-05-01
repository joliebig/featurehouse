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

	 private void init( Node target, byte[] key, long lsn, int state){ this.target=target; this.key=key; this.lsn=lsn; this.state=(byte)state; }

	 public byte[] getKey(){ return key; }

	 public void setKey( byte[] key){ this.key=key; state|=DIRTY_BIT; }

	 public Node fetchTarget( DatabaseImpl database, IN in) throws DatabaseException { if (target == null) { if (lsn == DbLsn.NULL_LSN) { if (!isKnownDeleted()) { throw new DatabaseException(IN.makeFetchErrorMsg("NULL_LSN without KnownDeleted",in,lsn,state)); } } else { try { EnvironmentImpl env=database.getDbEnvironment(); Node node=(Node)env.getLogManager().get(lsn); node.postFetchInit(database,lsn); target=node; this.hook613(in); } catch ( LogFileNotFoundException LNFE) { if (!isKnownDeleted() && !isPendingDeleted()) { throw new DatabaseException(IN.makeFetchErrorMsg(LNFE.toString(),in,lsn,state),LNFE); } }
catch ( Exception e) { throw new DatabaseException(IN.makeFetchErrorMsg(e.toString(),in,lsn,state),e); } } } return target; }

	 byte getState(){ return state; }

	 public Node getTarget(){ return target; }

	 public void setTarget( Node target){ this.target=target; }

	 public void clearTarget(){ this.target=null; }

	 public long getLsn(){ return lsn; }

	 public void setLsn( long lsn){ this.lsn=lsn; state|=DIRTY_BIT; }

	 private boolean isPendingDeleted(){ return ((state & PENDING_DELETED_BIT) != 0); }

	 public boolean isKnownDeleted(){ return ((state & KNOWN_DELETED_BIT) != 0); }

	 private boolean isDirty(){ return ((state & DIRTY_BIT) != 0); }

	 public boolean getMigrate(){ return (state & MIGRATE_BIT) != 0; }

	 public void setMigrate( boolean migrate){ if (migrate) { state|=MIGRATE_BIT; } else { state&=CLEAR_MIGRATE_BIT; } }

	 public int getLogSize(){ return LogUtils.getByteArrayLogSize(key) + LogUtils.getLongLogSize() + 1; }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeByteArray(logBuffer,key); assert lsn != DbLsn.NULL_LSN; LogUtils.writeLong(logBuffer,lsn); logBuffer.put(state); state&=CLEAR_DIRTY_BIT; }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion){ key=LogUtils.readByteArray(itemBuffer); lsn=LogUtils.readLong(itemBuffer); state=itemBuffer.get(); state&=CLEAR_DIRTY_BIT; }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<ref knownDeleted=\"").append(isKnownDeleted()); sb.append("\" pendingDeleted=\"").append(isPendingDeleted()); sb.append("\">"); sb.append(Key.dumpString(key,0)); sb.append(DbLsn.toString(lsn)); sb.append("</ref>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }

	 String dumpString( int nspaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); if (lsn == DbLsn.NULL_LSN) { sb.append(TreeUtils.indent(nspaces)); sb.append("<lsn/>"); } else { sb.append(DbLsn.dumpString(lsn,nspaces)); } sb.append('\n'); if (key == null) { sb.append(TreeUtils.indent(nspaces)); sb.append("<key/>"); } else { sb.append(Key.dumpString(key,nspaces)); } sb.append('\n'); if (target == null) { sb.append(TreeUtils.indent(nspaces)); sb.append("<target/>"); } else { sb.append(target.dumpString(nspaces,true)); } sb.append('\n'); sb.append(TreeUtils.indent(nspaces)); sb.append("<knownDeleted val=\""); sb.append(isKnownDeleted()).append("\"/>"); sb.append("<pendingDeleted val=\""); sb.append(isPendingDeleted()).append("\"/>"); sb.append("<dirty val=\"").append(isDirty()).append("\"/>"); return sb.toString(); }

	 public String toString(){ return dumpString(0,false); }

	 protected void hook613( IN in) throws DatabaseException, LogFileNotFoundException, Exception { }


}
