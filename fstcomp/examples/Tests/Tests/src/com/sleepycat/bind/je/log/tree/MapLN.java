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

	 public boolean isDeleted(){ return deleted; }

	 void makeDeleted(){ deleted=true; databaseImpl.getTree().setRoot(null,true); }

	 public DatabaseImpl getDatabase(){ return databaseImpl; }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { databaseImpl.setEnvironmentImpl(db.getDbEnvironment()); }

	 public String toString(){ return dumpString(0,true); }

	 public String beginTag(){ return BEGIN_TAG; }

	 public String endTag(){ return END_TAG; }

	 public String dumpString( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(super.dumpString(nSpaces,dumpTags)); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces)); sb.append("<deleted val=\"").append(Boolean.toString(deleted)); sb.append("\">"); sb.append('\n'); sb.append(databaseImpl.dumpString(nSpaces)); return sb.toString(); }

	 protected LogEntryType getTransactionalLogType(){ return LogEntryType.LOG_MAPLN_TRANSACTIONAL; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_MAPLN; }

	 public int getLogSize(){ return super.getLogSize() + databaseImpl.getLogSize() + LogUtils.getBooleanLogSize(); }

	 public void writeToLog( ByteBuffer logBuffer){ super.writeToLog(logBuffer); databaseImpl.writeToLog(logBuffer); LogUtils.writeBoolean(logBuffer,deleted); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); databaseImpl.readFromLog(itemBuffer,entryTypeVersion); deleted=LogUtils.readBoolean(itemBuffer); }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ databaseImpl.dumpLog(sb,true); }


}
