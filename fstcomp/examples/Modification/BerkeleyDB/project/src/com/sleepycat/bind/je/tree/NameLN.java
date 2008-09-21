package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogUtils; 
import de.ovgu.cide.jakutil.*; 
public final  class  NameLN  extends LN {
	 private static final String BEGIN_TAG="<nameLN>";

	 private static final String END_TAG="</nameLN>";

	 private DatabaseId id;

	 private boolean deleted;

	 public NameLN( DatabaseId id){ super(new byte[0]); this.id=id; deleted=false; }

	 public NameLN(){ super(); id=new DatabaseId(); }

	 public boolean isDeleted__wrappee__base(){ return deleted; }

	 public boolean isDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void makeDeleted__wrappee__base(){ deleted=true; }

	 void makeDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getId__wrappee__base(){ return id; }

	 public DatabaseId getId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setId__wrappee__base( DatabaseId id){ this.id=id; }

	 public void setId( DatabaseId id){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(super.dumpString(nSpaces,dumpTags)); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces)); sb.append("<deleted val=\"").append(Boolean.toString(deleted)); sb.append("\">"); sb.append('\n'); sb.append(TreeUtils.indent(nSpaces)); sb.append("<id val=\"").append(id); sb.append("\">"); sb.append('\n'); return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected LogEntryType getTransactionalLogType__wrappee__base(){ return LogEntryType.LOG_NAMELN_TRANSACTIONAL; }

	 protected LogEntryType getTransactionalLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionalLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_NAMELN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return super.getLogSize() + id.getLogSize() + LogUtils.getBooleanLogSize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ super.writeToLog(logBuffer); id.writeToLog(logBuffer); LogUtils.writeBoolean(logBuffer,deleted); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); id.readFromLog(itemBuffer,entryTypeVersion); deleted=LogUtils.readBoolean(itemBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb, boolean verbose){ id.dumpLog(sb,true); }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
