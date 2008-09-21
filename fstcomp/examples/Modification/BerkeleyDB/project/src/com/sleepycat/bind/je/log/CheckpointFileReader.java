package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  CheckpointFileReader  extends FileReader {
	 private boolean isRoot;

	 private boolean isCheckpointEnd;

	 private boolean isCheckpointStart;

	 public CheckpointFileReader( EnvironmentImpl env, int readBufferSize, boolean forward, long startLsn, long finishLsn, long endOfFileLsn) throws IOException, DatabaseException { super(env,readBufferSize,forward,startLsn,null,endOfFileLsn,finishLsn); }

	 protected boolean isTargetEntry__wrappee__base( byte logEntryTypeNumber, byte logEntryTypeVersion){ boolean isTarget=false; isRoot=false; isCheckpointEnd=false; isCheckpointStart=false; if (LogEntryType.LOG_CKPT_END.equalsType(logEntryTypeNumber,logEntryTypeVersion)) { isTarget=true; isCheckpointEnd=true; } else if (LogEntryType.LOG_CKPT_START.equalsType(logEntryTypeNumber,logEntryTypeVersion)) { isTarget=true; isCheckpointStart=true; } else if (LogEntryType.LOG_ROOT.equalsType(logEntryTypeNumber,logEntryTypeVersion)) { isTarget=true; isRoot=true; } return isTarget; }

	 protected boolean isTargetEntry( byte logEntryTypeNumber, byte logEntryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTargetEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean processEntry__wrappee__base( ByteBuffer entryBuffer) throws DatabaseException { return true; }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isRoot__wrappee__base(){ return isRoot; }

	 public boolean isRoot(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isCheckpointEnd__wrappee__base(){ return isCheckpointEnd; }

	 public boolean isCheckpointEnd(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isCheckpointEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isCheckpointStart__wrappee__base(){ return isCheckpointStart; }

	 public boolean isCheckpointStart(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isCheckpointStart__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
