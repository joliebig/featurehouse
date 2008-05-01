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

	 protected boolean isTargetEntry( byte logEntryTypeNumber, byte logEntryTypeVersion){ boolean isTarget=false; isRoot=false; isCheckpointEnd=false; isCheckpointStart=false; if (LogEntryType.LOG_CKPT_END.equalsType(logEntryTypeNumber,logEntryTypeVersion)) { isTarget=true; isCheckpointEnd=true; } else if (LogEntryType.LOG_CKPT_START.equalsType(logEntryTypeNumber,logEntryTypeVersion)) { isTarget=true; isCheckpointStart=true; } else if (LogEntryType.LOG_ROOT.equalsType(logEntryTypeNumber,logEntryTypeVersion)) { isTarget=true; isRoot=true; } return isTarget; }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { return true; }

	 public boolean isRoot(){ return isRoot; }

	 public boolean isCheckpointEnd(){ return isCheckpointEnd; }

	 public boolean isCheckpointStart(){ return isCheckpointStart; }


}
