package com.sleepycat.je.cleaner; 
import java.util.ArrayList; 
import java.util.List; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  UtilizationTracker {
	 private EnvironmentImpl env;

	 private Cleaner cleaner;

	 private List files;

	 private long activeFile;

	 private TrackedFileSummary[] snapshot;

	 private long bytesSinceActivate;

	 public UtilizationTracker( EnvironmentImpl env) throws DatabaseException { this(env,env.getCleaner()); }

	 UtilizationTracker( EnvironmentImpl env, Cleaner cleaner) throws DatabaseException { assert cleaner != null; this.env=env; this.cleaner=cleaner; files=new ArrayList(); snapshot=new TrackedFileSummary[0]; activeFile=-1; }

	 public EnvironmentImpl getEnvironment(){ return env; }

	 public void activateCleaner(){ env.getCleaner().wakeup(); bytesSinceActivate=0; }

	 public TrackedFileSummary[] getTrackedFiles(){ return snapshot; }

	 public TrackedFileSummary getTrackedFile( long fileNum){ TrackedFileSummary[] a=snapshot; for (int i=0; i < a.length; i+=1) { if (a[i].getFileNumber() == fileNum) { return a[i]; } } return null; }

	 public boolean countNewLogEntry( long lsn, LogEntryType type, int size){ TrackedFileSummary file=getFile(DbLsn.getFileNumber(lsn)); file.totalCount+=1; file.totalSize+=size; if (type.isNodeType()) { if (inArray(type,LogEntryType.IN_TYPES)) { file.totalINCount+=1; file.totalINSize+=size; } else { file.totalLNCount+=1; file.totalLNSize+=size; } } bytesSinceActivate+=size; return (bytesSinceActivate >= env.getCleaner().cleanerBytesInterval); }

	 public void countObsoleteNode( long lsn, LogEntryType type){ TrackedFileSummary file=getFile(DbLsn.getFileNumber(lsn)); countOneNode(file,type); file.trackObsolete(DbLsn.getFileOffset(lsn)); }

	 public void countObsoleteNodeInexact( long lsn, LogEntryType type){ TrackedFileSummary file=getFile(DbLsn.getFileNumber(lsn)); countOneNode(file,type); }

	 private void countOneNode( TrackedFileSummary file, LogEntryType type){ if (type == null || type.isNodeType()) { if (type == null || !inArray(type,LogEntryType.IN_TYPES)) { file.obsoleteLNCount+=1; } else { file.obsoleteINCount+=1; } } }

	 public void addSummary( long fileNumber, TrackedFileSummary other){ TrackedFileSummary file=getFile(fileNumber); file.addTrackedSummary(other); }

	 public TrackedFileSummary getUnflushableTrackedSummary( long fileNum) throws DatabaseException { TrackedFileSummary file=getFile(fileNum); file.setAllowFlush(false); return file; }

	 private TrackedFileSummary getFile( long fileNum){ if (activeFile < fileNum) { activeFile=fileNum; } int size=files.size(); for (int i=0; i < size; i+=1) { TrackedFileSummary file=(TrackedFileSummary)files.get(i); if (file.getFileNumber() == fileNum) { return file; } } TrackedFileSummary file=new TrackedFileSummary(this,fileNum,cleaner.trackDetail); files.add(file); takeSnapshot(); return file; }

	 void resetFile( TrackedFileSummary file){ if (file.getFileNumber() < activeFile && file.getAllowFlush()) { files.remove(file); takeSnapshot(); } }

	 private void takeSnapshot(){ TrackedFileSummary[] a=new TrackedFileSummary[files.size()]; files.toArray(a); snapshot=a; }

	 private boolean inArray( Object o, Object[] a){ for (int i=0; i < a.length; i+=1) { if (a[i] == o) { return true; } } return false; }


}
