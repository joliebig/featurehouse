package com.sleepycat.je.cleaner; 
import java.util.Collections; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Map; 
import java.util.Set; 
import java.util.SortedSet; 
import java.util.TreeSet; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.tree.LN; 
import de.ovgu.cide.jakutil.*; 
 
class  FileSelector {
	 private SortedSet toBeCleanedFiles;

	 private Set beingCleanedFiles;

	 private Set cleanedFiles;

	 private Set checkpointedFiles;

	 private Set fullyProcessedFiles;

	 private Set safeToDeleteFiles;

	 private Map pendingLNs;

	 private boolean anyPendingDuringCheckpoint;

	 private Set lowUtilizationFiles;

	 FileSelector(){ toBeCleanedFiles=new TreeSet(); cleanedFiles=new HashSet(); checkpointedFiles=new HashSet(); fullyProcessedFiles=new HashSet(); safeToDeleteFiles=new HashSet(); pendingLNs=new HashMap(); this.hook163(); lowUtilizationFiles=Collections.EMPTY_SET; beingCleanedFiles=new HashSet(); }

	 Long selectFileForCleaning__wrappee__base( UtilizationProfile profile, boolean forceCleaning, boolean calcLowUtilizationFiles, int maxBatchFiles) throws DatabaseException { Set newLowUtilizationFiles=calcLowUtilizationFiles ? (new HashSet()) : null; while (true) { if (maxBatchFiles > 0) {
synchronized (this) { if (toBeCleanedFiles.size() >= maxBatchFiles) { break; } } } Long fileNum=profile.getBestFileForCleaning(this,forceCleaning,newLowUtilizationFiles); if (fileNum == null) { break; }
synchronized (this) { toBeCleanedFiles.add(fileNum); } } if (newLowUtilizationFiles != null) { lowUtilizationFiles=newLowUtilizationFiles; } SortedSet availableFiles;
synchronized (this) { availableFiles=new TreeSet(toBeCleanedFiles); } Long file=profile.getCheapestFileToClean(availableFiles);
synchronized (this) { toBeCleanedFiles.remove(file); beingCleanedFiles.add(file); } return file; }

	 Long selectFileForCleaning( UtilizationProfile profile, boolean forceCleaning, boolean calcLowUtilizationFiles, int maxBatchFiles) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	selectFileForCleaning__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized boolean isFileCleaningInProgress__wrappee__base( Long file){ return toBeCleanedFiles.contains(file) || beingCleanedFiles.contains(file) || cleanedFiles.contains(file)|| checkpointedFiles.contains(file)|| fullyProcessedFiles.contains(file)|| safeToDeleteFiles.contains(file); }

	 synchronized boolean isFileCleaningInProgress( Long file){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isFileCleaningInProgress__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void putBackFileForCleaning__wrappee__base( Long fileNum){ toBeCleanedFiles.add(fileNum); beingCleanedFiles.remove(fileNum); }

	 synchronized void putBackFileForCleaning( Long fileNum){ t.in(Thread.currentThread().getStackTrace()[1].toString());	putBackFileForCleaning__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void addCleanedFile__wrappee__base( Long fileNum){ cleanedFiles.add(fileNum); beingCleanedFiles.remove(fileNum); }

	 synchronized void addCleanedFile( Long fileNum){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addCleanedFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Set getLowUtilizationFiles__wrappee__base(){ return lowUtilizationFiles; }

	 Set getLowUtilizationFiles(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLowUtilizationFiles__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized Set getMustBeCleanedFiles__wrappee__base(){ Set set=new HashSet(toBeCleanedFiles); set.addAll(beingCleanedFiles); return set; }

	 synchronized Set getMustBeCleanedFiles(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMustBeCleanedFiles__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized int getBacklog__wrappee__base(){ return toBeCleanedFiles.size(); }

	 synchronized int getBacklog(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBacklog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized Set\[\] getFilesAtCheckpointStart__wrappee__base(){ anyPendingDuringCheckpoint=!pendingLNs.isEmpty(); this.hook164(); Set[] files=new Set[2]; files[0]=(cleanedFiles.size() > 0) ? (new HashSet(cleanedFiles)) : null; files[1]=(fullyProcessedFiles.size() > 0) ? (new HashSet(fullyProcessedFiles)) : null; return (files[0] != null || files[1] != null) ? files : null; }

	 synchronized Set[] getFilesAtCheckpointStart(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFilesAtCheckpointStart__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void updateFilesAtCheckpointEnd__wrappee__base( Set[] files){ if (files != null) { Set previouslyCleanedFiles=files[0]; if (previouslyCleanedFiles != null) { if (anyPendingDuringCheckpoint) { checkpointedFiles.addAll(previouslyCleanedFiles); } else { safeToDeleteFiles.addAll(previouslyCleanedFiles); } cleanedFiles.removeAll(previouslyCleanedFiles); } Set previouslyProcessedFiles=files[1]; if (previouslyProcessedFiles != null) { safeToDeleteFiles.addAll(previouslyProcessedFiles); fullyProcessedFiles.removeAll(previouslyProcessedFiles); } updateProcessedFiles(); } }

	 synchronized void updateFilesAtCheckpointEnd( Set[] files){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateFilesAtCheckpointEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized boolean addPendingLN__wrappee__base( LN ln, DatabaseId dbId, byte[] key, byte[] dupKey){ assert ln != null; boolean added=pendingLNs.put(new Long(ln.getNodeId()),new LNInfo(ln,dbId,key,dupKey)) != null; anyPendingDuringCheckpoint=true; return added; }

	 synchronized boolean addPendingLN( LN ln, DatabaseId dbId, byte[] key, byte[] dupKey){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addPendingLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized LNInfo\[\] getPendingLNs__wrappee__base(){ if (pendingLNs.size() > 0) { LNInfo[] lns=new LNInfo[pendingLNs.size()]; pendingLNs.values().toArray(lns); return lns; } else { return null; } }

	 synchronized LNInfo[] getPendingLNs(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPendingLNs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void removePendingLN__wrappee__base( long nodeId){ pendingLNs.remove(new Long(nodeId)); updateProcessedFiles(); }

	 synchronized void removePendingLN( long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removePendingLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized Set copySafeToDeleteFiles__wrappee__base(){ if (safeToDeleteFiles.size() == 0) { return null; } else { return new HashSet(safeToDeleteFiles); } }

	 synchronized Set copySafeToDeleteFiles(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	copySafeToDeleteFiles__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void removeDeletedFile__wrappee__base( Long fileNum){ safeToDeleteFiles.remove(fileNum); }

	 synchronized void removeDeletedFile( Long fileNum){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeDeletedFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void updateProcessedFiles__wrappee__base(){ boolean b=pendingLNs.isEmpty(); b=this.hook165(b); if (b) { fullyProcessedFiles.addAll(checkpointedFiles); checkpointedFiles.clear(); } }

	 private void updateProcessedFiles(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	updateProcessedFiles__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook163__wrappee__base(){ }

	 protected void hook163(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook163__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook164__wrappee__base(){ }

	 protected void hook164(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook164__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean hook165__wrappee__base( boolean b){ return b; }

	 protected boolean hook165( boolean b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook165__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
