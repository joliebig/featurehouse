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

	 Long selectFileForCleaning( UtilizationProfile profile, boolean forceCleaning, boolean calcLowUtilizationFiles, int maxBatchFiles) throws DatabaseException { Set newLowUtilizationFiles=calcLowUtilizationFiles ? (new HashSet()) : null; while (true) { if (maxBatchFiles > 0) {
synchronized (this) { if (toBeCleanedFiles.size() >= maxBatchFiles) { break; } } } Long fileNum=profile.getBestFileForCleaning(this,forceCleaning,newLowUtilizationFiles); if (fileNum == null) { break; }
synchronized (this) { toBeCleanedFiles.add(fileNum); } } if (newLowUtilizationFiles != null) { lowUtilizationFiles=newLowUtilizationFiles; } SortedSet availableFiles;
synchronized (this) { availableFiles=new TreeSet(toBeCleanedFiles); } Long file=profile.getCheapestFileToClean(availableFiles);
synchronized (this) { toBeCleanedFiles.remove(file); beingCleanedFiles.add(file); } return file; }

	 synchronized boolean isFileCleaningInProgress( Long file){ return toBeCleanedFiles.contains(file) || beingCleanedFiles.contains(file) || cleanedFiles.contains(file)|| checkpointedFiles.contains(file)|| fullyProcessedFiles.contains(file)|| safeToDeleteFiles.contains(file); }

	 synchronized void putBackFileForCleaning( Long fileNum){ toBeCleanedFiles.add(fileNum); beingCleanedFiles.remove(fileNum); }

	 synchronized void addCleanedFile( Long fileNum){ cleanedFiles.add(fileNum); beingCleanedFiles.remove(fileNum); }

	 Set getLowUtilizationFiles(){ return lowUtilizationFiles; }

	 synchronized Set getMustBeCleanedFiles(){ Set set=new HashSet(toBeCleanedFiles); set.addAll(beingCleanedFiles); return set; }

	 synchronized int getBacklog(){ return toBeCleanedFiles.size(); }

	 synchronized Set[] getFilesAtCheckpointStart(){ anyPendingDuringCheckpoint=!pendingLNs.isEmpty(); this.hook164(); Set[] files=new Set[2]; files[0]=(cleanedFiles.size() > 0) ? (new HashSet(cleanedFiles)) : null; files[1]=(fullyProcessedFiles.size() > 0) ? (new HashSet(fullyProcessedFiles)) : null; return (files[0] != null || files[1] != null) ? files : null; }

	 synchronized void updateFilesAtCheckpointEnd( Set[] files){ if (files != null) { Set previouslyCleanedFiles=files[0]; if (previouslyCleanedFiles != null) { if (anyPendingDuringCheckpoint) { checkpointedFiles.addAll(previouslyCleanedFiles); } else { safeToDeleteFiles.addAll(previouslyCleanedFiles); } cleanedFiles.removeAll(previouslyCleanedFiles); } Set previouslyProcessedFiles=files[1]; if (previouslyProcessedFiles != null) { safeToDeleteFiles.addAll(previouslyProcessedFiles); fullyProcessedFiles.removeAll(previouslyProcessedFiles); } updateProcessedFiles(); } }

	 synchronized boolean addPendingLN( LN ln, DatabaseId dbId, byte[] key, byte[] dupKey){ assert ln != null; boolean added=pendingLNs.put(new Long(ln.getNodeId()),new LNInfo(ln,dbId,key,dupKey)) != null; anyPendingDuringCheckpoint=true; return added; }

	 synchronized LNInfo[] getPendingLNs(){ if (pendingLNs.size() > 0) { LNInfo[] lns=new LNInfo[pendingLNs.size()]; pendingLNs.values().toArray(lns); return lns; } else { return null; } }

	 synchronized void removePendingLN( long nodeId){ pendingLNs.remove(new Long(nodeId)); updateProcessedFiles(); }

	 synchronized Set copySafeToDeleteFiles(){ if (safeToDeleteFiles.size() == 0) { return null; } else { return new HashSet(safeToDeleteFiles); } }

	 synchronized void removeDeletedFile( Long fileNum){ safeToDeleteFiles.remove(fileNum); }

	 private void updateProcessedFiles(){ boolean b=pendingLNs.isEmpty(); b=this.hook165(b); if (b) { fullyProcessedFiles.addAll(checkpointedFiles); checkpointedFiles.clear(); } }

	 protected void hook163(){ }

	 protected void hook164(){ }

	 protected boolean hook165( boolean b){ return b; }


}
