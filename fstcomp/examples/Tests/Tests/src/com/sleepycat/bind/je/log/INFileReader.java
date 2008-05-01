package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.HashMap; 
import java.util.Map; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.TrackedFileSummary; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DbTree; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.entry.INContainingEntry; 
import com.sleepycat.je.log.entry.INLogEntry; 
import com.sleepycat.je.log.entry.LNLogEntry; 
import com.sleepycat.je.log.entry.LogEntry; 
import com.sleepycat.je.log.entry.NodeLogEntry; 
import com.sleepycat.je.tree.FileSummaryLN; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.INDeleteInfo; 
import com.sleepycat.je.tree.INDupDeleteInfo; 
import com.sleepycat.je.tree.MapLN; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  INFileReader  extends FileReader {
	 private boolean lastEntryWasDelete;

	 private boolean lastEntryWasDupDelete;

	 private LogEntryType fromLogType;

	 private boolean isProvisional;

	 private Map targetEntryMap;

	 private LogEntry targetLogEntry;

	 private Map dbIdTrackingMap;

	 private LNLogEntry dbIdTrackingEntry;

	 private Map txnIdTrackingMap;

	 private LNLogEntry txnIdTrackingEntry;

	 private Map otherNodeTrackingMap;

	 private NodeLogEntry nodeTrackingEntry;

	 private INLogEntry inTrackingEntry;

	 private LNLogEntry fsTrackingEntry;

	 private boolean trackIds;

	 private long maxNodeId;

	 private int maxDbId;

	 private long maxTxnId;

	 private boolean mapDbOnly;

	 private long partialCkptStart;

	 private UtilizationTracker tracker;

	 private Map fileSummaryLsns;

	 public INFileReader( EnvironmentImpl env, int readBufferSize, long startLsn, long finishLsn, boolean trackIds, boolean mapDbOnly, long partialCkptStart, Map fileSummaryLsns) throws IOException, DatabaseException { super(env,readBufferSize,true,startLsn,null,DbLsn.NULL_LSN,finishLsn); this.trackIds=trackIds; this.mapDbOnly=mapDbOnly; targetEntryMap=new HashMap(); if (trackIds) { maxNodeId=0; maxDbId=0; tracker=env.getUtilizationTracker(); this.partialCkptStart=partialCkptStart; this.fileSummaryLsns=fileSummaryLsns; fsTrackingEntry=(LNLogEntry)LogEntryType.LOG_FILESUMMARYLN.getNewLogEntry(); dbIdTrackingMap=new HashMap(); txnIdTrackingMap=new HashMap(); otherNodeTrackingMap=new HashMap(); dbIdTrackingMap.put(LogEntryType.LOG_MAPLN_TRANSACTIONAL,LogEntryType.LOG_MAPLN_TRANSACTIONAL.getNewLogEntry()); dbIdTrackingMap.put(LogEntryType.LOG_MAPLN,LogEntryType.LOG_MAPLN.getNewLogEntry()); txnIdTrackingMap.put(LogEntryType.LOG_LN_TRANSACTIONAL,LogEntryType.LOG_LN_TRANSACTIONAL.getNewLogEntry()); txnIdTrackingMap.put(LogEntryType.LOG_MAPLN_TRANSACTIONAL,LogEntryType.LOG_MAPLN_TRANSACTIONAL.getNewLogEntry()); txnIdTrackingMap.put(LogEntryType.LOG_NAMELN_TRANSACTIONAL,LogEntryType.LOG_NAMELN_TRANSACTIONAL.getNewLogEntry()); txnIdTrackingMap.put(LogEntryType.LOG_DEL_DUPLN_TRANSACTIONAL,LogEntryType.LOG_DEL_DUPLN_TRANSACTIONAL.getNewLogEntry()); txnIdTrackingMap.put(LogEntryType.LOG_DUPCOUNTLN_TRANSACTIONAL,LogEntryType.LOG_DUPCOUNTLN_TRANSACTIONAL.getNewLogEntry()); } }

	 public void addTargetType( LogEntryType entryType) throws DatabaseException { targetEntryMap.put(entryType,entryType.getNewLogEntry()); }

	 protected boolean isTargetEntry( byte entryTypeNum, byte entryTypeVersion) throws DatabaseException { lastEntryWasDelete=false; lastEntryWasDupDelete=false; targetLogEntry=null; dbIdTrackingEntry=null; txnIdTrackingEntry=null; nodeTrackingEntry=null; inTrackingEntry=null; fsTrackingEntry=null; isProvisional=LogEntryType.isProvisional(entryTypeVersion); fromLogType=LogEntryType.findType(entryTypeNum,entryTypeVersion); LogEntry possibleTarget=(LogEntry)targetEntryMap.get(fromLogType); if (!isProvisional) { targetLogEntry=possibleTarget; } if (LogEntryType.LOG_IN_DELETE_INFO.equals(fromLogType)) { lastEntryWasDelete=true; } if (LogEntryType.LOG_IN_DUPDELETE_INFO.equals(fromLogType)) { lastEntryWasDupDelete=true; } if (trackIds) { if (!isProvisional) { dbIdTrackingEntry=(LNLogEntry)dbIdTrackingMap.get(fromLogType); txnIdTrackingEntry=(LNLogEntry)txnIdTrackingMap.get(fromLogType); } if (fromLogType.isNodeType()) { if (possibleTarget != null) { nodeTrackingEntry=(NodeLogEntry)possibleTarget; } else if (dbIdTrackingEntry != null) { nodeTrackingEntry=dbIdTrackingEntry; } else if (txnIdTrackingEntry != null) { nodeTrackingEntry=txnIdTrackingEntry; } else { nodeTrackingEntry=(NodeLogEntry)otherNodeTrackingMap.get(fromLogType); if (nodeTrackingEntry == null) { nodeTrackingEntry=(NodeLogEntry)fromLogType.getNewLogEntry(); otherNodeTrackingMap.put(fromLogType,nodeTrackingEntry); } } if (nodeTrackingEntry instanceof INLogEntry) { inTrackingEntry=(INLogEntry)nodeTrackingEntry; } if (LogEntryType.LOG_FILESUMMARYLN.equals(fromLogType)) { fsTrackingEntry=(LNLogEntry)nodeTrackingEntry; } } tracker.countNewLogEntry(getLastLsn(),fromLogType,LogManager.HEADER_BYTES + currentEntrySize); return (targetLogEntry != null) || (dbIdTrackingEntry != null) || (txnIdTrackingEntry != null)|| (nodeTrackingEntry != null); } else { return (targetLogEntry != null); } }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { boolean useEntry=false; boolean entryLoaded=false; if (targetLogEntry != null) { targetLogEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); DatabaseId dbId=getDatabaseId(); boolean isMapDb=dbId.equals(DbTree.ID_DB_ID); useEntry=(!mapDbOnly || isMapDb); entryLoaded=true; } if (trackIds) { LNLogEntry lnEntry=null; if (dbIdTrackingEntry != null) { lnEntry=dbIdTrackingEntry; lnEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); entryLoaded=true; MapLN mapLN=(MapLN)lnEntry.getMainItem(); int dbId=mapLN.getDatabase().getId().getId(); if (dbId > maxDbId) { maxDbId=dbId; } } if (txnIdTrackingEntry != null) { if (lnEntry == null) { lnEntry=txnIdTrackingEntry; lnEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); entryLoaded=true; } long txnId=lnEntry.getTxnId().longValue(); if (txnId > maxTxnId) { maxTxnId=txnId; } } if (fsTrackingEntry != null) { if (!entryLoaded) { nodeTrackingEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); entryLoaded=true; } byte[] keyBytes=fsTrackingEntry.getKey(); FileSummaryLN fsln=(FileSummaryLN)fsTrackingEntry.getMainItem(); long fileNum=fsln.getFileNumber(keyBytes); TrackedFileSummary trackedLN=tracker.getTrackedFile(fileNum); if (trackedLN != null) { trackedLN.reset(); } fileSummaryLsns.put(new Long(fileNum),new Long(getLastLsn())); } if (nodeTrackingEntry != null) { if (!entryLoaded) { nodeTrackingEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,false); entryLoaded=true; } long nodeId=nodeTrackingEntry.getNodeId(); maxNodeId=(nodeId > maxNodeId) ? nodeId : maxNodeId; } if (inTrackingEntry != null) { assert entryLoaded : "All nodes should have been loaded"; long oldLsn=inTrackingEntry.getObsoleteLsn(); if (oldLsn != DbLsn.NULL_LSN) { long newLsn=getLastLsn(); if (!isObsoleteLsnAlreadyCounted(oldLsn,newLsn)) { tracker.countObsoleteNodeInexact(oldLsn,fromLogType); } } if (isProvisional && partialCkptStart != DbLsn.NULL_LSN) { oldLsn=getLastLsn(); if (DbLsn.compareTo(partialCkptStart,oldLsn) < 0) { tracker.countObsoleteNodeInexact(oldLsn,fromLogType); } } } } return useEntry; }

	 private boolean isObsoleteLsnAlreadyCounted( long oldLsn, long newLsn){ Long fileNum=new Long(DbLsn.getFileNumber(oldLsn)); long fileSummaryLsn=DbLsn.longToLsn((Long)fileSummaryLsns.get(fileNum)); int cmpFsLsnToNewLsn=(fileSummaryLsn != DbLsn.NULL_LSN) ? DbLsn.compareTo(fileSummaryLsn,newLsn) : -1; return (cmpFsLsnToNewLsn >= 0); }

	 public IN getIN() throws DatabaseException { return ((INContainingEntry)targetLogEntry).getIN(env); }

	 public DatabaseId getDatabaseId(){ if (lastEntryWasDelete) { return ((INDeleteInfo)targetLogEntry.getMainItem()).getDatabaseId(); } else if (lastEntryWasDupDelete) { return ((INDupDeleteInfo)targetLogEntry.getMainItem()).getDatabaseId(); } else { return ((INContainingEntry)targetLogEntry).getDbId(); } }

	 public long getMaxNodeId(){ return maxNodeId; }

	 public int getMaxDbId(){ return maxDbId; }

	 public long getMaxTxnId(){ return maxTxnId; }

	 public boolean isDeleteInfo(){ return lastEntryWasDelete; }

	 public boolean isDupDeleteInfo(){ return lastEntryWasDupDelete; }

	 public long getDeletedNodeId(){ return ((INDeleteInfo)targetLogEntry.getMainItem()).getDeletedNodeId(); }

	 public byte[] getDeletedIdKey(){ return ((INDeleteInfo)targetLogEntry.getMainItem()).getDeletedIdKey(); }

	 public long getDupDeletedNodeId(){ return ((INDupDeleteInfo)targetLogEntry.getMainItem()).getDeletedNodeId(); }

	 public byte[] getDupDeletedMainKey(){ return ((INDupDeleteInfo)targetLogEntry.getMainItem()).getDeletedMainKey(); }

	 public byte[] getDupDeletedDupKey(){ return ((INDupDeleteInfo)targetLogEntry.getMainItem()).getDeletedDupKey(); }

	 public long getLsnOfIN(){ return ((INContainingEntry)targetLogEntry).getLsnOfIN(getLastLsn()); }

	 public LogEntryType getLogEntryType(){ return LogEntryType.findType(currentEntryTypeNum,currentEntryTypeVersion); }


}
