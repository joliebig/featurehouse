package com.sleepycat.je.log; 
import java.io.IOException; 
import java.util.HashSet; 
import java.util.Set; 
import java.util.StringTokenizer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  DumpFileReader  extends FileReader {
	 private Set targetEntryTypes;

	 protected Set targetTxnIds;

	 protected boolean verbose;

	 public DumpFileReader( EnvironmentImpl env, int readBufferSize, long startLsn, long finishLsn, String entryTypes, String txnIds, boolean verbose) throws IOException, DatabaseException { super(env,readBufferSize,true,startLsn,null,DbLsn.NULL_LSN,finishLsn); targetEntryTypes=new HashSet(); if (entryTypes != null) { StringTokenizer tokenizer=new StringTokenizer(entryTypes,","); while (tokenizer.hasMoreTokens()) { String typeString=(String)tokenizer.nextToken(); targetEntryTypes.add(new Byte(typeString.trim())); } } targetTxnIds=new HashSet(); if (txnIds != null) { StringTokenizer tokenizer=new StringTokenizer(txnIds,","); while (tokenizer.hasMoreTokens()) { String txnIdString=(String)tokenizer.nextToken(); targetTxnIds.add(new Long(txnIdString.trim())); } } this.verbose=verbose; }

	 protected boolean isTargetEntry__wrappee__base( byte logEntryTypeNumber, byte logEntryTypeVersion){ if (targetEntryTypes.size() == 0) { return true; } else { return targetEntryTypes.contains(new Byte(logEntryTypeNumber)); } }

	 protected boolean isTargetEntry( byte logEntryTypeNumber, byte logEntryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTargetEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void summarize__wrappee__base(){ }

	 public void summarize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	summarize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
