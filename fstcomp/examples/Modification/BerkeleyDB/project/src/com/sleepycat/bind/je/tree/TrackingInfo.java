package com.sleepycat.je.tree; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  TrackingInfo {
	 private long lsn;

	 private long nodeId;

	 public TrackingInfo( long lsn, long nodeId){ this.lsn=lsn; this.nodeId=nodeId; }

	 public String toString__wrappee__base(){ return "lsn=" + DbLsn.getNoFormatString(lsn) + " node="+ nodeId; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
