package com.sleepycat.je.tree; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  TreeLocation {
	 public BIN bin;

	 public int index;

	 public byte[] lnKey;

	 public long childLsn=DbLsn.NULL_LSN;

	 public void reset__wrappee__base(){ bin=null; index=-1; lnKey=null; childLsn=DbLsn.NULL_LSN; }

	 public void reset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer("<TreeLocation bin=\""); if (bin == null) { sb.append("null"); } else { sb.append(bin.getNodeId()); } sb.append("\" index=\""); sb.append(index); sb.append("\" lnKey=\""); sb.append(Key.dumpString(lnKey,0)); sb.append("\" childLsn=\""); sb.append(DbLsn.toString(childLsn)); sb.append("\">"); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
