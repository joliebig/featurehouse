package com.sleepycat.bind; 
import com.sleepycat.compat.DbCompat; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  RecordNumberBinding  implements EntryBinding {
	 public RecordNumberBinding(){ }

	 public Object entryToObject__wrappee__base( DatabaseEntry entry){ return new Long(entryToRecordNumber(entry)); }

	 public Object entryToObject( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, DatabaseEntry entry){ recordNumberToEntry(((Number)object).longValue(),entry); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long entryToRecordNumber__wrappee__base( DatabaseEntry entry){ return DbCompat.getRecordNumber(entry) & 0xFFFFFFFFL; }

	 public static long entryToRecordNumber( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToRecordNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void recordNumberToEntry__wrappee__base( long recordNumber, DatabaseEntry entry){ entry.setData(new byte[4],0,4); DbCompat.setRecordNumber(entry,(int)recordNumber); }

	 public static void recordNumberToEntry( long recordNumber, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	recordNumberToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
