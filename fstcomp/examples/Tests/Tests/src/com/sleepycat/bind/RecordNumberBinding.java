package com.sleepycat.bind; 
import com.sleepycat.compat.DbCompat; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  RecordNumberBinding  implements EntryBinding {
	 public RecordNumberBinding(){ }

	 public Object entryToObject( DatabaseEntry entry){ return new Long(entryToRecordNumber(entry)); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ recordNumberToEntry(((Number)object).longValue(),entry); }

	 public static long entryToRecordNumber( DatabaseEntry entry){ return DbCompat.getRecordNumber(entry) & 0xFFFFFFFFL; }

	 public static void recordNumberToEntry( long recordNumber, DatabaseEntry entry){ entry.setData(new byte[4],0,4); DbCompat.setRecordNumber(entry,(int)recordNumber); }


}
