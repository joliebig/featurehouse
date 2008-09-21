package com.sleepycat.je.utilint; 
import java.util.Arrays; 
import com.sleepycat.je.log.FileManager; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.tree.TreeUtils; 
import de.ovgu.cide.jakutil.*; 
public  class  DbLsn {
	 static final long INT_MASK=0xFFFFFFFFL;

	 public static final long MAX_FILE_OFFSET=0xFFFFFFFFL;

	 public static final long NULL_LSN=-1;

	 private DbLsn(){ }

	 public static long makeLsn__wrappee__base( long fileNumber, long fileOffset){ return fileOffset & INT_MASK | ((fileNumber & INT_MASK) << 32); }

	 public static long makeLsn( long fileNumber, long fileOffset){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long longToLsn__wrappee__base( Long lsn){ if (lsn == null) { return NULL_LSN; } return lsn.longValue(); }

	 public static long longToLsn( Long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	longToLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long getFileNumber__wrappee__base( long lsn){ return (lsn >> 32) & INT_MASK; }

	 public static long getFileNumber( long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long getFileOffset__wrappee__base( long lsn){ return (lsn & INT_MASK); }

	 public static long getFileOffset( long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static int compareLong__wrappee__base( long l1, long l2){ if (l1 < l2) { return -1; } else if (l1 > l2) { return 1; } else { return 0; } }

	 private static int compareLong( long l1, long l2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int compareTo__wrappee__base( long lsn1, long lsn2){ if (lsn1 == NULL_LSN || lsn2 == NULL_LSN) { throw new NullPointerException(); } long fileNumber1=getFileNumber(lsn1); long fileNumber2=getFileNumber(lsn2); if (fileNumber1 == fileNumber2) { return compareLong(getFileOffset(lsn1),getFileOffset(lsn2)); } else { return compareLong(fileNumber1,fileNumber2); } }

	 public static int compareTo( long lsn1, long lsn2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String toString__wrappee__base( long lsn){ return "<DbLsn val=\"0x" + Long.toHexString(getFileNumber(lsn)) + "/0x"+ Long.toHexString(getFileOffset(lsn))+ "\"/>"; }

	 public static String toString( long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String getNoFormatString__wrappee__base( long lsn){ return "0x" + Long.toHexString(getFileNumber(lsn)) + "/0x"+ Long.toHexString(getFileOffset(lsn)); }

	 public static String getNoFormatString( long lsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoFormatString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String dumpString__wrappee__base( long lsn, int nSpaces){ StringBuffer sb=new StringBuffer(); sb.append(TreeUtils.indent(nSpaces)); sb.append(toString(lsn)); return sb.toString(); }

	 public static String dumpString( long lsn, int nSpaces){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long getNoCleaningDistance__wrappee__base( long thisLsn, long otherLsn, long logFileSize){ long diff=0; assert thisLsn != NULL_LSN; long myFile=getFileNumber(thisLsn); if (otherLsn == NULL_LSN) { otherLsn=0; } long otherFile=getFileNumber(otherLsn); if (myFile == otherFile) { diff=Math.abs(getFileOffset(thisLsn) - getFileOffset(otherLsn)); } else if (myFile > otherFile) { diff=calcDiff(myFile - otherFile,logFileSize,thisLsn,otherLsn); } else { diff=calcDiff(otherFile - myFile,logFileSize,otherLsn,thisLsn); } return diff; }

	 public static long getNoCleaningDistance( long thisLsn, long otherLsn, long logFileSize){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoCleaningDistance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long getWithCleaningDistance__wrappee__base( long thisLsn, FileManager fileManager, long otherLsn, long logFileSize){ long diff=0; assert thisLsn != NULL_LSN; long myFile=getFileNumber(thisLsn); if (otherLsn == NULL_LSN) { otherLsn=0; } long otherFile=getFileNumber(otherLsn); if (myFile == otherFile) { diff=Math.abs(getFileOffset(thisLsn) - getFileOffset(otherLsn)); } else { Long[] fileNums=fileManager.getAllFileNumbers(); int myFileIdx=Arrays.binarySearch(fileNums,new Long(myFile)); int otherFileIdx=Arrays.binarySearch(fileNums,new Long(otherFile)); if (myFileIdx > otherFileIdx) { diff=calcDiff(myFileIdx - otherFileIdx,logFileSize,thisLsn,otherLsn); } else { diff=calcDiff(otherFileIdx - myFileIdx,logFileSize,otherLsn,thisLsn); } } return diff; }

	 public static long getWithCleaningDistance( long thisLsn, FileManager fileManager, long otherLsn, long logFileSize){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWithCleaningDistance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static long calcDiff__wrappee__base( long fileDistance, long logFileSize, long laterLsn, long earlierLsn){ long diff=fileDistance * logFileSize; diff+=getFileOffset(laterLsn); diff-=getFileOffset(earlierLsn); return diff; }

	 private static long calcDiff( long fileDistance, long logFileSize, long laterLsn, long earlierLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	calcDiff__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactionalX__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactionalX(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactionalX__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionIdX__wrappee__base(){ return 0; }

	 public long getTransactionIdX(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionIdX__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
