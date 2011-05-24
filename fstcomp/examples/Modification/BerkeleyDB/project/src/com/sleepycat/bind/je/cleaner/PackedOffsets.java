package com.sleepycat.je.cleaner; 
import java.nio.ByteBuffer; 
import java.util.Arrays; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import de.ovgu.cide.jakutil.*; 
public  class  PackedOffsets  implements LogWritable, LogReadable {
	 private short[] data;

	 private int size;

	 public PackedOffsets(){ }

	
 
class  Iterator {
		 private int index;

		 private long priorVal;

		 private Iterator(){ }

		 boolean hasNext__wrappee__base(){ return data != null && index < data.length; }

		 boolean hasNext(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 long next__wrappee__base(){ long val=priorVal; for (int shift=0; ; shift+=15) { long s=data[index++]; if (s < 0) { val+=(-1 - s) << shift; } else { val+=s << shift; break; } } priorVal=val; return val; }

		 long next(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	next__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 Iterator iterator__wrappee__base(){ return new Iterator(); }

	 Iterator iterator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	iterator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void pack__wrappee__base( long[] offsets){ short[] newData=new short[offsets.length * 3]; Arrays.sort(offsets); int dataIndex=0; long priorVal=0; for (int i=0; i < offsets.length; i+=1) { long val=offsets[i]; dataIndex=append(newData,dataIndex,val - priorVal); priorVal=val; } data=new short[dataIndex]; System.arraycopy(newData,0,data,0,dataIndex); size=offsets.length; }

	 public void pack( long[] offsets){ t.in(Thread.currentThread().getStackTrace()[1].toString());	pack__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long\[\] toArray__wrappee__base(){ long[] offsets=new long[size]; int index=0; Iterator iter=iterator(); while (iter.hasNext()) { offsets[index++]=iter.next(); } assert index == size; return offsets; }

	 long[] toArray(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private int append__wrappee__base( short[] to, int index, long val){ assert val >= 0; while (true) { short s=(short)(val & 0x7fff); val>>>=15; if (val > 0) { to[index++]=(short)(-1 - s); } else { to[index++]=s; break; } } return index; }

	 private int append( short[] to, int index, long val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	append__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return (2 * LogUtils.getIntLogSize()) + ((data != null) ? (data.length * LogUtils.SHORT_BYTES) : 0); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer buf){ LogUtils.writeInt(buf,size); if (data != null) { LogUtils.writeInt(buf,data.length); for (int i=0; i < data.length; i+=1) { LogUtils.writeShort(buf,data[i]); } } else { LogUtils.writeInt(buf,0); } }

	 public void writeToLog( ByteBuffer buf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer buf, byte entryTypeVersion){ size=LogUtils.readInt(buf); int len=LogUtils.readInt(buf); if (len > 0) { data=new short[len]; for (int i=0; i < len; i+=1) { data[i]=LogUtils.readShort(buf); } } }

	 public void readFromLog( ByteBuffer buf, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer buf, boolean verbose){ if (size > 0) { Iterator i=iterator(); buf.append("<offsets size=\""); buf.append(size); buf.append("\">"); while (i.hasNext()) { buf.append("0x"); buf.append(Long.toHexString(i.next())); buf.append(' '); } buf.append("</offsets>"); } else { buf.append("<offsets size=\"0\"/>"); } }

	 public void dumpLog( StringBuffer buf, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return -1; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer buf=new StringBuffer(); dumpLog(buf,true); return buf.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
