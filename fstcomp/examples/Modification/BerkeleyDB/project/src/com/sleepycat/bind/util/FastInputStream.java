package com.sleepycat.util; 
import java.io.IOException; 
import java.io.InputStream; 
import de.ovgu.cide.jakutil.*; 
public  class  FastInputStream  extends InputStream {
	 protected int len;

	 protected int off;

	 protected int mark;

	 protected byte[] buf;

	 public FastInputStream( byte[] buffer){ buf=buffer; len=buffer.length; }

	 public FastInputStream( byte[] buffer, int offset, int length){ buf=buffer; off=offset; len=length; }

	 public int available__wrappee__base(){ return len - off; }

	 public int available(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	available__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean markSupported__wrappee__base(){ return true; }

	 public boolean markSupported(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	markSupported__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void mark__wrappee__base( int pos){ mark=pos; }

	 public void mark( int pos){ t.in(Thread.currentThread().getStackTrace()[1].toString());	mark__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void reset__wrappee__base(){ off=mark; }

	 public void reset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long skip__wrappee__base( long count){ int myCount=(int)count; if (myCount + off > len) { myCount=len - off; } off+=myCount; return myCount; }

	 public long skip( long count){ t.in(Thread.currentThread().getStackTrace()[1].toString());	skip__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int read__wrappee__base() throws IOException { return readFast(); }

	 public int read() throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	read__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int read__wrappee__base( byte[] toBuf) throws IOException { return readFast(toBuf,0,toBuf.length); }

	 public int read( byte[] toBuf) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	read__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int read__wrappee__base( byte[] toBuf, int offset, int length) throws IOException { return readFast(toBuf,offset,length); }

	 public int read( byte[] toBuf, int offset, int length) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	read__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int readFast__wrappee__base(){ return (off < len) ? (buf[off++] & 0xff) : (-1); }

	 public final int readFast(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int readFast__wrappee__base( byte[] toBuf){ return readFast(toBuf,0,toBuf.length); }

	 public final int readFast( byte[] toBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int readFast__wrappee__base( byte[] toBuf, int offset, int length){ int avail=len - off; if (avail <= 0) { return -1; } if (length > avail) { length=avail; } System.arraycopy(buf,off,toBuf,offset,length); off+=length; return length; }

	 public final int readFast( byte[] toBuf, int offset, int length){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final byte\[\] getBufferBytes__wrappee__base(){ return buf; }

	 public final byte[] getBufferBytes(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBufferBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int getBufferOffset__wrappee__base(){ return off; }

	 public final int getBufferOffset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBufferOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int getBufferLength__wrappee__base(){ return len; }

	 public final int getBufferLength(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBufferLength__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
