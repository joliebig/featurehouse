package com.sleepycat.util; 
import java.io.IOException; 
import java.io.OutputStream; 
import java.io.UnsupportedEncodingException; 
import de.ovgu.cide.jakutil.*; 
public  class  FastOutputStream  extends OutputStream {
	 public static final int DEFAULT_INIT_SIZE=100;

	 public static final int DEFAULT_BUMP_SIZE=0;

	 private int len;

	 private int bumpLen;

	 private byte[] buf;

	 private static byte[] ZERO_LENGTH_BYTE_ARRAY=new byte[0];

	 public FastOutputStream(){ initBuffer(DEFAULT_INIT_SIZE,DEFAULT_BUMP_SIZE); }

	 public FastOutputStream( int initialSize){ initBuffer(initialSize,DEFAULT_BUMP_SIZE); }

	 public FastOutputStream( int initialSize, int bumpSize){ initBuffer(initialSize,bumpSize); }

	 public FastOutputStream( byte[] buffer){ buf=buffer; bumpLen=DEFAULT_BUMP_SIZE; }

	 public FastOutputStream( byte[] buffer, int bumpSize){ buf=buffer; bumpLen=bumpSize; }

	 private void initBuffer__wrappee__base( int bufferSize, int bumpLen){ buf=new byte[bufferSize]; this.bumpLen=bumpLen; }

	 private void initBuffer( int bufferSize, int bumpLen){ t.in(Thread.currentThread().getStackTrace()[1].toString());	initBuffer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int size__wrappee__base(){ return len; }

	 public int size(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	size__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void reset__wrappee__base(){ len=0; }

	 public void reset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void write__wrappee__base( int b) throws IOException { writeFast(b); }

	 public void write( int b) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	write__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void write__wrappee__base( byte[] fromBuf) throws IOException { writeFast(fromBuf); }

	 public void write( byte[] fromBuf) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	write__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void write__wrappee__base( byte[] fromBuf, int offset, int length) throws IOException { writeFast(fromBuf,offset,length); }

	 public void write( byte[] fromBuf, int offset, int length) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	write__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeTo__wrappee__base( OutputStream out) throws IOException { out.write(buf,0,len); }

	 public void writeTo( OutputStream out) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	writeTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return new String(buf,0,len); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base( String encoding) throws UnsupportedEncodingException { return new String(buf,0,len,encoding); }

	 public String toString( String encoding) throws UnsupportedEncodingException { t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] toByteArray__wrappee__base(){ if (len == 0) { return ZERO_LENGTH_BYTE_ARRAY; } else { byte[] toBuf=new byte[len]; System.arraycopy(buf,0,toBuf,0,len); return toBuf; } }

	 public byte[] toByteArray(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toByteArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final void writeFast__wrappee__base( int b){ if (len + 1 > buf.length) bump(1); buf[len++]=(byte)b; }

	 public final void writeFast( int b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeFast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final void writeFast__wrappee__base( byte[] fromBuf){ int needed=len + fromBuf.length - buf.length; if (needed > 0) bump(needed); System.arraycopy(fromBuf,0,buf,len,fromBuf.length); len+=fromBuf.length; }

	 public final void writeFast( byte[] fromBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeFast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final void writeFast__wrappee__base( byte[] fromBuf, int offset, int length){ int needed=len + length - buf.length; if (needed > 0) bump(needed); System.arraycopy(fromBuf,offset,buf,len,length); len+=length; }

	 public final void writeFast( byte[] fromBuf, int offset, int length){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeFast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getBufferBytes__wrappee__base(){ return buf; }

	 public byte[] getBufferBytes(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBufferBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getBufferOffset__wrappee__base(){ return 0; }

	 public int getBufferOffset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBufferOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getBufferLength__wrappee__base(){ return len; }

	 public int getBufferLength(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBufferLength__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void makeSpace__wrappee__base( int sizeNeeded){ int needed=len + sizeNeeded - buf.length; if (needed > 0) bump(needed); }

	 public void makeSpace( int sizeNeeded){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeSpace__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addSize__wrappee__base( int sizeAdded){ len+=sizeAdded; }

	 public void addSize( int sizeAdded){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void bump__wrappee__base( int needed){ int bump=(bumpLen > 0) ? bumpLen : buf.length; byte[] toBuf=new byte[buf.length + needed + bump]; System.arraycopy(buf,0,toBuf,0,len); buf=toBuf; }

	 private void bump( int needed){ t.in(Thread.currentThread().getStackTrace()[1].toString());	bump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
