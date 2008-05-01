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

	 private void initBuffer( int bufferSize, int bumpLen){ buf=new byte[bufferSize]; this.bumpLen=bumpLen; }

	 public int size(){ return len; }

	 public void reset(){ len=0; }

	 public void write( int b) throws IOException { writeFast(b); }

	 public void write( byte[] fromBuf) throws IOException { writeFast(fromBuf); }

	 public void write( byte[] fromBuf, int offset, int length) throws IOException { writeFast(fromBuf,offset,length); }

	 public void writeTo( OutputStream out) throws IOException { out.write(buf,0,len); }

	 public String toString(){ return new String(buf,0,len); }

	 public String toString( String encoding) throws UnsupportedEncodingException { return new String(buf,0,len,encoding); }

	 public byte[] toByteArray(){ if (len == 0) { return ZERO_LENGTH_BYTE_ARRAY; } else { byte[] toBuf=new byte[len]; System.arraycopy(buf,0,toBuf,0,len); return toBuf; } }

	 public final void writeFast( int b){ if (len + 1 > buf.length) bump(1); buf[len++]=(byte)b; }

	 public final void writeFast( byte[] fromBuf){ int needed=len + fromBuf.length - buf.length; if (needed > 0) bump(needed); System.arraycopy(fromBuf,0,buf,len,fromBuf.length); len+=fromBuf.length; }

	 public final void writeFast( byte[] fromBuf, int offset, int length){ int needed=len + length - buf.length; if (needed > 0) bump(needed); System.arraycopy(fromBuf,offset,buf,len,length); len+=length; }

	 public byte[] getBufferBytes(){ return buf; }

	 public int getBufferOffset(){ return 0; }

	 public int getBufferLength(){ return len; }

	 public void makeSpace( int sizeNeeded){ int needed=len + sizeNeeded - buf.length; if (needed > 0) bump(needed); }

	 public void addSize( int sizeAdded){ len+=sizeAdded; }

	 private void bump( int needed){ int bump=(bumpLen > 0) ? bumpLen : buf.length; byte[] toBuf=new byte[buf.length + needed + bump]; System.arraycopy(buf,0,toBuf,0,len); buf=toBuf; }


}
