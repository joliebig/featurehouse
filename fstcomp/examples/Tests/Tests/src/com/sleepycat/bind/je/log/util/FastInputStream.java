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

	 public int available(){ return len - off; }

	 public boolean markSupported(){ return true; }

	 public void mark( int pos){ mark=pos; }

	 public void reset(){ off=mark; }

	 public long skip( long count){ int myCount=(int)count; if (myCount + off > len) { myCount=len - off; } off+=myCount; return myCount; }

	 public int read() throws IOException { return readFast(); }

	 public int read( byte[] toBuf) throws IOException { return readFast(toBuf,0,toBuf.length); }

	 public int read( byte[] toBuf, int offset, int length) throws IOException { return readFast(toBuf,offset,length); }

	 public final int readFast(){ return (off < len) ? (buf[off++] & 0xff) : (-1); }

	 public final int readFast( byte[] toBuf){ return readFast(toBuf,0,toBuf.length); }

	 public final int readFast( byte[] toBuf, int offset, int length){ int avail=len - off; if (avail <= 0) { return -1; } if (length > avail) { length=avail; } System.arraycopy(buf,off,toBuf,offset,length); off+=length; return length; }

	 public final byte[] getBufferBytes(){ return buf; }

	 public final int getBufferOffset(){ return off; }

	 public final int getBufferLength(){ return len; }


}
