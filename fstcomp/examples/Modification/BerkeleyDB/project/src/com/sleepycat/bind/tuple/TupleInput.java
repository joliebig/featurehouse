package com.sleepycat.bind.tuple; 
import com.sleepycat.util.FastInputStream; 
import com.sleepycat.util.UtfOps; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleInput  extends FastInputStream {
	 public TupleInput( byte[] buffer){ super(buffer); }

	 public TupleInput( byte[] buffer, int offset, int length){ super(buffer,offset,length); }

	 public TupleInput( TupleOutput output){ super(output.getBufferBytes(),output.getBufferOffset(),output.getBufferLength()); }

	 public final String readString__wrappee__base() throws IndexOutOfBoundsException, IllegalArgumentException { byte[] buf=getBufferBytes(); int off=getBufferOffset(); if (available() >= 2 && buf[off] == TupleOutput.NULL_STRING_UTF_VALUE && buf[off + 1] == 0) { skip(2); return null; } else { int byteLen=UtfOps.getZeroTerminatedByteLength(buf,off); skip(byteLen + 1); return UtfOps.bytesToString(buf,off,byteLen); } }

	 public final String readString() throws IndexOutOfBoundsException, IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final char readChar__wrappee__base() throws IndexOutOfBoundsException { return (char)readUnsignedShort(); }

	 public final char readChar() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readChar__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final boolean readBoolean__wrappee__base() throws IndexOutOfBoundsException { int c=readFast(); if (c < 0) { throw new IndexOutOfBoundsException(); } return (c != 0); }

	 public final boolean readBoolean() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final byte readByte__wrappee__base() throws IndexOutOfBoundsException { return (byte)(readUnsignedByte() ^ 0x80); }

	 public final byte readByte() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readByte__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final short readShort__wrappee__base() throws IndexOutOfBoundsException { return (short)(readUnsignedShort() ^ 0x8000); }

	 public final short readShort() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int readInt__wrappee__base() throws IndexOutOfBoundsException { return (int)(readUnsignedInt() ^ 0x80000000); }

	 public final int readInt() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final long readLong__wrappee__base() throws IndexOutOfBoundsException { return readUnsignedLong() ^ 0x8000000000000000L; }

	 public final long readLong() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final float readFloat__wrappee__base() throws IndexOutOfBoundsException { return Float.intBitsToFloat((int)readUnsignedInt()); }

	 public final float readFloat() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFloat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final double readDouble__wrappee__base() throws IndexOutOfBoundsException { return Double.longBitsToDouble(readUnsignedLong()); }

	 public final double readDouble() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readDouble__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int readUnsignedByte__wrappee__base() throws IndexOutOfBoundsException { int c=readFast(); if (c < 0) { throw new IndexOutOfBoundsException(); } return c; }

	 public final int readUnsignedByte() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readUnsignedByte__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final int readUnsignedShort__wrappee__base() throws IndexOutOfBoundsException { int c1=readFast(); int c2=readFast(); if ((c1 | c2) < 0) { throw new IndexOutOfBoundsException(); } return ((c1 << 8) | c2); }

	 public final int readUnsignedShort() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readUnsignedShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final long readUnsignedInt__wrappee__base() throws IndexOutOfBoundsException { long c1=readFast(); long c2=readFast(); long c3=readFast(); long c4=readFast(); if ((c1 | c2 | c3| c4) < 0) { throw new IndexOutOfBoundsException(); } return ((c1 << 24) | (c2 << 16) | (c3 << 8)| c4); }

	 public final long readUnsignedInt() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readUnsignedInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private final long readUnsignedLong__wrappee__base() throws IndexOutOfBoundsException { long c1=readFast(); long c2=readFast(); long c3=readFast(); long c4=readFast(); long c5=readFast(); long c6=readFast(); long c7=readFast(); long c8=readFast(); if ((c1 | c2 | c3| c4| c5| c6| c7| c8) < 0) { throw new IndexOutOfBoundsException(); } return ((c1 << 56) | (c2 << 48) | (c3 << 40)| (c4 << 32)| (c5 << 24)| (c6 << 16)| (c7 << 8)| c8); }

	 private final long readUnsignedLong() throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readUnsignedLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final String readBytes__wrappee__base( int length) throws IndexOutOfBoundsException { StringBuffer buf=new StringBuffer(length); for (int i=0; i < length; i++) { int c=readFast(); if (c < 0) { throw new IndexOutOfBoundsException(); } buf.append((char)c); } return buf.toString(); }

	 public final String readBytes( int length) throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final String readChars__wrappee__base( int length) throws IndexOutOfBoundsException { StringBuffer buf=new StringBuffer(length); for (int i=0; i < length; i++) { buf.append(readChar()); } return buf.toString(); }

	 public final String readChars( int length) throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readChars__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final void readBytes__wrappee__base( char[] chars) throws IndexOutOfBoundsException { for (int i=0; i < chars.length; i++) { int c=readFast(); if (c < 0) { throw new IndexOutOfBoundsException(); } chars[i]=(char)c; } }

	 public final void readBytes( char[] chars) throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final void readChars__wrappee__base( char[] chars) throws IndexOutOfBoundsException { for (int i=0; i < chars.length; i++) { chars[i]=readChar(); } }

	 public final void readChars( char[] chars) throws IndexOutOfBoundsException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readChars__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final String readString__wrappee__base( int length) throws IndexOutOfBoundsException, IllegalArgumentException { char[] chars=new char[length]; readString(chars); return new String(chars); }

	 public final String readString( int length) throws IndexOutOfBoundsException, IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final void readString__wrappee__base( char[] chars) throws IndexOutOfBoundsException, IllegalArgumentException { byte[] buf=getBufferBytes(); off=UtfOps.bytesToChars(buf,off,chars,0,chars.length,false); }

	 public final void readString( char[] chars) throws IndexOutOfBoundsException, IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
