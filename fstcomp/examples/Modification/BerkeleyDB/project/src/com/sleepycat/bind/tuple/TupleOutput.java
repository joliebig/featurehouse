package com.sleepycat.bind.tuple; 
import com.sleepycat.util.FastOutputStream; 
import com.sleepycat.util.UtfOps; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleOutput  extends FastOutputStream {
	 static final int NULL_STRING_UTF_VALUE=((byte)0xFF);

	 public TupleOutput(){ super(); }

	 public TupleOutput( byte[] buffer){ super(buffer); }

	 public final TupleOutput writeBytes__wrappee__base( String val){ writeBytes(val.toCharArray()); return this; }

	 public final TupleOutput writeBytes( String val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeChars__wrappee__base( String val){ writeChars(val.toCharArray()); return this; }

	 public final TupleOutput writeChars( String val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeChars__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeString__wrappee__base( String val){ if (val != null) { writeString(val.toCharArray()); } else { writeFast(NULL_STRING_UTF_VALUE); } writeFast(0); return this; }

	 public final TupleOutput writeString( String val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeChar__wrappee__base( int val){ writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 public final TupleOutput writeChar( int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeChar__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeBoolean__wrappee__base( boolean val){ writeFast(val ? (byte)1 : (byte)0); return this; }

	 public final TupleOutput writeBoolean( boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeByte__wrappee__base( int val){ writeUnsignedByte(val ^ 0x80); return this; }

	 public final TupleOutput writeByte( int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeByte__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeShort__wrappee__base( int val){ writeUnsignedShort(val ^ 0x8000); return this; }

	 public final TupleOutput writeShort( int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeInt__wrappee__base( int val){ writeUnsignedInt(val ^ 0x80000000); return this; }

	 public final TupleOutput writeInt( int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeLong__wrappee__base( long val){ writeUnsignedLong(val ^ 0x8000000000000000L); return this; }

	 public final TupleOutput writeLong( long val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeFloat__wrappee__base( float val){ writeUnsignedInt(Float.floatToIntBits(val)); return this; }

	 public final TupleOutput writeFloat( float val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeFloat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeDouble__wrappee__base( double val){ writeUnsignedLong(Double.doubleToLongBits(val)); return this; }

	 public final TupleOutput writeDouble( double val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeDouble__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeBytes__wrappee__base( char[] chars){ for (int i=0; i < chars.length; i++) { writeFast((byte)chars[i]); } return this; }

	 public final TupleOutput writeBytes( char[] chars){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeChars__wrappee__base( char[] chars){ for (int i=0; i < chars.length; i++) { writeFast((byte)(chars[i] >>> 8)); writeFast((byte)chars[i]); } return this; }

	 public final TupleOutput writeChars( char[] chars){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeChars__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeString__wrappee__base( char[] chars){ if (chars.length == 0) return this; int utfLength=UtfOps.getByteLength(chars); makeSpace(utfLength); UtfOps.charsToBytes(chars,0,getBufferBytes(),getBufferLength(),chars.length); addSize(utfLength); return this; }

	 public final TupleOutput writeString( char[] chars){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeUnsignedByte__wrappee__base( int val){ writeFast(val); return this; }

	 public final TupleOutput writeUnsignedByte( int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeUnsignedByte__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeUnsignedShort__wrappee__base( int val){ writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 public final TupleOutput writeUnsignedShort( int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeUnsignedShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public final TupleOutput writeUnsignedInt__wrappee__base( long val){ writeFast((byte)(val >>> 24)); writeFast((byte)(val >>> 16)); writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 public final TupleOutput writeUnsignedInt( long val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeUnsignedInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private final TupleOutput writeUnsignedLong__wrappee__base( long val){ writeFast((byte)(val >>> 56)); writeFast((byte)(val >>> 48)); writeFast((byte)(val >>> 40)); writeFast((byte)(val >>> 32)); writeFast((byte)(val >>> 24)); writeFast((byte)(val >>> 16)); writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 private final TupleOutput writeUnsignedLong( long val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeUnsignedLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
