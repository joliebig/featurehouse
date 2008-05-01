package com.sleepycat.bind.tuple; 
import com.sleepycat.util.FastOutputStream; 
import com.sleepycat.util.UtfOps; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleOutput  extends FastOutputStream {
	 static final int NULL_STRING_UTF_VALUE=((byte)0xFF);

	 public TupleOutput(){ super(); }

	 public TupleOutput( byte[] buffer){ super(buffer); }

	 public final TupleOutput writeBytes( String val){ writeBytes(val.toCharArray()); return this; }

	 public final TupleOutput writeChars( String val){ writeChars(val.toCharArray()); return this; }

	 public final TupleOutput writeString( String val){ if (val != null) { writeString(val.toCharArray()); } else { writeFast(NULL_STRING_UTF_VALUE); } writeFast(0); return this; }

	 public final TupleOutput writeChar( int val){ writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 public final TupleOutput writeBoolean( boolean val){ writeFast(val ? (byte)1 : (byte)0); return this; }

	 public final TupleOutput writeByte( int val){ writeUnsignedByte(val ^ 0x80); return this; }

	 public final TupleOutput writeShort( int val){ writeUnsignedShort(val ^ 0x8000); return this; }

	 public final TupleOutput writeInt( int val){ writeUnsignedInt(val ^ 0x80000000); return this; }

	 public final TupleOutput writeLong( long val){ writeUnsignedLong(val ^ 0x8000000000000000L); return this; }

	 public final TupleOutput writeFloat( float val){ writeUnsignedInt(Float.floatToIntBits(val)); return this; }

	 public final TupleOutput writeDouble( double val){ writeUnsignedLong(Double.doubleToLongBits(val)); return this; }

	 public final TupleOutput writeBytes( char[] chars){ for (int i=0; i < chars.length; i++) { writeFast((byte)chars[i]); } return this; }

	 public final TupleOutput writeChars( char[] chars){ for (int i=0; i < chars.length; i++) { writeFast((byte)(chars[i] >>> 8)); writeFast((byte)chars[i]); } return this; }

	 public final TupleOutput writeString( char[] chars){ if (chars.length == 0) return this; int utfLength=UtfOps.getByteLength(chars); makeSpace(utfLength); UtfOps.charsToBytes(chars,0,getBufferBytes(),getBufferLength(),chars.length); addSize(utfLength); return this; }

	 public final TupleOutput writeUnsignedByte( int val){ writeFast(val); return this; }

	 public final TupleOutput writeUnsignedShort( int val){ writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 public final TupleOutput writeUnsignedInt( long val){ writeFast((byte)(val >>> 24)); writeFast((byte)(val >>> 16)); writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }

	 private final TupleOutput writeUnsignedLong( long val){ writeFast((byte)(val >>> 56)); writeFast((byte)(val >>> 48)); writeFast((byte)(val >>> 40)); writeFast((byte)(val >>> 32)); writeFast((byte)(val >>> 24)); writeFast((byte)(val >>> 16)); writeFast((byte)(val >>> 8)); writeFast((byte)val); return this; }


}
