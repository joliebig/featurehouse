package com.sleepycat.je.log; 
import java.nio.ByteBuffer; 
import java.sql.Timestamp; 
import javax.transaction.xa.Xid; 
import de.ovgu.cide.jakutil.*; 
public  class  LogUtils {
	 public static final int SHORT_BYTES=2;

	 public static final int INT_BYTES=4;

	 public static final int LONG_BYTES=8;

	 public static final int UNSIGNED_INT_BYTES=4;

	 private static final boolean DEBUG=false;

	 public static final byte[] ZERO_LENGTH_BYTE_ARRAY=new byte[0];

	
public static  class  XidImpl  implements Xid {
		 private int formatId;

		 private byte[] gid;

		 private byte[] bqual;

		 public XidImpl( int formatId, byte[] gid, byte[] bqual){ this.formatId=formatId; this.gid=gid; this.bqual=bqual; }

		 public int getFormatId__wrappee__base(){ return formatId; }

		 public int getFormatId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFormatId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public byte\[\] getGlobalTransactionId__wrappee__base(){ return gid; }

		 public byte[] getGlobalTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getGlobalTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public byte\[\] getBranchQualifier__wrappee__base(){ return bqual; }

		 public byte[] getBranchQualifier(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBranchQualifier__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public boolean equals__wrappee__base( Object o){ if (!(o instanceof XidImpl)) { return false; } XidImpl xid=(XidImpl)o; if (xid.getFormatId() != formatId) { return false; } if (compareByteArrays(xid.getGlobalTransactionId(),gid) && compareByteArrays(xid.getBranchQualifier(),bqual)) { return true; } return false; }

		 public boolean equals( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public int hashCode__wrappee__base(){ int code=formatId; if (gid != null) { for (int i=0; i < gid.length; i++) { code+=gid[i]; } } if (bqual != null) { for (int i=0; i < bqual.length; i++) { code+=bqual[i]; } } return code; }

		 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 private boolean compareByteArrays__wrappee__base( byte[] b1, byte[] b2){ if (b1 == null || b2 == null) { return b1 == b2; } if (b1.length != b2.length) { return false; } for (int i=0; i < b1.length; i++) { if (b1[i] != b2[i]) { return false; } } return true; }

		 private boolean compareByteArrays( byte[] b1, byte[] b2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareByteArrays__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer(); sb.append("<Xid formatId=\"").append(formatId); sb.append("\" gTxnId=\""); if (gid == null) { sb.append("null"); } else { sb.append(new String(gid)); } sb.append("\" bqual=\""); if (bqual == null) { sb.append("null"); } else { sb.append(new String(bqual)); } sb.append("\"/>"); return sb.toString(); }

		 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public static void writeUnsignedInt__wrappee__base( ByteBuffer buf, long value){ buf.put((byte)(value >>> 0)); buf.put((byte)(value >>> 8)); buf.put((byte)(value >>> 16)); buf.put((byte)(value >>> 24)); }

	 public static void writeUnsignedInt( ByteBuffer buf, long value){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeUnsignedInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long getUnsignedInt__wrappee__base( ByteBuffer buf){ long ret=(buf.get() & 0xFFL) << 0; ret+=(buf.get() & 0xFFL) << 8; ret+=(buf.get() & 0xFFL) << 16; ret+=(buf.get() & 0xFFL) << 24; return ret; }

	 public static long getUnsignedInt( ByteBuffer buf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getUnsignedInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeShort__wrappee__base( ByteBuffer logBuf, short i){ byte b=(byte)((i >> 0) & 0xff); logBuf.put(b); b=(byte)((i >> 8) & 0xff); logBuf.put(b); }

	 public static void writeShort( ByteBuffer logBuf, short i){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static short readShort__wrappee__base( ByteBuffer logBuf){ return (short)(((logBuf.get() & 0xFF) << 0) + ((logBuf.get() & 0xFF) << 8)); }

	 public static short readShort( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeInt__wrappee__base( ByteBuffer logBuf, int i){ byte b=(byte)((i >> 0) & 0xff); logBuf.put(b); b=(byte)((i >> 8) & 0xff); logBuf.put(b); b=(byte)((i >> 16) & 0xff); logBuf.put(b); b=(byte)((i >> 24) & 0xff); logBuf.put(b); }

	 public static void writeInt( ByteBuffer logBuf, int i){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int readInt__wrappee__base( ByteBuffer logBuf){ int ret=(logBuf.get() & 0xFF) << 0; ret+=(logBuf.get() & 0xFF) << 8; ret+=(logBuf.get() & 0xFF) << 16; ret+=(logBuf.get() & 0xFF) << 24; return ret; }

	 public static int readInt( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getIntLogSize__wrappee__base(){ return INT_BYTES; }

	 public static int getIntLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getIntLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeIntMSB__wrappee__base( ByteBuffer logBuf, int i){ byte b=(byte)((i >> 24) & 0xff); logBuf.put(b); b=(byte)((i >> 16) & 0xff); logBuf.put(b); b=(byte)((i >> 8) & 0xff); logBuf.put(b); b=(byte)((i >> 0) & 0xff); logBuf.put(b); }

	 public static void writeIntMSB( ByteBuffer logBuf, int i){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeIntMSB__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int readIntMSB__wrappee__base( ByteBuffer logBuf){ int ret=(logBuf.get() & 0xFF) << 24; ret+=(logBuf.get() & 0xFF) << 16; ret+=(logBuf.get() & 0xFF) << 8; ret+=(logBuf.get() & 0xFF) << 0; return ret; }

	 public static int readIntMSB( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readIntMSB__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeLong__wrappee__base( ByteBuffer logBuf, long l){ byte b=(byte)(l >>> 0); logBuf.put(b); b=(byte)(l >>> 8); logBuf.put(b); b=(byte)(l >>> 16); logBuf.put(b); b=(byte)(l >>> 24); logBuf.put(b); b=(byte)(l >>> 32); logBuf.put(b); b=(byte)(l >>> 40); logBuf.put(b); b=(byte)(l >>> 48); logBuf.put(b); b=(byte)(l >>> 56); logBuf.put(b); }

	 public static void writeLong( ByteBuffer logBuf, long l){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long readLong__wrappee__base( ByteBuffer logBuf){ long ret=(logBuf.get() & 0xFFL) << 0; ret+=(logBuf.get() & 0xFFL) << 8; ret+=(logBuf.get() & 0xFFL) << 16; ret+=(logBuf.get() & 0xFFL) << 24; ret+=(logBuf.get() & 0xFFL) << 32; ret+=(logBuf.get() & 0xFFL) << 40; ret+=(logBuf.get() & 0xFFL) << 48; ret+=(logBuf.get() & 0xFFL) << 56; return ret; }

	 public static long readLong( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getLongLogSize__wrappee__base(){ return LONG_BYTES; }

	 public static int getLongLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLongLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeByteArray__wrappee__base( ByteBuffer logBuf, byte[] b){ writeInt(logBuf,b.length); logBuf.put(b); }

	 public static void writeByteArray( ByteBuffer logBuf, byte[] b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeByteArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static byte\[\] readByteArray__wrappee__base( ByteBuffer logBuf){ int size=readInt(logBuf); if (DEBUG) { System.out.println("pos = " + logBuf.position() + " byteArray is "+ size+ " on read"); } if (size == 0) { return ZERO_LENGTH_BYTE_ARRAY; } byte[] b=new byte[size]; logBuf.get(b); return b; }

	 public static byte[] readByteArray( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readByteArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getByteArrayLogSize__wrappee__base( byte[] b){ return INT_BYTES + b.length; }

	 public static int getByteArrayLogSize( byte[] b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getByteArrayLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeString__wrappee__base( ByteBuffer logBuf, String stringVal){ writeByteArray(logBuf,stringVal.getBytes()); }

	 public static void writeString( ByteBuffer logBuf, String stringVal){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String readString__wrappee__base( ByteBuffer logBuf){ return new String(readByteArray(logBuf)); }

	 public static String readString( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getStringLogSize__wrappee__base( String s){ return INT_BYTES + s.length(); }

	 public static int getStringLogSize( String s){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getStringLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeTimestamp__wrappee__base( ByteBuffer logBuf, Timestamp time){ writeLong(logBuf,time.getTime()); }

	 public static void writeTimestamp( ByteBuffer logBuf, Timestamp time){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeTimestamp__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Timestamp readTimestamp__wrappee__base( ByteBuffer logBuf){ long millis=readLong(logBuf); return new Timestamp(millis); }

	 public static Timestamp readTimestamp( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readTimestamp__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getTimestampLogSize__wrappee__base(){ return LONG_BYTES; }

	 public static int getTimestampLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTimestampLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeBoolean__wrappee__base( ByteBuffer logBuf, boolean bool){ byte val=bool ? (byte)1 : (byte)0; logBuf.put(val); }

	 public static void writeBoolean( ByteBuffer logBuf, boolean bool){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean readBoolean__wrappee__base( ByteBuffer logBuf){ byte val=logBuf.get(); return (val == (byte)1) ? true : false; }

	 public static boolean readBoolean( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getBooleanLogSize__wrappee__base(){ return 1; }

	 public static int getBooleanLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBooleanLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean dumpBoolean__wrappee__base( ByteBuffer itemBuffer, StringBuffer sb, String tag){ sb.append("<"); sb.append(tag); sb.append(" exists = \""); boolean exists=readBoolean(itemBuffer); sb.append(exists); if (exists) { sb.append("\">"); } else { sb.append("\"/>"); } return exists; }

	 public static boolean dumpBoolean( ByteBuffer itemBuffer, StringBuffer sb, String tag){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getXidSize__wrappee__base( Xid xid){ byte[] gid=xid.getGlobalTransactionId(); byte[] bqual=xid.getBranchQualifier(); return INT_BYTES + 1 + 1+ (gid == null ? 0 : gid.length)+ (bqual == null ? 0 : bqual.length); }

	 public static int getXidSize( Xid xid){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getXidSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void writeXid__wrappee__base( ByteBuffer logBuf, Xid xid){ byte[] gid=xid.getGlobalTransactionId(); byte[] bqual=xid.getBranchQualifier(); writeInt(logBuf,xid.getFormatId()); if (gid == null) { logBuf.put((byte)-1); } else { logBuf.put((byte)(gid.length)); logBuf.put(gid); } if (bqual == null) { logBuf.put((byte)-1); } else { logBuf.put((byte)(bqual.length)); logBuf.put(bqual); } }

	 public static void writeXid( ByteBuffer logBuf, Xid xid){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeXid__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Xid readXid__wrappee__base( ByteBuffer logBuf){ int formatId=readInt(logBuf); int gidLen=logBuf.get(); byte[] gid=null; if (gidLen >= 0) { gid=new byte[gidLen]; logBuf.get(gid); } int bqualLen=logBuf.get(); byte[] bqual=null; if (bqualLen >= 0) { bqual=new byte[bqualLen]; logBuf.get(bqual); } return new XidImpl(formatId,gid,bqual); }

	 public static Xid readXid( ByteBuffer logBuf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readXid__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
