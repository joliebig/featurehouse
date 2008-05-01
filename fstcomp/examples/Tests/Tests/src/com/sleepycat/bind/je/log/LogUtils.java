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

	 public static void writeUnsignedInt( ByteBuffer buf, long value){ buf.put((byte)(value >>> 0)); buf.put((byte)(value >>> 8)); buf.put((byte)(value >>> 16)); buf.put((byte)(value >>> 24)); }

	 public static long getUnsignedInt( ByteBuffer buf){ long ret=(buf.get() & 0xFFL) << 0; ret+=(buf.get() & 0xFFL) << 8; ret+=(buf.get() & 0xFFL) << 16; ret+=(buf.get() & 0xFFL) << 24; return ret; }

	 public static void writeShort( ByteBuffer logBuf, short i){ byte b=(byte)((i >> 0) & 0xff); logBuf.put(b); b=(byte)((i >> 8) & 0xff); logBuf.put(b); }

	 public static short readShort( ByteBuffer logBuf){ return (short)(((logBuf.get() & 0xFF) << 0) + ((logBuf.get() & 0xFF) << 8)); }

	 public static void writeInt( ByteBuffer logBuf, int i){ byte b=(byte)((i >> 0) & 0xff); logBuf.put(b); b=(byte)((i >> 8) & 0xff); logBuf.put(b); b=(byte)((i >> 16) & 0xff); logBuf.put(b); b=(byte)((i >> 24) & 0xff); logBuf.put(b); }

	 public static int readInt( ByteBuffer logBuf){ int ret=(logBuf.get() & 0xFF) << 0; ret+=(logBuf.get() & 0xFF) << 8; ret+=(logBuf.get() & 0xFF) << 16; ret+=(logBuf.get() & 0xFF) << 24; return ret; }

	 public static int getIntLogSize(){ return INT_BYTES; }

	 public static void writeIntMSB( ByteBuffer logBuf, int i){ byte b=(byte)((i >> 24) & 0xff); logBuf.put(b); b=(byte)((i >> 16) & 0xff); logBuf.put(b); b=(byte)((i >> 8) & 0xff); logBuf.put(b); b=(byte)((i >> 0) & 0xff); logBuf.put(b); }

	 public static int readIntMSB( ByteBuffer logBuf){ int ret=(logBuf.get() & 0xFF) << 24; ret+=(logBuf.get() & 0xFF) << 16; ret+=(logBuf.get() & 0xFF) << 8; ret+=(logBuf.get() & 0xFF) << 0; return ret; }

	 public static void writeLong( ByteBuffer logBuf, long l){ byte b=(byte)(l >>> 0); logBuf.put(b); b=(byte)(l >>> 8); logBuf.put(b); b=(byte)(l >>> 16); logBuf.put(b); b=(byte)(l >>> 24); logBuf.put(b); b=(byte)(l >>> 32); logBuf.put(b); b=(byte)(l >>> 40); logBuf.put(b); b=(byte)(l >>> 48); logBuf.put(b); b=(byte)(l >>> 56); logBuf.put(b); }

	 public static long readLong( ByteBuffer logBuf){ long ret=(logBuf.get() & 0xFFL) << 0; ret+=(logBuf.get() & 0xFFL) << 8; ret+=(logBuf.get() & 0xFFL) << 16; ret+=(logBuf.get() & 0xFFL) << 24; ret+=(logBuf.get() & 0xFFL) << 32; ret+=(logBuf.get() & 0xFFL) << 40; ret+=(logBuf.get() & 0xFFL) << 48; ret+=(logBuf.get() & 0xFFL) << 56; return ret; }

	 public static int getLongLogSize(){ return LONG_BYTES; }

	 public static void writeByteArray( ByteBuffer logBuf, byte[] b){ writeInt(logBuf,b.length); logBuf.put(b); }

	 public static byte[] readByteArray( ByteBuffer logBuf){ int size=readInt(logBuf); if (DEBUG) { System.out.println("pos = " + logBuf.position() + " byteArray is "+ size+ " on read"); } if (size == 0) { return ZERO_LENGTH_BYTE_ARRAY; } byte[] b=new byte[size]; logBuf.get(b); return b; }

	 public static int getByteArrayLogSize( byte[] b){ return INT_BYTES + b.length; }

	 public static void writeString( ByteBuffer logBuf, String stringVal){ writeByteArray(logBuf,stringVal.getBytes()); }

	 public static String readString( ByteBuffer logBuf){ return new String(readByteArray(logBuf)); }

	 public static int getStringLogSize( String s){ return INT_BYTES + s.length(); }

	 public static void writeTimestamp( ByteBuffer logBuf, Timestamp time){ writeLong(logBuf,time.getTime()); }

	 public static Timestamp readTimestamp( ByteBuffer logBuf){ long millis=readLong(logBuf); return new Timestamp(millis); }

	 public static int getTimestampLogSize(){ return LONG_BYTES; }

	 public static void writeBoolean( ByteBuffer logBuf, boolean bool){ byte val=bool ? (byte)1 : (byte)0; logBuf.put(val); }

	 public static boolean readBoolean( ByteBuffer logBuf){ byte val=logBuf.get(); return (val == (byte)1) ? true : false; }

	 public static int getBooleanLogSize(){ return 1; }

	 public static boolean dumpBoolean( ByteBuffer itemBuffer, StringBuffer sb, String tag){ sb.append("<"); sb.append(tag); sb.append(" exists = \""); boolean exists=readBoolean(itemBuffer); sb.append(exists); if (exists) { sb.append("\">"); } else { sb.append("\"/>"); } return exists; }

	 public static int getXidSize( Xid xid){ byte[] gid=xid.getGlobalTransactionId(); byte[] bqual=xid.getBranchQualifier(); return INT_BYTES + 1 + 1+ (gid == null ? 0 : gid.length)+ (bqual == null ? 0 : bqual.length); }

	 public static void writeXid( ByteBuffer logBuf, Xid xid){ byte[] gid=xid.getGlobalTransactionId(); byte[] bqual=xid.getBranchQualifier(); writeInt(logBuf,xid.getFormatId()); if (gid == null) { logBuf.put((byte)-1); } else { logBuf.put((byte)(gid.length)); logBuf.put(gid); } if (bqual == null) { logBuf.put((byte)-1); } else { logBuf.put((byte)(bqual.length)); logBuf.put(bqual); } }

	 public static Xid readXid( ByteBuffer logBuf){ int formatId=readInt(logBuf); int gidLen=logBuf.get(); byte[] gid=null; if (gidLen >= 0) { gid=new byte[gidLen]; logBuf.get(gid); } int bqualLen=logBuf.get(); byte[] bqual=null; if (bqualLen >= 0) { bqual=new byte[bqualLen]; logBuf.get(bqual); } return new XidImpl(formatId,gid,bqual); }

	
public static  class  XidImpl  implements Xid {
		 private int formatId;

		 private byte[] gid;

		 private byte[] bqual;

		 public XidImpl( int formatId, byte[] gid, byte[] bqual){ this.formatId=formatId; this.gid=gid; this.bqual=bqual; }

		 public int getFormatId(){ return formatId; }

		 public byte[] getGlobalTransactionId(){ return gid; }

		 public byte[] getBranchQualifier(){ return bqual; }

		 public boolean equals( Object o){ if (!(o instanceof XidImpl)) { return false; } XidImpl xid=(XidImpl)o; if (xid.getFormatId() != formatId) { return false; } if (compareByteArrays(xid.getGlobalTransactionId(),gid) && compareByteArrays(xid.getBranchQualifier(),bqual)) { return true; } return false; }

		 public int hashCode(){ int code=formatId; if (gid != null) { for (int i=0; i < gid.length; i++) { code+=gid[i]; } } if (bqual != null) { for (int i=0; i < bqual.length; i++) { code+=bqual[i]; } } return code; }

		 private boolean compareByteArrays( byte[] b1, byte[] b2){ if (b1 == null || b2 == null) { return b1 == b2; } if (b1.length != b2.length) { return false; } for (int i=0; i < b1.length; i++) { if (b1[i] != b2[i]) { return false; } } return true; }

		 public String toString(){ StringBuffer sb=new StringBuffer(); sb.append("<Xid formatId=\"").append(formatId); sb.append("\" gTxnId=\""); if (gid == null) { sb.append("null"); } else { sb.append(new String(gid)); } sb.append("\" bqual=\""); if (bqual == null) { sb.append("null"); } else { sb.append(new String(bqual)); } sb.append("\"/>"); return sb.toString(); }


	}


}
