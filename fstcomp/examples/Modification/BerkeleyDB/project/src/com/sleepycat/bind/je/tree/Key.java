package com.sleepycat.je.tree; 
import java.util.Comparator; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public final  class  Key  implements Comparable {
	 public static boolean DUMP_BINARY=true;

	 public static boolean DUMP_INT_BINDING=false;

	 public static final byte[] EMPTY_KEY=new byte[0];

	 private byte[] key;

	 public Key( byte[] key){ if (key == null) { this.key=null; } else { this.key=new byte[key.length]; System.arraycopy(key,0,this.key,0,key.length); } }

	 public static byte\[\] makeKey__wrappee__base( DatabaseEntry dbt){ byte[] entryKey=dbt.getData(); if (entryKey == null) { return EMPTY_KEY; } else { byte[] newKey=new byte[dbt.getSize()]; System.arraycopy(entryKey,dbt.getOffset(),newKey,0,dbt.getSize()); return newKey; } }

	 public static byte[] makeKey( DatabaseEntry dbt){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base(){ return key; }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int compareTo__wrappee__base( Object o){ if (o == null) { throw new NullPointerException(); } Key argKey=(Key)o; return compareUnsignedBytes(this.key,argKey.key); }

	 public int compareTo( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean equals__wrappee__base( Object o){ return (o instanceof Key) && (compareTo(o) == 0); }

	 public boolean equals( Object o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ int code=0; for (int i=0; i < key.length; i+=1) { code+=key[i]; } return code; }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int compareKeys__wrappee__base( byte[] key1, byte[] key2, Comparator comparator){ if (comparator != null) { return comparator.compare(key1,key2); } else { return compareUnsignedBytes(key1,key2); } }

	 public static int compareKeys( byte[] key1, byte[] key2, Comparator comparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareKeys__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static int compareUnsignedBytes__wrappee__base( byte[] key1, byte[] key2){ int a1Len=key1.length; int a2Len=key2.length; int limit=Math.min(a1Len,a2Len); for (int i=0; i < limit; i++) { byte b1=key1[i]; byte b2=key2[i]; if (b1 == b2) { continue; } else { return (b1 & 0xff) - (b2 & 0xff); } } return (a1Len - a2Len); }

	 private static int compareUnsignedBytes( byte[] key1, byte[] key2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareUnsignedBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String dumpString__wrappee__base( byte[] key, int nspaces){ StringBuffer sb=new StringBuffer(); sb.append(TreeUtils.indent(nspaces)); sb.append("<key v=\""); if (DUMP_BINARY) { if (key == null) { sb.append("<null>"); } else { sb.append(TreeUtils.dumpByteArray(key)); } } else if (DUMP_INT_BINDING) { if (key == null) { sb.append("<null>"); } else { DatabaseEntry e=new DatabaseEntry(key); } } else { sb.append(key == null ? "" : new String(key)); } sb.append("\"/>"); return sb.toString(); }

	 public static String dumpString( byte[] key, int nspaces){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String getNoFormatString__wrappee__base( byte[] key){ return "key=" + dumpString(key,0); }

	 public static String getNoFormatString( byte[] key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoFormatString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
