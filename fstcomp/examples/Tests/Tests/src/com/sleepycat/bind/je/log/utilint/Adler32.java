package com.sleepycat.je.utilint; 
import java.util.zip.Checksum; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  Adler32  implements Checksum {
	 public static Checksum makeChecksum(){ if (EnvironmentImpl.JAVA5_AVAILABLE) { return new java.util.zip.Adler32(); } else { return new Adler32(); } }

	 private long adler=1;

	 private static final int BASE=65521;

	 private static final int NMAX=5552;

	 public void update( int b){ long s1=adler & 0xffff; long s2=(adler >> 16) & 0xffff; s1=(s1 + (b & 0xff)) % BASE; s2=(s1 + s2) % BASE; adler=(s2 << 16) | s1; }

	 public void update( byte[] b, int off, int len){ long s1=adler & 0xffff; long s2=(adler >> 16) & 0xffff; while (len > 0) { int k=len < NMAX ? len : NMAX; len-=k; while (k-- > 0) { s1+=(b[off++] & 0xff); s2+=s1; } s1%=BASE; s2%=BASE; } adler=(s2 << 16) | s1; }

	 public void reset(){ adler=1; }

	 public long getValue(){ return adler; }


}
