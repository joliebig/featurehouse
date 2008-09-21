package com.sleepycat.je.utilint; 
import java.util.zip.Checksum; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  Adler32  implements Checksum {
	 private long adler=1;

	 private static final int BASE=65521;

	 private static final int NMAX=5552;

	 public static Checksum makeChecksum__wrappee__base(){ if (EnvironmentImpl.JAVA5_AVAILABLE) { return new java.util.zip.Adler32(); } else { return new Adler32(); } }

	 public static Checksum makeChecksum(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeChecksum__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void update__wrappee__base( int b){ long s1=adler & 0xffff; long s2=(adler >> 16) & 0xffff; s1=(s1 + (b & 0xff)) % BASE; s2=(s1 + s2) % BASE; adler=(s2 << 16) | s1; }

	 public void update( int b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	update__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void update__wrappee__base( byte[] b, int off, int len){ long s1=adler & 0xffff; long s2=(adler >> 16) & 0xffff; while (len > 0) { int k=len < NMAX ? len : NMAX; len-=k; while (k-- > 0) { s1+=(b[off++] & 0xff); s2+=s1; } s1%=BASE; s2%=BASE; } adler=(s2 << 16) | s1; }

	 public void update( byte[] b, int off, int len){ t.in(Thread.currentThread().getStackTrace()[1].toString());	update__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void reset__wrappee__base(){ adler=1; }

	 public void reset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getValue__wrappee__base(){ return adler; }

	 public long getValue(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getValue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
