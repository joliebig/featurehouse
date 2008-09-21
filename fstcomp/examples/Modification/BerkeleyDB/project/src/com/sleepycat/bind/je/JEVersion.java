package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  JEVersion {
	 public static final JEVersion CURRENT_VERSION=new JEVersion(2,1,30,null);

	 private int majorNum;

	 private int minorNum;

	 private int patchNum;

	 private String name;

	 private JEVersion( int majorNum, int minorNum, int patchNum, String name){ this.majorNum=majorNum; this.minorNum=minorNum; this.patchNum=patchNum; this.name=name; }

	 public String toString__wrappee__base(){ return getVersionString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getMajor__wrappee__base(){ return majorNum; }

	 public int getMajor(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMajor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getMinor__wrappee__base(){ return minorNum; }

	 public int getMinor(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMinor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getPatch__wrappee__base(){ return patchNum; }

	 public int getPatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getNumericVersionString__wrappee__base(){ StringBuffer version=new StringBuffer(); version.append(majorNum).append("."); version.append(minorNum).append("."); version.append(patchNum); return version.toString(); }

	 public String getNumericVersionString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNumericVersionString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getVersionString__wrappee__base(){ StringBuffer version=new StringBuffer(); version.append(majorNum).append("."); version.append(minorNum).append("."); version.append(patchNum); if (name != null) { version.append(" ("); version.append(name).append(")"); } return version.toString(); }

	 public String getVersionString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getVersionString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
