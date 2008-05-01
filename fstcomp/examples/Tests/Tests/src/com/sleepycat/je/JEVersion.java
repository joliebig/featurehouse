package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  JEVersion {
	 public static final JEVersion CURRENT_VERSION=new JEVersion(2,1,30,null);

	 private int majorNum;

	 private int minorNum;

	 private int patchNum;

	 private String name;

	 private JEVersion( int majorNum, int minorNum, int patchNum, String name){ this.majorNum=majorNum; this.minorNum=minorNum; this.patchNum=patchNum; this.name=name; }

	 public String toString(){ return getVersionString(); }

	 public int getMajor(){ return majorNum; }

	 public int getMinor(){ return minorNum; }

	 public int getPatch(){ return patchNum; }

	 public String getNumericVersionString(){ StringBuffer version=new StringBuffer(); version.append(majorNum).append("."); version.append(minorNum).append("."); version.append(patchNum); return version.toString(); }

	 public String getVersionString(){ StringBuffer version=new StringBuffer(); version.append(majorNum).append("."); version.append(minorNum).append("."); version.append(patchNum); if (name != null) { version.append(" ("); version.append(name).append(")"); } return version.toString(); }


}
