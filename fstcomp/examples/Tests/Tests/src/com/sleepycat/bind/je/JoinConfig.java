package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  JoinConfig  implements Cloneable {
	 static JoinConfig DEFAULT=new JoinConfig();

	 private boolean noSort;

	 public JoinConfig(){ }

	 public void setNoSort( boolean noSort){ this.noSort=noSort; }

	 public boolean getNoSort(){ return noSort; }

	 JoinConfig cloneConfig(){ try { return (JoinConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }


}
