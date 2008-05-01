package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  CheckpointConfig {
	 public final static CheckpointConfig DEFAULT=new CheckpointConfig();

	 private boolean force=false;

	 private boolean minimizeRecoveryTime=false;

	 public CheckpointConfig(){ }

	 public void setForce( boolean force){ this.force=force; }

	 public boolean getForce(){ return force; }

	 public void setMinimizeRecoveryTime( boolean minimizeRecoveryTime){ this.minimizeRecoveryTime=minimizeRecoveryTime; }

	 public boolean getMinimizeRecoveryTime(){ return minimizeRecoveryTime; }


}
