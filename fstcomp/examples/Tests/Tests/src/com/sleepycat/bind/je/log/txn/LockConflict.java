package com.sleepycat.je.txn; 
import de.ovgu.cide.jakutil.*; 
 
class  LockConflict {
	 static final LockConflict ALLOW=new LockConflict(true,false);

	 static final LockConflict BLOCK=new LockConflict(false,false);

	 static final LockConflict RESTART=new LockConflict(false,true);

	 private boolean allowed;

	 private boolean restart;

	 private LockConflict( boolean allowed, boolean restart){ this.allowed=allowed; this.restart=restart; }

	 boolean getAllowed(){ return allowed; }

	 boolean getRestart(){ return restart; }


}
