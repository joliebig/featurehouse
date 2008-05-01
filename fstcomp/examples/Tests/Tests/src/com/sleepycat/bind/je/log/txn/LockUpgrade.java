package com.sleepycat.je.txn; 
import de.ovgu.cide.jakutil.*; 
 
class  LockUpgrade {
	 static final LockUpgrade ILLEGAL=new LockUpgrade(null,false,true);

	 static final LockUpgrade EXISTING=new LockUpgrade(null,false,false);

	 static final LockUpgrade WRITE_PROMOTE=new LockUpgrade(LockType.WRITE,true,false);

	 static final LockUpgrade RANGE_READ_IMMED=new LockUpgrade(LockType.RANGE_READ,false,false);

	 static final LockUpgrade RANGE_WRITE_IMMED=new LockUpgrade(LockType.RANGE_WRITE,false,false);

	 static final LockUpgrade RANGE_WRITE_PROMOTE=new LockUpgrade(LockType.RANGE_WRITE,true,false);

	 private LockType upgrade;

	 private boolean promotion;

	 private boolean illegal;

	 private LockUpgrade( LockType upgrade, boolean promotion, boolean illegal){ this.upgrade=upgrade; this.promotion=promotion; this.illegal=illegal; }

	 boolean getIllegal(){ return illegal; }

	 LockType getUpgrade(){ return upgrade; }

	 boolean getPromotion(){ return promotion; }


}
