package com.sleepycat.je.txn; 
import de.ovgu.cide.jakutil.*; 
public  class  LockType {
	 public static final LockType READ=new LockType(0,false,"READ");

	 public static final LockType WRITE=new LockType(1,true,"WRITE");

	 public static final LockType RANGE_READ=new LockType(2,false,"RANGE_READ");

	 public static final LockType RANGE_WRITE=new LockType(3,true,"RANGE_WRITE");

	 public static final LockType RANGE_INSERT=new LockType(4,false,"RANGE_INSERT");

	 public static final LockType NONE=new LockType(5,false,"NONE");

	 public static final LockType RESTART=new LockType(6,false,"RESTART");

	
static { RANGE_READ.setCausesRestart(); RANGE_WRITE.setCausesRestart(); }

	 private static LockConflict[][] conflictMatrix={{LockConflict.ALLOW,LockConflict.BLOCK,LockConflict.ALLOW,LockConflict.BLOCK,LockConflict.ALLOW},{LockConflict.BLOCK,LockConflict.BLOCK,LockConflict.BLOCK,LockConflict.BLOCK,LockConflict.ALLOW},{LockConflict.ALLOW,LockConflict.BLOCK,LockConflict.ALLOW,LockConflict.BLOCK,LockConflict.BLOCK},{LockConflict.BLOCK,LockConflict.BLOCK,LockConflict.BLOCK,LockConflict.BLOCK,LockConflict.BLOCK},{LockConflict.ALLOW,LockConflict.ALLOW,LockConflict.RESTART,LockConflict.RESTART,LockConflict.ALLOW}};

	 private static LockUpgrade[][] upgradeMatrix={{LockUpgrade.EXISTING,LockUpgrade.WRITE_PROMOTE,LockUpgrade.RANGE_READ_IMMED,LockUpgrade.RANGE_WRITE_PROMOTE,LockUpgrade.ILLEGAL},{LockUpgrade.EXISTING,LockUpgrade.EXISTING,LockUpgrade.RANGE_WRITE_IMMED,LockUpgrade.RANGE_WRITE_IMMED,LockUpgrade.ILLEGAL},{LockUpgrade.EXISTING,LockUpgrade.RANGE_WRITE_PROMOTE,LockUpgrade.EXISTING,LockUpgrade.RANGE_WRITE_PROMOTE,LockUpgrade.ILLEGAL},{LockUpgrade.EXISTING,LockUpgrade.EXISTING,LockUpgrade.EXISTING,LockUpgrade.EXISTING,LockUpgrade.ILLEGAL},{LockUpgrade.ILLEGAL,LockUpgrade.ILLEGAL,LockUpgrade.ILLEGAL,LockUpgrade.ILLEGAL,LockUpgrade.EXISTING}};

	 private int index;

	 private boolean write;

	 private String name;

	 private boolean causesRestart;

	 private LockType( int index, boolean write, String name){ this.index=index; this.write=write; this.name=name; }

	 public final boolean isWriteLock(){ return write; }

	 private void setCausesRestart(){ causesRestart=true; }

	 final boolean getCausesRestart(){ return causesRestart; }

	 LockConflict getConflict( LockType requestedType){ return conflictMatrix[index][requestedType.index]; }

	 LockUpgrade getUpgrade( LockType requestedType){ LockUpgrade upgrade=upgradeMatrix[index][requestedType.index]; assert !upgrade.getIllegal() : toString() + " to " + requestedType; return upgrade; }

	 public String toString(){ return name; }


}
