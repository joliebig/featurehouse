package com.sleepycat.je.cleaner; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.tree.LN; 
import de.ovgu.cide.jakutil.*; 
public final  class  LNInfo {
	 private LN ln;

	 private DatabaseId dbId;

	 private byte[] key;

	 private byte[] dupKey;

	 public LNInfo( LN ln, DatabaseId dbId, byte[] key, byte[] dupKey){ this.ln=ln; this.dbId=dbId; this.key=key; this.dupKey=dupKey; }

	 LN getLN(){ return ln; }

	 DatabaseId getDbId(){ return dbId; }

	 byte[] getKey(){ return key; }

	 byte[] getDupKey(){ return dupKey; }


}
