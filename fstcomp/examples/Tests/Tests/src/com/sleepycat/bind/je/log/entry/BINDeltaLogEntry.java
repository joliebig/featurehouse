package com.sleepycat.je.log.entry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.tree.BINDelta; 
import com.sleepycat.je.tree.IN; 
import de.ovgu.cide.jakutil.*; 
public  class  BINDeltaLogEntry  extends SingleItemLogEntry  implements INContainingEntry {
	 public BINDeltaLogEntry( Class logClass){ super(logClass); }

	 public IN getIN( EnvironmentImpl env) throws DatabaseException { BINDelta delta=(BINDelta)getMainItem(); return delta.reconstituteBIN(env); }

	 public DatabaseId getDbId(){ BINDelta delta=(BINDelta)getMainItem(); return delta.getDbId(); }

	 public long getLsnOfIN( long lastReadLsn){ BINDelta delta=(BINDelta)getMainItem(); return delta.getLastFullLsn(); }


}
