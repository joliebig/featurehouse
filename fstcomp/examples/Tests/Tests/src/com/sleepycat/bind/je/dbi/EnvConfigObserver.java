package com.sleepycat.je.dbi; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  interface  EnvConfigObserver {
	 void envConfigUpdate( DbConfigManager configMgr) throws DatabaseException ;


}
