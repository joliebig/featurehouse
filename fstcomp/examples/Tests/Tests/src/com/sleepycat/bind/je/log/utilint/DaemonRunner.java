package com.sleepycat.je.utilint; 
import de.ovgu.cide.jakutil.*; 
public  interface  DaemonRunner {
	 void runOrPause( boolean run);

	 void requestShutdown();

	 void shutdown();

	 int getNWakeupRequests();


}
