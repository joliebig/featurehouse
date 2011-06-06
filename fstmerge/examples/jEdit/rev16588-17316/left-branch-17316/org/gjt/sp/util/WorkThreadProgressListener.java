

package org.gjt.sp.util;

import java.util.EventListener;


public interface WorkThreadProgressListener extends EventListener
{
	
	void statusUpdate(WorkThreadPool threadPool, int threadIndex);

	
	void progressUpdate(WorkThreadPool threadPool, int threadIndex);
}
