
package org.gjt.sp.util;

import java.awt.*;


public class ThreadUtilities
{
	
	public static void runInDispatchThread(Runnable runnable)
	{
		if(EventQueue.isDispatchThread())
			runnable.run();
		else
			EventQueue.invokeLater(runnable);
	}

	private ThreadUtilities()
	{
	}
}
