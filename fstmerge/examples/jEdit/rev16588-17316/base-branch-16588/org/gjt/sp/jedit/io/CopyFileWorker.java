

package org.gjt.sp.jedit.io;


import java.awt.Component;
import java.io.IOException;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.WorkRequest;



public class CopyFileWorker extends WorkRequest
{
	private final Component comp;
	private final String source;
	
	private final String target;

	
	
	
	public CopyFileWorker(Component comp, String source, String target) 
	{
		if (source == null || target == null)
			throw new NullPointerException("The source and target cannot be null");
		this.comp = comp;
		this.source = source;
		this.target = target;
	} 

	
	public void run() 
	{
		try
		{
			VFS.copy(this, source, target, comp, false);
		}
		catch (IOException e)
		{
			Log.log(Log.ERROR,this, e, e);
		}
	} 
}
