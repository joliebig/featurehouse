

package org.gjt.sp.jedit;


import javax.swing.Timer;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import org.gjt.sp.util.Log;



class Autosave implements ActionListener
{
	
	public static void setInterval(int interval)
	{
		if(interval == 0)
		{
			if(timer != null)
			{
				timer.stop();
				timer = null;
			}

			return;
		}

		interval *= 1000;

		if(timer == null)
		{
			timer = new Timer(interval,new Autosave());
			timer.start();
		}
		else
			timer.setDelay(interval);
	} 

	
	public static void stop()
	{
		if(timer != null)
			timer.stop();
	} 

	
	public void actionPerformed(ActionEvent evt)
	{
		if (jEdit.getIntegerProperty("autosave",0) == 0)
				return;
		
		

		
		if(jEdit.getViewCount() != 0
			&& PerspectiveManager.isPerspectiveDirty())
		{
			PerspectiveManager.setPerspectiveDirty(false);
			PerspectiveManager.savePerspective(true);
		}
		boolean autosaveUntitled = jEdit.getBooleanProperty("autosaveUntitled");
		Buffer[] bufferArray = jEdit.getBuffers();
		for(int i = 0; i < bufferArray.length; i++)
		{
			Buffer buffer = bufferArray[i];
			if (autosaveUntitled || !buffer.isUntitled())
				buffer.autosave();
		}

		
		Log.flushStream();
	} 

	
	private static Timer timer;

	private Autosave() {}
	
}
