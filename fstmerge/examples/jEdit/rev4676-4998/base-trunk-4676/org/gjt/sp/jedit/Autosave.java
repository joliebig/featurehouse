

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
		
		

		
		if(jEdit.getViewCount() != 0)
			PerspectiveManager.savePerspective(true);

		Buffer[] bufferArray = jEdit.getBuffers();
		for(int i = 0; i < bufferArray.length; i++)
			bufferArray[i].autosave();

		
		Log.flushStream();
	} 

	
	private static Timer timer;

	private Autosave() {}
	
}
