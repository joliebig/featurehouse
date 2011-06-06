
package org.gjt.sp.jedit.bufferio;

import org.gjt.sp.jedit.buffer.KillRing;
import org.gjt.sp.jedit.buffer.UndoManager;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;

import java.io.*;


public class JEditKillRing extends KillRing
{
	
	public void load()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File killRing = new File(MiscUtilities.constructPath(
			settingsDirectory,"killring.xml"));
		if(!killRing.exists())
			return;

		killRingModTime = killRing.lastModified();
		Log.log(Log.MESSAGE,KillRing.class,"Loading killring.xml");

		KillRingHandler handler = new KillRingHandler();
		try
		{
			XMLUtilities.parseXML(new FileInputStream(killRing), handler);
		}
		catch (IOException ioe)
		{
			Log.log(Log.ERROR, this, ioe);
		}

		ring = handler.list.toArray(
			new UndoManager.Remove[handler.list.size()]);
		count = 0;
		wrap = true;
	} 

	
	public void save()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File file1 = new File(MiscUtilities.constructPath(
			settingsDirectory, "#killring.xml#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			settingsDirectory, "killring.xml"));
		if(file2.exists() && file2.lastModified() != killRingModTime)
		{
			Log.log(Log.WARNING,KillRing.class,file2
				+ " changed on disk; will not save recent"
				+ " files");
			return;
		}

		jEdit.backupSettingsFile(file2);

		Log.log(Log.MESSAGE,KillRing.class,"Saving killring.xml");

		String lineSep = System.getProperty("line.separator");

		BufferedWriter out = null;

		try
		{
			out = new BufferedWriter(
				new OutputStreamWriter(
					new FileOutputStream(file1),
					"UTF-8"));

			out.write("<?xml version=\"1.1\" encoding=\"UTF-8\"?>");
			out.write(lineSep);
			out.write("<!DOCTYPE KILLRING SYSTEM \"killring.dtd\">");
			out.write(lineSep);
			out.write("<KILLRING>");
			out.write(lineSep);

			int size = getSize();
			for(int i = size - 1; i >=0; i--)
			{
				out.write("<ENTRY>");
				out.write(XMLUtilities.charsToEntities(
					getElementAt(i).toString(),true));
				out.write("</ENTRY>");
				out.write(lineSep);
			}

			out.write("</KILLRING>");
			out.write(lineSep);

			out.close();

			
			file2.delete();
			file1.renameTo(file2);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,KillRing.class,e);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(IOException e)
			{
			}
		}

		killRingModTime = file2.lastModified();
	} 
}
