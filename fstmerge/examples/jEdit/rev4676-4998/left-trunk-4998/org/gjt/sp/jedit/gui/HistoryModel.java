

package org.gjt.sp.jedit.gui;


import javax.swing.AbstractListModel;
import java.io.*;
import java.util.*;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.util.Log;



public class HistoryModel extends AbstractListModel
{
	
	
	public HistoryModel(String name)
	{
		this.name = name;

		data = new Vector(max);
	} 

	
	
	public void addItem(String text)
	{
		if(text == null || text.length() == 0)
			return;

		modified = true;

		int index = data.indexOf(text);
		if(index != -1)
			data.removeElementAt(index);

		data.insertElementAt(text,0);

		while(getSize() > max)
			data.removeElementAt(data.size() - 1);
	} 

	
	
	public String getItem(int index)
	{
		return (String)data.elementAt(index);
	} 

	
	
	public Object getElementAt(int index)
	{
		return getItem(index);
	} 

	
	
	public void clear()
	{
		modified = true;
		data.removeAllElements();
	} 

	
	
	public int getSize()
	{
		return data.size();
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public static HistoryModel getModel(String name)
	{
		if(models == null)
			models = new Hashtable();

		HistoryModel model = (HistoryModel)models.get(name);
		if(model == null)
		{
			model = new HistoryModel(name);
			models.put(name,model);
		}

		return model;
	} 

	
	public static void loadHistory()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		history = new File(MiscUtilities.constructPath(
			settingsDirectory,"history"));
		if(!history.exists())
			return;

		historyModTime = history.lastModified();

		Log.log(Log.MESSAGE,HistoryModel.class,"Loading history");

		if(models == null)
			models = new Hashtable();

		BufferedReader in = null;

		try
		{
			in = new BufferedReader(new FileReader(history));

			HistoryModel currentModel = null;
			String line;

			while((line = in.readLine()) != null)
			{
				if(line.startsWith("[") && line.endsWith("]"))
				{
					if(currentModel != null)
					{
						models.put(currentModel.getName(),
							currentModel);
					}

					String modelName = MiscUtilities
						.escapesToChars(line.substring(
						1,line.length() - 1));
					currentModel = new HistoryModel(
						modelName);
				}
				else if(currentModel == null)
				{
					throw new IOException("History data starts"
						+ " before model name");
				}
				else
				{
					currentModel.data.addElement(MiscUtilities
						.escapesToChars(line));
				}
			}

			if(currentModel != null)
			{
				models.put(currentModel.getName(),currentModel);
			}
		}
		catch(FileNotFoundException fnf)
		{
			
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,HistoryModel.class,io);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(IOException io)
			{
			}
		}
	} 

	
	public static void saveHistory()
	{
		if(!modified)
			return;

		Log.log(Log.MESSAGE,HistoryModel.class,"Saving history");
		File file1 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "#history#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "history"));
		if(file2.exists() && file2.lastModified() != historyModTime)
		{
			Log.log(Log.WARNING,HistoryModel.class,file2
				+ " changed on disk; will not save history");
			return;
		}

		jEdit.backupSettingsFile(file2);

		String lineSep = System.getProperty("line.separator");

		try
		{
			BufferedWriter out = new BufferedWriter(
				new FileWriter(file1));

			if(models != null)
			{
				Enumeration modelEnum = models.elements();
				while(modelEnum.hasMoreElements())
				{
					HistoryModel model = (HistoryModel)modelEnum
						.nextElement();
					if(model.getSize() == 0)
						continue;
	
					out.write('[');
					out.write(MiscUtilities.charsToEscapes(
						model.getName(),TO_ESCAPE));
					out.write(']');
					out.write(lineSep);
	
					for(int i = 0; i < model.getSize(); i++)
					{
						out.write(MiscUtilities.charsToEscapes(
							model.getItem(i),
							TO_ESCAPE));
						out.write(lineSep);
					}
				}
			}

			out.close();

			
			file2.delete();
			file1.renameTo(file2);
			modified = false;
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,HistoryModel.class,io);
		}

		historyModTime = file2.lastModified();
	} 

	
	public static void propertiesChanged()
	{
		max = jEdit.getIntegerProperty("history",25);
	} 

	
	private static final String TO_ESCAPE = "\r\n\t\\\"'[]";
	private static int max;

	private String name;
	private Vector data;
	private static Hashtable models;

	private static boolean modified;
	private static File history;
	private static long historyModTime;
	
}
