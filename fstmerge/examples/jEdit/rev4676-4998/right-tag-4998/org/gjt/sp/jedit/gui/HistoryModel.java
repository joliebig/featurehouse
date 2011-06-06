

package org.gjt.sp.jedit.gui;


import java.io.*;
import java.util.*;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.util.Log;



public class HistoryModel
{
	
	
	public HistoryModel(String name)
	{
		this.name = name;

		max = jEdit.getIntegerProperty("history",25);
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
		if(!loaded)
			loadHistory();

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

	
	public static void saveHistory()
	{
		if(loaded && modified)
		{
			Log.log(Log.MESSAGE,HistoryModel.class,"Saving " + history);
			File file1 = new File(MiscUtilities.constructPath(
				jEdit.getSettingsDirectory(), "#history#save#"));
			File file2 = new File(MiscUtilities.constructPath(
				jEdit.getSettingsDirectory(), "history"));
			if(file2.exists() && file2.lastModified() != historyModTime)
			{
				Log.log(Log.WARNING,HistoryModel.class,file2 + " changed"
					+ " on disk; will not save history");
			}
			else
			{
				jEdit.backupSettingsFile(file2);
				saveHistory(file1);
				file2.delete();
				file1.renameTo(file2);
			}
			historyModTime = file2.lastModified();
			modified = false;
		}
	} 

	
	private static final String TO_ESCAPE = "\n\t\\\"'[]";
	private String name;
	private int max;
	private Vector data;
	private static Hashtable models;

	private static boolean loaded;
	private static boolean modified;
	private static File history;
	private static long historyModTime;

	
	private static void loadHistory()
	{
		history = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(),"history"));
		if(history.exists())
			historyModTime = history.lastModified();
		loadHistory(history);
		loaded = true;
	} 

	
	
	private static void loadHistory(File file)
	{
		Log.log(Log.MESSAGE,HistoryModel.class,"Loading " + file);

		if(models == null)
			models = new Hashtable();

		try
		{
			BufferedReader in = new BufferedReader(new FileReader(file));

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
					currentModel = new HistoryModel(line
						.substring(1,line.length() - 1));
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

			in.close();
		}
		catch(FileNotFoundException fnf)
		{
			
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,HistoryModel.class,io);
		}
	} 

	
	
	private static void saveHistory(File file)
	{
		String lineSep = System.getProperty("line.separator");
		try
		{
			BufferedWriter out = new BufferedWriter(
				new FileWriter(file));

			if(models == null)
			{
				out.close();
				return;
			}

			Enumeration modelEnum = models.elements();
			while(modelEnum.hasMoreElements())
			{
				HistoryModel model = (HistoryModel)modelEnum
					.nextElement();

				out.write('[');
				out.write(model.getName());
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

			out.close();
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,HistoryModel.class,io);
		}
	} 

	
}
