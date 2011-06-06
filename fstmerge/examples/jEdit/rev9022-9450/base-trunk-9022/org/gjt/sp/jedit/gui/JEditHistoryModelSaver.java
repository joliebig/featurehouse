
package org.gjt.sp.jedit.gui;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.jEdit;

import java.io.*;
import java.util.*;


public class JEditHistoryModelSaver implements HistoryModelSaver
{
	
	public Map<String, HistoryModel> load(Map<String, HistoryModel> models)
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return models;

		history = new File(MiscUtilities.constructPath(
			settingsDirectory,"history"));
		if(!history.exists())
			return models;

		historyModTime = history.lastModified();

		Log.log(Log.MESSAGE,HistoryModel.class,"Loading history");

		if(models == null)
			models = Collections.synchronizedMap(new HashMap<String, HistoryModel>());

		BufferedReader in = null;

		try
		{
			in = new BufferedReader(new FileReader(history));

			HistoryModel currentModel = null;
			String line;

			while((line = in.readLine()) != null)
			{
				if(line.length() > 0 && line.charAt(0) == '[' && line.charAt(line.length() - 1) == ']')
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
					currentModel.addElement(MiscUtilities
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
			IOUtilities.closeQuietly(in);
		}
		return models;
	} 

	
	public boolean save(Map<String, HistoryModel> models)
	{
		Log.log(Log.MESSAGE,HistoryModel.class,"Saving history");
		File file1 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "#history#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "history"));
		if(file2.exists() && file2.lastModified() != historyModTime)
		{
			Log.log(Log.WARNING,HistoryModel.class,file2
				+ " changed on disk; will not save history");
			return false;
		}

		jEdit.backupSettingsFile(file2);

		String lineSep = System.getProperty("line.separator");

		BufferedWriter out = null;

		try
		{
			out = new BufferedWriter(new FileWriter(file1));

			if(models != null)
			{
				Collection<HistoryModel> values = models.values();
				for (HistoryModel model : values)
				{
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
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,HistoryModel.class,io);
		}
		finally
		{
			IOUtilities.closeQuietly(out);
		}

		historyModTime = file2.lastModified();
		return true;
	} 

	
	private static final String TO_ESCAPE = "\r\n\t\\\"'[]";
	private static File history;
	private static long historyModTime;
	

}
