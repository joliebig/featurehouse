

package org.gjt.sp.jedit.search;


import java.awt.Component;
import org.gjt.sp.jedit.View;



public abstract class BufferListSet implements SearchFileSet
{
	
	public synchronized String getFirstFile(View view)
	{
		if(files == null)
			files = _getFiles(view);

		if(files == null || files.length == 0)
			return null;
		else
			return files[0];
	} 

	
	public synchronized String getNextFile(View view, String file)
	{
		if(files == null)
			files = _getFiles(view);

		if(files == null || files.length == 0)
			return null;

		if(file == null)
		{
			file = view.getBuffer().getPath();

			for(int i = 0; i < files.length; i++)
			{
				if(files[i].equals(file))
					return file;
			}

			return getFirstFile(view);
		}
		else
		{
			
			for(int i = 0; i < files.length - 1; i++)
			{
				if(files[i].equals(file))
					return files[i+1];
			}

			return null;
		}
	} 

	
	public synchronized String[] getFiles(View view)
	{
		if(files == null)
			files = _getFiles(view);

		if(files == null || files.length == 0)
			return null;
		else
			return files;
	} 

	
	public synchronized int getFileCount(View view)
	{
		if(files == null)
			files = _getFiles(view);

		if(files == null)
			return 0;
		else
			return files.length;
	} 

	
	public String getCode()
	{
		
		return null;
	} 

	
	public void invalidateCachedList()
	{
		files = null;
	} 

	protected abstract String[] _getFiles(Component comp);

	private String[] files;
}
