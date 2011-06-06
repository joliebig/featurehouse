

package org.gjt.sp.jedit.search;


import java.awt.Component;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.io.*;



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
			file = view.getBuffer().getSymlinkPath();
			VFS vfs = VFSManager.getVFSForPath(file);
			boolean ignoreCase = ((vfs.getCapabilities()
				& VFS.CASE_INSENSITIVE_CAP) != 0);

			for(int i = 0; i < files.length; i++)
			{
				if(MiscUtilities.compareStrings(
					files[i],file,ignoreCase) == 0)
				{
					return file;
				}
			}

			return getFirstFile(view);
		}
		else
		{
			
			VFS vfs = VFSManager.getVFSForPath(file);
			boolean ignoreCase = ((vfs.getCapabilities()
				& VFS.CASE_INSENSITIVE_CAP) != 0);

			for(int i = 0; i < files.length - 1; i++)
			{
				if(MiscUtilities.compareStrings(
					files[i],file,ignoreCase) == 0)
				{
					return files[i+1];
				}
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

	
	public synchronized void invalidateCachedList()
	{
		files = null;
	} 

	
	protected abstract String[] _getFiles(Component comp);

	private String[] files;
}
