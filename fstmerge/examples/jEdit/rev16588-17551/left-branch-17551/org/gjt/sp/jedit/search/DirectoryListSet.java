

package org.gjt.sp.jedit.search;


import javax.swing.SwingUtilities;
import java.awt.Component;
import java.io.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;



public class DirectoryListSet extends BufferListSet
{
	
	public DirectoryListSet(String directory, String glob, boolean recurse)
	{
		this.directory = directory;
		this.glob = glob;
		this.recurse = recurse;
		this.skipBinary = jEdit.getBooleanProperty("search.skipBinary.toggle");
		this.skipHidden = jEdit.getBooleanProperty("search.skipHidden.toggle");
	} 



	
	public String getDirectory()
	{
		return directory;
	} 

	
	
	public void setDirectory(String directory)
	{
		this.directory = directory;
		invalidateCachedList();
	} 

	
	public String getFileFilter()
	{
		return glob;
	} 

	
	
	public void setFileFilter(String glob)
	{
		this.glob = glob;
		invalidateCachedList();
	} 

	
	public boolean isRecursive()
	{
		return recurse;
	} 

	
	
	public void setRecursive(boolean recurse)
	{
		this.recurse = recurse;
		invalidateCachedList();
	} 

	
	@Override
	public String getCode()
	{
		return "new DirectoryListSet(\"" + StandardUtilities.charsToEscapes(directory)
			+ "\",\"" + StandardUtilities.charsToEscapes(glob) + "\","
			+ recurse + ')';
	} 

	
	@Override
	protected String[] _getFiles(final Component comp)
	{
		skipBinary = jEdit.getBooleanProperty("search.skipBinary.toggle");
		skipHidden = jEdit.getBooleanProperty("search.skipHidden.toggle");
		final VFS vfs = VFSManager.getVFSForPath(directory);
		Object session;
		if(SwingUtilities.isEventDispatchThread())
		{
			session = vfs.createVFSSession(directory,comp);
		}
		else
		{
			final Object[] returnValue = new Object[1];

			try
			{
				SwingUtilities.invokeAndWait(new Runnable()
				{
					public void run()
					{
						returnValue[0] = vfs.createVFSSession(directory,comp);
					}
				});
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);
			}

			session = returnValue[0];
		}

		if(session == null)
			return null;

		try
		{
			try
			{
				return vfs._listDirectory(session,directory,glob,recurse,comp, skipBinary, skipHidden);
			}
			finally
			{
				vfs._endVFSSession(session, comp);
			}
		}
		catch(IOException io)
		{
			VFSManager.error(comp,directory,"ioerror",new String[]
				{ io.toString() });
			return null;
		}
	} 

	
	private String directory;
	private String glob;
	private boolean recurse;
	private boolean skipHidden;
	private boolean skipBinary;
	
}
