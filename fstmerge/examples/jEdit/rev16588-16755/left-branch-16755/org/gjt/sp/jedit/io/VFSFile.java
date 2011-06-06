

package org.gjt.sp.jedit.io;


import java.awt.Color;
import java.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.browser.FileCellRenderer;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.IOUtilities;

import javax.swing.*;



public class VFSFile implements Serializable
{
	
	
	public static int findCompletion(VFSFile[] files, int start, int end,
		String str, boolean dirsOnly)
	{
		for(int i = start; i < end; i++)
		{
			VFSFile file = files[i];
			String matchAgainst = (MiscUtilities.isAbsolutePath(str)
				? file.getPath() : file.getName());

			if(dirsOnly && file.getType() == FILE)
				continue;
			
			else if(matchAgainst.equals(str))
				return i;
			else if(matchAgainst.regionMatches(true,0,str,0,str.length()))
				return i;
		}

		return -1;
	} 

	
	public static String findCompletion(String path, String complete,
		VFSBrowser browser, boolean dirsOnly)
	{
		Log.log(Log.DEBUG,VFSFile.class,"findCompletion(" + path + ',' + complete
			+ ',' + dirsOnly + ')');

		if(complete.equals("~"))
			return System.getProperty("user.home");
		else if(complete.equals("-"))
			return browser.getView().getBuffer().getDirectory();
		else if(complete.equals(".."))
			return MiscUtilities.getParentOfPath(path);

		if(MiscUtilities.isAbsolutePath(complete))
		{
			if(MiscUtilities.isURL(complete))
				return complete;
			else
				path = "roots:";
		}

		VFS vfs = VFSManager.getVFSForPath(path);
		if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) == 0)
			return null;
		Object session = vfs.createVFSSession(path,browser);
		if(session == null)
			return null;

		try
		{
			VFSFile[] files = vfs._listFiles(session,path,browser);
			int index = findCompletion(files,0,files.length,complete,dirsOnly);
			if(index != -1)
				return files[index].path;
		}
		catch(IOException e)
		{
			VFSManager.error(e,path,browser);
		}
		finally
		{
			try
			{
				vfs._endVFSSession(session,browser);
			}
			catch(IOException e)
			{
				VFSManager.error(e,path,browser);
			}
		}
		
		return null;
	} 

	
	
	public final Icon getIcon(boolean expanded)
	{
		return getIcon(expanded, jEdit._getBuffer(getSymlinkPath()) != null);
	} 

	
	
	public Icon getIcon(boolean expanded, boolean openBuffer)
	{
		return getDefaultIcon(expanded, openBuffer);
	} 

	
	
	public final Icon getDefaultIcon(boolean expanded, boolean openBuffer)
	{
		if(getType() == DIRECTORY)
			return expanded ? FileCellRenderer.openDirIcon : FileCellRenderer.dirIcon;
		else if(getType() == FILESYSTEM)
			return FileCellRenderer.filesystemIcon;
		else if(openBuffer)
			return FileCellRenderer.openFileIcon;
		else
			return FileCellRenderer.fileIcon;
	} 

	
	
	public final Icon getDefaultIcon(boolean expanded)
	{
		return getDefaultIcon(expanded, jEdit._getBuffer(getSymlinkPath()) != null);
	} 

	
	public static final int FILE = 0;
	public static final int DIRECTORY = 1;
	public static final int FILESYSTEM = 2;
	

	
	
	public String name;
	
	public String path;
	
	public String symlinkPath;
	
	public String deletePath;
	
	public int type;
	
	public long length;
	
	public boolean hidden;
	
	public boolean canRead;
	
	public boolean canWrite;
	

	
	
	public VFSFile()
	{
	} 

	
	public VFSFile(String name, String path, String deletePath,
		int type, long length, boolean hidden)
	{
		this.name = name;
		this.path = path;
		this.deletePath = deletePath;
		this.symlinkPath = path;
		this.type = type;
		this.length = length;
		this.hidden = hidden;
		if(path != null)
		{
			
			VFS vfs = VFSManager.getVFSForPath(path);
			canRead = ((vfs.getCapabilities() & VFS.READ_CAP) != 0);
			canWrite = ((vfs.getCapabilities() & VFS.WRITE_CAP) != 0);
		}
	} 

	
	
	public VFS getVFS()
	{
		return VFSManager.getVFSForPath(path);
	} 
	
	
	public String getName()
	{
		return name;
	} 

	
	public void setName(String name)
	{
		this.name = name;
	} 

	
	
	public boolean isBinary(Object session)
		throws IOException
	{
		InputStream in = getVFS()._createInputStream(session,getPath(),
			false,jEdit.getActiveView());
		if(in == null)
			throw new IOException("Unable to get a Stream for this file "+this);

		try
		{
			return MiscUtilities.isBinary(in);
		}
		finally
		{
			IOUtilities.closeQuietly(in);
		}
	} 

	
	public String getPath()
	{
		return path;
	} 

	
	public void setPath(String path)
	{
		this.path = path;
	} 

	
	public String getSymlinkPath()
	{
		return symlinkPath;
	} 

	
	public void setSymlinkPath(String symlinkPath)
	{
		this.symlinkPath = symlinkPath;
	} 

	
	public String getDeletePath()
	{
		return deletePath;
	} 

	
	public void setDeletePath(String deletePath)
	{
		this.deletePath = deletePath;
	} 

	
	public int getType()
	{
		return type;
	} 

	
	public void setType(int type)
	{
		this.type = type;
	} 

	
	public long getLength()
	{
		return length;
	} 

	
	public void setLength(long length)
	{
		this.length = length;
	} 

	
	public boolean isHidden()
	{
		return hidden;
	} 

	
	public void setHidden(boolean hidden)
	{
		this.hidden = hidden;
	} 

	
	public boolean isReadable()
	{
		return canRead;
	} 

	
	public void setReadable(boolean canRead)
	{
		this.canRead = canRead;
	} 

	
	public boolean isWriteable()
	{
		return canWrite;
	} 

	
	public void setWriteable(boolean canWrite)
	{
		this.canWrite = canWrite;
	} 

	protected boolean colorCalculated;
	protected Color color;

	
	
	public String getExtendedAttribute(String name)
	{
		if(name.equals(VFS.EA_TYPE))
		{
			switch(getType())
			{
			case FILE:
				return jEdit.getProperty("vfs.browser.type.file");
			case DIRECTORY:
				return jEdit.getProperty("vfs.browser.type.directory");
			case FILESYSTEM:
				return jEdit.getProperty("vfs.browser.type.filesystem");
			default:
				throw new IllegalArgumentException();
			}
		}
		else if(name.equals(VFS.EA_STATUS))
		{
			if(isReadable())
			{
				if(isWriteable())
					return jEdit.getProperty("vfs.browser.status.rw");
				else
					return jEdit.getProperty("vfs.browser.status.ro");
			}
			else
			{
				if(isWriteable())
					return jEdit.getProperty("vfs.browser.status.append");
				else
					return jEdit.getProperty("vfs.browser.status.no");
			}
		}
		else if(name.equals(VFS.EA_SIZE))
		{
			if(getType() != FILE)
				return null;
			else
				return MiscUtilities.formatFileSize(getLength());
		}
		else
			return null;
	} 

	
	
	public Color getColor()
	{
		if(!colorCalculated)
		{
			colorCalculated = true;
			color = VFS.getDefaultColorFor(name);
		}

		return color;
	} 

	
	public String toString()
	{
		return name;
	} 
	
	
	
	protected boolean fetchedAttrs()
	{
		return fetchedAttrs;
	} 
	
	
	
	protected void fetchAttrs()
	{
		fetchedAttrs = true;
	} 

	
	private boolean fetchedAttrs;
}
