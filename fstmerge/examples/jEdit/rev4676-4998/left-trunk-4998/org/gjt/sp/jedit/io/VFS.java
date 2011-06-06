

package org.gjt.sp.jedit.io;


import gnu.regexp.*;
import java.awt.Color;
import java.awt.Component;
import java.io.*;
import java.util.*;
import org.gjt.sp.jedit.buffer.BufferIORequest;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public abstract class VFS
{
	

	
	public static final int READ_CAP = 1 << 0;

	
	public static final int WRITE_CAP = 1 << 1;

	
	public static final int BROWSE_CAP = 1 << 2;

	
	public static final int DELETE_CAP = 1 << 3;

	
	public static final int RENAME_CAP = 1 << 4;

	
	public static final int MKDIR_CAP = 1 << 5;

	
	public static final int LOW_LATENCY_CAP = 1 << 6;

	
	public static final int CASE_INSENSITIVE_CAP = 1 << 7;

	

	
	
	public static final String EA_TYPE = "type";

	
	public static final String EA_STATUS = "status";

	
	public static final String EA_SIZE = "size";

	
	public static final String EA_MODIFIED = "modified";
	

	
	
	public VFS(String name)
	{
		this(name,0);
	} 

	
	
	public VFS(String name, int caps)
	{
		this.name = name;
		this.caps = caps;
		
		this.extAttrs = new String[] { EA_SIZE, EA_TYPE };
	} 

	
	
	public VFS(String name, int caps, String[] extAttrs)
	{
		this.name = name;
		this.caps = caps;
		this.extAttrs = extAttrs;
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public int getCapabilities()
	{
		return caps;
	} 

	
	
	public String[] getExtendedAttributes()
	{
		return extAttrs;
	} 

	
	
	public String showBrowseDialog(Object[] session, Component comp)
	{
		return null;
	} 

	
	
	public String getFileName(String path)
	{
		if(path.equals("/"))
			return path;

		if(path.endsWith("/") || path.endsWith(File.separator))
			path = path.substring(0,path.length() - 1);

		int index = Math.max(path.lastIndexOf('/'),
			path.lastIndexOf(File.separatorChar));
		if(index == -1)
			index = path.indexOf(':');

		
		if(index == -1 || index == path.length() - 1)
			return path;

		return path.substring(index + 1);
	} 

	
	
	public String getParentOfPath(String path)
	{
		
		
		int count = Math.max(0,path.length() - 2);
		int index = path.lastIndexOf(File.separatorChar,count);
		if(index == -1)
			index = path.lastIndexOf('/',count);
		if(index == -1)
		{
			
			
			index = path.lastIndexOf(':');
		}

		return path.substring(0,index + 1);
	} 

	
	
	public String constructPath(String parent, String path)
	{
		return parent + path;
	} 

	
	
	public char getFileSeparator()
	{
		return '/';
	} 

	
	
	public String getTwoStageSaveName(String path)
	{
		return MiscUtilities.constructPath(getParentOfPath(path),
			'#' + getFileName(path) + "#save#");
	} 

	
	
	public void reloadDirectory(String path) {} 

	
	
	public Object createVFSSession(String path, Component comp)
	{
		return new Object();
	} 

	
	
	public boolean load(View view, Buffer buffer, String path)
	{
		if((getCapabilities() & READ_CAP) == 0)
		{
			VFSManager.error(view,path,"vfs.not-supported.load",new String[] { name });
			return false;
		}

		Object session = createVFSSession(path,view);
		if(session == null)
			return false;

		if((getCapabilities() & WRITE_CAP) == 0)
			buffer.setReadOnly(true);

		BufferIORequest request = new BufferIORequest(
			BufferIORequest.LOAD,view,buffer,session,this,path);
		if(buffer.isTemporary())
			
			request.run();
		else
			VFSManager.runInWorkThread(request);

		return true;
	} 

	
	
	public boolean save(View view, Buffer buffer, String path)
	{
		if((getCapabilities() & WRITE_CAP) == 0)
		{
			VFSManager.error(view,path,"vfs.not-supported.save",new String[] { name });
			return false;
		}

		Object session = createVFSSession(path,view);
		if(session == null)
			return false;

		
		if(!path.equals(buffer.getPath()))
			buffer.unsetProperty(Buffer.BACKED_UP);

		VFSManager.runInWorkThread(new BufferIORequest(
			BufferIORequest.SAVE,view,buffer,session,this,path));
		return true;
	} 

	
	
	public boolean insert(View view, Buffer buffer, String path)
	{
		if((getCapabilities() & READ_CAP) == 0)
		{
			VFSManager.error(view,path,"vfs.not-supported.load",new String[] { name });
			return false;
		}

		Object session = createVFSSession(path,view);
		if(session == null)
			return false;

		VFSManager.runInWorkThread(new BufferIORequest(
			BufferIORequest.INSERT,view,buffer,session,this,path));
		return true;
	} 

	

	
	
	public String _canonPath(Object session, String path, Component comp)
		throws IOException
	{
		return path;
	} 

	
	
	public String[] _listDirectory(Object session, String directory,
		String glob, boolean recursive, Component comp)
		throws IOException
	{
		Log.log(Log.DEBUG,this,"Listing " + directory);
		ArrayList files = new ArrayList(100);

		RE filter;
		try
		{
			filter = new RE(MiscUtilities.globToRE(glob),
				RE.REG_ICASE);
		}
		catch(REException e)
		{
			Log.log(Log.ERROR,this,e);
			return null;
		}

		_listDirectory(session,new ArrayList(),files,directory,filter,
			recursive,comp);

		String[] retVal = (String[])files.toArray(new String[files.size()]);

		Arrays.sort(retVal,new MiscUtilities.StringICaseCompare());

		return retVal;
	} 

	
	
	public DirectoryEntry[] _listDirectory(Object session, String directory,
		Component comp)
		throws IOException
	{
		VFSManager.error(comp,directory,"vfs.not-supported.list",new String[] { name });
		return null;
	} 

	
	
	public DirectoryEntry _getDirectoryEntry(Object session, String path,
		Component comp)
		throws IOException
	{
		return null;
	} 

	
	
	public static class DirectoryEntry implements Serializable
	{
		
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
		

		
		
		public DirectoryEntry()
		{
		} 

		
		public DirectoryEntry(String name, String path, String deletePath,
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
				canRead = ((vfs.getCapabilities() & READ_CAP) != 0);
				canWrite = ((vfs.getCapabilities() & WRITE_CAP) != 0);
			}
		} 

		protected boolean colorCalculated;
		protected Color color;

		
		
		public String getExtendedAttribute(String name)
		{
			if(name.equals(EA_TYPE))
			{
				switch(type)
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
			else if(name.equals(EA_STATUS))
			{
				if(canRead)
				{
					if(canWrite)
						return jEdit.getProperty("vfs.browser.status.rw");
					else
						return jEdit.getProperty("vfs.browser.status.ro");
				}
				else
				{
					if(canWrite)
						return jEdit.getProperty("vfs.browser.status.append");
					else
						return jEdit.getProperty("vfs.browser.status.no");
				}
			}
			else if(name.equals(EA_SIZE))
			{
				if(type != FILE)
					return null;
				else
					return MiscUtilities.formatFileSize(length);
			}
			else
				return null;
		} 

		
		public Color getColor()
		{
			if(!colorCalculated)
			{
				colorCalculated = true;
				color = getDefaultColorFor(name);
			}

			return color;
		} 

		
		public String toString()
		{
			return name;
		} 
	} 

	
	
	public boolean _delete(Object session, String path, Component comp)
		throws IOException
	{
		return false;
	} 

	
	
	public boolean _rename(Object session, String from, String to,
		Component comp) throws IOException
	{
		return false;
	} 

	
	
	public boolean _mkdir(Object session, String directory, Component comp)
		throws IOException
	{
		return false;
	} 

	
	
	public void _backup(Object session, String path, Component comp)
		throws IOException
	{
	} 

	
	
	public InputStream _createInputStream(Object session,
		String path, boolean ignoreErrors, Component comp)
		throws IOException
	{
		VFSManager.error(comp,path,"vfs.not-supported.load",new String[] { name });
		return null;
	} 

	
	
	public OutputStream _createOutputStream(Object session,
		String path, Component comp)
		throws IOException
	{
		VFSManager.error(comp,path,"vfs.not-supported.save",new String[] { name });
		return null;
	} 

	
	
	public void _saveComplete(Object session, Buffer buffer, String path,
		Component comp) throws IOException {} 

	
	
	public void _endVFSSession(Object session, Component comp)
		throws IOException
	{
	} 

	
	
	public static Color getDefaultColorFor(String name)
	{
		synchronized(lock)
		{
			if(colors == null)
				loadColors();

			for(int i = 0; i < colors.size(); i++)
			{
				ColorEntry entry = (ColorEntry)colors.elementAt(i);
				if(entry.re.isMatch(name))
					return entry.color;
			}

			return null;
		}
	} 

	
	
	public static class DirectoryEntryCompare implements MiscUtilities.Compare
	{
		private boolean sortIgnoreCase, sortMixFilesAndDirs;

		
		public DirectoryEntryCompare(boolean sortMixFilesAndDirs,
			boolean sortIgnoreCase)
		{
			this.sortMixFilesAndDirs = sortMixFilesAndDirs;
			this.sortIgnoreCase = sortIgnoreCase;
		}

		public int compare(Object obj1, Object obj2)
		{
			VFS.DirectoryEntry file1 = (VFS.DirectoryEntry)obj1;
			VFS.DirectoryEntry file2 = (VFS.DirectoryEntry)obj2;

			if(!sortMixFilesAndDirs)
			{
				if(file1.type != file2.type)
					return file2.type - file1.type;
			}

			return MiscUtilities.compareStrings(file1.name,
				file2.name,sortIgnoreCase);
		}
	} 

	
	private String name;
	private int caps;
	private String[] extAttrs;
	private static Vector colors;
	private static Object lock = new Object();

	
	static
	{
		EditBus.addToBus(new EBComponent()
		{
			public void handleMessage(EBMessage msg)
			{
				if(msg instanceof PropertiesChanged)
				{
					synchronized(lock)
					{
						colors = null;
					}
				}
			}
		});
	} 

	
	private void _listDirectory(Object session, ArrayList stack,
		ArrayList files, String directory, RE glob, boolean recursive,
		Component comp) throws IOException
	{
		if(stack.contains(directory))
		{
			Log.log(Log.ERROR,this,
				"Recursion in _listDirectory(): "
				+ directory);
			return;
		}
		else
			stack.add(directory);

		VFS.DirectoryEntry[] _files = _listDirectory(session,directory,
			comp);
		if(_files == null || _files.length == 0)
			return;

		for(int i = 0; i < _files.length; i++)
		{
			VFS.DirectoryEntry file = _files[i];

			if(file.type == VFS.DirectoryEntry.DIRECTORY
				|| file.type == VFS.DirectoryEntry.FILESYSTEM)
			{
				if(recursive)
				{
					
					String canonPath = _canonPath(session,file.path,comp);
					if(!MiscUtilities.isURL(canonPath))
						canonPath = MiscUtilities.resolveSymlinks(canonPath);

					_listDirectory(session,stack,files,
						canonPath,glob,recursive,
						comp);
				}
			}
			else
			{
				if(!glob.isMatch(file.name))
					continue;

				Log.log(Log.DEBUG,this,file.path);

				files.add(file.path);
			}
		}
	} 

	
	private static void loadColors()
	{
		synchronized(lock)
		{
			colors = new Vector();

			if(!jEdit.getBooleanProperty("vfs.browser.colorize"))
				return;

			String glob;
			int i = 0;
			while((glob = jEdit.getProperty("vfs.browser.colors." + i + ".glob")) != null)
			{
				try
				{
					colors.addElement(new ColorEntry(
						new RE(MiscUtilities.globToRE(glob)),
						jEdit.getColorProperty(
						"vfs.browser.colors." + i + ".color",
						Color.black)));
				}
				catch(REException e)
				{
					Log.log(Log.ERROR,VFS.class,"Invalid regular expression: "
						+ glob);
					Log.log(Log.ERROR,VFS.class,e);
				}

				i++;
			}
		}
	} 

	
	static class ColorEntry
	{
		RE re;
		Color color;

		ColorEntry(RE re, Color color)
		{
			this.re = re;
			this.color = color;
		}
	} 

	
}
