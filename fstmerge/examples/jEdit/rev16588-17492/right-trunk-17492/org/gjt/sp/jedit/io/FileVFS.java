

package org.gjt.sp.jedit.io;


import javax.swing.filechooser.FileSystemView;
import javax.swing.*;
import java.awt.Component;
import java.io.*;
import java.text.*;
import java.util.Date;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class FileVFS extends VFS
{
	public static final String PERMISSIONS_PROPERTY = "FileVFS__perms";

	
	public FileVFS()
	{
		super("file",READ_CAP | WRITE_CAP | BROWSE_CAP | DELETE_CAP
			| RENAME_CAP | MKDIR_CAP | LOW_LATENCY_CAP
			| (OperatingSystem.isCaseInsensitiveFS()
			? CASE_INSENSITIVE_CAP : 0),
			new String[] { EA_TYPE, EA_SIZE, EA_STATUS,
			EA_MODIFIED });
	} 

	
	@Override
	public String getParentOfPath(String path)
	{
		if(OperatingSystem.isDOSDerived())
		{
			if(path.length() == 2 && path.charAt(1) == ':')
				return FileRootsVFS.PROTOCOL + ':';
			else if(path.length() == 3 && path.endsWith(":\\"))
				return FileRootsVFS.PROTOCOL + ':';
			else if(path.startsWith("\\\\") && path.indexOf('\\',2) == -1)
				return path;
		}

		return super.getParentOfPath(path);
	} 

	
	@Override
	public String constructPath(String parent, String path)
	{
		if(parent.endsWith(File.separator)
			|| parent.endsWith("/"))
			return parent + path;
		else
			return parent + File.separator + path;
	} 

	
	@Override
	public char getFileSeparator()
	{
		return File.separatorChar;
	} 

	
	
	@Override
	public String getTwoStageSaveName(String path)
	{
		File parent = new File(getParentOfPath(path));
		
		
		
		
		
		
		
		
		return (parent.canWrite() || OperatingSystem.isWindows())
			? super.getTwoStageSaveName(path)
			: null;
	} 

	
	@Override
	public boolean save(View view, Buffer buffer, String path)
	{
		if(OperatingSystem.isUnix())
		{
			int permissions = getPermissions(buffer.getPath());
			Log.log(Log.DEBUG,this,buffer.getPath() + " has permissions 0"
				+ Integer.toString(permissions,8));
			buffer.setIntegerProperty(PERMISSIONS_PROPERTY,permissions);
		}

		return super.save(view,buffer,path);
	} 

	
	@Override
	public boolean insert(View view, Buffer buffer, String path)
	{
		File file = new File(path);

		
		if(!file.exists())
			return false;

		if(file.isDirectory())
		{
			VFSManager.error(view,file.getPath(),
				"ioerror.open-directory",null);
			return false;
		}

		if(!file.canRead())
		{
			VFSManager.error(view,file.getPath(),
				"ioerror.no-read",null);
			return false;
		} 

		return super.insert(view,buffer,path);
	} 

	
	
	public static boolean recursiveDelete(File path)
	{
		if (path.exists())
		{
			File[] files = path.listFiles();
			for (int i = 0; i < files.length; i++)
			{
				if (files[i].isDirectory())
				{
					recursiveDelete(files[i]);
				}
				else
				{
					files[i].delete();
				}
			}
		}
		return path.delete();
	} 

	
	
	@Override
	public String _canonPath(Object session, String path, Component comp)
		throws IOException
	{
		return MiscUtilities.canonPath(path);
	} 

	
	public static class LocalFile extends VFSFile
	{
		private File file;

		
		public static DateFormat DATE_FORMAT
			= DateFormat.getInstance();

		
		@Deprecated
		public long modified;

		
		public LocalFile(File file)
		{
			this.file = file;

			
			setName(file.getName());
			String path = file.getPath();
			setPath(path);
			setDeletePath(path);
			setHidden(file.isHidden());
			setType(file.isDirectory()
				? VFSFile.DIRECTORY
				: VFSFile.FILE);
		} 

		
		@Override
		public String getExtendedAttribute(String name)
		{
			fetchAttrs();
			if (name.equals(EA_MODIFIED))
			{
				return DATE_FORMAT.format(new Date(modified));
			}
			else
			{
				return super.getExtendedAttribute(name);
			}
		} 

		
		
		@Override
		protected void fetchAttrs()
		{
			if(fetchedAttrs())
				return;

			super.fetchAttrs();

			setSymlinkPath(MiscUtilities.resolveSymlinks(
				file.getPath()));
			setReadable(file.canRead());
			setWriteable(file.canWrite());
			setLength(file.length());
			setModified(file.lastModified());
		} 

		
		
		@Override
		public Icon getIcon(boolean expanded, boolean openBuffer)
		{
			if (icon == null)
			{
				if (fsView == null)
					fsView = FileSystemView.getFileSystemView();

				icon = fsView.getSystemIcon(file);
			}
			return icon;  
		} 

		
		@Override
		public String getSymlinkPath()
		{
			fetchAttrs();
			return super.getSymlinkPath();
		} 

		
		@Override
		public long getLength()
		{
			fetchAttrs();
			return super.getLength();
		} 

		
		@Override
		public boolean isReadable()
		{
			fetchAttrs();
			return super.isReadable();
		} 

		
		@Override
		public boolean isWriteable()
		{
			fetchAttrs();
			return super.isWriteable();
		} 

		
		public long getModified()
		{
			fetchAttrs();
			return modified;
		} 

		
		public void setModified(long modified)
		{
			this.modified = modified;
		} 

		private transient FileSystemView fsView;
		private transient Icon icon;
	} 

	
	@Override
	public VFSFile[] _listFiles(Object session, String path,
		Component comp)
	{
		
		
		if(OperatingSystem.isWindows())
		{
			if(path.length() == 2 && path.charAt(1) == ':')
				path = path.concat(File.separator);
		} 

		File directory = new File(path);
		File[] list = null;
		if(directory.exists())
			list = fsView.getFiles(directory,false);

		if(list == null)
		{
			VFSManager.error(comp,path,"ioerror.directory-error-nomsg",null);
			return null;
		}

		VFSFile[] list2 = new VFSFile[list.length];
		for(int i = 0; i < list.length; i++)
			list2[i] = new LocalFile(list[i]);

		return list2;
	} 

	
	@Override
	public VFSFile _getFile(Object session, String path,
		Component comp)
	{
		if(path.equals("/") && OperatingSystem.isUnix())
		{
			return new VFS.DirectoryEntry(path,path,path,
				VFSFile.DIRECTORY,0L,false);
		}

		File file = new File(path);
		if(!file.exists())
			return null;

		return new LocalFile(file);
	} 

	
	@Override
	public boolean _delete(Object session, String path, Component comp)
	{
		File file = new File(path);
		
		
		String canonPath;
		try
		{
			canonPath = file.getCanonicalPath();
		}
		catch(IOException io)
		{
			canonPath = path;
		}
		
		boolean retVal;
		if (!file.isDirectory())
		{
			retVal = file.delete();
		} 
		else 
		{
			retVal = recursiveDelete(file);
		}
		if(retVal)
			VFSManager.sendVFSUpdate(this,canonPath,true);
		return retVal;
	} 

	
	@Override
	public boolean _rename(Object session, String from, String to,
		Component comp)
	{
		File _to = new File(to);

		String toCanonPath;
		try
		{
			toCanonPath = _to.getCanonicalPath();
		}
		catch(IOException io)
		{
			toCanonPath = to;
		}

		
		
		File parent = new File(_to.getParent());
		if(parent.exists())
		{
			if(!parent.isDirectory())
				return false;
		}
		else
		{
			parent.mkdirs();
			if(!parent.exists())
				return false;
		}

		File _from = new File(from);

		String fromCanonPath;
		try
		{
			fromCanonPath = _from.getCanonicalPath();
		}
		catch(IOException io)
		{
			fromCanonPath = from;
		}

		
		if(!fromCanonPath.equalsIgnoreCase(toCanonPath))
			_to.delete();

		boolean retVal = _from.renameTo(_to);
		VFSManager.sendVFSUpdate(this,fromCanonPath,true);
		VFSManager.sendVFSUpdate(this,toCanonPath,true);
		return retVal;
	} 

	
	@Override
	public boolean _mkdir(Object session, String directory, Component comp)
	{
		String parent = getParentOfPath(directory);
		if(!new File(parent).exists())
		{
			if(!_mkdir(session,parent,comp))
				return false;
		}

		File file = new File(directory);

		boolean retVal = file.mkdir();
		String canonPath;
		try
		{
			canonPath = file.getCanonicalPath();
		}
		catch(IOException io)
		{
			canonPath = directory;
		}
		VFSManager.sendVFSUpdate(this,canonPath,true);
		return retVal;
	} 

	
	@Override
	public void _backup(Object session, String path, Component comp)
		throws IOException
	{
		

		String backupPrefix = jEdit.getProperty("backup.prefix");
		String backupSuffix = jEdit.getProperty("backup.suffix");

		String backupDirectory = jEdit.getProperty("backup.directory");

		int backupTimeDistance = jEdit.getIntegerProperty("backup.minTime",0);
		File file = new File(path);

		if (!file.exists())
			return;

		
		
		if(backupDirectory == null || backupDirectory.length() == 0)
			backupDirectory = file.getParent();
		else
		{
			backupDirectory = MiscUtilities.constructPath(
				System.getProperty("user.home"), backupDirectory);

			
			
			backupDirectory = MiscUtilities.concatPath(
				backupDirectory,file.getParent());

			File dir = new File(backupDirectory);

			if (!dir.exists())
				dir.mkdirs();
		}

		MiscUtilities.saveBackup(file, jEdit.getIntegerProperty("backups",1), backupPrefix,
			backupSuffix,backupDirectory,backupTimeDistance);
	} 

	
	@Override
	public InputStream _createInputStream(Object session, String path,
		boolean ignoreErrors, Component comp) throws IOException
	{
		try
		{
			return new FileInputStream(path);
		}
		catch(IOException io)
		{
			if(ignoreErrors)
				return null;
			else
				throw io;
		}
	} 

	
	@Override
	public OutputStream _createOutputStream(Object session, String path,
		Component comp) throws IOException
	{
		return new FileOutputStream(path);
	} 

	
	@Override
	public void _saveComplete(Object session, Buffer buffer, String path,
		Component comp)
	{
		int permissions = buffer.getIntegerProperty(PERMISSIONS_PROPERTY,0);
		setPermissions(path,permissions);
	} 

	

	
	

	
	
	public static int getPermissions(String path)
	{
		int permissions = 0;

		if(jEdit.getBooleanProperty("chmodDisabled"))
			return permissions;

		if(OperatingSystem.isUnix())
		{
			String[] cmdarray = { "ls", "-ld", path };

			InputStreamReader isr = null;
			BufferedReader reader = null;
			try
			{
				Process process = Runtime.getRuntime().exec(cmdarray);
				isr = new InputStreamReader(process.getInputStream());
				reader = new BufferedReader(isr);

				String output = reader.readLine();

				if(output != null)
				{
					String s = output.substring(1, 10);

					permissions = MiscUtilities
						.parsePermissions(s);
				}
			}

			
			
			
			catch (Throwable t)
			{
			}
			finally
			{
				try
				{
					if (reader != null)
						reader.close();
					else if (isr != null)
						isr.close();
				}
				catch (IOException e)
				{
				}
			}
		}

		return permissions;
	} 

	
	
	public static void setPermissions(String path, int permissions)
	{
		if(jEdit.getBooleanProperty("chmodDisabled"))
			return;

		if(permissions != 0)
		{
			if(OperatingSystem.isUnix())
			{
				String[] cmdarray = { "chmod", Integer.toString(permissions, 8), path };

				try
				{
					Process process = Runtime.getRuntime().exec(cmdarray);
					process.getInputStream().close();
					process.getOutputStream().close();
					process.getErrorStream().close();
					
					
					
					
				}

				
				
				
				catch (Throwable t)
				{
				}
			}
		}
	} 

	

	
	private static final FileSystemView fsView = FileSystemView.getFileSystemView();
	
}
