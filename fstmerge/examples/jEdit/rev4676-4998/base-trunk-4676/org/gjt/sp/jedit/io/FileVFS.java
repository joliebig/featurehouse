

package org.gjt.sp.jedit.io;


import java.awt.Component;
import java.io.*;
import java.util.Date;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class FileVFS extends VFS
{
	public static final String PERMISSIONS_PROPERTY = "FileVFS__perms";

	
	public FileVFS()
	{
		super("file",READ_CAP | WRITE_CAP | DELETE_CAP
			| RENAME_CAP | MKDIR_CAP | LOW_LATENCY_CAP);
	} 

	
	public String getParentOfPath(String path)
	{
		if(OperatingSystem.isDOSDerived())
		{
			if(path.length() == 2 && path.charAt(1) == ':')
				return FileRootsVFS.PROTOCOL + ":";
			else if(path.length() == 3 && path.endsWith(":\\"))
				return FileRootsVFS.PROTOCOL + ":";
			else if(path.startsWith("\\\\") && path.indexOf('\\',2) == -1)
				return path;
		}

		return super.getParentOfPath(path);
	} 

	
	public String constructPath(String parent, String path)
	{
		if(parent.endsWith(File.separator))
			return parent + path;
		else
			return parent + File.separator + path;
	} 

	
	public char getFileSeparator()
	{
		return File.separatorChar;
	} 

	
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

	
	
	public String _canonPath(Object session, String path, Component comp)
		throws IOException
	{
		return MiscUtilities.resolveSymlinks(
			MiscUtilities.canonPath(path));
	} 

	
	public static class LocalDirectoryEntry extends VFS.DirectoryEntry
	{
		public long modified;

		public LocalDirectoryEntry(File file)
		{
			super(file.getName(),file.getPath(),
				file.getPath(),file.isDirectory() ? DIRECTORY : FILE,file.length(),file.isHidden());
			this.modified = file.lastModified();
			this.canRead = file.canRead();
			this.canWrite = file.canWrite();
		}

		public String getExtendedAttribute(String name)
		{
			if(name.equals(EA_MODIFIED))
				return new Date(modified).toString();
			else
				return super.getExtendedAttribute(name);
		}
	} 

	
	public VFS.DirectoryEntry[] _listDirectory(Object session, String path,
		Component comp)
	{
		
		
		if(OperatingSystem.isWindows())
		{
			if(path.length() == 2 && path.charAt(1) == ':')
				path = path.concat(File.separator);
		} 

		File directory = new File(path);
		File[] list = directory.listFiles();
		if(list == null)
		{
			VFSManager.error(comp,path,"ioerror.directory-error-nomsg",null);
			return null;
		}

		VFS.DirectoryEntry[] list2 = new VFS.DirectoryEntry[list.length];
		for(int i = 0; i < list.length; i++)
			list2[i] = new LocalDirectoryEntry(list[i]);

		return list2;
	} 

	
	public DirectoryEntry _getDirectoryEntry(Object session, String path,
		Component comp)
	{
		if(path.equals("/") && OperatingSystem.isUnix())
		{
			return new VFS.DirectoryEntry(path,path,path,
				VFS.DirectoryEntry.DIRECTORY,0L,false);
		}

		File file = new File(path);
		if(!file.exists())
			return null;

		return new LocalDirectoryEntry(file);
	} 

	
	public boolean _delete(Object session, String path, Component comp)
	{
		boolean retVal = new File(path).delete();
		if(retVal)
			VFSManager.sendVFSUpdate(this,path,true);
		return retVal;
	} 

	
	public boolean _rename(Object session, String from, String to,
		Component comp)
	{
		File _to = new File(to);

		
		if(!from.equalsIgnoreCase(to))
			_to.delete();

		boolean retVal = new File(from).renameTo(_to);
		VFSManager.sendVFSUpdate(this,from,true);
		VFSManager.sendVFSUpdate(this,to,true);
		return retVal;
	} 

	
	public boolean _mkdir(Object session, String directory, Component comp)
	{
		String parent = getParentOfPath(directory);
		if(!new File(parent).exists())
		{
			if(!_mkdir(session,parent,comp))
				return false;
		}

		boolean retVal = new File(directory).mkdir();
		VFSManager.sendVFSUpdate(this,directory,true);
		return retVal;
	} 

	
	public void _backup(Object session, String path, Component comp)
		throws IOException
	{
		
		int backups = jEdit.getIntegerProperty("backups",1);

		if(backups == 0)
			return;

		String backupPrefix = jEdit.getProperty("backup.prefix");
		String backupSuffix = jEdit.getProperty("backup.suffix");

		String backupDirectory = MiscUtilities.canonPath(
			jEdit.getProperty("backup.directory"));

		File file = new File(path);

		
		
		if(backupDirectory == null || backupDirectory.length() == 0)
			backupDirectory = file.getParent();
		else
		{
			backupDirectory = MiscUtilities.constructPath(
				System.getProperty("user.home"),backupDirectory);

			
			
			backupDirectory = MiscUtilities.concatPath(
				backupDirectory,file.getParent());

			File dir = new File(backupDirectory);

			if (!dir.exists())
				dir.mkdirs();
		}

		MiscUtilities.saveBackup(file,backups,backupPrefix,
			backupSuffix,backupDirectory);
	} 

	
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

	
	public OutputStream _createOutputStream(Object session, String path,
		Component comp) throws IOException
	{
		OutputStream retVal = new FileOutputStream(path);

		
		
		
		return retVal;
	} 

	
	public void _saveComplete(Object session, Buffer buffer, String path,
		Component comp)
	{
		int permissions = buffer.getIntegerProperty(PERMISSIONS_PROPERTY,0);
		setPermissions(path,permissions);
	} 

	

	
	

	
	
	public static int getPermissions(String path)
	{
		int permissions = 0;

		if(OperatingSystem.isUnix())
		{
			String[] cmdarray = { "ls", "-ld", path };

			try
			{
				Process process = Runtime.getRuntime().exec(cmdarray);

				BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));

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
		}

		return permissions;
	} 

	
	
	public static void setPermissions(String path, int permissions)
	{
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
					int exitCode = process.waitFor();
					if(exitCode != 0)
						Log.log(Log.NOTICE,FileVFS.class,"chmod exited with code " + exitCode);
				}

				
				
				
				catch (Throwable t)
				{
				}
			}
		}
	} 

	
}
