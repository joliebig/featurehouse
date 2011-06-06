

package org.gjt.sp.jedit.browser;


import java.io.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;



class BrowserIORequest extends WorkRequest
{
	
	
	public static final int LIST_DIRECTORY = 0;

	
	public static final int DELETE = 1;

	
	public static final int RENAME = 2;

	
	public static final int MKDIR = 3;
	

	
	
	BrowserIORequest(int type, VFSBrowser browser,
		Object session, VFS vfs, String path1, String path2,
		Object[] loadInfo)
	{
		this.type = type;
		this.browser = browser;
		this.session = session;
		this.vfs = vfs;
		this.path1 = path1;
		this.path2 = path2;
		this.loadInfo = loadInfo;
	} 

	
	public void run()
	{
		switch(type)
		{
		case LIST_DIRECTORY:
			listDirectory();
			break;
		case DELETE:
			delete();
			break;
		case RENAME:
			rename();
			break;
		case MKDIR:
			mkdir();
			break;
		}
	} 

	
	public String toString()
	{
		String typeString;
		switch(type)
		{
		case LIST_DIRECTORY:
			typeString = "LIST_DIRECTORY";
			break;
		case DELETE:
			typeString = "DELETE";
			break;
		case RENAME:
			typeString = "RENAME";
			break;
		case MKDIR:
			typeString = "MKDIR";
			break;
		default:
			typeString = "UNKNOWN!!!";
			break;
		}

		return getClass().getName() + "[type=" + typeString
			+ ",vfs=" + vfs + ",path1=" + path1
			+ ",path2=" + path2 + "]";
	} 

	

	
	private int type;
	private VFSBrowser browser;
	private Object session;
	private VFS vfs;
	private String path1;
	private String path2;
	private Object[] loadInfo;
	

	
	private void listDirectory()
	{
		VFSFile[] directory = null;

		String[] args = { path1 };
		setStatus(jEdit.getProperty("vfs.status.listing-directory",args));

		String canonPath = path1;

		try
		{
			setAbortable(true);

			canonPath = vfs._canonPath(session,path1,browser);
			directory = vfs._listFiles(session,canonPath,browser);
		}
		catch(IOException io)
		{
			setAbortable(false);
			Log.log(Log.ERROR,this,io);
			String[] pp = { io.toString() };
			VFSManager.error(browser,path1,"ioerror.directory-error",pp);
		}
		catch(WorkThread.Abort a)
		{
		}
		finally
		{
			try
			{
				vfs._endVFSSession(session,browser);
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(browser,path1,"ioerror.directory-error",pp);
			}
		}

		setAbortable(false);

		loadInfo[0] = canonPath;
		loadInfo[1] = directory;
	} 

	
	private void delete()
	{
		try
		{
			setAbortable(true);
			String[] args = { path1 };
			setStatus(jEdit.getProperty("vfs.status.deleting",args));

			try
			{
				path1 = vfs._canonPath(session,path1,browser);


				if(!vfs._delete(session,path1,browser))
					VFSManager.error(browser,path1,"ioerror.delete-error",null);
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(browser,path1,"ioerror.directory-error",pp);
			}
		}
		catch(WorkThread.Abort a)
		{
		}
		finally
		{
			try
			{
				vfs._endVFSSession(session,browser);
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(browser,path1,"ioerror.directory-error",pp);
			}
		}
	} 

	
	private void rename()
	{
		try
		{
			setAbortable(true);
			String[] args = { path1, path2 };
			setStatus(jEdit.getProperty("vfs.status.renaming",args));

			try
			{
				path1 = vfs._canonPath(session,path1,browser);
				path2 = vfs._canonPath(session,path2,browser);

				VFSFile file = vfs._getFile(session,path2,browser);
				if(file != null)
				{
					if((OperatingSystem.isCaseInsensitiveFS())
						&& path1.equalsIgnoreCase(path2))
					{
						
						
					}
					else
					{
						VFSManager.error(browser,path1,
							"ioerror.rename-exists",
							new String[] { path2 });
						return;
					}
				}

				if(!vfs._rename(session,path1,path2,browser))
					VFSManager.error(browser,path1,"ioerror.rename-error",
						new String[] { path2 });
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(browser,path1,"ioerror.directory-error",pp);
			}
		}
		catch(WorkThread.Abort a)
		{
		}
		finally
		{
			try
			{
				vfs._endVFSSession(session,browser);
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(browser,path1,"ioerror.directory-error",pp);
			}
		}
	} 

	
	private void mkdir()
	{
		try
		{
			setAbortable(true);
			String[] args = { path1 };
			setStatus(jEdit.getProperty("vfs.status.mkdir",args));

			try
			{
				path1 = vfs._canonPath(session,path1,browser);

				if(!vfs._mkdir(session,path1,browser))
					VFSManager.error(browser,path1,"ioerror.mkdir-error",null);
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				args[0] = io.toString();
				VFSManager.error(browser,path1,"ioerror",args);
			}
		}
		catch(WorkThread.Abort a)
		{
		}
		finally
		{
			try
			{
				vfs._endVFSSession(session,browser);
			}
			catch(IOException io)
			{
				setAbortable(false);
				Log.log(Log.ERROR,this,io);
				String[] args = { io.toString() };
				VFSManager.error(browser,path1,"ioerror",args);
			}
		}
	} 

	
}
