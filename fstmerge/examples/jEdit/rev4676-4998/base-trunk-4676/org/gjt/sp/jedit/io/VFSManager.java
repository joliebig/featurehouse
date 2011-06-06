

package org.gjt.sp.jedit.io;


import java.util.Enumeration;
import java.util.Hashtable;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import java.awt.*;
import java.util.Vector;
import org.gjt.sp.jedit.gui.ErrorListDialog;
import org.gjt.sp.jedit.msg.VFSUpdate;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.WorkThreadPool;



public class VFSManager
{
	
	public static final String SERVICE = "org.gjt.sp.jedit.io.VFS";

	
	
	public static void init()
	{
		int count = jEdit.getIntegerProperty("ioThreadCount",4);
		ioThreadPool = new WorkThreadPool("jEdit I/O",count);
	} 

	
	
	public static void start()
	{
		ioThreadPool.start();
	} 

	

	
	
	public static VFS getFileVFS()
	{
		return fileVFS;
	} 

	
	
	public static VFS getUrlVFS()
	{
		return urlVFS;
	} 

	
	
	public static VFS getVFSByName(String name)
	{
		
		VFS vfs = (VFS)ServiceManager.getService(SERVICE,name);
		if(vfs == null)
			return (VFS)vfsHash.get(name);
		else
			return vfs;
	} 

	
	
	public static VFS getVFSForProtocol(String protocol)
	{
		if(protocol.equals("file"))
			return fileVFS;
		else
		{
			VFS vfs = (VFS)ServiceManager.getService(SERVICE,protocol);
			if(vfs == null)
				vfs = (VFS)protocolHash.get(protocol);

			if(vfs != null)
				return vfs;
			else
				return urlVFS;
		}
	} 

	
	
	public static VFS getVFSForPath(String path)
	{
		if(MiscUtilities.isURL(path))
			return getVFSForProtocol(MiscUtilities.getProtocolOfURL(path));
		else
			return fileVFS;
	} 

	
	
	public static void registerVFS(String protocol, VFS vfs)
	{
		Log.log(Log.DEBUG,VFSManager.class,"Registered "
			+ vfs.getName() + " filesystem for "
			+ protocol + " protocol");
		vfsHash.put(vfs.getName(),vfs);
		protocolHash.put(protocol,vfs);
	} 

	
	
	public static Enumeration getFilesystems()
	{
		return vfsHash.elements();
	} 

	
	
	public static String[] getVFSs()
	{
		
		
		Vector returnValue = new Vector();
		String[] newAPI = ServiceManager.getServiceNames(SERVICE);
		if(newAPI != null)
		{
			for(int i = 0; i < newAPI.length; i++)
			{
				returnValue.add(newAPI[i]);
			}
		}
		Enumeration oldAPI = vfsHash.keys();
		while(oldAPI.hasMoreElements())
			returnValue.add(oldAPI.nextElement());
		return (String[])returnValue.toArray(new String[
			returnValue.size()]);
	} 

	

	

	
	
	public static WorkThreadPool getIOThreadPool()
	{
		return ioThreadPool;
	} 

	
	
	public static void waitForRequests()
	{
		ioThreadPool.waitForRequests();
	} 

	
	
	public static boolean errorOccurred()
	{
		return error;
	} 

	
	
	public static int getRequestCount()
	{
		return ioThreadPool.getRequestCount();
	} 

	
	
	public static void runInAWTThread(Runnable run)
	{
		ioThreadPool.addWorkRequest(run,true);
	} 

	
	
	public static void runInWorkThread(Runnable run)
	{
		ioThreadPool.addWorkRequest(run,false);
	} 

	

	
	
	public static void error(final Component comp, final String error, final Object[] args)
	{
		
		if(SwingUtilities.isEventDispatchThread())
		{
			GUIUtilities.error(comp,error,args);
			return;
		}

		
		
		
		
		
		
		VFSManager.error = true;

		runInAWTThread(new Runnable()
		{
			public void run()
			{
				VFSManager.error = false;

				if(comp == null || !comp.isShowing())
					GUIUtilities.error(null,error,args);
				else
					GUIUtilities.error(comp,error,args);
			}
		});
	} 

	
	
	public static void error(Component comp,
		final String path,
		String messageProp,
		Object[] args)
	{
		final Frame frame = JOptionPane.getFrameForComponent(comp);

		synchronized(errorLock)
		{
			error = true;

			errors.addElement(new ErrorListDialog.ErrorEntry(
				path,messageProp,args));

			if(errors.size() == 1)
			{
				

				VFSManager.runInAWTThread(new Runnable()
				{
					public void run()
					{
						String caption = jEdit.getProperty(
							"ioerror.caption" + (errors.size() == 1
							? "-1" : ""),new Integer[] {
							new Integer(errors.size()) });
						new ErrorListDialog(
							frame.isShowing()
							? frame
							: jEdit.getFirstView(),
							jEdit.getProperty("ioerror.title"),
							caption,errors,false);
						errors.removeAllElements();
						error = false;
					}
				});
			}
		}
	} 

	
	
	public static void sendVFSUpdate(VFS vfs, String path, boolean parent)
	{
		if(parent)
		{
			sendVFSUpdate(vfs,vfs.getParentOfPath(path),false);
			sendVFSUpdate(vfs,path,false);
		}
		else
		{
			
			if(path.length() != 1 && (path.endsWith("/")
				|| path.endsWith(java.io.File.separator)))
				path = path.substring(0,path.length() - 1);

			synchronized(vfsUpdateLock)
			{
				for(int i = 0; i < vfsUpdates.size(); i++)
				{
					VFSUpdate msg = (VFSUpdate)vfsUpdates
						.elementAt(i);
					if(msg.getPath().equals(path))
					{
						
						
						return;
					}
				}

				vfsUpdates.addElement(new VFSUpdate(path));

				if(vfsUpdates.size() == 1)
				{
					
					
					
					VFSManager.runInAWTThread(new SendVFSUpdatesSafely());
				}
			}
		}
	} 

	
	static class SendVFSUpdatesSafely implements Runnable
	{
		public void run()
		{
			synchronized(vfsUpdateLock)
			{
				for(int i = 0; i < vfsUpdates.size(); i++)
				{
					EditBus.send((VFSUpdate)vfsUpdates.elementAt(i));
				}

				vfsUpdates.removeAllElements();
			}
		}
	} 

	

	
	private static WorkThreadPool ioThreadPool;
	private static VFS fileVFS;
	private static VFS urlVFS;
	private static Hashtable vfsHash;
	private static Hashtable protocolHash;
	private static boolean error;
	private static Object errorLock;
	private static Vector errors;
	private static Object vfsUpdateLock;
	private static Vector vfsUpdates;
	

	
	static
	{
		errorLock = new Object();
		errors = new Vector();
		fileVFS = new FileVFS();
		urlVFS = new UrlVFS();
		vfsHash = new Hashtable();
		protocolHash = new Hashtable();
		vfsUpdateLock = new Object();
		vfsUpdates = new Vector();
	} 

	private VFSManager() {}
	
}
