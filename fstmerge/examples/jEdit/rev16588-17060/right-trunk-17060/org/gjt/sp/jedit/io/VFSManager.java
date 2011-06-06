

package org.gjt.sp.jedit.io;


import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Frame;
import java.io.IOException;
import java.util.*;

import org.gjt.sp.jedit.gui.ErrorListDialog;
import org.gjt.sp.jedit.msg.VFSUpdate;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.WorkThreadPool;
import org.gjt.sp.util.StandardUtilities;



public class VFSManager
{
	
	public static final String SERVICE = "org.gjt.sp.jedit.io.VFS";

	
	
	public static void init()
	{
		int count = jEdit.getIntegerProperty("ioThreadCount",4);
		ioThreadPool = new WorkThreadPool("jEdit I/O",count);
		JARClassLoader classLoader = new JARClassLoader();
		for(int i = 0; i < ioThreadPool.getThreadCount(); i++)
		{
			ioThreadPool.getThread(i).setContextClassLoader(
				classLoader);
		}
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
			return vfsHash.get(name);
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
				vfs = protocolHash.get(protocol);

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

	
	
	public static Enumeration<VFS> getFilesystems()
	{
		return vfsHash.elements();
	} 

	
	
	public static String[] getVFSs()
	{
		
		
		List<String> returnValue = new LinkedList<String>();
		String[] newAPI = ServiceManager.getServiceNames(SERVICE);
		if(newAPI != null)
		{
			for(int i = 0; i < newAPI.length; i++)
			{
				returnValue.add(newAPI[i]);
			}
		}
		Enumeration<String> oldAPI = vfsHash.keys();
		while(oldAPI.hasMoreElements())
			returnValue.add(oldAPI.nextElement());
		return returnValue.toArray(new String[returnValue.size()]);
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

	

	
	
	public static void error(IOException e, String path, Component comp)
	{
		Log.log(Log.ERROR,VFSManager.class,e);
		VFSManager.error(comp,path,"ioerror",new String[] { e.toString() });
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

			errors.add(new ErrorListDialog.ErrorEntry(
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
							Integer.valueOf(errors.size())});
						new ErrorListDialog(
							frame.isShowing()
							? frame
							: jEdit.getFirstView(),
							jEdit.getProperty("ioerror.title"),
							caption,errors,false);
						errors.clear();
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
					VFSUpdate msg = vfsUpdates.get(i);
					if(msg.getPath().equals(path))
					{
						
						
						return;
					}
				}

				vfsUpdates.add(new VFSUpdate(path));

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
				
				
				
				
				
				Collections.sort(vfsUpdates,
					new StandardUtilities.StringCompare<VFSUpdate>()
				);
				for(int i = 0; i < vfsUpdates.size(); i++)
				{
					EditBus.send(vfsUpdates.get(i));
				}

				vfsUpdates.clear();
			}
		}
	} 

	

	
	private static WorkThreadPool ioThreadPool;
	private static VFS fileVFS;
	private static VFS urlVFS;
	private static final Hashtable<String, VFS> vfsHash;
	private static final Map<String, VFS> protocolHash;
	private static boolean error;
	private static final Object errorLock = new Object();
	private static final Vector<ErrorListDialog.ErrorEntry> errors;
	private static final Object vfsUpdateLock = new Object();
	private static final List<VFSUpdate> vfsUpdates;
	

	
	static
	{
		errors = new Vector<ErrorListDialog.ErrorEntry>();
		fileVFS = new FileVFS();
		urlVFS = new UrlVFS();
		vfsHash = new Hashtable<String, VFS>();
		protocolHash = new Hashtable<String, VFS>();
		vfsUpdates = new ArrayList<VFSUpdate>(10);
	} 

	private VFSManager() {}
	
}
