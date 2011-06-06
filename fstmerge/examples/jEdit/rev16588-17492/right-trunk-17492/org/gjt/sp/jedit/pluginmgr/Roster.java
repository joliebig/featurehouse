

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.SwingUtilities;
import java.awt.Component;
import java.io.*;
import java.net.*;
import java.util.zip.*;
import java.util.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.IOUtilities;

import static org.gjt.sp.jedit.io.FileVFS.recursiveDelete;



class Roster
{
	
	Roster()
	{
		operations = new ArrayList<Operation>();
		toLoad = new ArrayList<String>();
	} 

	
	
	void addRemove(String jar)
	{
		addOperation(new Remove(jar));
	} 

	
	void addInstall(String installed, String url, String installDirectory,
		int size)
	{
		addOperation(new Install(installed,url,installDirectory,size));
	} 

	
	public Operation getOperation(int i)
	{
		return operations.get(i);
	} 

	
	int getOperationCount()
	{
		return operations.size();
	} 

	
	boolean isEmpty()
	{
		return operations.isEmpty();
	} 

	
	void performOperationsInWorkThread(PluginManagerProgress progress)
	{
		for(int i = 0; i < operations.size(); i++)
		{
			Operation op = operations.get(i);
			op.runInWorkThread(progress);
			progress.done();

			if(Thread.interrupted())
				return;
		}
	} 

	
	void performOperationsInAWTThread(Component comp)
	{
		for(int i = 0; i < operations.size(); i++)
		{
			Operation op = operations.get(i);
			op.runInAWTThread(comp);
		}

		
		
		for(int i = 0; i < toLoad.size(); i++)
		{
			String pluginName = toLoad.get(i);
			if(jEdit.getPluginJAR(pluginName) != null)
			{
				Log.log(Log.WARNING,this,"Already loaded: "
					+ pluginName);
			}
			else
				jEdit.addPluginJAR(pluginName);
		}

		for(int i = 0; i < toLoad.size(); i++)
		{
			String pluginName = toLoad.get(i);
			PluginJAR plugin = jEdit.getPluginJAR(pluginName);
			if(plugin != null)
				plugin.checkDependencies();
		}

		
		for(int i = 0; i < toLoad.size(); i++)
		{
			String pluginName = toLoad.get(i);
			PluginJAR plugin = jEdit.getPluginJAR(pluginName);
			if(plugin != null)
				plugin.activatePluginIfNecessary();
		}
	} 

	
	private static File downloadDir;

	private List<Operation> operations;
	private List<String> toLoad;

	
	private void addOperation(Operation op)
	{
		for(int i = 0; i < operations.size(); i++)
		{
			if(operations.get(i).equals(op))
				return;
		}

		operations.add(op);
	} 

	
	private static String getDownloadDir()
	{
		if(downloadDir == null)
		{
			String settings = jEdit.getSettingsDirectory();
			if(settings == null)
				settings = System.getProperty("user.home");
			downloadDir = new File(MiscUtilities.constructPath(
				settings,"PluginManager.download"));
			downloadDir.mkdirs();
		}

		return downloadDir.getPath();
	} 

	

	
	abstract static class Operation
	{
		public void runInWorkThread(PluginManagerProgress progress)
		{
		}

		public void runInAWTThread(Component comp)
		{
		}

		public int getMaximum()
		{
			return 0;
		}
	} 

	
	class Remove extends Operation
	{
		
		Remove(String jar)
		{
			this.jar = jar;
		} 

		
		public void runInAWTThread(Component comp)
		{
			
			PluginJAR jar = jEdit.getPluginJAR(this.jar);
			if(jar != null)
			{
				unloadPluginJAR(jar);
			}

			toLoad.remove(this.jar);

			

			
			File jarFile = new File(this.jar);
			File srcFile = new File(this.jar.substring(0, this.jar.length() - 4));

			Log.log(Log.NOTICE,this,"Deleting " + jarFile);

			boolean ok = jarFile.delete();

			if(srcFile.exists())
			{
				ok &= recursiveDelete(srcFile);
			}

			if(!ok)
			{
				String[] args = {this.jar};
				GUIUtilities.error(comp,"plugin-manager.remove-failed",args);
			}
		} 

		
		
		private void unloadPluginJAR(PluginJAR jar)
		{
			String[] dependents = jar.getDependentPlugins();
			for (String path: dependents) 
			{
				PluginJAR _jar = jEdit.getPluginJAR(path);
				if(_jar != null)
				{
					toLoad.add(path);
					unloadPluginJAR(_jar);
					
					String cachePath = jar.getCachePath();
					if(cachePath != null)
						new File(cachePath).delete();

				}
			}
			jEdit.removePluginJAR(jar,false);
			
		} 

		
		public boolean equals(Object o)
		{
			return o instanceof Remove
			       && ((Remove) o).jar.equals(jar);
		} 

		
		private final String jar;
		
	} 

	
	class Install extends Operation
	{
		int size;

		
		Install(String installed, String url, String installDirectory,
			int size)
		{
			
			if(url == null)
				throw new NullPointerException();

			this.installed = installed;
			this.url = url;
			this.installDirectory = installDirectory;
			this.size = size;
		} 

		
		public int getMaximum()
		{
			return size;
		} 

		
		public void runInWorkThread(PluginManagerProgress progress)
		{
			String fileName = MiscUtilities.getFileName(url);

			path = download(progress,fileName,url);
		} 

		
		public void runInAWTThread(Component comp)
		{
			
			if(path == null)
				return;

			
			if(installed != null)
				new Remove(installed).runInAWTThread(comp);

			ZipFile zipFile = null;

			try
			{
				zipFile = new ZipFile(path);

				Enumeration<? extends ZipEntry> e = zipFile.entries();
				while(e.hasMoreElements())
				{
					ZipEntry entry = e.nextElement();
					String name = entry.getName().replace('/',File.separatorChar);
					File file = new File(installDirectory,name);
					if(entry.isDirectory())
						file.mkdirs();
					else
					{
						new File(file.getParent()).mkdirs();
						InputStream in = null;
						FileOutputStream out = null;
						try
						{
							in = zipFile.getInputStream(entry);
							out = new FileOutputStream(file);
							IOUtilities.copyStream(4096,
								null,
								in,
								out,false);
						}
						finally
						{
							IOUtilities.closeQuietly(in);
							IOUtilities.closeQuietly(out);
						}
						if(file.getName().toLowerCase().endsWith(".jar"))
							toLoad.add(file.getPath());
					}
				}
			}
			catch(InterruptedIOException iio)
			{
			}
			catch(ZipException e)
			{
				Log.log(Log.ERROR,this,e);
				GUIUtilities.error(null,"plugin-error-download",new Object[]{""});
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);

				String[] args = { io.getMessage() };
				GUIUtilities.error(null,"ioerror",args);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);
			}
			finally
			{
				try
				{
					if(zipFile != null)
						zipFile.close();
				}
				catch(IOException io)
				{
					Log.log(Log.ERROR,this,io);
				}

				if(jEdit.getBooleanProperty(
					"plugin-manager.deleteDownloads"))
				{
					new File(path).delete();
				}
			}
		} 

		
		public boolean equals(Object o)
		{
			return o instanceof Install
			       && ((Install) o).url.equals(url);
		} 

		
		private String installed;
		private final String url;
		private String installDirectory;
		private String path;

		
		private String download(PluginManagerProgress progress,
			String fileName, String url)
		{
			try
			{
				String host = jEdit.getProperty("plugin-manager.mirror.id");
				if (host == null || host.equals(MirrorList.Mirror.NONE))
					host = "default";
				
				String path = MiscUtilities.constructPath(getDownloadDir(),fileName);
				URLConnection conn = new URL(url).openConnection();
				progress.setStatus(jEdit.getProperty("plugin-manager.progress",new String[] {fileName, host}));
				InputStream in = null;
				FileOutputStream out = null;
				try
				{
					in = conn.getInputStream();
					out = new FileOutputStream(path);
					if(!IOUtilities.copyStream(progress,in,out,true))
						return null;
				}
				finally
				{
					IOUtilities.closeQuietly(in);
					IOUtilities.closeQuietly(out);
				}
				
				return path;
			}
			catch(InterruptedIOException iio)
			{
				
				return null;
			}
			catch(FileNotFoundException e)
			{
				Log.log(Log.ERROR,this,e);

				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						GUIUtilities.error(null,"plugin-error-download",new Object[]{""});
					}
				});

				return null;
			}
			catch(final IOException io)
			{
				Log.log(Log.ERROR,this,io);

				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						String[] args = { io.getMessage() };
						GUIUtilities.error(null,"plugin-error-download",args);
					}
				});

				return null;
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);

				return null;
			}
		} 

		
	} 
}
