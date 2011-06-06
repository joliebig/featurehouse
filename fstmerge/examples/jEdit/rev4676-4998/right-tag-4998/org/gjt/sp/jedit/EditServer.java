

package org.gjt.sp.jedit;


import bsh.NameSpace;
import javax.swing.SwingUtilities;
import java.io.*;
import java.net.*;
import java.util.Random;
import org.gjt.sp.jedit.io.FileVFS;
import org.gjt.sp.util.Log;



public class EditServer extends Thread
{
	
	EditServer(String portFile)
	{
		super("jEdit server daemon [" + portFile + "]");
		setDaemon(true);
		this.portFile = portFile;

		try
		{
			
			
			
			
			
			if(OperatingSystem.isUnix())
			{
				new File(portFile).createNewFile();
				FileVFS.setPermissions(portFile,0600);
			}

			
			
			socket = new ServerSocket(0, 2,
				InetAddress.getByName("127.0.0.1"));
			authKey = Math.abs(new Random().nextInt());
			int port = socket.getLocalPort();

			FileWriter out = new FileWriter(portFile);
			out.write("b\n");
			out.write(String.valueOf(port));
			out.write("\n");
			out.write(String.valueOf(authKey));
			out.write("\n");
			out.close();

			Log.log(Log.DEBUG,this,"jEdit server started on port "
				+ socket.getLocalPort());
			Log.log(Log.DEBUG,this,"Authorization key is "
				+ authKey);

			ok = true;
		}
		catch(IOException io)
		{
			
			Log.log(Log.NOTICE,this,io);
		}
	} 

	
	public void run()
	{
		for(;;)
		{
			if(abort)
				return;

			Socket client = null;
			try
			{
				client = socket.accept();

				
				
				
				client.setSoTimeout(1000);

				Log.log(Log.MESSAGE,this,client + ": connected");

				DataInputStream in = new DataInputStream(
					client.getInputStream());
				OutputStream out = client.getOutputStream();

				if(!handleClient(client,in))
					abort = true;
			}
			catch(Exception e)
			{
				if(!abort)
					Log.log(Log.ERROR,this,e);
				abort = true;
			}
			finally
			{
				
			}
		}
	} 

	
	
	public static void handleClient(boolean restore, String parent,
		String[] args)
	{
		handleClient(restore,false,false,parent,args);
	} 

	
	
	public static Buffer handleClient(boolean restore,
		boolean newView, boolean newPlainView, String parent,
		String[] args)
	{
		
		if(jEdit.getFirstView() == null)
		{
			
			
			

			Buffer buffer = jEdit.openFiles(null,parent,args);

			boolean restoreFiles = restore && jEdit.getBooleanProperty("restore")
				&& (buffer == null || jEdit.getBooleanProperty("restore.cli"));

			View view = PerspectiveManager.loadPerspective(restoreFiles);

			if(view == null)
			{
				if(buffer == null)
					buffer = jEdit.getFirstBuffer();
				view = jEdit.newView(null,buffer);
			}
			else if(buffer != null)
				view.setBuffer(buffer);

			return buffer;
		}
		else if(newPlainView)
		{
			
			Buffer buffer = jEdit.openFiles(null,parent,args);
			if(buffer == null)
				buffer = jEdit.getFirstBuffer();
			jEdit.newView(null,buffer,true);
			return buffer;
		}
		else if(newView)
		{
			
			Buffer buffer = jEdit.openFiles(null,parent,args);
			if(buffer == null)
				buffer = jEdit.getFirstBuffer();
			jEdit.newView(jEdit.getActiveView(),buffer,false);
			return buffer;
		}
		else
		{
			
			View view = jEdit.getActiveView();

			Buffer buffer = jEdit.openFiles(view,parent,args);

			
			
			
			
			if (jEdit.getBooleanProperty("server.brokenToFront"))
				view.setState(java.awt.Frame.ICONIFIED);

			
			view.setState(java.awt.Frame.NORMAL);
			view.requestFocus();
			view.toFront();

			return buffer;
		}
	} 

	
	boolean isOK()
	{
		return ok;
	} 

	
	void stopServer()
	{
		abort = true;
		try
		{
			socket.close();
		}
		catch(IOException io)
		{
		}

		new File(portFile).delete();
	} 

	

	
	private String portFile;
	private ServerSocket socket;
	private int authKey;
	private boolean ok;
	private boolean abort;
	

	
	private boolean handleClient(final Socket client, DataInputStream in)
		throws Exception
	{
		int key = in.readInt();
		if(key != authKey)
		{
			Log.log(Log.ERROR,this,client + ": wrong"
				+ " authorization key (got " + key
				+ ", expected " + authKey + ")");
			in.close();
			client.close();

			return false;
		}
		else
		{
			
			client.setSoTimeout(0);

			Log.log(Log.DEBUG,this,client + ": authenticated"
				+ " successfully");

			final String script = in.readUTF();
			Log.log(Log.DEBUG,this,script);

			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					try
					{
						NameSpace ns = new NameSpace(
							BeanShell.getNameSpace(),
							"EditServer namespace");
						ns.setVariable("socket",client);
						BeanShell.eval(null,ns,script);
					}
					catch(bsh.EvalError e)
					{
						Log.log(Log.ERROR,this,e);
					}
					finally
					{
						try
						{
							BeanShell.getNameSpace().setVariable("socket",null);
						}
						catch(bsh.EvalError e)
						{
							Log.log(Log.ERROR,this,e);
						}
					}
				}
			});

			return true;
		}
	} 

	
}
