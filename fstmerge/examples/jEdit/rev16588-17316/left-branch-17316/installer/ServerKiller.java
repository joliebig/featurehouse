

package installer;

import java.io.*;
import java.net.*;


public class ServerKiller
{
	
	
	public static boolean quitjEditServer()
	{
		
		
		String settingsDirectory = System.getProperty("user.home");
		File portFile;
		File f = new File(settingsDirectory);
		portFile = new File(f,".jedit/server");
		
		
		if(portFile.exists())
		{
			try
			{
				BufferedReader in = new BufferedReader(new FileReader(portFile));
				String check = in.readLine();
				if(!check.equals("b"))
				{
					System.out.println("Wrong port file format");
					return false;
				}
 
				int port = Integer.parseInt(in.readLine());
				int key = Integer.parseInt(in.readLine());

				Socket socket = new Socket(InetAddress.getByName("127.0.0.1"),port);
				DataOutputStream out = new DataOutputStream(
					socket.getOutputStream());
				out.writeInt(key);

				
				
				
				String script;
					script = "jEdit.exit(null,true);\n";

				out.writeUTF(script);

				
				try
				{
					socket.getInputStream().read();
				}
				catch(Exception e)
				{
					
				}

				in.close();
				out.close();
			}
			catch(FileNotFoundException fnfe)
			{
				
			}
			catch(UnknownHostException uhe)
			{
				
			}
			catch(IOException ioe)
			{
				System.out.println("Exception while trying to connect to existing server:");
				System.out.println(ioe);
				System.out.println("Don't worry too much !");
				return false; 
			}
		}
		return true;
	}
	
	
	public static void main(String[] args)
	{
		boolean success = quitjEditServer();
		if(!success)
		{
			System.exit(-1);
		}
	}
}

 	  	 
