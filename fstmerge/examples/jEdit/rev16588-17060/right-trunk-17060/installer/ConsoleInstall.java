

package installer;

import java.io.*;
import java.util.Vector;


public class ConsoleInstall
{
	public ConsoleInstall()
	{
		installer = new Install();

		String appName = installer.getProperty("app.name");
		String appVersion = installer.getProperty("app.version");

		BufferedReader in = new BufferedReader(new InputStreamReader(
			System.in));

		System.out.println("*** " + appName + " " + appVersion + " installer");

		OperatingSystem os = OperatingSystem.getOperatingSystem();

		String installDir = os.getInstallDirectory(appName,appVersion);

		System.out.print("Installation directory: [" + installDir + "] ");
		System.out.flush();

		String _installDir = readLine(in);
		if(_installDir.length() != 0)
			installDir = _installDir;
		else 
			System.out.println("Will use default");
		
		OperatingSystem.OSTask[] osTasks = os.getOSTasks(installer);

		for(int i = 0; i < osTasks.length; i++)
		{
			OperatingSystem.OSTask osTask = osTasks[i];
			String label = osTask.getLabel();
			
			if(label != null)
			{
				String dir = osTask.getDirectory();
				System.out.print(label + " [" + dir + "] ");
				System.out.flush();

				dir = readLine(in);
				osTask.setEnabled(true);
				if(dir.length() != 0)
				{
					if(dir.equals("off"))
						osTask.setEnabled(false);
					else
						osTask.setDirectory(dir);
				}
				else
					System.out.println("will use default");
			}
		}

		int compCount = installer.getIntegerProperty("comp.count");
		Vector components = new Vector(compCount);

		System.out.println("*** Program components to install");
		for(int i = 0; i < compCount; i++)
		{
			String fileset = installer.getProperty("comp." + i + ".fileset");

			String osDep = installer.getProperty("comp." + i + ".os");
			if(osDep != null)
			{
				if(!os.getClass().getName().endsWith(osDep))
				{
					continue;
				}
			}

			System.out.print("Install "
				+ installer.getProperty("comp." + i + ".name")
				+ " ("
				+ installer.getProperty("comp." + i + ".disk-size")
				+ "Kb) [Y/n]? ");

			String line = readLine(in);
			if(line.length() == 0 || line.charAt(0) == 'y'
				|| line.charAt(0) == 'Y')
				components.addElement(fileset);
		}

		System.out.println("*** Starting installation...");
		ConsoleProgress progress = new ConsoleProgress();
		InstallThread thread = new InstallThread(
			installer,progress,installDir,osTasks,
			0 ,components);
		thread.start();
	}

	
	private Install installer;

	private String readLine(BufferedReader in)
	{
		try
		{
			String line = in.readLine();
			if(line == null)
			{
				System.err.println("\nEOF in input!");
				System.exit(1);
				
				throw new InternalError();
			}
			return line;
		}
		catch(IOException io)
		{
			System.err.println("\nI/O error: " + io);
			System.exit(1);
			
			throw new InternalError();
		}
	}
}
