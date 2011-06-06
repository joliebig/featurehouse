

package installer;

import java.io.*;
import java.util.Vector;


public abstract class OperatingSystem
{
	public abstract String getInstallDirectory(String name, String version);

	public abstract static class OSTask
	{
		protected Install installer;
		protected String name;
		protected String label;
		protected String directory;
		protected boolean enabled;

		public OSTask(Install installer, String name)
		{
			this.installer = installer;
			this.name = name;
			this.label = installer.getProperty("ostask." + name + ".label");
			this.directory = getDefaultDirectory(installer);

			
			enabled = true;
		}

		public String getName()
		{
			return name;
		}

		public String getLabel()
		{
			return label;
		}

		public String getDefaultDirectory(Install installer)
		{
			return null;
		}

		public String getDirectory()
		{
			return directory;
		}

		public boolean isEnabled()
		{
			return enabled;
		}

		public void setEnabled(boolean enabled)
		{
			this.enabled = enabled;
		}

		public void setDirectory(String directory)
		{
			this.directory = directory;
		}

		public abstract void perform(String installDir,
			Vector filesets) throws IOException;
	}

	public OSTask[] getOSTasks(Install installer)
	{
		return new OSTask[0];
	}

	public void mkdirs(String directory) throws IOException
	{
		File file = new File(directory);
		if(!file.exists())
			file.mkdirs();
	}

	public static OperatingSystem getOperatingSystem()
	{
		if(os != null)
			return os;

		if(System.getProperty("mrj.version") != null)
		{
			os = new MacOS();
		}
		else
		{
			String osName = System.getProperty("os.name");
			if(osName.indexOf("Windows") != -1)
				os = new Windows();
			else if(osName.indexOf("OS/2") != -1)
				os = new HalfAnOS();
			else if(osName.indexOf("VMS") != -1)
				os = new VMS();
			else
				os = new Unix();
		}

		return os;
	}

	public static class Unix extends OperatingSystem
	{
		public String getInstallDirectory(String name, String version)
		{
			String dir = "/usr/local/share/";
			if(!new File(dir).canWrite())
			{
				dir = System.getProperty("user.home");
			}

			return new File(dir,name.toLowerCase() + "/" + version).getPath();
		}

		public String getExtraClassPath()
		{
			return "";
		}

		public class ScriptOSTask extends OSTask
		{
			public ScriptOSTask(Install installer)
			{
				super(installer,"unix-script");
			}

			public String getDefaultDirectory(Install installer)
			{
				String dir = "/usr/local/";
				if(!new File(dir).canWrite())
				{
					dir = System.getProperty("user.home");
				}

				return new File(dir,"bin").getPath();
			}

			public void perform(String installDir,
				Vector filesets) throws IOException
			{
				if(!enabled)
				{
					return;
				}

				mkdirs(directory);

				String name = installer.getProperty("app.name");

				
				String script = directory + File.separatorChar
					+ name.toLowerCase();

				
				new File(script).delete();

				
				FileWriter out = new FileWriter(script);
				out.write("#!/bin/sh\n");
				out.write("#\n");
				out.write("# Runs jEdit - Programmer's Text Editor.\n");
				out.write("#\n");
				out.write("\n");
				out.write("# Set jvm heap initial and maximum sizes (in megabytes).\n");
				out.write("JAVA_HEAP_MAX_SIZE=192\n");
				out.write("\n");
				out.write("DEFAULT_JAVA_HOME=\""
					+ System.getProperty("java.home")
					+ "\"\n");
				out.write("if [ -z \"$JAVA_HOME\" ]; then\n");
				out.write("\tJAVA_HOME=\"$DEFAULT_JAVA_HOME\"\n");
				out.write("fi\n");
				out.write("\n");
				out.write("# Launch application.\n");
				out.write("\n");
				
				String jar = "\""+ installDir + File.separator
					+ name.toLowerCase() + ".jar"+"\"";

                
 				out.write("exec \"$JAVA_HOME/bin/java\"" +
 					  " -Xmx${JAVA_HEAP_MAX_SIZE}M -jar " +
					  jar + " \"$@\"\n");
				out.close();

				
				String[] chmodArgs = { "chmod", "755", script };
				exec(chmodArgs);
			}
		}

		public class ManPageOSTask extends OSTask
		{
			public ManPageOSTask(Install installer)
			{
				super(installer,"unix-man");
			}

			public String getDefaultDirectory(Install installer)
			{
				String dir = "/usr/local/";
				if(!new File(dir).canWrite())
					dir = System.getProperty("user.home");

				return new File(dir,"man/man1").getPath();
			}

			public void perform(String installDir,
				Vector filesets) throws IOException
			{
				if(!enabled)
					return;

				mkdirs(directory);

				String name = installer.getProperty("app.name");

				
				String manpage = installer.getProperty("ostask.unix-man.manpage");

				InputStream in = getClass().getResourceAsStream("/" + manpage);
				installer.copy(in,new File(directory,manpage).getPath(),
					null);
			}
		}

		public OSTask[] getOSTasks(Install installer)
		{
			return new OSTask[] { new ScriptOSTask(installer),
				new ManPageOSTask(installer) };
		}

		public void mkdirs(String directory) throws IOException
		{
			File file = new File(directory);
			if(!file.exists())
			{
				String[] mkdirArgs = { "mkdir", "-m", "755",
					"-p", directory };
				exec(mkdirArgs);
			}
		}

		public void exec(String[] args) throws IOException
		{
			Process proc = Runtime.getRuntime().exec(args);
			proc.getInputStream().close();
			proc.getOutputStream().close();
			proc.getErrorStream().close();
			try
			{
				proc.waitFor();
			}
			catch(InterruptedException ie)
			{
			}
		}
	}

	public static class MacOS extends Unix
	{
		public String getInstallDirectory(String name, String version)
		{
			return "/Applications/" + name + " " + version;
		}

		public String getExtraClassPath()
		{
			return "/System/Library/Java/:";
		}
	}

	public static class Windows extends OperatingSystem
	{
		public String getInstallDirectory(String name, String version)
		{
			String programDir = System.getenv("ProgramFiles");
			
			
			
			
			if(programDir == null)
			{
				
				programDir = "%ProgramFiles%";
			}
			return programDir + "\\" + name + " " + version;
		}

		public class JEditLauncherOSTask extends OSTask
		{
			public JEditLauncherOSTask(Install installer)
			{
				super(installer,"jedit-launcher");
			}

			public String getDefaultDirectory(Install installer)
			{
				return null;
			}

			public void perform(String installDir,
				Vector filesets)
			{
				if(!enabled
					|| !filesets.contains("jedit-windows"))
					return;

				
				File executable = new File(installDir,"jedit.exe");
				if(!executable.exists())
					return;

				String[] args = { executable.getPath(), "/i",
					System.getProperty("java.home")
					+ File.separator
					+ "bin" };

				try
				{
					Runtime.getRuntime().exec(args).waitFor();
				}
				catch(IOException io)
				{
				}
				catch(InterruptedException ie)
				{
				}
			}
		}

		public OSTask[] getOSTasks(Install installer)
		{
			return new OSTask[] {  };
		}
	}

	public static class HalfAnOS extends OperatingSystem
	{
		public String getInstallDirectory(String name, String version)
		{
			return "C:\\" + name + " " + version;
		}
	}

	public static class VMS extends OperatingSystem
	{
		public String getInstallDirectory(String name, String version)
		{
			return "./" + name.toLowerCase() + "/" + version;
		}
	}

	
	private static OperatingSystem os;
}
