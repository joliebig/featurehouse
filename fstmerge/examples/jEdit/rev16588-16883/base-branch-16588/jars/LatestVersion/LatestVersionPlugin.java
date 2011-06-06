

import javax.swing.JOptionPane;
import java.io.*;
import java.net.URL;
import org.gjt.sp.jedit.*;

public class LatestVersionPlugin extends EditPlugin
{
	public static void doVersionCheck(View view)
	{
		view.showWaitCursor();

		try
		{
			URL url = new URL(jEdit.getProperty(
				"version-check.url"));
			InputStream in = url.openStream();
			BufferedReader bin = new BufferedReader(
				new InputStreamReader(in));

			String line;
			String develBuild = null;
			String stableBuild = null;
			while((line = bin.readLine()) != null)
			{
				if(line.startsWith(".build"))
					develBuild = line.substring(6).trim();
				else if(line.startsWith(".stablebuild"))
					stableBuild = line.substring(12).trim();
			}

			bin.close();

			if(develBuild != null && stableBuild != null)
			{
				doVersionCheck(view,stableBuild,develBuild);
			}
		}
		catch(IOException e)
		{
			String[] args = { jEdit.getProperty("version-check.url"),
				e.toString() };
			GUIUtilities.error(view,"read-error",args);
		}

		view.hideWaitCursor();
	}

	public static void doVersionCheck(View view, String stableBuild,
		String develBuild)
	{
		String myBuild = jEdit.getBuild();
		String pre = myBuild.substring(6,7);
		String variant;
		String build;

		if(pre.equals("99"))
		{
			variant = "stable";
			build = stableBuild;
		}
		else
		{
			variant = "devel";
			build = develBuild;
		}

		
		if(develBuild.compareTo(stableBuild) < 0)
			variant += "-nodevel";

		int retVal = GUIUtilities.confirm(view,"version-check." + variant,
			new String[] { MiscUtilities.buildToVersion(myBuild),
				MiscUtilities.buildToVersion(stableBuild),
				MiscUtilities.buildToVersion(develBuild) },
				JOptionPane.YES_NO_OPTION,
				JOptionPane.QUESTION_MESSAGE);
		if(retVal == JOptionPane.YES_OPTION)
			jEdit.openFile(view,jEdit.getProperty("version-check.url"));
	}
}
