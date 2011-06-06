

package org.gjt.sp.jedit;

import java.io.File;
import org.gjt.sp.jedit.msg.VFSUpdate;
import org.gjt.sp.jedit.search.*;

class SettingsReloader implements EBComponent
{
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof VFSUpdate)
		{
			VFSUpdate vmsg = (VFSUpdate)msg;
			maybeReload(vmsg.getPath());
		}
	}

	private void maybeReload(String path)
	{
		
		SearchFileSet fileset = SearchAndReplace.getSearchFileSet();
		if(fileset instanceof DirectoryListSet)
		{
			DirectoryListSet dirset = (DirectoryListSet)fileset;
			if(path.startsWith(dirset.getDirectory()))
				dirset.invalidateCachedList();
		}

		String jEditHome = jEdit.getJEditHome();
		String settingsDirectory = jEdit.getSettingsDirectory();
		
		if(OperatingSystem.isDOSDerived() || OperatingSystem.isMacOS())
		{
			path = path.toLowerCase();
			if(jEditHome != null)
				jEditHome = jEditHome.toLowerCase();
			if(settingsDirectory != null)
				settingsDirectory = settingsDirectory.toLowerCase();
		}

		if(jEditHome != null && path.startsWith(jEditHome))
			path = path.substring(jEditHome.length());
		else if(settingsDirectory != null && path.startsWith(settingsDirectory))
			path = path.substring(settingsDirectory.length());
		else
		{
			
			
			return;
		}

		if(path.startsWith(File.separator) || path.startsWith("/"))
			path = path.substring(1);

		if(path.startsWith("macros"))
			Macros.loadMacros();
		else if(path.startsWith("modes") && (path.endsWith(".xml")
			|| path.endsWith("catalog")))
			jEdit.reloadModes();
	}
}
