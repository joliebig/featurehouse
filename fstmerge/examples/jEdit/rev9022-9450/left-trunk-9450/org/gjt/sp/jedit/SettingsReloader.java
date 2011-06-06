

package org.gjt.sp.jedit;


import java.io.File;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.VFSManager;
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
		String jEditHome = jEdit.getJEditHome();
		String settingsDirectory = jEdit.getSettingsDirectory();

		if(!MiscUtilities.isURL(path))
			path = MiscUtilities.resolveSymlinks(path);

		
		if((VFSManager.getVFSForPath(path).getCapabilities()
			& VFS.CASE_INSENSITIVE_CAP) != 0)
		{
			path = path.toLowerCase();
			jEditHome = jEditHome.toLowerCase();
			settingsDirectory = settingsDirectory.toLowerCase();
		}

		
		SearchFileSet fileset = SearchAndReplace.getSearchFileSet();
		if(fileset instanceof DirectoryListSet)
		{
			DirectoryListSet dirset = (DirectoryListSet)fileset;
			String dir = MiscUtilities.resolveSymlinks(
				dirset.getDirectory());
			if(path.startsWith(dir))
				dirset.invalidateCachedList();
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
