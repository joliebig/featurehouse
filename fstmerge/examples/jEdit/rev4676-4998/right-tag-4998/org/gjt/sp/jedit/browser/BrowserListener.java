

package org.gjt.sp.jedit.browser;

import java.util.EventListener;

import org.gjt.sp.jedit.io.VFS;


public interface BrowserListener extends EventListener
{
	
	void filesSelected(VFSBrowser browser, VFS.DirectoryEntry[] files);

	
	void filesActivated(VFSBrowser browser, VFS.DirectoryEntry[] files);
}
