

package org.gjt.sp.jedit.browser;

import java.util.EventListener;

import org.gjt.sp.jedit.io.VFSFile;


public interface BrowserListener extends EventListener
{
	
	void filesSelected(VFSBrowser browser, VFSFile[] files);

	
	void filesActivated(VFSBrowser browser, VFSFile[] files);
}
