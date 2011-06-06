package org.gjt.sp.jedit.gui;

import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.View.ViewConfig;
import org.gjt.sp.jedit.gui.DockableWindowManager.DockingLayout;


   
public class DockableWindowManagerProvider implements DockingFrameworkProvider
{
	public DockableWindowManager create(View view,
			DockableWindowFactory instance, ViewConfig config)
	{
		return new DockableWindowManagerImpl(view, instance, config);
	}

	public DockingLayout createDockingLayout()
	{
		return new DockableWindowManagerImpl.DockableWindowConfig();
	}

}
