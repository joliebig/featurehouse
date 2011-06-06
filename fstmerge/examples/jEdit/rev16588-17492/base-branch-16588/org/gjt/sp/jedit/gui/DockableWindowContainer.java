

package org.gjt.sp.jedit.gui;


interface DockableWindowContainer
{
	void register(DockableWindowManagerImpl.Entry entry);
	void remove(DockableWindowManagerImpl.Entry entry);
	void unregister(DockableWindowManagerImpl.Entry entry);
	void show(DockableWindowManagerImpl.Entry entry);
	boolean isVisible(DockableWindowManagerImpl.Entry entry);
}
