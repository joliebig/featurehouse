

package org.gjt.sp.jedit.gui;


public interface DockableWindowContainer
{
	void register(DockableWindowManager.Entry entry);
	void remove(DockableWindowManager.Entry entry);
	void unregister(DockableWindowManager.Entry entry);
	void show(DockableWindowManager.Entry entry);
	boolean isVisible(DockableWindowManager.Entry entry);
}
