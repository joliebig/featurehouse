

package org.gjt.sp.jedit.menu;

import javax.swing.JMenu;


public interface DynamicMenuProvider
{
	
	boolean updateEveryTime();

	
	void update(JMenu menu);
}
