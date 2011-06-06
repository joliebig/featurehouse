

package org.gjt.sp.jedit.gui.statusbar;

import javax.swing.JComponent;


public interface Widget 
{
	
	JComponent getComponent();
	
	
	void propertiesChanged();
	
	
	void update();
}
