

package org.gjt.sp.jedit;

import java.awt.Component;


public interface OptionPane
{
	
	String getName();

	
	Component getComponent();

	
	void init();

	
	void save();
}
