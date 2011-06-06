package org.gjt.sp.jedit.help;

import java.awt.Component;
import java.beans.PropertyChangeListener;


public interface HelpViewerInterface 
{
	
	
	public void gotoURL(String url, boolean addToHistory, int scrollPos);
	
	public String getBaseURL();
	
	public void addPropertyChangeListener(PropertyChangeListener l);
	
	
	public void dispose();
	
	public Component getComponent();
	public String getShortURL();
	
	public void queueTOCReload();
	public void setTitle(String newTitle);
}
