

package macos.menu;


import java.awt.*;
import java.awt.event.*;
import java.io.File;
import javax.swing.*;
import javax.swing.event.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import macos.*;


public class MacOSMenu extends JMenu implements ActionListener, MenuListener
{
	private MacMenuItem showCurrent;
	private MacMenuItem showCurrentDir;
	
	
	public MacOSMenu()
	{
		super(jEdit.getProperty("MacOSPlugin.menu.label"));
		
		showCurrent = new MacMenuItem(
			jEdit.getProperty("MacOSPlugin.menu.showCurrent"));
		showCurrent.addActionListener(this);
		showCurrentDir = new MacMenuItem(
			jEdit.getProperty("MacOSPlugin.menu.showCurrentDir"));
		showCurrentDir.addActionListener(this);
		add(showCurrent);
		add(showCurrentDir);
		addSeparator();
		add(new ShowBufferMenu());
		add(new ShowRecentMenu());
		add(new ShowRecentDirMenu());
		
		addMenuListener(this);
	} 
	
	
	private void construct()
	{
		File buff = new File(jEdit.getActiveView().getBuffer().getPath());
		showCurrent.setPath(buff.getPath());
		showCurrent.setEnabled(buff.exists());
		showCurrentDir.setPath(buff.getParent());
	} 
	
	
	public void menuSelected(MenuEvent e)
	{
		construct();
	} 
	
	
	public void menuDeselected(MenuEvent e)
	{
	} 
	
	
	public void menuCanceled(MenuEvent e)
	{
	} 
	
	
	public void actionPerformed(ActionEvent e)
	{
		Object src = e.getSource();
		if (src == showCurrent)
			MacOSActions.showInFinder(showCurrent.getPath());
		else if (src == showCurrentDir)
			MacOSActions.showInFinder(showCurrentDir.getPath());
	} 
	
	
	class MacMenuItem extends JMenuItem
	{
		String path;
		
		public MacMenuItem(String name)
		{
			super(name);
		}
		
		public String getPath()
		{
			return path;
		}
		
		public void setPath(String path)
		{
			this.path = path;
		}
	} 
}
