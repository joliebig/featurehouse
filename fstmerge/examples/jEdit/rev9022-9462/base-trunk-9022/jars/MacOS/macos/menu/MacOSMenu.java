

package macos.menu;


import java.awt.*;
import java.awt.event.*;
import java.io.File;
import javax.swing.*;
import javax.swing.event.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.menu.*;
import org.gjt.sp.util.Log;
import macos.*;


public class MacOSMenu implements DynamicMenuProvider
{	
	
	public MacOSMenu()
	{
		
	} 
	
	
	public boolean updateEveryTime()
	{
		return true;
	} 
	
	
	public void update(JMenu menu)
	{
		File buff = new File(jEdit.getActiveView().getBuffer().getPath());
		
		JMenuItem showCurrent = new JMenuItem(
			jEdit.getProperty("MacOSPlugin.menu.showCurrent"));
		showCurrent.addActionListener(new ShowFileAction(buff.getPath()));
		showCurrent.setEnabled(buff.exists());
		JMenuItem showCurrentDir = new JMenuItem(
			jEdit.getProperty("MacOSPlugin.menu.showCurrentDir"));
		showCurrentDir.addActionListener(new ShowDirAction(buff.getParent()));
		showCurrent.setEnabled(buff.getParentFile().exists());
		menu.add(showCurrent);
		menu.add(showCurrentDir);
		menu.addSeparator();
		menu.add(new ShowBufferMenu());
		menu.add(new ShowRecentMenu());
		menu.add(new ShowRecentDirMenu());
	} 
	
	
	class ShowFileAction implements ActionListener
	{
		private String path;
		
		public ShowFileAction(String path)
		{
			this.path = path;
		}
		
		public void actionPerformed(ActionEvent e)
		{
			MacOSActions.showInFinder(path);
		}
	} 
	
	
	class ShowDirAction implements ActionListener
	{
		private String path;
		
		public ShowDirAction(String path)
		{
			this.path = path;
		}
		
		public void actionPerformed(ActionEvent e)
		{
			MacOSActions.showInFinder(path);
		}
	} 
}
