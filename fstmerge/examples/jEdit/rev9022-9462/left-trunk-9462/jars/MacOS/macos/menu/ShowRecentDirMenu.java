

package macos.menu;


import java.awt.event.*;
import java.io.File;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.gui.*;
import macos.*;


public class ShowRecentDirMenu extends JMenu implements MenuListener
{
	
	public ShowRecentDirMenu()
	{
		super(jEdit.getProperty("MacOSPlugin.menu.recentDir.label"));
		addMenuListener(this);
	} 
	
	
	private void construct()
	{
		HistoryModel model = HistoryModel.getModel("vfs.browser.path");
		JMenuItem item;
		File file;
		int max = model.getSize();
		
		if (max == 0)
		{
			item = new JMenuItem(jEdit.getProperty("MacOSPlugin.menu.recentDir.none"));
			item.setEnabled(false);
			add(item);
			return;
		}
		
		for (int i=0; i < max ; i++)
		{
			file = new File(model.getItem(i));
			item = new ShowRecentDirMenuItem(file.getName(),file.getPath());
			item.setIcon(FileCellRenderer.dirIcon);
			add(item);
		}
	} 
	
	
	public void menuSelected(MenuEvent e)
	{
		construct();
	} 
	
	
	public void menuDeselected(MenuEvent e)
	{
		removeAll();
	} 
	
	
	public void menuCanceled(MenuEvent e)
	{
	} 
	
	
	class ShowRecentDirMenuItem extends JMenuItem
	{
		String path;
		
		public ShowRecentDirMenuItem(String name, String path)
		{
			super(name);
			this.path = path;
			addActionListener(new ShowFileAction());
		}
		
		class ShowFileAction implements ActionListener
		{
			public void actionPerformed(ActionEvent e)
			{
				MacOSActions.showInFinder(path);
			}
		}
	} 
}
