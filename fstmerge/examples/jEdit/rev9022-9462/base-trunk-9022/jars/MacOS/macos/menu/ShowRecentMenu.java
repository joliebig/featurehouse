

package macos.menu;


import java.awt.event.*;
import java.io.File;
import java.util.List;
import javax.swing.*;
import javax.swing.event.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.*;
import macos.*;


public class ShowRecentMenu extends JMenu implements MenuListener
{
	
	public ShowRecentMenu()
	{
		super(jEdit.getProperty("MacOSPlugin.menu.recent.label"));
		addMenuListener(this);
	} 
	
	
	private void construct()
	{
		List recent = BufferHistory.getHistory();
		JMenuItem item;
		File file;
		int max = recent.size();
		int min = max - 20;
		
		if (max == 0)
		{
			item = new JMenuItem(jEdit.getProperty("MacOSPlugin.menu.recent.none"));
			item.setEnabled(false);
			add(item);
			return;
		}
		
		if (min < 0)
			min = 0;
		
		for (int i=max-1; i >= min ; i--)
		{
			file = new File(((BufferHistory.Entry)recent.get(i)).path);
			item = new ShowRecentMenuItem(file.getName(),file.getPath());
			item.setIcon(FileCellRenderer.fileIcon);
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
	
	
	class ShowRecentMenuItem extends JMenuItem
	{
		String path;
		
		public ShowRecentMenuItem(String name, String path)
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
