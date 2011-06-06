

package macos.menu;


import java.awt.event.*;
import java.io.File;
import javax.swing.*;
import javax.swing.event.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.*;
import macos.*;


public class ShowBufferMenu extends JMenu implements MenuListener
{
	
	public ShowBufferMenu()
	{
		super(jEdit.getProperty("MacOSPlugin.menu.buffers.label"));
		addMenuListener(this);
	} 
	
	
	private void construct()
	{
		JMenuItem item;
		removeAll();
		
		Buffer[] buffs = jEdit.getBuffers();
		for (int i=0; i < buffs.length; i++)
		{
			if (!buffs[i].isUntitled())
			{
				item = add(new ShowBufferMenuItem(
					buffs[i].getName(),buffs[i].getPath()));
				item.setIcon(FileCellRenderer.fileIcon);
				add(item);
			}
		}
		
		if (getItemCount() == 0)
		{
			item = new JMenuItem(jEdit.getProperty("MacOSPlugin.menu.buffers.none"));
			item.setEnabled(false);
			add(item);
		}
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
	
	
	class ShowBufferMenuItem extends JMenuItem
	{
		String path;
		
		public ShowBufferMenuItem(String name, String path)
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
