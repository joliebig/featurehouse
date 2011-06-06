

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;



public class FloatingWindowContainer extends JFrame implements DockableWindowContainer
{
	
	public FloatingWindowContainer(DockableWindowManager dockableWindowManager,
		boolean clone)
	{
		this.dockableWindowManager = dockableWindowManager;
		this.clone = clone;
		setIconImage(GUIUtilities.getPluginIcon());
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		Box caption = new Box(BoxLayout.X_AXIS);
		caption.add(menu = new RolloverButton(GUIUtilities
			.loadIcon("ToolbarMenu.gif")));
		menu.addMouseListener(new MouseHandler());
		Box separatorBox = new Box(BoxLayout.Y_AXIS);
		separatorBox.add(Box.createVerticalStrut(3));
		separatorBox.add(new JSeparator(JSeparator.HORIZONTAL));
		separatorBox.add(Box.createVerticalStrut(3));
		caption.add(separatorBox);
		getContentPane().add(BorderLayout.NORTH,caption);
	} 

	
	public void register(DockableWindowManager.Entry entry)
	{
		this.entry = entry;
		setTitle(entry.title);

		getContentPane().add(BorderLayout.CENTER,entry.win);

		pack();
		GUIUtilities.loadGeometry(this,entry.factory.name);
		show();
	} 

	
	public void remove(DockableWindowManager.Entry entry)
	{
		entry.container = null;
		dispose();
	} 

	
	public void unregister(DockableWindowManager.Entry entry)
	{
		dispose();
	} 

	
	public void show(final DockableWindowManager.Entry entry)
	{
		if(entry == null)
			dispose();
		else
		{
			toFront();
			requestFocus();
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					if(entry.win instanceof DefaultFocusComponent)
					{
						((DefaultFocusComponent)entry.win)
							.focusOnDefaultComponent();
					}
					else
					{
						entry.win.requestDefaultFocus();
					}
				}
			});
		}
	} 

	
	public boolean isVisible(DockableWindowManager.Entry entry)
	{
		return true;
	} 

	
	public void dispose()
	{
		GUIUtilities.saveGeometry(this,entry.factory.name);
		entry.container = null;
		entry.win = null;
		super.dispose();
	} 

	
	public DockableWindowManager getDockableWindowManager()
	{
		return dockableWindowManager;
	} 

	
	public Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	private DockableWindowManager dockableWindowManager;
	private boolean clone;
	private DockableWindowManager.Entry entry;
	private JButton menu;
	

	
	class MouseHandler extends MouseAdapter
	{
		JPopupMenu popup;

		public void mousePressed(MouseEvent evt)
		{
			if(popup != null && popup.isVisible())
				popup.setVisible(false);
			else
			{
				popup = dockableWindowManager.createPopupMenu(
					FloatingWindowContainer.this,
					entry.factory.name,clone);
				GUIUtilities.showPopupMenu(popup,
					menu,menu.getX(),menu.getY() + menu.getHeight(),
					false);
			}
		}
	} 
}
