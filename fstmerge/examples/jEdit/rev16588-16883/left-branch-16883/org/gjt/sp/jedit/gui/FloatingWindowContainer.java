

package org.gjt.sp.jedit.gui;


import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;



public class FloatingWindowContainer extends JFrame implements DockableWindowContainer,
	PropertyChangeListener
{
	String dockableName = null;
	
	public FloatingWindowContainer(DockableWindowManagerImpl dockableWindowManager,
		boolean clone)
	{
		this.dockableWindowManager = dockableWindowManager;

		dockableWindowManager.addPropertyChangeListener(this);
		this.clone = clone;
		setIconImage(GUIUtilities.getPluginIcon());
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		Box caption = new Box(BoxLayout.X_AXIS);
		caption.add(menu = new RolloverButton(GUIUtilities
			.loadIcon(jEdit.getProperty("dropdown-arrow.icon"))));
		menu.addMouseListener(new MouseHandler());
		menu.setToolTipText(jEdit.getProperty("docking.menu.label"));
		Box separatorBox = new Box(BoxLayout.Y_AXIS);
		separatorBox.add(Box.createVerticalStrut(3));
		separatorBox.add(new JSeparator(JSeparator.HORIZONTAL));
		separatorBox.add(Box.createVerticalStrut(3));
		caption.add(separatorBox);
		getContentPane().add(BorderLayout.NORTH,caption);
	
		
	} 

	
	public void register(DockableWindowManagerImpl.Entry entry)
	{
		this.entry = entry;
		dockableName = entry.factory.name;
		
		setTitle(entry.shortTitle());

		getContentPane().add(BorderLayout.CENTER,entry.win);

		pack();
		Container parent = dockableWindowManager.getView();
		GUIUtilities.loadGeometry(this, parent, dockableName);
		GUIUtilities.addSizeSaver(this, parent, dockableName);
		KeyListener listener = dockableWindowManager.closeListener(dockableName);
		addKeyListener(listener);
		getContentPane().addKeyListener(listener);
		menu.addKeyListener(listener);
		entry.win.addKeyListener(listener);
		setVisible(true);
		if (! entry.win.isVisible())
			entry.win.setVisible(true);
	} 

	
	public void remove(DockableWindowManagerImpl.Entry entry)
	{
		dispose();
	} 

	
	public void unregister(DockableWindowManagerImpl.Entry entry)
	{
		this.entry = null;
		entry.btn = null;
		entry.container = null;
		
		
		
		super.dispose();
	} 

	
	public void show(final DockableWindowManagerImpl.Entry entry)
	{
		if(entry == null)
			dispose();
		else
		{
			setTitle(entry.longTitle());
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
						entry.win.requestFocus();
					}
				}
			});
		}
	} 

	
	public boolean isVisible(DockableWindowManagerImpl.Entry entry)
	{
		return true;
	} 

	
	@Override
	public void dispose()
	{
		entry.container = null;
		entry.win = null;
		super.dispose();
	} 

	
	public DockableWindowManagerImpl getDockableWindowManager()
	{
		return dockableWindowManager;
	} 

	
	@Override
	public Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	private final DockableWindowManagerImpl dockableWindowManager;
	private final boolean clone;
	private DockableWindowManagerImpl.Entry entry;
	private final JButton menu;
	

	
	class MouseHandler extends MouseAdapter
	{
		JPopupMenu popup;

		@Override
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
	public void propertyChange(PropertyChangeEvent evt)
	{
		if (dockableName == null) return;
		String pn = evt.getPropertyName();
		if (pn.startsWith(dockableName) && pn.endsWith("title"))
			setTitle(evt.getNewValue().toString());
	}
	
}

