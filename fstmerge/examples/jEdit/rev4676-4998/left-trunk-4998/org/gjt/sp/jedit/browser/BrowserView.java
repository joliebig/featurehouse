

package org.gjt.sp.jedit.browser;


import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import java.util.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;



class BrowserView extends JPanel
{
	
	public BrowserView(final VFSBrowser browser)
	{
		this.browser = browser;

		tmpExpanded = new HashSet();

		parentDirectories = new JList();

		parentDirectories.getSelectionModel().setSelectionMode(
			ListSelectionModel.SINGLE_SELECTION);

		parentDirectories.setCellRenderer(new ParentDirectoryRenderer());
		parentDirectories.setVisibleRowCount(5);
		parentDirectories.addMouseListener(new ParentMouseHandler());

		final JScrollPane parentScroller = new JScrollPane(parentDirectories);
		parentScroller.setMinimumSize(new Dimension(0,0));

		table = new VFSDirectoryEntryTable(this);
		table.addMouseListener(new TableMouseHandler());
		JScrollPane tableScroller = new JScrollPane(table);
		tableScroller.setMinimumSize(new Dimension(0,0));
		tableScroller.getViewport().setBackground(table.getBackground());
		tableScroller.getViewport().addMouseListener(new TableMouseHandler());
		splitPane = new JSplitPane(
			browser.isHorizontalLayout()
			? JSplitPane.HORIZONTAL_SPLIT : JSplitPane.VERTICAL_SPLIT,
			parentScroller,tableScroller);
		splitPane.setOneTouchExpandable(true);

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				String prop = browser.isHorizontalLayout() ? "vfs.browser.horizontalSplitter" : "vfs.browser.splitter";
				int loc = jEdit.getIntegerProperty(prop,-1);
				if(loc == -1)
					loc = parentScroller.getPreferredSize().height;

				splitPane.setDividerLocation(loc);
				parentDirectories.ensureIndexIsVisible(
					parentDirectories.getModel()
					.getSize());
			}
		});

		if(browser.isMultipleSelectionEnabled())
			table.getSelectionModel().setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		else
			table.getSelectionModel().setSelectionMode(
				ListSelectionModel.SINGLE_SELECTION);

		setLayout(new BorderLayout());

		add(BorderLayout.CENTER,splitPane);

		propertiesChanged();
	} 

	
	public void focusOnFileView()
	{
		table.requestFocus();
	} 

	
	public void removeNotify()
	{
		String prop = browser.isHorizontalLayout() ? "vfs.browser.horizontalSplitter" : "vfs.browser.splitter";
		jEdit.setIntegerProperty(prop,splitPane.getDividerLocation());

		super.removeNotify();
	} 

	
	public VFS.DirectoryEntry[] getSelectedFiles()
	{
		return table.getSelectedFiles();
	} 

	
	public void selectNone()
	{
		table.clearSelection();
	} 

	
	public void saveExpansionState()
	{
		tmpExpanded.clear();
		table.getExpandedDirectories(tmpExpanded);
	} 

	
	public void clearExpansionState()
	{
		tmpExpanded.clear();
	} 

	
	public void loadDirectory(Object node, String path)
	{
		path = MiscUtilities.constructPath(browser.getDirectory(),path);
		VFS vfs = VFSManager.getVFSForPath(path);

		Object session = vfs.createVFSSession(path,this);
		if(session == null)
			return;

		if(node == null)
		{
			parentDirectories.setListData(new Object[] {
				new LoadingPlaceholder() });
		}

		Object[] loadInfo = new Object[2];

		VFSManager.runInWorkThread(new BrowserIORequest(
			BrowserIORequest.LIST_DIRECTORY,browser,
			session,vfs,path,null,node,loadInfo));
		browser.directoryLoaded(node,loadInfo);
	} 

	
	public void directoryLoaded(Object node, String path, ArrayList directory)
	{
		
		if(node == null)
		{
			DefaultListModel parentList = new DefaultListModel();

			String parent = path;

			for(;;)
			{
				VFS _vfs = VFSManager.getVFSForPath(
					parent);
				
				
				
				
				parentList.insertElementAt(new VFS.DirectoryEntry(
					_vfs.getFileName(parent),
					parent,parent,
					VFS.DirectoryEntry.DIRECTORY,
					0L,false),0);
				String newParent = _vfs.getParentOfPath(parent);

				if(newParent == null ||
					VFSBrowser.pathsEqual(parent,newParent))
					break;
				else
					parent = newParent;
			}

			parentDirectories.setModel(parentList);
			int index = parentList.getSize() - 1;
			parentDirectories.setSelectedIndex(index);
			parentDirectories.ensureIndexIsVisible(index);
		} 

		table.setDirectory(VFSManager.getVFSForPath(path),
			node,directory,tmpExpanded);
	} 

	
	public void updateFileView()
	{
		table.repaint();
	} 

	
	public void maybeReloadDirectory(String path)
	{
		String browserDir = browser.getDirectory();
		String symlinkBrowserDir;
		if(MiscUtilities.isURL(browserDir))
		{
			symlinkBrowserDir = browserDir;
		}
		else
		{
			symlinkBrowserDir = MiscUtilities.resolveSymlinks(
				browserDir);
		}

		if(VFSBrowser.pathsEqual(path,symlinkBrowserDir))
		{
			saveExpansionState();
			loadDirectory(null,browserDir);
		}

		
		
		
		
		
		
		
		
		
		
		

		if(!browserDir.startsWith(FavoritesVFS.PROTOCOL)
			&& !browserDir.startsWith(FileRootsVFS.PROTOCOL)
			&& !path.startsWith(symlinkBrowserDir))
			return;

		if(browserDir.startsWith(FileRootsVFS.PROTOCOL)
			&& MiscUtilities.isURL(path)
			&& !MiscUtilities.getProtocolOfURL(path)
			.equals("file"))
			return;

		table.maybeReloadDirectory(path);
	} 

	
	public void propertiesChanged()
	{
		showIcons = jEdit.getBooleanProperty("vfs.browser.showIcons");
		table.propertiesChanged();

		splitPane.setBorder(null);
	} 

	
	
	public VFSBrowser getBrowser()
	{
		return browser;
	} 

	
	public VFSDirectoryEntryTable getTable()
	{
		return table;
	} 

	
	public JList getParentDirectoryList()
	{
		return parentDirectories;
	} 

	

	
	private VFSBrowser browser;

	private JSplitPane splitPane;
	private JList parentDirectories;
	private VFSDirectoryEntryTable table;
	private Set tmpExpanded;
	private BrowserCommandsMenu popup;
	private boolean showIcons;
	

	
	private void showFilePopup(VFS.DirectoryEntry[] files, Component comp,
		Point point)
	{
		popup = new BrowserCommandsMenu(browser,files);
		
		
		
		popup.addPopupMenuListener(new PopupMenuListener()
		{
			public void popupMenuCanceled(PopupMenuEvent e) {}

			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {}

			public void popupMenuWillBecomeInvisible(PopupMenuEvent e)
			{
				
				
				
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						int index = parentDirectories
							.getModel()
							.getSize() - 1;
						parentDirectories.setSelectedIndex(index);
					}
				});
			}
		});
		GUIUtilities.showPopupMenu(popup,comp,point.x,point.y);
	} 

	

	

	
	class ParentDirectoryRenderer extends DefaultListCellRenderer
	{
		Font plainFont, boldFont;

		ParentDirectoryRenderer()
		{
			plainFont = UIManager.getFont("Tree.font");
			if(plainFont == null)
				plainFont = jEdit.getFontProperty("metal.secondary.font");
			boldFont = new Font(plainFont.getName(),Font.BOLD,plainFont.getSize());
		}

		public Component getListCellRendererComponent(
			JList list,
			Object value,
			int index,
			boolean isSelected,
			boolean cellHasFocus)
		{
			super.getListCellRendererComponent(list,value,index,
				isSelected,cellHasFocus);

			ParentDirectoryRenderer.this.setBorder(new EmptyBorder(
				1,index * 5 + 1,1,1));

			if(value instanceof LoadingPlaceholder)
			{
				ParentDirectoryRenderer.this.setFont(plainFont);

				setIcon(showIcons ? FileCellRenderer.loadingIcon : null);
				setText(jEdit.getProperty("vfs.browser.tree.loading"));
			}
			else if(value instanceof VFS.DirectoryEntry)
			{
				VFS.DirectoryEntry dirEntry = (VFS.DirectoryEntry)value;
				ParentDirectoryRenderer.this.setFont(boldFont);

				setIcon(showIcons ? FileCellRenderer.getIconForFile(dirEntry,true)
					: null);
				setText(dirEntry.name);
			}
			else if(value == null)
				setText("VFS does not follow VFS API");

			return this;
		}
	} 

	
	class ParentMouseHandler extends MouseAdapter
	{
		public void mousePressed(MouseEvent evt)
		{
			int row = parentDirectories.locationToIndex(evt.getPoint());
			if(row != -1)
			{
				Object obj = parentDirectories.getModel()
					.getElementAt(row);
				if(obj instanceof VFS.DirectoryEntry)
				{
					VFS.DirectoryEntry dirEntry = ((VFS.DirectoryEntry)obj);
					if(GUIUtilities.isPopupTrigger(evt))
					{
						if(popup != null && popup.isVisible())
						{
							popup.setVisible(false);
							popup = null;
						}
						else
						{
							parentDirectories.setSelectedIndex(row);
							showFilePopup(new VFS.DirectoryEntry[] {
								dirEntry },parentDirectories,
								evt.getPoint());
						}
					}
				}
			}
		}

		public void mouseReleased(MouseEvent evt)
		{
			if(evt.getClickCount() % 2 != 0 &&
				!GUIUtilities.isMiddleButton(evt.getModifiers()))
				return;

			int row = parentDirectories.locationToIndex(evt.getPoint());
			if(row != -1)
			{
				Object obj = parentDirectories.getModel()
					.getElementAt(row);
				if(obj instanceof VFS.DirectoryEntry)
				{
					VFS.DirectoryEntry dirEntry = ((VFS.DirectoryEntry)obj);
					if(!GUIUtilities.isPopupTrigger(evt))
					{
						browser.setDirectory(dirEntry.path);
						if(browser.getMode() == VFSBrowser.BROWSER)
						focusOnFileView();
					}
				}
			}
		}
	} 

	
	class TableMouseHandler extends MouseAdapter
	{
		
		public void mouseClicked(MouseEvent evt)
		{
			Point p = evt.getPoint();
			int row = table.rowAtPoint(p);
			int column = table.columnAtPoint(p);
			if(row == -1)
				return;
			if(column == 0)
			{
				VFSDirectoryEntryTableModel.Entry entry
					= (VFSDirectoryEntryTableModel.Entry)
					table.getModel().getValueAt(row,0);
				if(FileCellRenderer.ExpansionToggleBorder
					.isExpansionToggle(entry.level,p.x))
				{
					return;
				}
			}

			if((evt.getModifiers() & MouseEvent.BUTTON1_MASK) != 0
				&& evt.getClickCount() % 2 == 0)
			{
				browser.filesActivated((evt.isShiftDown()
					? VFSBrowser.M_OPEN_NEW_VIEW
					: VFSBrowser.M_OPEN),true);
			}
			else if(GUIUtilities.isMiddleButton(evt.getModifiers()))
			{
				if(evt.isShiftDown())
					table.getSelectionModel().addSelectionInterval(row,row);
				else
					table.getSelectionModel().setSelectionInterval(row,row);
				browser.filesActivated((evt.isShiftDown()
					? VFSBrowser.M_OPEN_NEW_VIEW
					: VFSBrowser.M_OPEN),true);
			}
		} 

		
		public void mousePressed(MouseEvent evt)
		{
			Point p = evt.getPoint();
			if(evt.getSource() != table)
			{
				p.x -= table.getX();
				p.y -= table.getY();
			}

			int row = table.rowAtPoint(p);
			int column = table.columnAtPoint(p);
			if(column == 0 && row != -1)
			{
				VFSDirectoryEntryTableModel.Entry entry
					= (VFSDirectoryEntryTableModel.Entry)
					table.getModel().getValueAt(row,0);
				if(FileCellRenderer.ExpansionToggleBorder
					.isExpansionToggle(entry.level,p.x))
				{
					table.toggleExpanded(row);
					return;
				}
			}

			if(GUIUtilities.isMiddleButton(evt.getModifiers()))
			{
				if(row == -1)
					;
				else if(evt.isShiftDown())
					table.getSelectionModel().addSelectionInterval(row,row);
				else
					table.getSelectionModel().setSelectionInterval(row,row);
			}
			else if(GUIUtilities.isPopupTrigger(evt))
			{
				if(popup != null && popup.isVisible())
				{
					popup.setVisible(false);
					popup = null;
					return;
				}

				if(row == -1)
					showFilePopup(null,table,evt.getPoint());
				else
				{
					if(!table.getSelectionModel().isSelectedIndex(row))
						table.getSelectionModel().setSelectionInterval(row,row);
					showFilePopup(getSelectedFiles(),table,evt.getPoint());
				}
			}
		} 

		
		public void mouseReleased(MouseEvent evt)
		{
			if(!GUIUtilities.isPopupTrigger(evt)
				&& table.getSelectedRow() != -1)
			{
				browser.filesSelected();
			}
		} 
	} 

	static class LoadingPlaceholder {}
	
}
