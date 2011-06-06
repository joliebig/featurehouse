

package org.gjt.sp.jedit.browser;


import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Set;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.VFSFile;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.ActionContext;
import org.gjt.sp.jedit.EditAction;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.Log;



public class VFSDirectoryEntryTable extends JTable
{
	
	public VFSDirectoryEntryTable(BrowserView browserView)
	{
		super(new VFSDirectoryEntryTableModel());
		this.browserView = browserView;
		setShowGrid(false);

		setIntercellSpacing(new Dimension(0,0));

		setDefaultRenderer(VFSDirectoryEntryTableModel.Entry.class,
			renderer = new FileCellRenderer());

		header = getTableHeader();
		header.setReorderingAllowed(false);
		header.addMouseListener(new MouseHandler());
		header.setDefaultRenderer(new HeaderRenderer(
			(DefaultTableCellRenderer)header.getDefaultRenderer()));

		setRowSelectionAllowed(true);

		getColumnModel().addColumnModelListener(new ColumnHandler());

		setAutoResizeMode(AUTO_RESIZE_OFF);
	} 

	
	public boolean selectFile(String path)
	{
		for(int i = 0; i < getRowCount(); i++)
		{
			VFSDirectoryEntryTableModel.Entry entry =
				(VFSDirectoryEntryTableModel.Entry)
				getValueAt(i,1);
			if(entry.dirEntry.getPath().equals(path))
			{
				setSelectedRow(i);
				return true;
			}
		}

		return false;
	} 

	
	public void doTypeSelect(String str, boolean dirsOnly)
	{
		if(str.length() == 0)
			clearSelection();
		else if(getSelectedRow() == -1)
			doTypeSelect(str,0,getRowCount(),dirsOnly);
		else
		{
			int start = getSelectionModel().getMaxSelectionIndex();
			boolean retVal = doTypeSelect(str,start,getRowCount(),
				dirsOnly);

			if(!retVal)
			{
				
				
				doTypeSelect(str,0,start,dirsOnly);
			}
		}
	} 

	
	public VFSFile[] getSelectedFiles()
	{
		VFSDirectoryEntryTableModel model
			= (VFSDirectoryEntryTableModel)getModel();

		LinkedList returnValue = new LinkedList();
		int[] selectedRows = getSelectedRows();
		for(int i = 0; i < selectedRows.length; i++)
		{
			returnValue.add(model.files[selectedRows[i]].dirEntry);
		}
		return (VFSFile[])returnValue.toArray(new
		VFSFile[returnValue.size()]);
	} 

	
	public void getExpandedDirectories(Set set)
	{
		VFSDirectoryEntryTableModel model
			= (VFSDirectoryEntryTableModel)getModel();

		if(model.files != null)
		{
			for(int i = 0; i < model.files.length; i++)
			{
				if(model.files[i].expanded)
					set.add(model.files[i].dirEntry.getPath());
			}
		}
	} 

	
	public void toggleExpanded(final int row)
	{
		VFSDirectoryEntryTableModel model
		= (VFSDirectoryEntryTableModel)getModel();

		VFSDirectoryEntryTableModel.Entry entry = model.files[row];
		if(entry.dirEntry.getType() == VFSFile.FILE)
			return;

		if(entry.expanded)
		{
			model.collapse(VFSManager.getVFSForPath(
				entry.dirEntry.getPath()),row);
			resizeColumns();
		}
		else
		{
			browserView.clearExpansionState();
			browserView.loadDirectory(entry,entry.dirEntry.getPath(),
				false);
		}

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				setSelectedRow(row);
			}
		});
	} 

	
	public void setDirectory(VFS vfs, Object node, ArrayList list,
		Set tmpExpanded)
	{
		timer.stop();
		typeSelectBuffer.setLength(0);

		VFSDirectoryEntryTableModel model = ((VFSDirectoryEntryTableModel)getModel());
		int startIndex;
		if(node == null)
		{
			startIndex = 0;
			model.setRoot(vfs,list);
		}
		else
		{
			startIndex =
				model.expand(
				vfs,
				(VFSDirectoryEntryTableModel.Entry)node,
				list);
			startIndex++;
		}

		for(int i = 0; i < list.size(); i++)
		{
			VFSDirectoryEntryTableModel.Entry e
				= model.files[startIndex + i];
			String path = e.dirEntry.getPath();
			if(tmpExpanded.contains(path))
			{
				browserView.loadDirectory(e,path,false);
				tmpExpanded.remove(path);
			}
		}

		resizeColumns();
	} 

	
	public void maybeReloadDirectory(String path)
	{
		VFSDirectoryEntryTableModel model
		= (VFSDirectoryEntryTableModel)getModel();

		for(int i = 0; i < model.files.length; i++)
		{
			VFSDirectoryEntryTableModel.Entry e = model.files[i];
			if(!e.expanded || e.dirEntry.getType() == VFSFile.FILE)
				continue;

			VFSFile dirEntry = e.dirEntry;
			
			String otherPath;
			if(dirEntry.getSymlinkPath() == null)
				otherPath = dirEntry.getPath();
			else
				otherPath = dirEntry.getSymlinkPath();
			if(MiscUtilities.pathsEqual(path,otherPath))
			{
				browserView.saveExpansionState();
				browserView.loadDirectory(e,path,false);
				return;
			}
		}
	} 

	
	public void propertiesChanged()
	{
		renderer.propertiesChanged();

		VFSFile template = new VFSFile(
			"foo","foo","foo",VFSFile.FILE,0L,false);
		setRowHeight(renderer.getTableCellRendererComponent(
			this,new VFSDirectoryEntryTableModel.Entry(template,0),
			false,false,0,0).getPreferredSize().height);
		Dimension prefSize = getPreferredSize();
		setPreferredScrollableViewportSize(new Dimension(prefSize.width,
			getRowHeight() * 12));
	} 

	
	public void scrollRectToVisible(Rectangle rect)
	{
		
		rect.width = 0;
		super.scrollRectToVisible(rect);
	} 

	
	public void processKeyEvent(KeyEvent evt)
	{
		if(evt.getID() == KeyEvent.KEY_PRESSED)
		{
			VFSDirectoryEntryTableModel model =
				(VFSDirectoryEntryTableModel)getModel();
			int row = getSelectedRow();
			ActionContext ac = VFSBrowser.getActionContext();
			ActionContext jac = jEdit.getActionContext();
			VFSBrowser browser = browserView.getBrowser();
			switch(evt.getKeyCode())
			{
			case KeyEvent.VK_LEFT:
				evt.consume();
				if(row != -1)
				{
					if(model.files[row].expanded)
					{
						toggleExpanded(row);
						return;
					}

					for(int i = row - 1; i >= 0; i--)
					{
						if((model.files[i].expanded) && (model.files[i].level < model.files[row].level))
						{
							setSelectedRow(i);
							return;
						}
					}
				}

				String dir = browserView.getBrowser()
					.getDirectory();
				dir = MiscUtilities.getParentOfPath(dir);
				browserView.getBrowser().setDirectory(dir);
				break;
			case KeyEvent.VK_BACK_SPACE:
				evt.consume();
				EditAction ea = ac.getAction("vfs.browser.up");
				ac.invokeAction(evt, ea);
				break;
			case KeyEvent.VK_DELETE:
				evt.consume();
				ea = ac.getAction("vfs.browser.delete");
				ac.invokeAction(evt, ea);
				break;
			case KeyEvent.CTRL_MASK | KeyEvent.VK_N:  
				evt.consume();
				ea = ac.getAction("vfs.browser.new-file");
				ac.invokeAction(evt, ea);
				break;
			case KeyEvent.VK_INSERT:
				evt.consume();
				ea = ac.getAction("vfs.browser.new-directory");
				ac.invokeAction(evt, ea);
				break;
			case KeyEvent.VK_ESCAPE:
				ea = jac.getAction("close-docking-area");
				ea.invoke(jEdit.getActiveView());
				evt.consume();
				break;
			case KeyEvent.VK_F2:
				ea = ac.getAction("vfs.browser.rename");
				evt.consume();
				ac.invokeAction(evt, ea);
				break;
		          case KeyEvent.VK_F5:
                               evt.consume();
                               ea = ac.getAction("vfs.browser.reload");
                               ac.invokeAction(evt, ea);
                               break;
		          case KeyEvent.VK_F6:
		          case KeyEvent.VK_TAB:
		        	  browser.focusOnDefaultComponent();
		        	  evt.consume();
		        	  break;
		          case KeyEvent.VK_RIGHT:
				evt.consume();
				if(row != -1)
				{
					if(!model.files[row].expanded)
						toggleExpanded(row);
				}

				break;
			case KeyEvent.VK_ENTER:
				evt.consume();
				browserView.getBrowser().filesActivated(
					(evt.isShiftDown()
					? VFSBrowser.M_OPEN_NEW_VIEW
					: VFSBrowser.M_OPEN),false);

				break;
			}
		}
		else if(evt.getID() == KeyEvent.KEY_TYPED)
		{

			if(evt.isControlDown() || evt.isAltDown()
				|| evt.isMetaDown())
			{
				evt.consume();
				return;
			}

			
			if(evt.isShiftDown() && evt.getKeyChar() == '\n') {
				evt.consume();
				return;
			}


			VFSBrowser browser = browserView.getBrowser();

			switch(evt.getKeyChar())
			{
			case '~':
				evt.consume();
				if(browser.getMode() == VFSBrowser.BROWSER)
					browser.setDirectory(System.getProperty(
						"user.home"));
				break;
			case '/':
				evt.consume();
				if(browser.getMode() == VFSBrowser.BROWSER)
					browser.rootDirectory();
				break;
			case '-':
				evt.consume();
				if(browser.getMode() == VFSBrowser.BROWSER)
				{
					browser.setDirectory(
						browser.getView().getBuffer()
						.getDirectory());
				}
				break;
			default:
				evt.consume();
				typeSelectBuffer.append(evt.getKeyChar());
				doTypeSelect(typeSelectBuffer.toString(),
					browser.getMode() == VFSBrowser
					.CHOOSE_DIRECTORY_DIALOG);

				timer.stop();
				timer.setInitialDelay(750);
				timer.setRepeats(false);
				timer.start();
				return;
			}
		}

		if(!evt.isConsumed())
			super.processKeyEvent(evt);
	} 

	
	public void setSelectedRow(int row)
	{
		getSelectionModel().setSelectionInterval(row,row);
		scrollRectToVisible(getCellRect(row,0,true));
	} 

	
	private BrowserView browserView;
	private JTableHeader header;
	private FileCellRenderer renderer;
	private StringBuffer typeSelectBuffer = new StringBuffer();
	private Timer timer = new Timer(0,new ClearTypeSelect());
	private boolean resizingColumns;

	
	private boolean doTypeSelect(String str, int start, int end,
		boolean dirsOnly)
	{
		VFSFile[] files = ((VFSDirectoryEntryTableModel)
			getModel()).getFiles();

		int index = VFSFile.findCompletion(files,start,end,str,dirsOnly);
		if(index != -1)
		{
			setSelectedRow(index);
			return true;
		}
		else
			return false;
	} 

	
	private void resizeColumns()
	{
		VFSDirectoryEntryTableModel model = (VFSDirectoryEntryTableModel)getModel();

		FontRenderContext fontRenderContext = new FontRenderContext(
			null,false,false);
		int[] widths = new int[model.getColumnCount()];
		for(int i = 0; i < widths.length; i++)
		{
			String columnName = model.getColumnName(i);
			if(columnName != null)
			{
				widths[i] = (int)renderer.plainFont
					.getStringBounds(columnName,
					fontRenderContext).getWidth();
			}
		}

		for(int i = 1; i < widths.length; i++)
		{
			String extAttr = model.getExtendedAttribute(i);
			widths[i] = Math.max(widths[i],model.getColumnWidth(i));
		}

		for(int i = 0; i < model.files.length; i++)
		{
			VFSDirectoryEntryTableModel.Entry entry
				= model.files[i];
			Font font = (entry.dirEntry.getType()
				== VFSFile.FILE
				? renderer.plainFont : renderer.boldFont);

			widths[0] = Math.max(widths[0],renderer.getEntryWidth(
				entry,font,fontRenderContext));
		}

		widths[0] += 10;

		TableColumnModel columns = getColumnModel();

		try
		{
			resizingColumns = true;
			for(int i = 0; i < widths.length; i++)
			{
				columns.getColumn(i).setPreferredWidth(widths[i]);
				columns.getColumn(i).setWidth(widths[i]);
			}
		}
		finally
		{
			resizingColumns = false;
		}

		doLayout();
	} 

	
	private void saveWidths()
	{
		if(resizingColumns)
			return;

		VFSDirectoryEntryTableModel model = (VFSDirectoryEntryTableModel)getModel();
		TableColumnModel columns = getColumnModel();

		for(int i = 1; i < model.getColumnCount(); i++)
			model.setColumnWidth(i,columns.getColumn(i).getWidth());
	} 

	

	
	class ClearTypeSelect implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			typeSelectBuffer.setLength(0);
		}
	} 

	
	class ColumnHandler implements TableColumnModelListener
	{
		public void columnAdded(TableColumnModelEvent e) {}
		public void columnRemoved(TableColumnModelEvent e) {}
		public void columnMoved(TableColumnModelEvent e) {}
		public void columnSelectionChanged(ListSelectionEvent e) {}

		public void columnMarginChanged(ChangeEvent e)
		{
			saveWidths();
		}
	} 

	
	class MouseHandler extends MouseInputAdapter
	{
		public void mousePressed(MouseEvent evt)
		{
			
			if (evt.getSource() == header && evt.getClickCount() == 2)
			{
				VFSDirectoryEntryTableModel model = (VFSDirectoryEntryTableModel) header.getTable().getModel();
				TableColumnModel columnModel = header.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(evt.getX());
				int column = columnModel.getColumn(viewColumn).getModelIndex();
				if(model.sortByColumn(column))
					resizeColumns();
				Log.log(Log.DEBUG,this,"VFSDirectoryEntryTable sorted by " + model.getColumnName(column)
					+ (model.getAscending() ? " ascending" : " descending") );
				return;
			}
		}
	} 

	
	static class HeaderRenderer extends DefaultTableCellRenderer
	{
		private DefaultTableCellRenderer tcr;

		public HeaderRenderer(DefaultTableCellRenderer tcr)
		{
			this.tcr = tcr;
		}

		public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasFocus, int row, int column)
		{
			JLabel l = (JLabel)tcr.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
			VFSDirectoryEntryTableModel model = (VFSDirectoryEntryTableModel)table.getModel();
			Icon icon = (column == model.getSortColumn())
				? model.getAscending() ? ASC_ICON : DESC_ICON
				: null;
			l.setIcon(icon);
			
			return l;
		}
	} 

	

	static final Icon ASC_ICON  = GUIUtilities.loadIcon("arrow-asc.png");
	static final Icon DESC_ICON = GUIUtilities.loadIcon("arrow-desc.png");

	

}
