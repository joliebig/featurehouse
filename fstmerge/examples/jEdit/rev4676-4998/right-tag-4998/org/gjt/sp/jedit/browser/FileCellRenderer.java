

package org.gjt.sp.jedit.browser;


import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.table.*;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.*;


public class FileCellRenderer extends DefaultTableCellRenderer
{
	public static Icon fileIcon = GUIUtilities.loadIcon("File.png");
	public static Icon openFileIcon = GUIUtilities.loadIcon("OpenFile.png");
	public static Icon dirIcon = GUIUtilities.loadIcon("Folder.png");
	public static Icon openDirIcon = GUIUtilities.loadIcon("OpenFolder.png");
	public static Icon filesystemIcon = GUIUtilities.loadIcon("DriveSmall.png");
	public static Icon loadingIcon = GUIUtilities.loadIcon("ReloadSmall.png");

	
	public FileCellRenderer()
	{
		plainFont = UIManager.getFont("Tree.font");
		if(plainFont == null)
			plainFont = jEdit.getFontProperty("metal.secondary.font");
		boldFont = plainFont.deriveFont(Font.BOLD);
	} 

	
	public Component getTableCellRendererComponent(JTable table,
		Object value, boolean isSelected, boolean hasFocus, 
		int row, int column)
	{
		super.getTableCellRendererComponent(table,value,isSelected,
			hasFocus,row,column);

		if(value instanceof VFSDirectoryEntryTableModel.Entry)
		{
			VFSDirectoryEntryTableModel.Entry entry =
				(VFSDirectoryEntryTableModel.Entry)value;
			VFS.DirectoryEntry file = entry.dirEntry;

			setFont(file.type == VFS.DirectoryEntry.FILE
				? plainFont : boldFont);

			colorDetermined = false;
			this.isSelected = isSelected;
			this.file = file;

			if(column == 0)
			{
				underlined = false;
				if(file.type == VFS.DirectoryEntry.FILE)
					setIcon(null);
				else if(entry.expanded)
					setIcon(UIManager.getIcon("Tree.expandedIcon"));
				else
					setIcon(UIManager.getIcon("Tree.collapsedIcon"));
				setText(null);
				setBorder(new EmptyBorder(1,1,1,1));
			}
			else if(column == 1)
			{
				underlined = (jEdit.getBuffer(file.path) != null);

				setIcon(showIcons
					? getIconForFile(file,entry.expanded)
					: null);
				setText(file.name);
				setBorder(new EmptyBorder(1,entry.level * 10,1,1));
			}
			else
			{
				VFSDirectoryEntryTableModel model = (VFSDirectoryEntryTableModel)table.getModel();
				String extAttr = model.getExtendedAttribute(column);

				underlined = false;
				setIcon(null);
				setText(file.getExtendedAttribute(extAttr));
				setBorder(new EmptyBorder(1,1,1,1));
			}
		}

		return this;
	} 

	
	public void paintComponent(Graphics g)
	{
		if(!colorDetermined)
		{
			if(!isSelected)
			{
				Color color = file.getColor();

				setForeground(color == null
					? UIManager.getColor("Tree.foreground")
					: color);
			}
		}

		super.paintComponent(g);

		if(underlined)
		{
			Font font = getFont();

			FontMetrics fm = getFontMetrics(font);
			int x, y;
			if(getIcon() == null)
			{
				x = 0;
				y = fm.getAscent() + 2;
			}
			else
			{
				x = getIcon().getIconWidth() + getIconTextGap();
				y = Math.max(fm.getAscent() + 2,16);
			}

			Insets border = getBorder().getBorderInsets(this);
			x += border.left;

			g.setColor(getForeground());
			g.drawLine(x,y,x + fm.stringWidth(getText()),y);
		}
	} 

	
	public static Icon getIconForFile(VFS.DirectoryEntry file, boolean expanded)
	{
		if(file.type == VFS.DirectoryEntry.DIRECTORY)
			return (expanded ? openDirIcon : dirIcon);
		else if(file.type == VFS.DirectoryEntry.FILESYSTEM)
			return filesystemIcon;
		else if(jEdit.getBuffer(file.path) != null)
			return openFileIcon;
		else
			return fileIcon;
	} 

	
	boolean showIcons;

	
	void propertiesChanged()
	{
		showIcons = jEdit.getBooleanProperty("vfs.browser.showIcons");
	} 

	

	
	private Font plainFont;
	private Font boldFont;

	private boolean underlined;
	private boolean colorDetermined;
	private boolean isSelected;
	private VFS.DirectoryEntry file;
	
}
