

package org.gjt.sp.jedit.gui;


import java.awt.Component;
import java.awt.Font;
import java.util.Vector;
import javax.swing.*;
import javax.swing.table.*;



public class JCheckBoxList extends JTable
{
	
	
	public JCheckBoxList(Object[] items)
	{
		setModel(items);
	} 

	
	
	public JCheckBoxList(Vector items)
	{
		setModel(items);
	} 

	
	
	public void setModel(Object[] items)
	{
		setModel(new CheckBoxListModel(items));
		init();
	} 

	
	
	public void setModel(Vector items)
	{
		setModel(new CheckBoxListModel(items));
		init();
	} 

	
	public Object[] getCheckedValues()
	{
		Vector values = new Vector();
		CheckBoxListModel model = (CheckBoxListModel)getModel();
		for(int i = 0; i < model.items.size(); i++)
		{
			Entry entry = (Entry)model.items.elementAt(i);
			if(entry.checked && !entry.caption)
				values.addElement(entry.value);
		}

		Object[] retVal = new Object[values.size()];
		values.copyInto(retVal);
		return retVal;
	} 

	
	public void selectAll()
	{
		CheckBoxListModel model = (CheckBoxListModel)getModel();
		for(int i = 0; i < model.items.size(); i++)
		{
			Entry entry = (Entry)model.items.elementAt(i);
			if(!entry.caption)
				entry.checked = true;
		}

		model.fireTableRowsUpdated(0,model.getRowCount());
	} 

	
	public Entry[] getValues()
	{
		CheckBoxListModel model = (CheckBoxListModel)getModel();
		Entry[] retVal = new Entry[model.items.size()];
		model.items.copyInto(retVal);
		return retVal;
	} 

	
	public Object getSelectedValue()
	{
		int row = getSelectedRow();
		if(row == -1)
			return null;
		else
			return getModel().getValueAt(row,1);
	} 

	
	public TableCellRenderer getCellRenderer(int row, int column)
	{
		if(column == 0)
		{
			Entry entry = (Entry)((CheckBoxListModel)getModel()).items.get(row);
			if(entry.caption)
				return dummy;
		}

		return super.getCellRenderer(row,column);
	} 

	
	private TableCellRenderer dummy;

	
	private void init()
	{
		dummy = new DummyRenderer();
		getSelectionModel().setSelectionMode(ListSelectionModel
			.SINGLE_SELECTION);
		setShowGrid(false);
		setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		TableColumn column = getColumnModel().getColumn(0);
		int checkBoxWidth = new JCheckBox().getPreferredSize().width;
		column.setPreferredWidth(checkBoxWidth);
		column.setMinWidth(checkBoxWidth);
		column.setWidth(checkBoxWidth);
		column.setMaxWidth(checkBoxWidth);
		column.setResizable(false);

		column = getColumnModel().getColumn(1);
		column.setCellRenderer(new LabelRenderer());
	} 

	

	
	
	public static class Entry
	{
		boolean checked;
		boolean caption;
		Object value;

		public Entry(Object value)
		{
			this.caption = true;
			this.value = value;
		}

		public Entry(boolean checked, Object value)
		{
			this.checked = checked;
			this.value = value;
		}

		public boolean isChecked()
		{
			return checked;
		}

		public Object getValue()
		{
			return value;
		}
	} 

	
	private class DummyRenderer extends DefaultTableCellRenderer
	{
		public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasFocus, int row, int column)
		{
			return super.getTableCellRendererComponent(table,null ,
				isSelected,false ,row,column);
		}
	} 

	
	private class LabelRenderer extends DefaultTableCellRenderer
	{
		Font plainFont, boldFont;

		LabelRenderer()
		{
			plainFont = UIManager.getFont("Tree.font");
			boldFont = plainFont.deriveFont(Font.BOLD);
		}

		public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasFocus, int row, int column)
		{
			super.getTableCellRendererComponent(table,value,isSelected,
				hasFocus,row,column);

			Entry entry = (Entry)((CheckBoxListModel)getModel()).items.get(row);
			if(entry.caption)
				setFont(boldFont);
			else
				setFont(plainFont);
			return this;
		}
	} 
}

class CheckBoxListModel extends AbstractTableModel
{
	Vector items;

	CheckBoxListModel(Vector _items)
	{
		items = new Vector(_items.size());
		for(int i = 0; i < _items.size(); i++)
		{
			items.addElement(createEntry(_items.elementAt(i)));
		}
	}

	CheckBoxListModel(Object[] _items)
	{
		items = new Vector(_items.length);
		for(int i = 0; i < _items.length; i++)
		{
			items.addElement(createEntry(_items[i]));
		}
	}

	private JCheckBoxList.Entry createEntry(Object obj)
	{
		if(obj instanceof JCheckBoxList.Entry)
			return (JCheckBoxList.Entry)obj;
		else
			return new JCheckBoxList.Entry(false,obj);
	}

	public int getRowCount()
	{
		return items.size();
	}

	public int getColumnCount()
	{
		return 2;
	}

	public String getColumnName(int col)
	{
		return null;
	}

	public Object getValueAt(int row, int col)
	{
		JCheckBoxList.Entry entry = (JCheckBoxList.Entry)items.elementAt(row);
		switch(col)
		{
		case 0:
			return new Boolean(entry.checked);
		case 1:
			return entry.value;
		default:
			throw new InternalError();
		}
	}

	public Class getColumnClass(int col)
	{
		switch(col)
		{
		case 0:
			return Boolean.class;
		case 1:
			return String.class;
		default:
			throw new InternalError();
		}
	}

	public boolean isCellEditable(int row, int col)
	{
		JCheckBoxList.Entry entry = (JCheckBoxList.Entry)items.elementAt(row);
		return col == 0 && !entry.caption;
	}

	public void setValueAt(Object value, int row, int col)
	{
		if(col == 0)
		{
			JCheckBoxList.Entry entry = (JCheckBoxList.Entry)items.elementAt(row);
			if(!entry.caption)
			{
				entry.checked = (value.equals(Boolean.TRUE));
				fireTableRowsUpdated(row,row);
			}
		}
	}
}
