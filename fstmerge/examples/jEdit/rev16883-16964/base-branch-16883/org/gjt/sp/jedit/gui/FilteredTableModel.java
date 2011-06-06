
package org.gjt.sp.jedit.gui;

import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
import java.util.*;


public abstract class FilteredTableModel<E extends TableModel> extends AbstractTableModel implements TableModelListener
{
	
	protected E delegated;

	private Vector<Integer> filteredIndices;

	
	private Map<Integer, Integer> invertedIndices;

	private String filter;

	private JTable table;

	
	protected FilteredTableModel(E delegated)
	{
		this.delegated = delegated;
		delegated.addTableModelListener(this);
		resetFilter();
	} 

	
	
	public void setTable(JTable table)
	{
		if (table.getModel() != this)
			throw new IllegalArgumentException("The given table " + table + " doesn't use this model " + this);
		this.table = table;
	} 


	
	public E getDelegated()
	{
		return delegated;
	} 

	
	public void setDelegated(E delegated)
	{
		this.delegated.removeTableModelListener(this);
		delegated.addTableModelListener(this);
		this.delegated = delegated;
	} 

	
	private void resetFilter()
	{
		filteredIndices = null;
	} 

	
	public void setFilter(String filter)
	{
		Set<Integer> selectedIndices = saveSelection();
		this.filter = filter;
		if (filter != null && filter.length() > 0)
		{
			int size = delegated.getRowCount();
			filter = prepareFilter(filter);
			Vector<Integer> indices = new Vector<Integer>(size);
			Map<Integer, Integer> invertedIndices = new HashMap<Integer, Integer>();
			for (int i = 0; i < size; i++)
			{
				if (passFilter(i, filter))
				{
					Integer delegatedIndice = Integer.valueOf(i);
					indices.add(delegatedIndice);

					invertedIndices.put(delegatedIndice, indices.size() - 1);
				}
			}
			this.invertedIndices = invertedIndices;
			filteredIndices = indices;
		}
		else
			resetFilter();

		fireTableDataChanged();
		restoreSelection(selectedIndices);
	} 

	
	public String prepareFilter(String filter)
	{
		return filter;
	} 

	
	
	public abstract boolean passFilter(int row, String filter);
	

	

	private Set<Integer> saveSelection()
	{
		if (table == null)
			return null;
		int[] rows = table.getSelectedRows();
		if (rows.length == 0)
			return null;

		Set<Integer> selectedRows = new HashSet<Integer>(rows.length);
		for (int row : rows)
		{
			selectedRows.add(getTrueRow(row));
		}
		return selectedRows;
	} 

	
	private void restoreSelection(Set<Integer> selectedIndices)
	{
		if (selectedIndices == null || getRowCount() == 0)
			return; 
		
		for (Integer selectedIndex : selectedIndices)
		{
			int i = getInternal2ExternalRow(selectedIndex.intValue());
			if (i != -1)
				table.getSelectionModel().setSelectionInterval(i, i);
		}
	}  

	
	public int getRowCount()
	{
		if (filteredIndices == null)
			return delegated.getRowCount();
		return filteredIndices.size();
	} 

	
	public int getColumnCount()
	{
		return delegated.getColumnCount();
	} 

	
	public String getColumnName(int columnIndex)
	{
		return delegated.getColumnName(columnIndex);
	} 

	
	public Class<?> getColumnClass(int columnIndex)
	{
		return delegated.getColumnClass(columnIndex);
	} 

	
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		int trueRowIndex = getTrueRow(rowIndex);
		return delegated.isCellEditable(trueRowIndex, columnIndex);
	} 

	
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		int trueRowIndex = getTrueRow(rowIndex);
		return delegated.getValueAt(trueRowIndex, columnIndex);
	} 

	
	public void setValueAt(Object aValue, int rowIndex, int columnIndex)
	{
		int trueRowIndex = getTrueRow(rowIndex);
		delegated.setValueAt(aValue, trueRowIndex, columnIndex);
	} 

	
	
	public int getTrueRow(int rowIndex)
	{
		if (filteredIndices == null)
			return rowIndex;
		return filteredIndices.get(rowIndex).intValue();
	} 

	
	
	public int getInternal2ExternalRow(int internalRowIndex)
	{
		if (invertedIndices == null)
			return internalRowIndex;

		Integer externalRowIndex = invertedIndices.get(internalRowIndex);
		if (externalRowIndex == null)
			return -1;

		return externalRowIndex.intValue();
	} 

	
	public void tableChanged(TableModelEvent e)
	{
		setFilter(filter);
	}
}
