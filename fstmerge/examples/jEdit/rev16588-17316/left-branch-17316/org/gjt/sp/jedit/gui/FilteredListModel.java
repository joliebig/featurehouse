
package org.gjt.sp.jedit.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.util.*;


public abstract class FilteredListModel<E extends ListModel> extends AbstractListModel implements ListDataListener
{
	
	protected E delegated;

	private Vector<Integer> filteredIndices;

	
	private Map<Integer, Integer> invertedIndices;

	private String filter;

	private JList list;

	
	protected FilteredListModel(E delegated)
	{
		this.delegated = delegated;
		delegated.addListDataListener(this);
		resetFilter();
	} 

	
	
	public void setList(JList list)
	{
		if (list.getModel() != this)
			throw new IllegalArgumentException("The given list " + list + " doesn't use this model " + this);
		this.list = list;
	} 

	
	public E getDelegated()
	{
		return delegated;
	} 

	
	public void setDelegated(E delegated)
	{
		this.delegated.removeListDataListener(this);
		delegated.addListDataListener(this);
		this.delegated = delegated;
	} 

	
	private void resetFilter()
	{
		filteredIndices = null;
	} 

	
	public void setFilter(final String filter)
	{
		Runnable runner = new Runnable()
		{
			public void run()
			{
				Set<Integer> selectedIndices = saveSelection();
				list.clearSelection();
				FilteredListModel.this.filter = filter;
				if (filter != null && filter.length() > 0)
				{
					int size = delegated.getSize();
					String prepped_filter = prepareFilter(filter);
					Vector<Integer> indices = new Vector<Integer>(size);
					Map<Integer, Integer> invertedIndices = new HashMap<Integer, Integer>();
					for (int i = 0; i < size; i++)
					{
						if (passFilter(i, prepped_filter))
						{
							Integer delegatedIndice = Integer.valueOf(i);
							indices.add(delegatedIndice);

							invertedIndices.put(delegatedIndice, indices.size() - 1);
						}
					}
					FilteredListModel.this.invertedIndices = invertedIndices;
					filteredIndices = indices;
				}
				else
					resetFilter();

				fireContentsChanged(this, 0, getSize() - 1);
				restoreSelection(selectedIndices);
			}
		};
		SwingUtilities.invokeLater(runner);
	} 

	
	public String prepareFilter(String filter)
	{
		return filter;
	} 

	
	
	public abstract boolean passFilter(int row, String filter);
	

	
	protected Set<Integer> saveSelection()
	{
		if (list == null)
			return null;
		int[] rows = list.getSelectedIndices();
		if (rows.length == 0)
			return null;

		Set<Integer> selectedRows = new HashSet<Integer>(rows.length);
		for (int row : rows)
		{
			selectedRows.add(getTrueRow(row));
		}
		return selectedRows;
	} 

	
	protected void restoreSelection(Set<Integer> selectedIndices)
	{
		if (selectedIndices == null || getSize() == 0)
			return;

		
		
		
		
		
		
		Vector<Integer> sel = new Vector<Integer>(selectedIndices);
		Collections.sort(sel);
		int from = -1;
		int to = -1;
		for (Integer selectedIndex : sel)
		{
			int i = getInternal2ExternalRow(selectedIndex.intValue());
			if (i != -1)
			{
				if (from == -1)
					from = to = i;
				else if (i == to + 1)
					to = i;
				else
				{
					list.setSelectionInterval(from, to);
					from = to = i;
				}
			}
		}
		if (from != -1)
			list.setSelectionInterval(from, to);
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

	
	public Object getElementAt(int index)
	{
		int trueRowIndex = getTrueRow(index);
		return delegated.getElementAt(trueRowIndex);
	} 

	
	public int getSize()
	{
		if (filteredIndices == null)
			return delegated.getSize();
		return filteredIndices.size();
	} 

	
	public void contentsChanged(ListDataEvent e)
	{
		setFilter(filter);
	} 

	
	public void intervalAdded(ListDataEvent e)
	{
		setFilter(filter);
	} 

	
	public void intervalRemoved(ListDataEvent e)
	{
		setFilter(filter);
	} 
}
