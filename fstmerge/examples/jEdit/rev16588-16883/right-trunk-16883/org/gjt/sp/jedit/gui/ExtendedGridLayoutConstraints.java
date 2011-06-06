

package org.gjt.sp.jedit.gui;

import java.awt.Component;


public class ExtendedGridLayoutConstraints
{
	
	public static final int REMAINDER = Integer.MAX_VALUE;
	
	
	private int row;
	
	
	private int col;
	
	
	private int colspan;
	
	
	private int effectiveColspan;
	
	
	private int rowspan;
	
	
	private int effectiveRowspan;
	
	
	private boolean placeholder;
	
	
	private ExtendedGridLayoutConstraints mainConstraints;
	
	
	private Component component;
	
	
	public ExtendedGridLayoutConstraints(Component component)
	{
		this(0,0,1,1,component,false,null);
	}
	
	
	public ExtendedGridLayoutConstraints(int row, Component component)
	{
		this(row,0,1,1,component,false,null);
	}
	
	
	public ExtendedGridLayoutConstraints(int row, int colspan, int rowspan, Component component)
	{
		this(row,0,colspan,rowspan,component,false,null);
	}
	
	
	private ExtendedGridLayoutConstraints(int row, int col, int colspan, int rowspan, Component component, boolean placeholder, ExtendedGridLayoutConstraints mainConstraints)
	{
		if (row < 0)
		{
			throw new IllegalArgumentException("row must be non-negative (" + row + ')');
		}
		if (col < 0)
		{
			throw new IllegalArgumentException("col must be non-negative (" + col + ')');
		}
		if (colspan < 1)
		{
			throw new IllegalArgumentException("colspan must be at least 1 (" + colspan + ')');
		}
		if (rowspan < 1)
		{
			throw new IllegalArgumentException("rowspan must be at least 1 (" + rowspan + ')');
		}
		this.row = row;
		this.col = col;
		this.colspan = colspan;
		effectiveColspan = 1;
		this.rowspan = rowspan;
		effectiveRowspan = 1;
		this.component = component;
		this.placeholder = placeholder;
		this.mainConstraints = mainConstraints;
	}
	
	
	ExtendedGridLayoutConstraints getColspanPlaceholder(boolean forUsage)
	{
		if (1 == colspan)
		{
			return null;
		}
		ExtendedGridLayoutConstraints result = new ExtendedGridLayoutConstraints(row,col+1,colspan==REMAINDER ? REMAINDER : colspan-1,rowspan,component,true,null == mainConstraints ? this : mainConstraints);
		if (forUsage && (result.mainConstraints.row == row))
		{
			result.mainConstraints.effectiveColspan++;
		}
		return result;
	}
	
	
	ExtendedGridLayoutConstraints getRowspanPlaceholder(boolean forUsage)
	{
		if (1 == rowspan)
		{
			return null;
		}
		ExtendedGridLayoutConstraints result = new ExtendedGridLayoutConstraints(row+1,col,colspan,rowspan==REMAINDER ? REMAINDER : rowspan-1,component,true,null == mainConstraints ? this : mainConstraints);
		if (forUsage && (result.mainConstraints.col == col))
		{
			result.mainConstraints.effectiveRowspan++;
		}
		return result;
	}
	
	
	public int getRow()
	{
		return row;
	}
	
	
	public int getCol()
	{
		return col;
	}
	
	
	void setCol(int col)
	{
		if (col < 0)
		{
			throw new IllegalArgumentException("col must be non-negative (" + col + ')');
		}
		this.col = col;
	}
	
	
	public int getColspan()
	{
		return colspan;
	}
	
	
	int getEffectiveColspan()
	{
		return null == mainConstraints ? effectiveColspan : mainConstraints.effectiveColspan;
	}
	
	
	public int getRowspan()
	{
		return rowspan;
	}
	
	
	int getEffectiveRowspan()
	{
		return null == mainConstraints ? effectiveRowspan : mainConstraints.effectiveRowspan;
	}
	
	
	Component getComponent()
	{
		return component;
	}
	
	
	public boolean isPlaceholder()
	{
		return placeholder;
	}
	
	
	ExtendedGridLayoutConstraints getWorkCopy()
	{
		return new ExtendedGridLayoutConstraints(row,col,colspan,rowspan,component,placeholder,(null == mainConstraints ? null : mainConstraints.getWorkCopy()));
	}
	
	
	public boolean equals(Object o)
	{
		if ((o == null) ||
		    (!(o instanceof ExtendedGridLayoutConstraints)))
		{
			return false;
		}
		if (component == null)
		{
			return ((ExtendedGridLayoutConstraints)o).component == null;
		}
		return component.equals(((ExtendedGridLayoutConstraints)o).component);
	}
	
	
	public int hashCode()
	{
		if (null == component)
		{
			return 0;
		}
		return component.hashCode();
	}
	
	
	public String toString()
	{
		return getClass().getName() + "[row=" + row + ",col=" + col
			+ ",colspan=" + colspan + ",effectiveColspan=" + effectiveColspan
			+ ",rowspan=" + rowspan + ",effectiveRowspan=" + effectiveRowspan
			+ ",placeholder=" + placeholder + ",component=" + component
			+ ",mainConstraints=" + mainConstraints + "]";
	}
}
