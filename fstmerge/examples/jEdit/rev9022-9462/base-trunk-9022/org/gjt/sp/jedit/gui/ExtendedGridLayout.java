

package org.gjt.sp.jedit.gui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import static org.gjt.sp.jedit.gui.ExtendedGridLayoutConstraints.REMAINDER;


public class ExtendedGridLayout implements LayoutManager2
{
	
	private Hashtable<Component,ExtendedGridLayoutConstraints> comptable;
	
	
	private int hgap;
	
	
	private int vgap;
	
	
	private Insets distanceToBorders;
	
	
	private static enum LayoutSize { MINIMUM, PREFERRED, MAXIMUM }
	
	
	public ExtendedGridLayout(int hgap, int vgap, Insets distanceToBorders)
	{
		if (hgap < 0)
		{
			throw new IllegalArgumentException("hgap must be non-negative (" + hgap + ')');
		}
		if (vgap < 0)
		{
			throw new IllegalArgumentException("vgap must be non-negative (" + vgap + ')');
		}
		this.hgap = hgap;
		this.vgap = vgap;
		this.distanceToBorders = (Insets)distanceToBorders.clone();
		comptable = new Hashtable<Component,ExtendedGridLayoutConstraints>();
	}
	
	
	public ExtendedGridLayout()
	{
		this(0,0,new Insets(0,0,0,0));
	}
	
	
	public void addLayoutComponent(String name, Component component)
	{
		addLayoutComponent(component,name);
	}
	
	
	public void addLayoutComponent(Component component, Object constraints)
	{
		if (null == constraints)
		{
			constraints = new ExtendedGridLayoutConstraints(component);
		}
		if (constraints instanceof ExtendedGridLayoutConstraints)
		{
			ExtendedGridLayoutConstraints eglConstraints = (ExtendedGridLayoutConstraints)constraints;
			if (eglConstraints.isPlaceholder())
			{
				throw new IllegalArgumentException("constraints must not be a placeholder");
			}
			else if (component != eglConstraints.getComponent())
			{
				throw new IllegalArgumentException("constraints is not the right one for this component");
			}
			comptable.put(component,eglConstraints);
		}
		else 
		{
			throw new IllegalArgumentException("constraints must not be an ExtendedGridLayoutConstraints object");
		}
	}
	
	
	private ExtendedGridLayoutConstraints lookupConstraints(Component component)
	{
		if (null == component)
		{
			throw new NullPointerException("component must not be null");
		}
		ExtendedGridLayoutConstraints constraints = comptable.get(component);
		if (null == constraints)
		{
			constraints = new ExtendedGridLayoutConstraints(component);
			comptable.put(component,constraints);
		}
		return constraints;
	}
	
	
	public void removeLayoutComponent(Component component)
	{
		comptable.remove(component);
	}
	
	
	public float getLayoutAlignmentX(Container container)
	{
		return container.getAlignmentX();
	}
	
	
	public float getLayoutAlignmentY(Container container)
	{
		return container.getAlignmentY();
	}
	
	
	public Dimension minimumLayoutSize(Container parent)
	{
		synchronized (parent.getTreeLock())
		{
			List<List<ExtendedGridLayoutConstraints>> gridRows = new ArrayList<List<ExtendedGridLayoutConstraints>>();
			Set<ExtendedGridLayoutConstraints> colspans = new HashSet<ExtendedGridLayoutConstraints>();
			Set<ExtendedGridLayoutConstraints> rowspans = new HashSet<ExtendedGridLayoutConstraints>();
			Dimension gridSize = buildGrid(parent,gridRows,colspans,rowspans);
			return getSize(parent,LayoutSize.MINIMUM,false,gridSize,gridRows,colspans,rowspans,new int[0][0]);
		}
	}
	
	
	public Dimension preferredLayoutSize(Container parent)
	{
		synchronized (parent.getTreeLock())
		{
			List<List<ExtendedGridLayoutConstraints>> gridRows = new ArrayList<List<ExtendedGridLayoutConstraints>>();
			Set<ExtendedGridLayoutConstraints> colspans = new HashSet<ExtendedGridLayoutConstraints>();
			Set<ExtendedGridLayoutConstraints> rowspans = new HashSet<ExtendedGridLayoutConstraints>();
			Dimension gridSize = buildGrid(parent,gridRows,colspans,rowspans);
			return getSize(parent,LayoutSize.PREFERRED,false,gridSize,gridRows,colspans,rowspans,new int[0][0]);
		}
	}
	
	
	public Dimension maximumLayoutSize(Container parent)
	{
		synchronized (parent.getTreeLock())
		{
			List<List<ExtendedGridLayoutConstraints>> gridRows = new ArrayList<List<ExtendedGridLayoutConstraints>>();
			Set<ExtendedGridLayoutConstraints> colspans = new HashSet<ExtendedGridLayoutConstraints>();
			Set<ExtendedGridLayoutConstraints> rowspans = new HashSet<ExtendedGridLayoutConstraints>();
			Dimension gridSize = buildGrid(parent,gridRows,colspans,rowspans);
			return getSize(parent,LayoutSize.MAXIMUM,false,gridSize,gridRows,colspans,rowspans,new int[0][0]);
		}
	}
	
	
	public void invalidateLayout(Container container)
	{
	}
	
	
	public void layoutContainer(Container parent)
	{
		synchronized (parent.getTreeLock())
		{
			
			List<List<ExtendedGridLayoutConstraints>> gridRows = new ArrayList<List<ExtendedGridLayoutConstraints>>();
			Set<ExtendedGridLayoutConstraints> colspans = new HashSet<ExtendedGridLayoutConstraints>();
			Set<ExtendedGridLayoutConstraints> rowspans = new HashSet<ExtendedGridLayoutConstraints>();
			Dimension gridSize = buildGrid(parent,gridRows,colspans,rowspans);
			
			
			int[][] layoutSizes = new int[6][];
			Dimension preferredSize = getSize(parent,LayoutSize.PREFERRED,true,gridSize,gridRows,colspans,rowspans,layoutSizes);
			int[] minimumColWidths = layoutSizes[0];
			int[] minimumRowHeights = layoutSizes[1];
			int[] preferredColWidths = layoutSizes[2];
			int[] preferredRowHeights = layoutSizes[3];
			int[] maximumColWidths = layoutSizes[4];
			int[] maximumRowHeights = layoutSizes[5];
			
			
			Dimension parentSize = parent.getSize();
			Insets insets = parent.getInsets();
			int freeWidth = parentSize.width
					- insets.left - insets.right
					- (gridSize.width - 1) * hgap
					- distanceToBorders.left - distanceToBorders.right;
			int freeHeight = parentSize.height
					 - insets.top - insets.bottom
					 - (gridSize.height - 1) * vgap
					 - distanceToBorders.top - distanceToBorders.bottom;
			redistributeSpace(preferredSize.width,
					  freeWidth,
					  gridSize.width,
					  preferredColWidths,
					  minimumColWidths,
					  maximumColWidths);
			redistributeSpace(preferredSize.height,
					  freeHeight,
					  gridSize.height,
					  preferredRowHeights,
					  minimumRowHeights,
					  maximumRowHeights);
			
			
			for (int row=0, y=insets.top+distanceToBorders.top ; row<gridSize.height ; y+=preferredRowHeights[row]+vgap, row++)
			{
				List<ExtendedGridLayoutConstraints> gridRow = gridRows.get(row);
				for (int col=0, x=insets.left+distanceToBorders.left ; col<gridSize.width; x+=preferredColWidths[col]+hgap, col++)
				{
					ExtendedGridLayoutConstraints cell = gridRow.get(col);
					if ((null != cell) && (null != cell.getComponent()) && !cell.isPlaceholder())
					{
						Component component = cell.getComponent();
						Dimension maxSize = component.getMaximumSize();
						int fromCol = cell.getCol();
						int colspan = cell.getEffectiveColspan();
						int toCol = fromCol + colspan;
						int width = 0;
						for (int col2=fromCol ; col2<toCol ; col2++)
						{
							width += preferredColWidths[col2];
						}
						width += (colspan - 1) * hgap;
						int fromRow = cell.getRow();
						int rowspan = cell.getEffectiveRowspan();
						int toRow = fromRow + rowspan;
						int height = 0;
						for (int row2=fromRow ; row2<toRow ; row2++)
						{
							height += preferredRowHeights[row2];
						}
						height += (rowspan - 1) * vgap;
						int xCorrection = 0;
						int yCorrection = 0;
						if (width > maxSize.width)
						{
							xCorrection = (int)((width - maxSize.width) * component.getAlignmentX());
							width = maxSize.width;
						}
						if (height > maxSize.height)
						{
							yCorrection = (int)((height-maxSize.height) * component.getAlignmentY());
							height = maxSize.height;
						}
						
						component.setBounds(x + xCorrection, y + yCorrection, width, height);
					}
				}
			}
		}
	}
	
	
	private void redistributeSpace(int totalSize, int freeSize,
				       int nelements, int[] preferredElementSizes,
				       int[] minimumElementSizes, int[] maximumElementSizes)
	{
		if (totalSize != freeSize)
		{
			boolean grow = totalSize < freeSize;
			
			freeSize = (freeSize - totalSize) * (grow ? 1 : -1);
			while (freeSize != 0)
			{
				
				
				int modifyableAmount = 0;
				int modifySize = 0;
				for (int i=0 ; i<nelements ; i++)
				{
					if ((grow && (preferredElementSizes[i] < maximumElementSizes[i])) ||
					    (!grow && (preferredElementSizes[i] > minimumElementSizes[i])))
					{
						modifyableAmount++;
						modifySize += preferredElementSizes[i];
					}
				}
				boolean checkBounds = true;
				
				if (0 == modifyableAmount)
				{
					for (int i= 0 ; i<nelements ; i++)
					{
						modifySize += preferredElementSizes[i];
					}
					checkBounds = false;
					modifyableAmount = nelements;
				}
				
				if (modifySize == 0)
				{
					break;
				}
				
				if (freeSize < modifyableAmount)
				{
					for (int i=0 ; i<nelements ; i++)
					{
						if ((freeSize != 0) &&
						    (!checkBounds ||
						     (checkBounds &&
						      (grow && (preferredElementSizes[i] < maximumElementSizes[i])) ||
						      (!grow && (preferredElementSizes[i] > minimumElementSizes[i])))))
						{
							preferredElementSizes[i] += (grow ? 1 : -1);
							if (0 > preferredElementSizes[i])
							{
								preferredElementSizes[i] = 0;
							}
							freeSize--;
						}
					}
				}
				else
				{
					int modifySizeAddition = 0;
					for (int i=0 ; i<nelements ; i++)
					{
						int modifyableSize = (checkBounds ? (grow ? maximumElementSizes[i] - preferredElementSizes[i] : preferredElementSizes[i] - minimumElementSizes[i]) : Integer.MAX_VALUE - preferredElementSizes[i]);
						int elementModifySize = (int)((double)freeSize / (double)modifySize * (double)preferredElementSizes[i]);
						if (elementModifySize <= modifyableSize)
						{
							preferredElementSizes[i] += (grow ? elementModifySize : -elementModifySize);
							modifySizeAddition += (grow ? elementModifySize : -elementModifySize);
							freeSize -= elementModifySize;
						}
						else
						{
							preferredElementSizes[i] += (grow ? modifyableSize : -modifyableSize);
							modifySizeAddition += (grow ? modifyableSize : -modifyableSize);
							freeSize -= modifyableSize;
						}
						if (0 > preferredElementSizes[i])
						{
							preferredElementSizes[i] = 0;
						}
					}
					modifySize += modifySizeAddition;
				}
			}
		}
	}
	
	
	private Dimension getSize(Container parent, LayoutSize layoutSize, boolean fillRawSizes,
				  Dimension gridSize, List<List<ExtendedGridLayoutConstraints>> gridRows,
				  Set<ExtendedGridLayoutConstraints> colspans,
				  Set<ExtendedGridLayoutConstraints> rowspans,
				  int[][] resultArrays)
	{
		if (fillRawSizes && (resultArrays.length < 6))
		{
			throw new IllegalArgumentException("If fillRawSizes is true, resultArrays.length must be >= 6 (" + resultArrays.length + ')');
		}
		long[] minimumColWidths = new long[gridSize.width];
		long[] minimumRowHeights = new long[gridSize.height];
		long[] preferredColWidths = new long[gridSize.width];
		long[] preferredRowHeights = new long[gridSize.height];
		long[] maximumColWidths = new long[gridSize.width];
		long[] maximumRowHeights = new long[gridSize.height];
		Arrays.fill(minimumColWidths,0);
		Arrays.fill(minimumRowHeights,0);
		Arrays.fill(preferredColWidths,0);
		Arrays.fill(preferredRowHeights,0);
		Arrays.fill(maximumColWidths,Long.MAX_VALUE);
		Arrays.fill(maximumRowHeights,Long.MAX_VALUE);
		
		
		
		
		
		
		for (int row=0 ; row<gridSize.height ; row++)
		{
			List<ExtendedGridLayoutConstraints> gridRow = gridRows.get(row);
			int rowHeight = 0;
			for (int col=0 ; col<gridSize.width ; col++)
			{
				ExtendedGridLayoutConstraints cell = gridRow.get(col);
				if ((null != cell) && (null != cell.getComponent()))
				{
					Component component = cell.getComponent();
					Dimension minimumSize = component.getMinimumSize();
					Dimension preferredSize = component.getPreferredSize();
					Dimension maximumSize = component.getMaximumSize();
					if (!colspans.contains(cell))
					{
						minimumColWidths[col] = Math.max(minimumColWidths[col],minimumSize.width);
						preferredColWidths[col] = Math.max(preferredColWidths[col],preferredSize.width);
						maximumColWidths[col] = Math.min(maximumColWidths[col],maximumSize.width);
					}
					if (!rowspans.contains(cell))
					{
						minimumRowHeights[row] = Math.max(minimumRowHeights[row],minimumSize.height);
						preferredRowHeights[row] = Math.max(preferredRowHeights[row],preferredSize.height);
						maximumRowHeights[row] = Math.min(maximumRowHeights[row],maximumSize.height);
					}
				}
			}
		}
		
		
		
		for (ExtendedGridLayoutConstraints cell : colspans)
		{
			int fromCol = cell.getCol();
			int colspan = cell.getEffectiveColspan();
			int toCol = fromCol + colspan;
			int currentMinimumColWidth = (colspan - 1) * hgap;
			int currentPreferredColWidth = (colspan - 1) * hgap;
			int currentMaximumColWidth = (colspan - 1) * hgap;
			for (int col=fromCol ; col<toCol ; col++)
			{
				currentMinimumColWidth += minimumColWidths[col];
				currentPreferredColWidth += preferredColWidths[col];
				currentMaximumColWidth += maximumColWidths[col];
			}
			Component component = cell.getComponent();
			int wantedMinimumColWidth = component.getMinimumSize().width;
			if (currentMinimumColWidth < wantedMinimumColWidth)
			{
				double factor = (double)wantedMinimumColWidth / (double)currentMinimumColWidth;
				for (int col=fromCol ; col<toCol ; col++)
				{
					minimumColWidths[col] = (int)(minimumColWidths[col] * factor);
				}
			}
			int wantedPreferredColWidth = component.getPreferredSize().width;
			if (currentPreferredColWidth < wantedPreferredColWidth)
			{
				double factor = (double)wantedPreferredColWidth / (double)currentPreferredColWidth;
				for (int col=fromCol ; col<toCol ; col++)
				{
					preferredColWidths[col] = (int)(preferredColWidths[col] * factor);
				}
			}
			int wantedMaximumColWidth = component.getMaximumSize().width;
			if (currentMaximumColWidth > wantedMaximumColWidth)
			{
				double factor = (double)wantedMaximumColWidth / (double)currentMaximumColWidth;
				for (int col=fromCol ; col<toCol ; col++)
				{
					maximumColWidths[col] = (int)(maximumColWidths[col] * factor);
				}
			}
		}
		
		
		
		for (ExtendedGridLayoutConstraints cell : rowspans)
		{
			int fromRow = cell.getRow();
			int rowspan = cell.getEffectiveRowspan();
			int toRow = fromRow + rowspan;
			int currentMinimumRowHeight = (rowspan - 1) * vgap;
			int currentPreferredRowHeight = (rowspan - 1) * vgap;
			int currentMaximumRowHeight = (rowspan - 1) * vgap;
			for (int row=fromRow ; row<toRow ; row++)
			{
				currentMinimumRowHeight += minimumRowHeights[row];
				currentPreferredRowHeight += preferredRowHeights[row];
				currentMaximumRowHeight += maximumRowHeights[row];
			}
			Component component = cell.getComponent();
			int wantedMinimumRowHeight = component.getMinimumSize().height;
			if (currentMinimumRowHeight < wantedMinimumRowHeight)
			{
				double factor = (double)wantedMinimumRowHeight / (double)currentMinimumRowHeight;
				for (int row=fromRow ; row<toRow ; row++)
				{
					minimumRowHeights[row] = (int)(minimumRowHeights[row] * factor);
				}
			}
			int wantedPreferredRowHeight = component.getPreferredSize().height;
			if (currentPreferredRowHeight < wantedPreferredRowHeight)
			{
				double factor = (double)wantedPreferredRowHeight / (double)currentPreferredRowHeight;
				for (int row=fromRow ; row<toRow ; row++)
				{
					preferredRowHeights[row] = (int)(preferredRowHeights[row] * factor);
				}
			}
			int wantedMaximumRowHeight = component.getMaximumSize().height;
			if (currentMaximumRowHeight > wantedMaximumRowHeight)
			{
				double factor = (double)wantedMaximumRowHeight / (double)currentMaximumRowHeight;
				for (int row=fromRow ; row<toRow ; row++)
				{
					maximumRowHeights[row] = (int)(maximumRowHeights[row] * factor);
				}
			}
		}
		
		
		
		
		for (int col=0 ; col<gridSize.width ; col++)
		{
			if (minimumColWidths[col] >= maximumColWidths[col])
			{
				maximumColWidths[col] = minimumColWidths[col];
				preferredColWidths[col] = minimumColWidths[col];
			}
			else if (preferredColWidths[col] < minimumColWidths[col])
			{
				preferredColWidths[col] = minimumColWidths[col];
			}
			else if (preferredColWidths[col] > maximumColWidths[col])
			{
				preferredColWidths[col] = maximumColWidths[col];
			}
		}
		
		
		
		
		for (int row=0 ; row<gridSize.height ; row++)
		{
			if (minimumRowHeights[row] >= maximumRowHeights[row])
			{
				maximumRowHeights[row] = minimumRowHeights[row];
				preferredRowHeights[row] = minimumRowHeights[row];
			}
			else if (preferredRowHeights[row] < minimumRowHeights[row])
			{
				preferredRowHeights[row] = minimumRowHeights[row];
			}
			else if (preferredRowHeights[row] > maximumRowHeights[row])
			{
				preferredRowHeights[row] = maximumRowHeights[row];
			}
		}
		
		
		if (fillRawSizes)
		{
			int[] resultMinimumColWidths = new int[minimumColWidths.length];
			int[] resultMinimumRowHeights = new int[minimumRowHeights.length];
			int[] resultPreferredColWidths = new int[preferredColWidths.length];
			int[] resultPreferredRowHeights = new int[preferredRowHeights.length];
			int[] resultMaximumColWidths = new int[maximumColWidths.length];
			int[] resultMaximumRowHeights = new int[maximumRowHeights.length];
			for (int col=0 ; col<gridSize.width ; col++)
			{
				long minimumColWidth = minimumColWidths[col];
				long preferredColWidth = preferredColWidths[col];
				long maximumColWidth = maximumColWidths[col];
				resultMinimumColWidths[col] = minimumColWidth > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)minimumColWidth;
				resultPreferredColWidths[col] = preferredColWidth > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)preferredColWidth;
				resultMaximumColWidths[col] = maximumColWidth > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)maximumColWidth;
			}
			for (int row=0 ; row<gridSize.height ; row++)
			{
				long minimumRowHeight = minimumRowHeights[row];
				long preferredRowHeight = preferredRowHeights[row];
				long maximumRowHeight = maximumRowHeights[row];
				resultMinimumRowHeights[row] = minimumRowHeight > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)minimumRowHeight;
				resultPreferredRowHeights[row] = preferredRowHeight > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)preferredRowHeight;
				resultMaximumRowHeights[row] = maximumRowHeight > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)maximumRowHeight;
			}
			resultArrays[0] = resultMinimumColWidths;
			resultArrays[1] = resultMinimumRowHeights;
			resultArrays[2] = resultPreferredColWidths;
			resultArrays[3] = resultPreferredRowHeights;
			resultArrays[4] = resultMaximumColWidths;
			resultArrays[5] = resultMaximumRowHeights;
		}
		
		
		long[] colWidths;
		long[] rowHeights;
		switch (layoutSize)
		{
			case MINIMUM:
				colWidths = minimumColWidths;
				rowHeights = minimumRowHeights;
				break;
			
			case PREFERRED:
				colWidths = preferredColWidths;
				rowHeights = preferredRowHeights;
				break;
			
			case MAXIMUM:
				colWidths = maximumColWidths;
				rowHeights = maximumRowHeights;
				break;
			
			default:
				throw new InternalError("Missing case branch for LayoutSize: " + layoutSize);
		}
		long totalWidth = 0;
		long totalHeight = 0;
		for (long width : colWidths)
		{
			totalWidth += width;
		}
		for (long height : rowHeights)
		{
			totalHeight += height;
		}
		
		
		
		if (!fillRawSizes)
		{
			Insets insets = parent.getInsets();
			totalWidth += insets.left + insets.right + ((gridSize.width - 1) * hgap) + distanceToBorders.left + distanceToBorders.right;
			totalHeight += insets.top + insets.bottom + ((gridSize.height - 1) * vgap) + distanceToBorders.top + distanceToBorders.bottom;
		}
		
		
		if (totalWidth > Integer.MAX_VALUE) {
			totalWidth = Integer.MAX_VALUE;
		}
		if (totalHeight > Integer.MAX_VALUE) {
			totalHeight = Integer.MAX_VALUE;
		}
		
		return new Dimension((int)totalWidth,(int)totalHeight);
	}
	
	
	private Dimension buildGrid(Container parent, List<List<ExtendedGridLayoutConstraints>> gridRows,
				    Set<ExtendedGridLayoutConstraints> colspans, Set<ExtendedGridLayoutConstraints> rowspans)
	{
		
		List<List<ExtendedGridLayoutConstraints>> rows = new ArrayList<List<ExtendedGridLayoutConstraints>>();
		Component[] components = parent.getComponents();
		for (Component component : components)
		{
			if (component.isVisible()) {
				ExtendedGridLayoutConstraints constraints = lookupConstraints(component).getWorkCopy();
				int rowNumber = constraints.getRow();
				for (int i=rowNumber, c=rows.size() ; i>=c ; i--)
				{
					rows.add(new ArrayList<ExtendedGridLayoutConstraints>());
				}
				List<ExtendedGridLayoutConstraints> row = rows.get(rowNumber);
				row.add(constraints);
			}
		}
		
		
		List<Iterator<ExtendedGridLayoutConstraints>> rowIterators = new ArrayList<Iterator<ExtendedGridLayoutConstraints>>();
		List<ListIterator<ExtendedGridLayoutConstraints>> gridRowIterators = new ArrayList<ListIterator<ExtendedGridLayoutConstraints>>();
		boolean haveNext = false;
		for (List<ExtendedGridLayoutConstraints> row : rows)
		{
			Iterator<ExtendedGridLayoutConstraints> rowIterator = row.iterator();
			rowIterators.add(rowIterator);
			if (rowIterator.hasNext())
			{
				haveNext = true;
			}
			List<ExtendedGridLayoutConstraints> gridRow = new ArrayList<ExtendedGridLayoutConstraints>();
			gridRows.add(gridRow);
			gridRowIterators.add(gridRow.listIterator());
		}
		
		
		int col = -1;
		while (haveNext)
		{
			col++;
			haveNext = false;
			for (int row=0, c=gridRows.size() ; row<c ; row++)
			{
				Iterator<ExtendedGridLayoutConstraints> rowIterator = rowIterators.get(row);
				ListIterator<ExtendedGridLayoutConstraints> gridRowIterator = gridRowIterators.get(row);
				
				
				if (row > 0)
				{
					ExtendedGridLayoutConstraints rowspanSource = gridRows.get(row-1).get(col);
					if (null != rowspanSource)
					{
						ExtendedGridLayoutConstraints rowspanPlaceholder = rowspanSource.getRowspanPlaceholder(true);
						if (null != rowspanPlaceholder)
						{
							rowspans.add(rowspanSource);
							gridRowIterator.add(rowspanPlaceholder);
							if (null != rowspanPlaceholder.getColspanPlaceholder(false))
							{
								switch (rowspanPlaceholder.getColspan())
								{
									case REMAINDER:
										break;
									
									default:
										haveNext = true;
								}
							}
							else if (rowIterator.hasNext())
							{
								haveNext = true;
							}
							continue;
						}
					}
				}
				
				
				if (gridRowIterator.hasPrevious())
				{
					ExtendedGridLayoutConstraints colspanSource = gridRowIterator.previous();
					gridRowIterator.next();
					if (null != colspanSource)
					{
						ExtendedGridLayoutConstraints colspanPlaceholder = colspanSource.getColspanPlaceholder(true);
						if (null != colspanPlaceholder)
						{
							colspans.add(colspanSource);
							gridRowIterator.add(colspanPlaceholder);
							if (null != colspanPlaceholder.getColspanPlaceholder(false))
							{
								switch (colspanPlaceholder.getColspan())
								{
									case REMAINDER:
										break;
									
									default:
										haveNext = true;
								}
							}
							else if (rowIterator.hasNext())
							{
								haveNext = true;
							}
							continue;
						}
					}
				}
				
				
				if (rowIterator.hasNext())
				{
					ExtendedGridLayoutConstraints newConstraints = rowIterator.next();
					newConstraints.setCol(col);
					gridRowIterator.add(newConstraints);
					if (null != newConstraints.getColspanPlaceholder(false))
					{
						switch (newConstraints.getColspan())
						{
							case REMAINDER:
								break;
							
							default:
								haveNext = true;
						}
					}
					else if (rowIterator.hasNext())
					{
						haveNext = true;
					}
				}
				else
				{
					gridRowIterator.add(null);
				}
			}
		}
		
		
		haveNext = false;
		ListIterator<ExtendedGridLayoutConstraints> gridRowIterator = gridRows.get(gridRows.size()-1).listIterator();
		while (gridRowIterator.hasNext())
		{
			ExtendedGridLayoutConstraints cell = gridRowIterator.next();
			if ((null != cell) &&
			    ((REMAINDER != cell.getRowspan()) &&
			     (null != cell.getRowspanPlaceholder(false))))
			{
				haveNext = true;
				break;
			}
		}
		while (haveNext)
		{
			haveNext = false;
			gridRowIterator = gridRows.get(gridRows.size()-1).listIterator();
			List<ExtendedGridLayoutConstraints> gridRow = new ArrayList<ExtendedGridLayoutConstraints>();
			gridRows.add(gridRow);
			ListIterator<ExtendedGridLayoutConstraints> newGridRowIterator = gridRow.listIterator();
			while (gridRowIterator.hasNext())
			{
				ExtendedGridLayoutConstraints cell = gridRowIterator.next();
				if ((null != cell) &&
				    (null != cell.getRowspanPlaceholder(false)))
				{
					rowspans.add(cell);
					ExtendedGridLayoutConstraints rowspanPlaceholder = cell.getRowspanPlaceholder(true);
					newGridRowIterator.add(rowspanPlaceholder);
				}
				else
				{
					newGridRowIterator.add(null);
				}
			}
			gridRowIterator = gridRow.listIterator();
			while (gridRowIterator.hasNext())
			{
				ExtendedGridLayoutConstraints cell = gridRowIterator.next();
				if ((null != cell) &&
				    ((REMAINDER != cell.getRowspan()) &&
				     (null != cell.getRowspanPlaceholder(false))))
				{
					haveNext = true;
					break;
				}
			}
		}
		
		return new Dimension(col+1,gridRows.size());
	}
	
	
	public String toString()
	{
		return getClass().getName() + "[hgap=" + hgap + ",vgap=" + vgap
			+ ",distanceToBorders=" + distanceToBorders
			+ ",comptable=" + comptable + "]";
	}
}
