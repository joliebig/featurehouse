

package org.gjt.sp.jedit.gui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager2;

import java.util.Arrays;


public class VariableGridLayout implements LayoutManager2, java.io.Serializable
{
	public static final int FIXED_NUM_ROWS = 1;
	public static final int FIXED_NUM_COLUMNS = 2;

	private static enum LayoutSize { MINIMUM, MAXIMUM, PREFERRED }

	
	public VariableGridLayout(int mode, int size, int hgap, int vgap, boolean takeSizesIntoAccount, Insets distanceToBorders)
	{
		if (mode != FIXED_NUM_ROWS && mode != FIXED_NUM_COLUMNS)
		{
			throw new IllegalArgumentException("illegal mode; value is " + mode);
		}
		if (size <= 0)
		{
			throw new IllegalArgumentException("size cannot be zero or less; value is " + size);
		}
		if (hgap < 0)
		{
			throw new IllegalArgumentException("hgap cannot be negative; value is " + hgap);
		}
		if (vgap < 0)
		{
			throw new IllegalArgumentException("vgap cannot be negative; value is " + vgap);
		}
		this.mode = mode;
		this.size = size;
		this.hgap = hgap;
		this.vgap = vgap;
		this.takeSizesIntoAccount = takeSizesIntoAccount;
		this.distanceToBorders = (Insets)distanceToBorders.clone();
	}

	
	public VariableGridLayout(int mode, int size, int hgap, int vgap, boolean takeSizesIntoAccount)
	{
		this(mode, size, hgap, vgap, takeSizesIntoAccount, new Insets(0,0,0,0));
	}

	
	public VariableGridLayout(int mode, int size, int hgap, int vgap)
	{
		this(mode, size, hgap, vgap, false, new Insets(0,0,0,0));
	}

	
	public VariableGridLayout(int mode, int size)
	{
		this(mode, size, 0, 0, false, new Insets(0,0,0,0));
	}

	
	public VariableGridLayout()
	{
		this(FIXED_NUM_ROWS, 1, 0, 0, false, new Insets(0,0,0,0));
	}

	
	public void addLayoutComponent(String name, Component component)
	{
	}

	
	public void addLayoutComponent(Component component, Object constraints)
	{
	}

	
	public void removeLayoutComponent(Component component)
	{
	}

	
	public float getLayoutAlignmentX(Container container)
	{
		return 0.5f;
	}

	
	public float getLayoutAlignmentY(Container container)
	{
		return 0.5f;
	}

	public Dimension preferredLayoutSize(Container parent)
	{
		return getLayoutSize(parent,LayoutSize.PREFERRED);
	}

	public Dimension minimumLayoutSize(Container parent)
	{
		return getLayoutSize(parent,LayoutSize.MINIMUM);
	}

	public Dimension maximumLayoutSize(Container parent)
	{
		return getLayoutSize(parent,LayoutSize.MAXIMUM);
	}

	public void layoutContainer(Container parent)
	{
		synchronized (parent.getTreeLock())
		{
			update(parent);

			int ncomponents = parent.getComponentCount();

			if (ncomponents == 0)
			{
				return;
			}

			
			int total_height = 0;
			Arrays.fill(row_heights,0);
			Arrays.fill(col_widths,0);
			if (takeSizesIntoAccount)
			{
				Arrays.fill(minimum_row_heights,0);
				Arrays.fill(minimum_col_widths,0);
				Arrays.fill(maximum_row_heights,Integer.MAX_VALUE);
				Arrays.fill(maximum_col_widths,Integer.MAX_VALUE);
			}
			for (int r = 0, i = 0; r < nrows; r++)
			{
				for (int c = 0; c < ncols; c++, i++)
				{
					if (i < ncomponents)
					{
						Component comp = parent.getComponent(i);
						Dimension d = comp.getPreferredSize();
						row_heights[r] = Math.max(row_heights[r], d.height);
						col_widths[c] = Math.max(col_widths[c], d.width);
						if (takeSizesIntoAccount)
						{
							d = comp.getMinimumSize();
							minimum_row_heights[r] = Math.max(minimum_row_heights[r], d.height);
							minimum_col_widths[c] = Math.max(minimum_col_widths[c], d.width);
							d = comp.getMaximumSize();
							maximum_row_heights[r] = Math.min(maximum_row_heights[r], d.height);
							maximum_col_widths[c] = Math.min(maximum_col_widths[c], d.width);
						}
					}
					else
					{
						break;
					}
				}
				if (takeSizesIntoAccount)
				{
					
					
					
					if (minimum_row_heights[r] >= maximum_row_heights[r])
					{
						maximum_row_heights[r] = minimum_row_heights[r];
						row_heights[r] = minimum_row_heights[r];
					}
					else if (row_heights[r] < minimum_row_heights[r])
					{
						row_heights[r] = minimum_row_heights[r];
					}
					else if (row_heights[r] > maximum_row_heights[r])
					{
						row_heights[r] = maximum_row_heights[r];
					}
				}
				total_height += row_heights[r];
			}

			int total_width = 0;
			for (int c = 0; c < ncols; c++)
			{
				if (takeSizesIntoAccount)
				{
					
					
					
					if (minimum_col_widths[c] >= maximum_col_widths[c])
					{
						maximum_col_widths[c] = minimum_col_widths[c];
						col_widths[c] = minimum_col_widths[c];
					}
					else if (col_widths[c] < minimum_col_widths[c])
					{
						col_widths[c] = minimum_col_widths[c];
					}
					else if (col_widths[c] > maximum_col_widths[c])
					{
						col_widths[c] = maximum_col_widths[c];
					}
				}
				total_width += col_widths[c];
			}

			
			Dimension parent_size = parent.getSize();
			Insets insets = parent.getInsets();
			int free_height = parent_size.height
					  - insets.top - insets.bottom
					  - (nrows - 1) * vgap
					  - distanceToBorders.top - distanceToBorders.bottom;
			int free_width = parent_size.width
					 - insets.left - insets.right
					 - (ncols - 1) * hgap
					 - distanceToBorders.left - distanceToBorders.right;

			redistributeSpace(total_height,free_height,
					  takeSizesIntoAccount,
					  nrows,row_heights,
					  minimum_row_heights,
					  maximum_row_heights);

			redistributeSpace(total_width,free_width,
					  takeSizesIntoAccount,
					  ncols,col_widths,
					  minimum_col_widths,
					  maximum_col_widths);

			
			for (int r = 0, y = insets.top + distanceToBorders.top, i = 0; r < nrows; y += row_heights[r] + vgap, r++)
			{
				for (int c = 0, x = insets.left + distanceToBorders.left; c < ncols; x += col_widths[c] + hgap, c++, i++)
				{
					if (i < ncomponents)
					{
						Component comp = parent.getComponent(i);
						Dimension d = comp.getMaximumSize();
						int width = col_widths[c];
						int height = row_heights[r];
						int xCorrection = 0;
						int yCorrection = 0;
						if (width > d.width)
						{
							xCorrection = (int)((width - d.width) * comp.getAlignmentX());
							width = d.width;
						}
						if (height > d.height)
						{
							yCorrection = (int)((height-d.height) * comp.getAlignmentY());
							height = d.height;
						}
						
						comp.setBounds(x + xCorrection, y + yCorrection, width, height);
					}
				}
			}
		} 
	}

	public void invalidateLayout(Container container)
	{
	}

	
	public String toString()
	{
		return getClass().getName() + "[mode="
			+ ((FIXED_NUM_ROWS == mode) ? "FIXED_NUM_ROWS"
			   : ((FIXED_NUM_COLUMNS == mode) ? "FIXED_NUM_COLUMNS"
			      : "UNKNOWN(" + mode + ")")) + ",size=" + size
			+ ",hgap=" + hgap + ",vgap=" + vgap
			+ ",takeSizesIntoAccount=" + takeSizesIntoAccount
			+ ",distanceToBorders=" + distanceToBorders + "]";
	}

	
	private Dimension getLayoutSize(Container parent, LayoutSize which)
	{
		synchronized (parent.getTreeLock())
		{
			update(parent);

			int ncomponents = parent.getComponentCount();
			long h = 0;
			long w = 0;

			for (int r = 0, i = 0; r < nrows; r++)
			{
				int row_height = 0;
				for (int c = 0; c < ncols; c++, i++)
				{
					if (i < ncomponents)
					{
						switch (which)
						{
							case MINIMUM:
								row_height = Math.max(row_height, parent.getComponent(i).getMinimumSize().height);
								break;
							
							case MAXIMUM:
								row_height = Math.max(row_height, parent.getComponent(i).getMaximumSize().height);
								break;
							
							case PREFERRED:
								row_height = Math.max(row_height, parent.getComponent(i).getPreferredSize().height);
								break;
							
							default:
								throw new InternalError("Missing case branch for LayoutSize: " + which);
						}
					}
				}
				h += row_height;
			}

			for (int c = 0; c < ncols; c++)
			{
				int col_width = 0;
				for (int r = 0; r < nrows; r++)
				{
					int i = r * ncols + c;
					if (i < ncomponents)
					{
						switch (which)
						{
							case MINIMUM:
								col_width = Math.max(col_width, parent.getComponent(i).getMinimumSize().width);
								break;
							
							case MAXIMUM:
								col_width = Math.max(col_width, parent.getComponent(i).getMaximumSize().width);
								break;
							
							case PREFERRED:
								col_width = Math.max(col_width, parent.getComponent(i).getPreferredSize().width);
								break;
							
							default:
								throw new InternalError("Missing case branch for LayoutSize: " + which);
						}
					}
				}
				w += col_width;
			}

			Insets insets = parent.getInsets();
			w += insets.left + insets.right + ((ncols - 1) * hgap) + distanceToBorders.left + distanceToBorders.right;
			h += insets.top + insets.bottom + ((nrows - 1) * vgap) + distanceToBorders.top + distanceToBorders.bottom;
			if (w > Integer.MAX_VALUE)
			{
				w = Integer.MAX_VALUE;
			}
			if (h > Integer.MAX_VALUE)
			{
				h = Integer.MAX_VALUE;
			}
			return new Dimension((int)w,(int)h);
		}
	}

	private void update(Container container)
	{
		int ncomponents = container.getComponentCount();
		int old_nrows = nrows;
		int old_ncols = ncols;
		if (this.mode == FIXED_NUM_ROWS)
		{
			nrows = this.size;
			ncols = (ncomponents + nrows - 1) / nrows;
		}
		else
		{
			ncols = this.size;
			nrows = (ncomponents + ncols - 1) / ncols;
		}
		if (old_nrows != nrows)
		{
			row_heights = new int[nrows];
			if (takeSizesIntoAccount)
			{
				minimum_row_heights = new int[nrows];
				maximum_row_heights = new int[nrows];
			}
		}
		if (old_ncols != ncols)
		{
			col_widths = new int[ncols];
			if (takeSizesIntoAccount)
			{
				minimum_col_widths = new int[ncols];
				maximum_col_widths = new int[ncols];
			}
		}
	}

	private void redistributeSpace(int total_size, int free_size, boolean takeSizesIntoAccount,
				       int nelements, int[] element_sizes,
				       int[] minimum_element_sizes, int[] maximum_element_sizes)
	{
		if (total_size != free_size)
		{
			if (takeSizesIntoAccount)
			{
				boolean grow = total_size < free_size;
				
				free_size = (free_size - total_size) * (grow ? 1 : -1);
				while (free_size != 0)
				{
					
					
					int modifyableAmount = 0;
					int modifySize = 0;
					for (int i = 0 ; i < nelements ; i++)
					{
						if ((grow && (element_sizes[i] < maximum_element_sizes[i])) ||
						    (!grow && (element_sizes[i] > minimum_element_sizes[i])))
						{
							modifyableAmount++;
							modifySize += element_sizes[i];
						}
					}
					boolean checkBounds = true;
					
					if (0 == modifyableAmount)
					{
						for (int i = 0 ; i < nelements ; i++)
						{
							modifySize += element_sizes[i];
						}
						checkBounds = false;
						modifyableAmount = nelements;
					}
					
					if (modifySize == 0)
					{
						break;
					}
					
					if (free_size < modifyableAmount)
					{
						for (int i = 0 ; i < nelements ; i++)
						{
							if ((free_size != 0) &&
							    (!checkBounds ||
							     (checkBounds &&
							      (grow && (element_sizes[i] < maximum_element_sizes[i])) ||
							      (!grow && (element_sizes[i] > minimum_element_sizes[i])))))
							{
								element_sizes[i] += (grow ? 1 : -1);
								if (0 > element_sizes[i])
								{
									element_sizes[i] = 0;
								}
								free_size--;
							}
						}
					}
					else
					{
						int modifySizeAddition = 0;
						for (int i = 0 ; i < nelements ; i++)
						{
							int modifyableSize = (checkBounds ? (grow ? maximum_element_sizes[i] - element_sizes[i] : element_sizes[i] - minimum_element_sizes[i]) : Integer.MAX_VALUE - element_sizes[i]);
							int elementModifySize = (int)((double)free_size / (double)modifySize * (double)element_sizes[i]);
							if (elementModifySize <= modifyableSize)
							{
								element_sizes[i] += (grow ? elementModifySize : -elementModifySize);
								modifySizeAddition += (grow ? elementModifySize : -elementModifySize);
								free_size -= elementModifySize;
							}
							else
							{
								element_sizes[i] += (grow ? modifyableSize : -modifyableSize);
								modifySizeAddition += (grow ? modifyableSize : -modifyableSize);
								free_size -= modifyableSize;
							}
							if (0 > element_sizes[i])
							{
								element_sizes[i] = 0;
							}
						}
						modifySize += modifySizeAddition;
					}
				}
			}
			else
			{
				double d = (double)free_size / (double)total_size;
				for (int i = 0; i < nelements; i++)
				{
					element_sizes[i] = (int)(element_sizes[i] * d);
				}
			}
		}
	}

	private int mode;
	private int size;
	private int hgap;
	private int vgap;
	private boolean takeSizesIntoAccount;
	private Insets distanceToBorders;
	private transient int nrows = -1;
	private transient int ncols = -1;
	private transient int[] minimum_row_heights = null;
	private transient int[] minimum_col_widths = null;
	private transient int[] row_heights = null;
	private transient int[] col_widths = null;
	private transient int[] maximum_row_heights = null;
	private transient int[] maximum_col_widths = null;
}
