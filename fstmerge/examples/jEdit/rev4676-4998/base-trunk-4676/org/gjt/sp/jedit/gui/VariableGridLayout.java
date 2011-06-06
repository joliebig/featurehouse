

package org.gjt.sp.jedit.gui;


import java.awt.*;


public class VariableGridLayout implements LayoutManager2, java.io.Serializable
{

	public static final int FIXED_NUM_ROWS = 1;
	public static final int FIXED_NUM_COLUMNS = 2;


	public VariableGridLayout(int mode, int size, int hgap, int vgap) {
		if (mode != FIXED_NUM_ROWS && mode != FIXED_NUM_COLUMNS) {
			throw new IllegalArgumentException("illegal mode; value is " + mode);
		}
		if (size <= 0) {
			throw new IllegalArgumentException("size cannot be zero or less; value is " + size);
		}
		if (hgap < 0) {
			throw new IllegalArgumentException("hgap cannot be negative; value is " + hgap);
		}
		if (vgap < 0) {
			throw new IllegalArgumentException("vgap cannot be negative; value is " + vgap);
		}
		this.mode = mode;
		this.size = size;
		this.hgap = hgap;
		this.vgap = vgap;
	}


	
	public VariableGridLayout(int mode, int size) {
		this(mode, size, 0, 0);
	}


	
	public VariableGridLayout() {
		this(FIXED_NUM_ROWS, 1, 0, 0);
	}


	
	public void addLayoutComponent(String name, Component component) { }


	
	public void addLayoutComponent(Component component, Object constraints) { }


	
	public void removeLayoutComponent(Component component) { }


	
	public float getLayoutAlignmentX(Container container) {
		return 0.5f;
	}


	
	public float getLayoutAlignmentY(Container container) {
		return 0.5f;
	}


	public Dimension preferredLayoutSize(Container parent) {
		return getLayoutSize(parent, 2);
	}


	public Dimension minimumLayoutSize(Container parent) {
		return getLayoutSize(parent, 0);
	}


	public Dimension maximumLayoutSize(Container parent) {
		return getLayoutSize(parent, 1);
	}


	public void layoutContainer(Container parent) {
		synchronized (parent.getTreeLock()) {
			update(parent);

			int ncomponents = parent.getComponentCount();

			if (ncomponents == 0) {
				return;
			}

			
			int total_height = 0;
			for (int r = 0, i = 0; r < nrows; r++) {
				for (int c = 0; c < ncols; c++, i++) {
					if (i < ncomponents) {
						Dimension d = parent.getComponent(i).getPreferredSize();
						row_heights[r] = Math.max(row_heights[r], d.height);
						col_widths[c] = Math.max(col_widths[c], d.width);
					} else {
						break;
					}
				}
				total_height += row_heights[r];
			}

			int total_width = 0;
			for (int c = 0; c < ncols; c++) {
				total_width += col_widths[c];
			}

			
			Dimension parent_size = parent.getSize();
			Insets insets = parent.getInsets();
			int free_height = parent_size.height - insets.top - insets.bottom - (nrows - 1) * vgap;
			int free_width = parent_size.width - insets.left - insets.right - (ncols - 1) * hgap;

			if (total_height != free_height) {
				double dy = (double)free_height / (double)total_height;
				for (int r = 0; r < nrows; r++) {
					row_heights[r] = (int) ((double)row_heights[r] * dy);
				}
			}

			if (total_width != free_width) {
				double dx = ((double)free_width) / ((double)total_width);
				for (int c = 0; c < ncols; c++) {
					col_widths[c] = (int) ((double)col_widths[c] * dx);
				}
			}

			
			for (int r = 0, y = insets.top, i = 0; r < nrows; y += row_heights[r] + vgap, r++) {
				for (int c = 0, x = insets.left; c < ncols; x += col_widths[c] + hgap, c++, i++) {
					if (i < ncomponents) {
						parent.getComponent(i).setBounds(x, y, col_widths[c], row_heights[r]);
					}
				}
			}

		} 
	}


	public void invalidateLayout(Container container) {}


	
	public String toString() {
		return getClass().getName() + "[mode=" + mode + ",size=" + size
			   + ",hgap=" + hgap + ",vgap=" + vgap + "]";
	}


	
	private Dimension getLayoutSize(Container parent, int which) {
		synchronized (parent.getTreeLock()){
			update(parent);

			int ncomponents = parent.getComponentCount();
			int h = 0;
			int w = 0;

			for (int r = 0, i = 0; r < nrows; r++) {
				int row_height = 0;
				for (int c = 0; c < ncols; c++, i++) {
					if (i < ncomponents) {
						switch (which) {
							case 0:
								row_height = Math.max(row_height, parent.getComponent(i).getMinimumSize().height);
								break;
							case 1:
								row_height = Math.max(row_height, parent.getComponent(i).getMaximumSize().height);
								break;
							default:
								row_height = Math.max(row_height, parent.getComponent(i).getPreferredSize().height);
								break;
						}
					} else {
						break;
					}
				}
				h += row_height;
			}

			for (int c = 0; c < ncols; c++) {
				int col_width = 0;
				for (int r = 0; r < nrows; r++) {
					int i = r * ncols + c;
					if (i < ncomponents) {
						switch (which) {
							case 0:
								col_width = Math.max(col_width, parent.getComponent(i).getMinimumSize().width);
								break;
							case 1:
								col_width = Math.max(col_width, parent.getComponent(i).getMaximumSize().width);
								break;
							default:
								col_width = Math.max(col_width, parent.getComponent(i).getPreferredSize().width);
								break;
						}
					} else {
						break;
					}
				}
				w += col_width;
			}

			Insets insets = parent.getInsets();
			return new Dimension(w + insets.left + insets.right + ((ncols - 1) * hgap),
								 h + insets.top + insets.bottom + ((nrows - 1) * vgap));
		}
	}


	private void update(Container container) {
		int ncomponents = container.getComponentCount();
		int old_nrows = nrows;
		int old_ncols = ncols;
		if (this.mode == FIXED_NUM_ROWS) {
			nrows = this.size;
			ncols = (ncomponents + nrows - 1) / nrows;
		} else {
			ncols = this.size;
			nrows = (ncomponents + ncols - 1) / ncols;
		}
		if (old_nrows != nrows) {
			row_heights = new int[nrows];
		}
		if (old_ncols != ncols) {
			col_widths = new int[ncols];
		}
	}


	private int mode;
	private int size;
	private int hgap;
	private int vgap;
	private transient int nrows = -1;
	private transient int ncols = -1;
	private transient int[] row_heights = null;
	private transient int[] col_widths = null;
}
