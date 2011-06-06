

package org.gjt.sp.jedit.gui;


import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineMetrics;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JToggleButton;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.metal.MetalLookAndFeel;

import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.OperatingSystem;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.DockableWindowManager.DockingArea;
import org.gjt.sp.jedit.msg.DockableWindowUpdate;
import org.gjt.sp.util.StandardUtilities;


public class PanelWindowContainer implements DockableWindowContainer, DockingArea
{
	
	public PanelWindowContainer(DockableWindowManagerImpl wm, String position,
		int dimension)
	{
		this.wm = wm;
		this.position = position;

		
		buttonPanel = new JPanel(new ButtonLayout());
		buttonPanel.setBorder(new EmptyBorder(1,1,1,1));

		closeBox = new JButton(GUIUtilities.loadIcon("closebox.gif"));
		closeBox.setRequestFocusEnabled(false);
		closeBox.setToolTipText(jEdit.getProperty("view.docking.close-tooltip"));
		if(OperatingSystem.isMacOSLF())
			closeBox.putClientProperty("JButton.buttonType","toolbar");

		closeBox.setMargin(new Insets(0,0,0,0));

		closeBox.addActionListener(new ActionHandler());

		menuBtn = new JButton(GUIUtilities.loadIcon(jEdit.getProperty("dropdown-arrow.icon")));
		menuBtn.setRequestFocusEnabled(false);
		menuBtn.setToolTipText(jEdit.getProperty("view.docking.menu-tooltip"));
		if(OperatingSystem.isMacOSLF())
			menuBtn.putClientProperty("JButton.buttonType","toolbar");

		menuBtn.setMargin(new Insets(0,0,0,0));

		menuBtn.addMouseListener(new MenuMouseHandler());

		buttonGroup = new ButtonGroup();
		
		buttonGroup.add(nullButton = new JToggleButton());
		

		dockables = new ArrayList<DockableWindowManagerImpl.Entry>();
		buttons = new ArrayList<AbstractButton>();
		dockablePanel = new DockablePanel(this);

		this.dimension = dimension;
	} 

	
	
	public DockableWindowManagerImpl getDockableWindowManager()
	{
		return wm;
	} 
	
	
	public void register(DockableWindowManagerImpl.Entry entry)
	{
		dockables.add(entry);

		
		int rotation;
		if(position.equals(DockableWindowManagerImpl.TOP)
			|| position.equals(DockableWindowManagerImpl.BOTTOM))
			rotation = RotatedTextIcon.NONE;
		else if(position.equals(DockableWindowManagerImpl.LEFT))
			rotation = RotatedTextIcon.CCW;
		else if(position.equals(DockableWindowManagerImpl.RIGHT))
			rotation = RotatedTextIcon.CW;
		else
			throw new InternalError("Invalid position: " + position);

		JToggleButton button = new JToggleButton();
		button.setMargin(new Insets(1,1,1,1));
		button.setRequestFocusEnabled(false);
		button.setIcon(new RotatedTextIcon(rotation,button.getFont(),
			entry.shortTitle()));
		button.setActionCommand(entry.factory.name);
		button.addActionListener(new ActionHandler());
		button.addMouseListener(new MenuMouseHandler());
		if(OperatingSystem.isMacOSLF())
			button.putClientProperty("JButton.buttonType","toolbar");
		

		buttonGroup.add(button);
		buttons.add(button);
		entry.btn = button;

		wm.revalidate();
	} 

	
	public void unregister(DockableWindowManagerImpl.Entry entry)
	{
		if(entry.factory.name.equals(mostRecent))
			mostRecent = null;

		if(entry.btn != null)
		{
			buttonPanel.remove(entry.btn);
			buttons.remove(entry.btn);
			entry.btn = null;
		}

		dockables.remove(entry);
		if(entry.win != null)
			dockablePanel.remove(entry.win);

		if(current == entry)
		{
			current = null;
			show(current);
		}
		else
		{
			wm.revalidate();
			dockablePanel.repaint();
			buttonPanel.repaint();
		}
	} 

	
	public void remove(DockableWindowManagerImpl.Entry entry)
	{
		if(entry.factory.name.equals(mostRecent))
			mostRecent = null;

		if(entry.win != null)
		{
			dockablePanel.remove(entry.win);
			entry.win = null;
		}

		if(current == entry)
		{
			current = null;
			show(current);
		}
		else
		{
			wm.revalidate();
			dockablePanel.repaint();
		}
	} 

	
	public void showMostRecent()
	{
		if(dockables.isEmpty())
		{
			Toolkit.getDefaultToolkit().beep();
			return;
		}

		if(mostRecent == null)
		{
			mostRecent = dockables.get(0).factory.name;
		}

		wm.showDockableWindow(mostRecent);
	} 

	
	public void show(DockableWindowManagerImpl.Entry entry)
	{
		if(current == entry)
		{
			if(entry != null)
			{
				if(entry.win instanceof DefaultFocusComponent)
				{
					((DefaultFocusComponent)entry.win)
						.focusOnDefaultComponent();
				}
				else
				{
					entry.win.requestDefaultFocus();
				}
			}
			return;
		}

		if(entry != null)
		{
			if(current == null)
			{
				
				
				dockablePanel.setBorder(new DockBorder(position));
			}

			mostRecent = entry.factory.name;
			this.current = entry;

			if(entry.win.getParent() != dockablePanel)
				dockablePanel.add(entry.factory.name,entry.win);

			dockablePanel.showDockable(entry.factory.name);

			entry.btn.setSelected(true);

			if(entry.win instanceof DefaultFocusComponent)
			{
				((DefaultFocusComponent)entry.win)
					.focusOnDefaultComponent();
			}
			else
			{
				entry.win.requestDefaultFocus();
			}
		}
		else
		{
			if (current != null)
			{
				
				Object reason = DockableWindowUpdate.DEACTIVATED;
				EditBus.send(new DockableWindowUpdate(wm, reason, current.factory.name));
			}
			current = null;
			nullButton.setSelected(true);
			
			dockablePanel.setBorder(null);

			wm.getView().getTextArea().requestFocus();
		}

		wm.revalidate();
		dockablePanel.repaint();
	} 

	
	public boolean isVisible(DockableWindowManagerImpl.Entry entry)
	{
		return current == entry;
	} 

	
	
	public String getCurrent()
	{
		if(current == null)
			return null;
		else
			return current.factory.name;
	} 

	
	
	public int getDimension()
	{
		return dimension;
	} 

	
	
	public String getPosition()
	{
		return position;
	} 

	
	public String[] getDockables()
	{
		String[] retVal = new String[dockables.size()];
		for(int i = 0; i < dockables.size(); i++)
		{
			DockableWindowManagerImpl.Entry entry = dockables.get(i);
			retVal[i] = entry.factory.name;
		}
		return retVal;
	} 

	
	static final int SPLITTER_WIDTH = 10;
	DockablePanel dockablePanel;
	JPanel buttonPanel;

	
	void save()
	{
		jEdit.setIntegerProperty("view.dock." + position + ".dimension",
			dimension);
		if(current == null)
			jEdit.unsetProperty("view.dock." + position + ".last");
		else
		{
			jEdit.setProperty("view.dock." + position + ".last",
				current.factory.name);
		}
	} 

	
	void setDimension(int dimension)
	{
		if(dimension > SPLITTER_WIDTH)
			this.dimension = dimension - SPLITTER_WIDTH;
	} 

	
	void sortDockables()
	{
		buttonPanel.removeAll();
		buttonPanel.add(closeBox);
		buttonPanel.add(menuBtn);
		Collections.sort(buttons,new DockableWindowCompare());
		for(int i = 0; i < buttons.size(); i++)
		{
			buttonPanel.add(buttons.get(i));
		}
	} 

	
	
	int getWrappedDimension(int dimension)
	{
		return ((ButtonLayout)buttonPanel.getLayout())
			.getWrappedDimension(buttonPanel,dimension);
	} 

	

	
	private final DockableWindowManagerImpl wm;
	private final String position;
	private final JButton closeBox;
	private final JButton menuBtn;
	private final ButtonGroup buttonGroup;
	private final JToggleButton nullButton;
	private int dimension;
	private final List<DockableWindowManagerImpl.Entry> dockables;
	private final List<AbstractButton> buttons;
	private DockableWindowManagerImpl.Entry current;
	private JPopupMenu popup;

	
	private String mostRecent;
	

	

	
	static class DockableWindowCompare implements Comparator<AbstractButton>
	{
		public int compare(AbstractButton o1, AbstractButton o2)
		{
			String name1 = o1.getActionCommand();
			String name2 = o2.getActionCommand();
			return StandardUtilities.compareStrings(
				jEdit.getProperty(name1 + ".title",""),
				jEdit.getProperty(name2 + ".title",""),
				true);
		}
	} 

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(popup != null && popup.isVisible())
				popup.setVisible(false);

			if(evt.getSource() == closeBox)
				show((DockableWindowManagerImpl.Entry)null);
			else
			{
				if(wm.isDockableWindowVisible(evt.getActionCommand()))
					show((DockableWindowManagerImpl.Entry)null);
				else
					wm.showDockableWindow(evt.getActionCommand());
			}
		}
	} 

	
	class MenuMouseHandler extends MouseAdapter
	{
		public void mousePressed(MouseEvent evt)
		{
			if(popup != null && popup.isVisible())
			{
				popup.setVisible(false);
				return;
			}

			Component comp = (Component)evt.getSource();
			String dockable;
			if(comp instanceof JToggleButton)
				dockable = ((JToggleButton)comp).getActionCommand();
			else
				dockable = getCurrent();

			if(comp == menuBtn || GUIUtilities.isPopupTrigger(evt))
			{
				if(dockable == null)
				{
					popup = wm.createPopupMenu(PanelWindowContainer.this,null,false);
				}
				else
				{
					popup = wm.createPopupMenu(PanelWindowContainer.this,dockable,false);
				}

				int x, y;
				boolean point;
				if(comp == menuBtn)
				{
					x = 0;
					y = menuBtn.getHeight();
					point = false;
				}
				else
				{
					x = evt.getX();
					y = evt.getY();
					point = true;
				}
				GUIUtilities.showPopupMenu(popup,
					comp,x,y,point);
			}
		}
	} 

	
	static class DockBorder implements Border
	{
		String position;
		Insets insets;
		Color color1;
		Color color2;
		Color color3;

		
		DockBorder(String position)
		{
			this.position = position;
			insets = new Insets(
				position.equals(DockableWindowManagerImpl.BOTTOM)
					? SPLITTER_WIDTH : 0,
				position.equals(DockableWindowManagerImpl.RIGHT)
					? SPLITTER_WIDTH : 0,
				position.equals(DockableWindowManagerImpl.TOP)
					? SPLITTER_WIDTH : 0,
				position.equals(DockableWindowManagerImpl.LEFT)
					? SPLITTER_WIDTH : 0);
		} 

		
		public void paintBorder(Component c, Graphics g,
			int x, int y, int width, int height)
		{
			updateColors();

			if(color1 == null || color2 == null || color3 == null)
				return;

			if(position.equals(DockableWindowManagerImpl.BOTTOM))
				paintHorizBorder(g,x,y,width);
			else if(position.equals(DockableWindowManagerImpl.RIGHT))
				paintVertBorder(g,x,y,height);
			else if(position.equals(DockableWindowManagerImpl.TOP))
			{
				paintHorizBorder(g,x,y + height
					- SPLITTER_WIDTH,width);
			}
			else if(position.equals(DockableWindowManagerImpl.LEFT))
			{
				paintVertBorder(g,x + width
					- SPLITTER_WIDTH,y,height);
			}
		} 

		
		public Insets getBorderInsets(Component c)
		{
			return insets;
		} 

		
		public boolean isBorderOpaque()
		{
			return false;
		} 

		
		private void paintHorizBorder(Graphics g, int x, int y, int width)
		{
			g.setColor(color3);
			g.fillRect(x,y,width,SPLITTER_WIDTH);

			for(int i = 0; i < width / 4 - 1; i++)
			{
				g.setColor(color1);
				g.drawLine(x + (i << 2) + 2,y + 3,
					x + (i << 2) + 2,y + 3);
				g.setColor(color2);
				g.drawLine(x + (i << 2) + 3,y + 4,
					x + (i << 2) + 3,y + 4);
				g.setColor(color1);
				g.drawLine(x + (i << 2) + 4,y + 5,
					x + (i << 2) + 4,y + 5);
				g.setColor(color2);
				g.drawLine(x + (i << 2) + 5,y + 6,
					x + (i << 2) + 5,y + 6);
			}
		} 

		
		private void paintVertBorder(Graphics g, int x, int y, int height)
		{
			g.setColor(color3);
			g.fillRect(x,y,SPLITTER_WIDTH,height);

			for(int i = 0; i < height / 4 - 1; i++)
			{
				g.setColor(color1);
				g.drawLine(x + 3,y + (i << 2) + 2,
					x + 3,y + (i << 2) + 2);
				g.setColor(color2);
				g.drawLine(x + 4,y + (i << 2) + 3,
					x + 4,y + (i << 2) + 3);
				g.setColor(color1);
				g.drawLine(x + 5,y + (i << 2) + 4,
					x + 5,y + (i << 2) + 4);
				g.setColor(color2);
				g.drawLine(x + 6,y + (i << 2) + 5,
					x + 6,y + (i << 2) + 5);
			}
		} 

		
		private void updateColors()
		{
			if(UIManager.getLookAndFeel() instanceof MetalLookAndFeel)
			{
				color1 = MetalLookAndFeel.getControlHighlight();
				color2 = MetalLookAndFeel.getControlDarkShadow();
				color3 = MetalLookAndFeel.getControl();
			}
			else
			{
				color1 = color2 = color3 = null;
			}
		} 
	} 

	
	public static class RotatedTextIcon implements Icon
	{
		public static final int NONE = 0;
		public static final int CW = 1;
		public static final int CCW = 2;

		
		public RotatedTextIcon(int rotate, Font font, String text)
		{
			this.rotate = rotate;
			this.font = font;

			FontRenderContext fontRenderContext
				= new FontRenderContext(null,true,true);
			glyphs = font.createGlyphVector(fontRenderContext,text);
			width = (int)glyphs.getLogicalBounds().getWidth() + 4;
			

			LineMetrics lineMetrics = font.getLineMetrics(text,fontRenderContext);
			ascent = lineMetrics.getAscent();
			height = (int)lineMetrics.getHeight();

			renderHints = new RenderingHints(
				RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
			renderHints.put(RenderingHints.KEY_FRACTIONALMETRICS,
				RenderingHints.VALUE_FRACTIONALMETRICS_ON);
			renderHints.put(RenderingHints.KEY_RENDERING,
				RenderingHints.VALUE_RENDER_QUALITY);
		} 

		
		public int getIconWidth()
		{
			return (int)(rotate == RotatedTextIcon.CW
				|| rotate == RotatedTextIcon.CCW
				? height : width);
		} 

		
		public int getIconHeight()
		{
			return (int)(rotate == RotatedTextIcon.CW
				|| rotate == RotatedTextIcon.CCW
				? width : height);
		} 

		
		public void paintIcon(Component c, Graphics g, int x, int y)
		{
			Graphics2D g2d = (Graphics2D)g;
			g2d.setFont(font);
			AffineTransform oldTransform = g2d.getTransform();
			RenderingHints oldHints = g2d.getRenderingHints();

			g2d.setRenderingHints(renderHints);
			g2d.setColor(c.getForeground());

			
			if(rotate == RotatedTextIcon.NONE)
			{
				g2d.drawGlyphVector(glyphs,x + 2,y + ascent);
			} 
			
			else if(rotate == RotatedTextIcon.CW)
			{
				AffineTransform trans = new AffineTransform();
				trans.concatenate(oldTransform);
				trans.translate(x,y + 2);
				trans.rotate(Math.PI / 2,
					height / 2, width / 2);
				g2d.setTransform(trans);
				g2d.drawGlyphVector(glyphs,(height - width) / 2,
					(width - height) / 2
					+ ascent);
			} 
			
			else if(rotate == RotatedTextIcon.CCW)
			{
				AffineTransform trans = new AffineTransform();
				trans.concatenate(oldTransform);
				trans.translate(x,y - 2);
				trans.rotate(Math.PI * 3 / 2,
					height / 2, width / 2);
				g2d.setTransform(trans);
				g2d.drawGlyphVector(glyphs,(height - width) / 2,
					(width - height) / 2
					+ ascent);
			} 

			g2d.setTransform(oldTransform);
			g2d.setRenderingHints(oldHints);
		} 

		
		private final int rotate;
		private final Font font;
		private final GlyphVector glyphs;
		private final float width;
		private final float height;
		private final float ascent;
		private final RenderingHints renderHints;
		
	} 

	
	class ButtonLayout implements LayoutManager
	{
		
		public void addLayoutComponent(String name, Component comp) {} 

		
		public void removeLayoutComponent(Component comp) {} 

		
		
		int getWrappedDimension(JComponent parent, int dimension)
		{
			Insets insets = parent.getBorder()
				.getBorderInsets(parent);

			Component[] comp = parent.getComponents();
			if(comp.length <= 2)
				return 0;

			Dimension dim = comp[2].getPreferredSize();

			if(position.equals(DockableWindowManagerImpl.TOP)
				|| position.equals(DockableWindowManagerImpl.BOTTOM))
			{
				int width = dimension - insets.right;
				Dimension returnValue = preferredLayoutSizeLR(insets, comp, dim, width);
				return returnValue.height;
			}
			else
			{
				Dimension returnValue = preferredLayoutSizeTB(dimension, insets, comp, dim);
				return returnValue.width;
			}
		} 

		
		public Dimension preferredLayoutSize(Container parent)
		{
			Insets insets = ((JComponent)parent).getBorder()
				.getBorderInsets(parent);

			Component[] comp = parent.getComponents();
			if(comp.length <= 2)
			{
				
				return new Dimension(0,0);
			}

			Dimension dim = comp[2].getPreferredSize();

			if(position.equals(DockableWindowManagerImpl.TOP)
				|| position.equals(DockableWindowManagerImpl.BOTTOM))
			{
				int width = parent.getWidth() - insets.right;
				Dimension returnValue = preferredLayoutSizeLR(insets, comp, dim, width);
				return returnValue;
			}
			else
			{
				Dimension returnValue = preferredLayoutSizeTB(parent.getHeight(), insets, comp, dim);
				return returnValue;
			}
		} 

		
		public Dimension minimumLayoutSize(Container parent)
		{
			return preferredLayoutSize(parent);
		} 

		
		public void layoutContainer(Container parent)
		{
			Insets insets = ((JComponent)parent).getBorder()
				.getBorderInsets(parent);

			Component[] comp = parent.getComponents();
			if(comp.length <= 2)
			{
				for(int i = 0; i < comp.length; i++)
				{
					comp[i].setVisible(false);
				}
				return;
			}

			comp[0].setVisible(true);
			comp[1].setVisible(true);

			Dimension dim = comp[2].getPreferredSize();

			if(position.equals(DockableWindowManagerImpl.TOP)
				|| position.equals(DockableWindowManagerImpl.BOTTOM))
			{
				int width = parent.getWidth() - insets.right;
				int rowHeight = Math.max(dim.height,closeBox.getPreferredSize().width);
				int x = (rowHeight << 1) + insets.left;
				int y = insets.top;
				closeBox.setBounds(insets.left,insets.top,rowHeight,rowHeight);
				menuBtn.setBounds(insets.left + rowHeight,insets.top,rowHeight,rowHeight);

				for(int i = 2; i < comp.length; i++)
				{
					int btnWidth = comp[i].getPreferredSize().width;
					if(btnWidth + x > width)
					{
						x = insets.left;
						y += rowHeight;
					}
					comp[i].setBounds(x,y,btnWidth,rowHeight);
					x += btnWidth;
				}

				
			}
			else
			{
				int height = parent.getHeight() - insets.bottom;
				int colWidth = Math.max(dim.width,closeBox.getPreferredSize().height);
				int x = insets.left;
				int y = (colWidth << 1) + insets.top;
				closeBox.setBounds(insets.left,insets.top,colWidth,colWidth);
				menuBtn.setBounds(insets.left,insets.top + colWidth,colWidth,colWidth);

				for(int i = 2; i < comp.length; i++)
				{
					int btnHeight = comp[i].getPreferredSize().height;
					if(btnHeight + y > height)
					{
						x += colWidth;
						y = insets.top;
					}
					comp[i].setBounds(x,y,colWidth,btnHeight);
					y += btnHeight;
				}

				
			}
		} 

		
		private Dimension preferredLayoutSizeLR(Insets insets, Component[] comp, Dimension dim, int width)
		{
			int rowHeight = Math.max(dim.height,closeBox.getPreferredSize().width);
			int x = (rowHeight << 1) + insets.left;
			Dimension returnValue = new Dimension(0,rowHeight
				+ insets.top + insets.bottom);

			for(int i = 2; i < comp.length; i++)
			{
				int btnWidth = comp[i].getPreferredSize().width;
				if(btnWidth + x > width)
				{
					returnValue.height += rowHeight;
					x = insets.left;
				}

				x += btnWidth;
			}
			return returnValue;
		} 

		
		private Dimension preferredLayoutSizeTB(int dimension, Insets insets, Component[] comp, Dimension dim)
		{
			int height = dimension - insets.bottom;
			int colWidth = Math.max(dim.width,closeBox.getPreferredSize().height);
			int y = (colWidth << 1) + insets.top;
			Dimension returnValue = new Dimension(colWidth
				+ insets.left + insets.right,0);

			for(int i = 2; i < comp.length; i++)
			{
				int btnHeight = comp[i].getPreferredSize().height;
				if(btnHeight + y > height)
				{
					returnValue.width += colWidth;
					y = insets.top;
				}

				y += btnHeight;
			}
			return returnValue;
		} 
	} 

	public void show(String name)
	{
		DockableWindowManagerImpl.Entry entry = null;
		if (name != null)
		{
			wm.showDockableWindow(name);
			wm.hideDockableWindow(name);
		}
		show(entry);
	}

	
}
