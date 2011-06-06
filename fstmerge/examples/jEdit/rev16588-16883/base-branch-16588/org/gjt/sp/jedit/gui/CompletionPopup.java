

package org.gjt.sp.jedit.gui;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;

import javax.swing.AbstractListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JWindow;
import javax.swing.ListSelectionModel;
import javax.swing.ListCellRenderer;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.View;



public class CompletionPopup extends JWindow
{
	
	
	public interface Candidates
	{
		
		public int getSize();

		
		public boolean isValid();

		
		public void complete(int index);
	
		
		public Component getCellRenderer(JList list, int index,
			boolean isSelected, boolean cellHasFocus);

		
		public String getDescription(int index);
	} 

	
	 
	public CompletionPopup(View view)
	{
		super(view);
		this.view = view;
		this.keyHandler = new KeyHandler();
		this.candidates = null;
		this.list = new JList();

		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.setCellRenderer(new CellRenderer());
		list.addKeyListener(keyHandler);
		list.addMouseListener(new MouseHandler());

		JPanel content = new JPanel(new BorderLayout());
		content.setFocusTraversalKeysEnabled(false);
		
		
		JScrollPane scroller = new JScrollPane(list,
			ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		content.add(scroller, BorderLayout.CENTER);
		setContentPane(content);
		addWindowFocusListener(new WindowFocusHandler());
	}

	public CompletionPopup(View view, Point location)
	{
		this(view);
		if (location != null)
		{
			setLocation(location);
		}
	} 

	
	
	public void dispose()
	{
		if (isDisplayable())
		{
			if (view.getKeyEventInterceptor() == keyHandler)
			{
				view.setKeyEventInterceptor(null);
			}
			super.dispose();

			
			
			
			
			
			
			
			
			
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					view.getTextArea().requestFocus();
				}
			});
		}
	} 

	
	
	public void reset(Candidates candidates, boolean active)
	{
		if(candidates == null || !candidates.isValid()
			|| candidates.getSize() <= 0)
		{
			dispose();
			return;
		}

		this.candidates = candidates;
		list.setModel(new CandidateListModel());
		list.setVisibleRowCount(Math.min(candidates.getSize(),8));
		pack();
		setLocation(fitInScreen(getLocation(null),this,
			view.getTextArea().getPainter().getFontMetrics().getHeight()));
		if (active)
		{
			setSelectedIndex(0);
			GUIUtilities.requestFocus(this,list);
		}
		setVisible(true);
		view.setKeyEventInterceptor(keyHandler);
	} 

	
	
	public Candidates getCandidates()
	{
		return candidates;
	} 

	
	
	public int getSelectedIndex()
	{
		return list.getSelectedIndex();
	} 

	
	
	public void setSelectedIndex(int index)
	{
		if (candidates != null
			&& 0 <= index && index < candidates.getSize())
		{
			list.setSelectedIndex(index);
			list.ensureIndexIsVisible(index);
			String description = candidates.getDescription(index);
			if (description != null)
			{
				view.getStatus().setMessageAndClear(description);
			}
		}
	} 

	
	
	public boolean doSelectedCompletion()
	{
		int selected = list.getSelectedIndex();
		if (candidates != null &&
			0 <= selected && selected < candidates.getSize())
		{
			candidates.complete(selected);
			dispose();
			return true;
		}
		return false;
	} 

	
	
	protected void keyPressed(KeyEvent e)
	{
	} 

	
	
	protected void keyTyped(KeyEvent e)
	{
	} 

	

	
	private final View view;
	private final KeyHandler keyHandler;
	private Candidates candidates;
	private final JList list;
	

	
	private static Point fitInScreen(Point p, Window w, int lineHeight)
	{
		Rectangle screenSize = w.getGraphicsConfiguration().getBounds();
		if(p.y + w.getHeight() >= screenSize.height)
			p.y = p.y - w.getHeight() - lineHeight;
		return p;
	} 

	
	private void moveRelative(int n)
	{
		int selected = list.getSelectedIndex();

		int newSelect = selected + n;
		if (newSelect < 0)
		{
			newSelect = 0;
		}
		else
		{
			int numItems = list.getModel().getSize();
			if(numItems < 1)
			{
				return;
			}
			if(newSelect >= numItems)
			{
				newSelect = numItems - 1;
			}
		}

		if(newSelect != selected)
		{
			setSelectedIndex(newSelect);
		}
	} 

	
	private void moveRelativePages(int n)
	{
		int pageSize = list.getVisibleRowCount() - 1;
		moveRelative(pageSize * n);
	} 

	
	private void passKeyEventToView(KeyEvent e)
	{
		
		assert (view.getKeyEventInterceptor() == keyHandler);
		view.setKeyEventInterceptor(null);

		
		
		
		view.getInputHandler().processKeyEvent(e, View.ACTION_BAR, false);

		
		
		if (this.isDisplayable())
		{
			view.setKeyEventInterceptor(keyHandler);
		}
	} 

	
	private class CandidateListModel extends AbstractListModel
	{
		public int getSize()
		{
			return candidates.getSize();
		}

		public Object getElementAt(int index)
		{
			
			
			
			return candidates;
		}
	} 

	
	private class CellRenderer implements ListCellRenderer
	{
		public Component getListCellRendererComponent(JList list,
			Object value, int index,
			boolean isSelected, boolean cellHasFocus)
		{
			return candidates.getCellRenderer(list, index,
				isSelected, cellHasFocus);
		}
	} 

	
	private class KeyHandler extends KeyAdapter
	{
		
		public void keyPressed(KeyEvent e)
		{
			CompletionPopup.this.keyPressed(e);

			if (candidates == null || !candidates.isValid())
			{
				dispose();
			}
			else if (!e.isConsumed())
			{
				switch(e.getKeyCode())
				{
				case KeyEvent.VK_TAB:
				case KeyEvent.VK_ENTER:
					if (doSelectedCompletion())
					{
						e.consume();
					}
					else
					{
						dispose();
					}
					break;
				case KeyEvent.VK_ESCAPE:
					dispose();
					e.consume();
					break;
				case KeyEvent.VK_UP:
					moveRelative(-1);
					e.consume();
					break;
				case KeyEvent.VK_DOWN:
					moveRelative(1);
					e.consume();
					break;
				case KeyEvent.VK_PAGE_UP:
					moveRelativePages(-1);
					e.consume();
					break;
				case KeyEvent.VK_PAGE_DOWN:
					moveRelativePages(1);
					e.consume();
					break;
				default:
					if(e.isActionKey()
						|| e.isControlDown()
						|| e.isAltDown()
						|| e.isMetaDown())
					{
						dispose();
					}
					break;
				}
			}

			if (!e.isConsumed())
			{
				passKeyEventToView(e);
			}
		} 

		
		public void keyTyped(KeyEvent e)
		{
			CompletionPopup.this.keyTyped(e);

			if (candidates == null || !candidates.isValid())
			{
				dispose();
			}

			if (!e.isConsumed())
			{
				passKeyEventToView(e);
			}
		} 
	} 

	
	private class MouseHandler extends MouseAdapter
	{
		public void mouseClicked(MouseEvent e)
		{
			if (doSelectedCompletion())
			{
				e.consume();
			}
			else
			{
				dispose();
			}
		}
	} 

	
	private class WindowFocusHandler implements WindowFocusListener
	{
		public void windowGainedFocus(WindowEvent e)
		{
		}

		public void windowLostFocus(WindowEvent e)
		{
			dispose();
		}
	} 

	
}
