

package org.gjt.sp.jedit.gui;


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.EditBus.EBHandler;
import org.gjt.sp.jedit.gui.RolloverButton;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.msg.BufferUpdate;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.msg.ViewUpdate;


public class MarkerViewer extends JPanel implements ActionListener
{
	
	public MarkerViewer(View view)
	{
		super(new BorderLayout());
		this.view = view;
		Box toolBar = new Box(BoxLayout.X_AXIS);

		toolBar.add(new JLabel(GUIUtilities.prettifyMenuLabel(
			jEdit.getProperty("markers.label"))));

		toolBar.add(Box.createGlue());

		RolloverButton addMarker = new RolloverButton(
			GUIUtilities.loadIcon("Plus.png"));
		addMarker.setToolTipText(GUIUtilities.prettifyMenuLabel(
			jEdit.getProperty("add-marker.label")));
		addMarker.addActionListener(this);
		addMarker.setActionCommand("add-marker");
		toolBar.add(addMarker);

		previous = new RolloverButton(GUIUtilities.loadIcon("ArrowL.png"));
		previous.setToolTipText(GUIUtilities.prettifyMenuLabel(
			jEdit.getProperty("prev-marker.label")));
		previous.addActionListener(this);
		previous.setActionCommand("prev-marker");
		toolBar.add(previous);

		next = new RolloverButton(GUIUtilities.loadIcon("ArrowR.png"));
		next.setToolTipText(GUIUtilities.prettifyMenuLabel(
			jEdit.getProperty("next-marker.label")));
		next.addActionListener(this);
		next.setActionCommand("next-marker");
		toolBar.add(next);

		clear = new RolloverButton(GUIUtilities.loadIcon("Clear.png"));
		clear.setToolTipText(GUIUtilities.prettifyMenuLabel(
			jEdit.getProperty("remove-all-markers.label")));
		clear.addActionListener(this);
		clear.setActionCommand("clear");
		toolBar.add(clear);


		add(BorderLayout.NORTH, toolBar);

		markerList = new JList();
		markerList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		markerList.setCellRenderer(new Renderer());
		markerList.addMouseListener(new MouseHandler());

		add(BorderLayout.CENTER,new JScrollPane(markerList));

		refreshList();
	} 

	
	public boolean requestDefaultFocus()
	{
		markerList.requestFocus();
		return true;
	} 

	
	public void actionPerformed(ActionEvent evt)
	{
		String cmd = evt.getActionCommand();
		if (cmd.equals("clear"))
			view.getBuffer().removeAllMarkers();
		else if (cmd.equals("add-marker"))
			view.getEditPane().addMarker();
		else if (cmd.equals("next-marker"))
		{
			view.getEditPane().goToNextMarker(false);
			updateSelection();
		}
		else if (cmd.equals("prev-marker"))
		{
			view.getEditPane().goToPrevMarker(false);
			updateSelection();
		}
	} 

	
	@EBHandler
	public void handleEditPaneUpdate(EditPaneUpdate epu)
	{
		if (epu.getEditPane().getView().equals(view) &&
			epu.getWhat().equals(EditPaneUpdate.BUFFER_CHANGED))
		{
			refreshList();
		}
	} 

	
	@EBHandler
	public void handleViewUpdate(ViewUpdate vu)
	{
		if (vu.getView().equals(view) &&
			vu.getWhat().equals(ViewUpdate.EDIT_PANE_CHANGED))
		{
			refreshList();
		}
	} 

	
	@EBHandler
	public void handleBufferUpdate(BufferUpdate bu)
	{
		if (view.getBuffer().equals(bu.getBuffer()) &&
			(bu.getWhat().equals(BufferUpdate.MARKERS_CHANGED) || bu.getWhat().equals(BufferUpdate.LOADED)))
		{
			refreshList();
		}
	}

	
	public void addNotify()
	{
		super.addNotify();
		EditBus.addToBus(this);
	} 

	
	public void removeNotify()
	{
		super.removeNotify();
		EditBus.removeFromBus(this);
	} 

	

	
	private JList markerList;
	private View view;
	private RolloverButton previous;
	private RolloverButton next;
	private RolloverButton clear;
	

	
	private void refreshList()
	{
		java.util.Vector markers = view.getBuffer().getMarkers();
		if (markers.size() > 0)
		{
			markerList.setListData(markers);
			markerList.setEnabled(true);
			next.setEnabled(true);
			previous.setEnabled(true);
			clear.setEnabled(true);
		}
		else
		{
			markerList.setListData(new Object[] {
				jEdit.getProperty("no-markers.label") });
			markerList.setEnabled(false);
			next.setEnabled(false);
			previous.setEnabled(false);
			clear.setEnabled(false);
		}

	} 

	
	private void goToSelectedMarker()
	{
		Object value = markerList.getSelectedValue();
		if(!(value instanceof Marker))
			return;

		Marker mark = (Marker)value;
		view.getTextArea().setCaretPosition(mark.getPosition());
		view.toFront();
		view.requestFocus();
		view.getTextArea().requestFocus();
	} 

	
	private void updateSelection()
	{
		ListModel model = markerList.getModel();
		int currentLine = view.getTextArea().getCaretLine();
		Buffer buffer = view.getBuffer();
		for (int i = 0; i < model.getSize(); i++)
		{
			Object o = model.getElementAt(i);
			if (o instanceof Marker)
			{
				Marker mark = (Marker)model.getElementAt(i);
				if (buffer.getLineOfOffset(mark.getPosition()) == currentLine)
				{
					markerList.setSelectedIndex(i);
					break;
				}
			}
		}

	} 

	

	

	
	class Renderer extends DefaultListCellRenderer
	{
		public Component getListCellRendererComponent(
			JList list, Object value, int index,
			boolean isSelected, boolean cellHasFocus)
		{
			super.getListCellRendererComponent(list,value,
			index,isSelected,cellHasFocus);

			if(value instanceof Marker)
			{
				Marker mark = (Marker)value;
				JEditTextArea textArea = view.getTextArea();
				int pos = textArea.getLineOfOffset(mark.getPosition());
				String txt = view.getTextArea().getLineText(pos);
				if (txt.equals(""))
					txt = jEdit.getProperty("markers.blank-line");
				char shortcut_char = mark.getShortcut();
				String shortcut = "";
				if (shortcut_char > 0)
					shortcut = "["+shortcut_char+"]";
				setText((pos+1)+" "+shortcut+": "+txt);
			}
			return this;
		}
	} 

	
	class MouseHandler extends MouseAdapter
	{
		public void mousePressed(MouseEvent evt)
		{
			if(evt.isConsumed())
				return;

			int index = markerList.locationToIndex(evt.getPoint());
			markerList.setSelectedIndex(index);

			goToSelectedMarker();
		}
	} 

	
}
