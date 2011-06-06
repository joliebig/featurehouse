

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import org.gjt.sp.jedit.*;


public class PastePrevious extends EnhancedDialog
implements ActionListener, ListSelectionListener, MouseListener
{
	
	public PastePrevious(View view)
	{
		super(view,jEdit.getProperty("pasteprev.title"),true);
		this.view = view;

		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		clipHistory = HistoryModel.getModel("clipboard");

		clips = new JList(new AbstractListModel() {
			public int getSize()
			{
				return clipHistory.getSize();
			}

			public Object getElementAt(int index)
			{
				StringBuffer buf = new StringBuffer();
				String item = clipHistory.getItem(index);
				
				
				if(item.toLowerCase().startsWith("<html>"))
					buf.append(' ');
				boolean ws = true;
				for(int i = 0; i < item.length(); i++)
				{
					char ch = item.charAt(i);
					if(Character.isWhitespace(ch))
					{
						if(ws)
							;
						else
						{
							buf.append(' ');
							ws = true;
						}
					}
					else
					{
						ws = false;
						buf.append(ch);
					}
				}
				return buf.toString();
			}
		});

		clips.setVisibleRowCount(16);

		clips.addMouseListener(this);
		clips.addListSelectionListener(this);

		insert = new JButton(jEdit.getProperty("pasteprev.insert"));
		cancel = new JButton(jEdit.getProperty("common.cancel"));

		JLabel label = new JLabel(jEdit.getProperty("pasteprev.caption"));
		label.setBorder(new EmptyBorder(0,0,6,0));
		content.add(BorderLayout.NORTH,label);

		JScrollPane scroller = new JScrollPane(clips);
		Dimension dim = scroller.getPreferredSize();
		scroller.setPreferredSize(new Dimension(640,dim.height));

		content.add(scroller, BorderLayout.CENTER);

		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel,BoxLayout.X_AXIS));
		panel.setBorder(new EmptyBorder(12,0,0,0));
		panel.add(Box.createGlue());
		panel.add(insert);
		panel.add(Box.createHorizontalStrut(6));
		panel.add(cancel);
		panel.add(Box.createGlue());
		content.add(panel, BorderLayout.SOUTH);

		if(clipHistory.getSize() >= 1)
			clips.setSelectedIndex(0);
		updateButtons();

		getRootPane().setDefaultButton(insert);
		insert.addActionListener(this);
		cancel.addActionListener(this);

		GUIUtilities.requestFocus(this,clips);

		pack();
		setLocationRelativeTo(view);
		show();
	} 

	
	public void ok()
	{
		int selected = clips.getSelectedIndex();

		if(selected == -1)
		{
			view.getToolkit().beep();
			return;
		}

		String clip = clipHistory.getItem(selected);
		view.getTextArea().setSelectedText(clip);

		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

	
	public void actionPerformed(ActionEvent evt)
	{
		Object source = evt.getSource();
		if(source == insert)
			ok();
		else if(source == cancel)
			cancel();
	} 

	
	public void mouseClicked(MouseEvent evt)
	{
		if(evt.getClickCount() == 2)
			ok();
	} 

	
	public void mouseEntered(MouseEvent evt) {}
	public void mouseExited(MouseEvent evt) {}
	public void mousePressed(MouseEvent evt) {}
	public void mouseReleased(MouseEvent evt) {}
	

	
	public void valueChanged(ListSelectionEvent evt)
	{
		updateButtons();
	} 

	

	
	private View view;
	private JList clips;
	private HistoryModel clipHistory;
	private JButton insert;
	private JButton cancel;
	

	
	private void updateButtons()
	{
		int selected = clips.getSelectedIndex();
		insert.setEnabled(selected != -1);
	} 

	
}
