

package org.gjt.sp.jedit.help;


import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.RolloverButton;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;



public class HistoryButton extends JPanel implements ActionListener
{
	public static final int BACK    = 0;
	public static final int FORWARD = 1;

	
	private int type;
	private HelpHistoryModel history;
	private RolloverButton arrow_button;
	private RolloverButton drop_button;
	private JPopupMenu historyList;
	private ActionListener arrowActionListener;
	
	
	
	public HistoryButton(int type, HelpHistoryModel model)
	{
		super();
		arrow_button = new RolloverButton(GUIUtilities.loadIcon(
			jEdit.getProperty(
				(type==BACK?
					"helpviewer.back.icon"
					:
					"helpviewer.forward.icon"
					))));
		arrow_button.setToolTipText(
			jEdit.getProperty((type==BACK?
				"helpviewer.back.label"
				:
				"helpviewer.forward.label"
				)));
		Box box = new Box(BoxLayout.X_AXIS);
		drop_button = new RolloverButton(GUIUtilities.loadIcon("ToolbarMenu.gif"));
		drop_button.addActionListener(new DropActionHandler());
		box.add(arrow_button);
		box.add(drop_button);
		this.setMaximumSize(new Dimension(
			drop_button.getPreferredSize().width +
			arrow_button.getPreferredSize().width +
			5,
			arrow_button.getPreferredSize().height+10)
			);
		this.add(box);
		this.type = type;
		this.history = model;
	} 
	
	
	public void setEnabled(boolean state)
	{
		super.setEnabled(state);
		drop_button.setEnabled(state);
		arrow_button.setEnabled(state);
	} 
	
	
	public void addActionListener(ActionListener al)
	{
		arrow_button.addActionListener(this);
		arrowActionListener = al;
	} 
	
	
	public void actionPerformed(ActionEvent evt)
	{
		arrowActionListener.actionPerformed(
			new ActionEvent(this,
				ActionEvent.ACTION_PERFORMED,
				evt.getActionCommand(),
				evt.getWhen(),
				evt.getModifiers()
				)
			);
	} 
	
	
	private HistoryButton getParentHistoryButton()
	{
		return this;
	} 
	
	
	
	
	class DropActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			historyList = new JPopupMenu();
			HelpHistoryModel.HistoryEntry[] urls;
			if (type == BACK)
				urls = history.getPreviousURLs();
			else
				urls = history.getNextURLs();
			if (urls != null)
			{
				if (type==BACK) {				
					for (int i=urls.length-1;i>=0;i--)
						if (urls[i]!=null)
							historyList.add(new HistoryListActionHandler(urls[i]));
				}
				else
				{					
					for (int i=0;i<urls.length;i++)
						if (urls[i]!=null)
							historyList.add(new HistoryListActionHandler(urls[i]));
				}

					
				historyList.show((JComponent)evt.getSource(),0,0);
			}
		} 
	} 
	
	
	class HistoryListActionHandler extends AbstractAction
	{
		HelpHistoryModel.HistoryEntry entry;
		
		
		HistoryListActionHandler(HelpHistoryModel.HistoryEntry entry)
		{
			super(entry.title);
			this.entry = entry;
			this.putValue(Action.ACTION_COMMAND_KEY,entry.url);
		} 
		
		
		public void actionPerformed(ActionEvent ae)
		{
			history.setCurrentEntry(entry);
			getParentHistoryButton().actionPerformed(ae);
		} 
	} 

	
}

