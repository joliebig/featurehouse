

package org.gjt.sp.jedit.gui;


import javax.swing.border.EmptyBorder;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
import java.util.Random;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class TipOfTheDay extends EnhancedDialog
{
	
	public TipOfTheDay(View view)
	{
		super(view,jEdit.getProperty("tip.title"),false);

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		JLabel label = new JLabel(jEdit.getProperty("tip.caption"));
		label.setFont(new Font("SansSerif",Font.PLAIN,24));
		label.setForeground(UIManager.getColor("Button.foreground"));
		content.add(BorderLayout.NORTH,label);

		tipText = new JEditorPane();
		tipText.setEditable(false);
		tipText.setContentType("text/html");

		nextTip();

		JScrollPane scroller = new JScrollPane(tipText);
		scroller.setPreferredSize(new Dimension(150,150));
		content.add(BorderLayout.CENTER,scroller);

		ActionHandler actionHandler = new ActionHandler();

		Box buttons = new Box(BoxLayout.X_AXIS);

		showNextTime = new JCheckBox(jEdit.getProperty("tip.show-next-time"),
			jEdit.getBooleanProperty("tip.show"));
		showNextTime.addActionListener(actionHandler);
		buttons.add(showNextTime);

		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(Box.createGlue());

		nextTip = new JButton(jEdit.getProperty("tip.next-tip"));
		nextTip.addActionListener(actionHandler);
		buttons.add(nextTip);

		buttons.add(Box.createHorizontalStrut(6));

		close = new JButton(jEdit.getProperty("common.close"));
		close.addActionListener(actionHandler);
		buttons.add(close);
		content.getRootPane().setDefaultButton(close);

		Dimension dim = nextTip.getPreferredSize();
		dim.width = Math.max(dim.width,close.getPreferredSize().width);
		nextTip.setPreferredSize(dim);
		close.setPreferredSize(dim);

		content.add(BorderLayout.SOUTH,buttons);

		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		pack();
		setLocationRelativeTo(view);
		setVisible(true);
	} 

	
	public void ok()
	{
		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

	

	
	private JCheckBox showNextTime;
	private JButton nextTip, close;
	private JEditorPane tipText;
	private int currentTip = -1;
	

	
	private void nextTip()
	{
		File[] tips = new File(MiscUtilities.constructPath(
			jEdit.getJEditHome(),"doc","tips")).listFiles();
		if(tips == null || tips.length == 0)
		{
			tipText.setText(jEdit.getProperty("tip.not-found"));
			return;
		}

		int count = tips.length;

		
		
		int tipToShow = currentTip;
		while(tipToShow == currentTip || !tips[tipToShow].getName().endsWith(".html"))
			tipToShow = Math.abs(new Random().nextInt()) % count;
		try
		{
			tipText.setPage(tips[tipToShow].toURL());
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
		}
	} 

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == showNextTime)
			{
				jEdit.setBooleanProperty("tip.show",showNextTime
					.isSelected());
			}
			else if(source == nextTip)
				nextTip();
			else if(source == close)
				dispose();
		}
	} 
}
