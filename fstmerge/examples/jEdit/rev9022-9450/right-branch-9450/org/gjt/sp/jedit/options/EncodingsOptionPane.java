

package org.gjt.sp.jedit.options;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.MiscUtilities.StringICaseCompare;




public class EncodingsOptionPane extends AbstractOptionPane
{
	
	public EncodingsOptionPane()
	{
		super("encodings");
	} 

	
	protected void _init()
	{
		setLayout(new BorderLayout());

		add(new JLabel(jEdit.getProperty("options.encodings.selectEncodings")),BorderLayout.NORTH);

		Box encodingsBox = Box.createVerticalBox();
		String[] encodings = MiscUtilities.getEncodings(false);
		Arrays.sort(encodings,new MiscUtilities.StringICaseCompare());
		int encodingsAmount = encodings.length;
		encodingCheckBoxArray = new JCheckBox[encodingsAmount];
		for (int i=0 ; i<encodingsAmount ; i++) {
			String encoding = encodings[i];
			JCheckBox encodingCheckBox = new JCheckBox(encoding,!jEdit.getBooleanProperty("encoding.opt-out."+encoding,false));
			encodingCheckBoxArray[i] = encodingCheckBox;
			encodingsBox.add(encodingCheckBox);
		}
		JScrollPane encodingsScrollPane = new JScrollPane(encodingsBox);
		Dimension d = encodingsBox.getPreferredSize();
		d.height = Math.min(d.height,200);
		encodingsScrollPane.setPreferredSize(d);
		add(encodingsScrollPane,BorderLayout.CENTER);

		ActionHandler actionHandler = new ActionHandler();
		Box buttonsBox = Box.createHorizontalBox();
		selectAllButton = new JButton(jEdit.getProperty("options.encodings.selectAll"));
		selectAllButton.addActionListener(actionHandler);
		buttonsBox.add(selectAllButton);
		selectNoneButton = new JButton(jEdit.getProperty("options.encodings.selectNone"));
		selectNoneButton.addActionListener(actionHandler);
		buttonsBox.add(selectNoneButton);
		add(buttonsBox,BorderLayout.SOUTH);
	} 

	
	protected void _save()
	{
		for (int i=0, c=encodingCheckBoxArray.length ; i<c ; i++)
		{
			JCheckBox encodingCheckBox = encodingCheckBoxArray[i];
			if (encodingCheckBox.isSelected())
			{
				jEdit.unsetProperty("encoding.opt-out."+encodingCheckBox.getText());
			}
			else
			{
				jEdit.setBooleanProperty("encoding.opt-out."+encodingCheckBox.getText(),true);
			}
		}
	} 

	

	
	private JCheckBox[] encodingCheckBoxArray;
	private JButton selectAllButton;
	private JButton selectNoneButton;
	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			boolean select;
			Object source = e.getSource();
			if (source == selectAllButton)
			{
				select = true;
			}
			else if (source == selectNoneButton)
			{
				select = false;
			}
			else
			{
				return;
			}
			for (int i=0, c=encodingCheckBoxArray.length ; i<c ; i++)
			{
				encodingCheckBoxArray[i].setSelected(select);
			}
		}
	} 
} 
