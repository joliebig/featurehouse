

package org.gjt.sp.jedit.options;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JTextField;
import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.gui.PingPongList;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.StandardUtilities;
import static java.awt.GridBagConstraints.BOTH;
import static java.util.Arrays.sort;
import static javax.swing.Box.createHorizontalBox;
import static javax.swing.Box.createHorizontalStrut;
import static org.gjt.sp.jedit.jEdit.getBooleanProperty;
import static org.gjt.sp.jedit.jEdit.getProperty;
import static org.gjt.sp.jedit.jEdit.setBooleanProperty;
import static org.gjt.sp.jedit.jEdit.unsetProperty;
import static org.gjt.sp.jedit.MiscUtilities.getEncodings;




public class EncodingsOptionPane extends AbstractOptionPane
{
	
	private JComboBox defaultEncoding;
	private JCheckBox encodingAutodetect;
	private JTextField encodingDetectors;
	private JTextField fallbackEncodings;

	private JButton selectAllButton;
	private JButton selectNoneButton;
	private PingPongList<String> pingPongList;
	

	
	public EncodingsOptionPane()
	{
		super("encodings");
	} 

	
	@Override
	protected void _init()
	{
		
		String[] encodings = getEncodings(true);
		sort(encodings,new StandardUtilities.StringCompare<String>(true));
		defaultEncoding = new JComboBox(encodings);
		defaultEncoding.setEditable(true);
		defaultEncoding.setSelectedItem(jEdit.getProperty("buffer."+JEditBuffer.ENCODING,
			System.getProperty("file.encoding")));
		addComponent(jEdit.getProperty("options.general.encoding"),defaultEncoding);

		
		encodingAutodetect = new JCheckBox(jEdit.getProperty(
			"options.general.encodingAutodetect"));
		encodingAutodetect.setSelected(jEdit.getBooleanProperty(
			"buffer.encodingAutodetect"));
		addComponent(encodingAutodetect,BOTH);
		
		
		encodingDetectors = new JTextField(jEdit.getProperty(
			"encodingDetectors","BOM XML-PI"));
		addComponent(jEdit.getProperty("options.general.encodingDetectors"),encodingDetectors);

		
		fallbackEncodings = new JTextField(jEdit.getProperty(
			"fallbackEncodings",""));
		fallbackEncodings.setToolTipText(jEdit.getProperty(
			"options.general.fallbackEncodings.tooltip"));
		addComponent(jEdit.getProperty("options.general.fallbackEncodings"),fallbackEncodings);

		
		encodings = getEncodings(false);
		sort(encodings,new StandardUtilities.StringCompare<String>(true));
		List<String> availableEncodings = new ArrayList<String>();
		List<String> selectedEncodings = new ArrayList<String>();
		for (String encoding : encodings)
		{
			boolean selected = !getBooleanProperty("encoding.opt-out."+encoding,false);
			if (selected)
				selectedEncodings.add(encoding);
			else
				availableEncodings.add(encoding);
		}
		pingPongList = new PingPongList<String>(availableEncodings, selectedEncodings);
		pingPongList.setLeftTitle(getProperty("options.encodings.available"));
		pingPongList.setRightTitle(getProperty("options.encodings.selected"));
		pingPongList.setLeftTooltip(getProperty("options.encodings.available.tooltip"));
		pingPongList.setRightTooltip(getProperty("options.encodings.selected.tooltip"));
		addComponent(pingPongList,BOTH);

		
		Box buttonsBox = createHorizontalBox();
		buttonsBox.add(createHorizontalStrut(12));
		
		ActionHandler actionHandler = new ActionHandler();
		selectAllButton = new JButton(getProperty("options.encodings.selectAll"));
		selectAllButton.addActionListener(actionHandler);
		selectAllButton.setEnabled(pingPongList.getLeftSize() != 0);
		buttonsBox.add(selectAllButton);
		buttonsBox.add(createHorizontalStrut(12));

		selectNoneButton = new JButton(getProperty("options.encodings.selectNone"));
		selectNoneButton.addActionListener(actionHandler);
		selectNoneButton.setEnabled(pingPongList.getRightSize() != 0);
		buttonsBox.add(selectNoneButton);
		buttonsBox.add(createHorizontalStrut(12));
		
		addComponent(buttonsBox);
	} 

	
	@Override
	protected void _save()
	{
		jEdit.setProperty("buffer."+ JEditBuffer.ENCODING,(String)
			defaultEncoding.getSelectedItem());
		jEdit.setBooleanProperty("buffer.encodingAutodetect",
			encodingAutodetect.isSelected());
		jEdit.setProperty("encodingDetectors",encodingDetectors.getText());
		jEdit.setProperty("fallbackEncodings",fallbackEncodings.getText());
		Iterator<String> available = pingPongList.getLeftDataIterator();
		while (available.hasNext())
		{
			String encoding = available.next();
			setBooleanProperty("encoding.opt-out."+encoding,true);

		}
		Iterator<String> selected = pingPongList.getRightDataIterator();
		while (selected.hasNext())
		{
			String encoding = selected.next();
			unsetProperty("encoding.opt-out."+encoding);
		}
	} 

	

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent ae)
		{
			Object source = ae.getSource();
			if (source == selectAllButton)
			{
				pingPongList.moveAllToRight();
			}
			else if (source == selectNoneButton)
			{
				pingPongList.moveAllToLeft();
			}      
		}
	} 
	
} 
