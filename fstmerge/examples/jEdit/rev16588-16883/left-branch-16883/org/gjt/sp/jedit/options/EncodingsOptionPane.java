

package org.gjt.sp.jedit.options;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.gui.JCheckBoxList;
import org.gjt.sp.jedit.gui.JCheckBoxList.Entry;
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
	
	private JCheckBoxList encodingsList;
	private JButton selectAllButton;
	private JButton selectNoneButton;
	

	
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
		Vector<Entry> encodingEntriesVector = new Vector<Entry>();
		boolean enableSelectAll = false;
		boolean enableSelectNone = false;
		for (String encoding : encodings)
		{
			boolean selected = !getBooleanProperty("encoding.opt-out."+encoding,false);
			enableSelectAll = enableSelectAll || !selected;
			enableSelectNone = enableSelectNone || selected;
			encodingEntriesVector.add(new Entry(selected,encoding));
		}
		encodingsList = new JCheckBoxList(encodingEntriesVector);
		encodingsList.getModel().addTableModelListener(new TableModelHandler());
		JScrollPane encodingsScrollPane = new JScrollPane(encodingsList);
		encodingsScrollPane.setBorder(
			new TitledBorder(getProperty("options.encodings.selectEncodings")));
		Dimension d = encodingsList.getPreferredSize();
		d.height = Math.min(d.height,200);
		encodingsScrollPane.setPreferredSize(d);
		addComponent(encodingsScrollPane,BOTH);

		
		Box buttonsBox = createHorizontalBox();
		buttonsBox.add(createHorizontalStrut(12));
		
		ActionHandler actionHandler = new ActionHandler();
		selectAllButton = new JButton(getProperty("options.encodings.selectAll"));
		selectAllButton.addActionListener(actionHandler);
		selectAllButton.setEnabled(enableSelectAll);
		buttonsBox.add(selectAllButton);
		buttonsBox.add(createHorizontalStrut(12));

		selectNoneButton = new JButton(getProperty("options.encodings.selectNone"));
		selectNoneButton.addActionListener(actionHandler);
		selectNoneButton.setEnabled(enableSelectNone);
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
		
		for (Entry entry : encodingsList.getValues())
		{
			if (entry.isChecked())
			{
				unsetProperty("encoding.opt-out."+entry.getValue());
			}
			else
			{
				setBooleanProperty("encoding.opt-out."+entry.getValue(),true);
			}
		}
	} 

	

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent ae)
		{
			Object source = ae.getSource();
			if (source == selectAllButton)
			{
				encodingsList.selectAll();
			}
			else if (source == selectNoneButton)
			{
				for (int i=0, c=encodingsList.getRowCount() ; i<c ; i++)
				{
					encodingsList.setValueAt(false,i,0);
				}
			}
		}
	} 

	
	private class TableModelHandler implements TableModelListener
	{
		public void tableChanged(TableModelEvent tme)
		{
			int checkedAmount = encodingsList.getCheckedValues().length;
			if (checkedAmount == 0)
			{
				selectNoneButton.setEnabled(false);
			}
			else
			{
				selectNoneButton.setEnabled(true);
			}
			if (encodingsList.getValues().length == checkedAmount)
			{
				selectAllButton.setEnabled(false);
			}
			else
			{
				selectAllButton.setEnabled(true);
			}
		}
	} 

	

} 
