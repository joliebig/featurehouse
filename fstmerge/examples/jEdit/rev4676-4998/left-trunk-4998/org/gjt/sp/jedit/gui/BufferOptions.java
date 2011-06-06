

package org.gjt.sp.jedit.gui;


import javax.swing.border.EmptyBorder;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Arrays;
import org.gjt.sp.jedit.buffer.FoldHandler;
import org.gjt.sp.jedit.*;



public class BufferOptions extends EnhancedDialog
{
	
	public BufferOptions(View view, Buffer buffer)
	{
		super(view,jEdit.getProperty("buffer-options.title"),true);
		this.view = view;
		this.buffer = buffer;

		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		ActionHandler actionListener = new ActionHandler();
		AbstractOptionPane panel = new AbstractOptionPane(null);

		panel.addComponent(GUIUtilities.createMultilineLabel(
			jEdit.getProperty("buffer-options.caption")));

		panel.addSeparator("buffer-options.loading-saving");

		
		String[] lineSeps = { jEdit.getProperty("lineSep.unix"),
			jEdit.getProperty("lineSep.windows"),
			jEdit.getProperty("lineSep.mac") };
		lineSeparator = new JComboBox(lineSeps);
		String lineSep = buffer.getStringProperty(Buffer.LINESEP);
		if(lineSep == null)
			lineSep = System.getProperty("line.separator");
		if("\n".equals(lineSep))
			lineSeparator.setSelectedIndex(0);
		else if("\r\n".equals(lineSep))
			lineSeparator.setSelectedIndex(1);
		else if("\r".equals(lineSep))
			lineSeparator.setSelectedIndex(2);
		panel.addComponent(jEdit.getProperty("buffer-options.lineSeparator"),
			lineSeparator);
		

		
		String[] encodings = MiscUtilities.getEncodings();
		Arrays.sort(encodings,new MiscUtilities.StringICaseCompare());
		encoding = new JComboBox(encodings);
		encoding.setEditable(true);
		encoding.setSelectedItem(buffer.getStringProperty(Buffer.ENCODING));
		panel.addComponent(jEdit.getProperty("buffer-options.encoding"),
			encoding);
		

		
		gzipped = new JCheckBox(jEdit.getProperty(
			"buffer-options.gzipped"));
		gzipped.setSelected(buffer.getBooleanProperty(Buffer.GZIPPED));
		panel.addComponent(gzipped);
		

		panel.addSeparator("buffer-options.editing");

		
		modes = jEdit.getModes();
		MiscUtilities.quicksort(modes,new MiscUtilities.StringICaseCompare());
		mode = new JComboBox(modes);
		mode.setSelectedItem(buffer.getMode());
		mode.addActionListener(actionListener);
		panel.addComponent(jEdit.getProperty("buffer-options.mode"),mode);
		

		
		String[] foldModes = FoldHandler.getFoldModes();

		folding = new JComboBox(foldModes);
		folding.setSelectedItem(buffer.getStringProperty("folding"));
		panel.addComponent(jEdit.getProperty("options.editing.folding"),
			folding);
		

		
		String[] wrapModes = {
			"none",
			"soft",
			"hard"
		};

		wrap = new JComboBox(wrapModes);
		wrap.setSelectedItem(buffer.getStringProperty("wrap"));
		panel.addComponent(jEdit.getProperty("options.editing.wrap"),
			wrap);
		

		
		String[] lineLengths = { "0", "72", "76", "80" };

		maxLineLen = new JComboBox(lineLengths);
		maxLineLen.setEditable(true);
		maxLineLen.setSelectedItem(buffer.getStringProperty("maxLineLen"));
		panel.addComponent(jEdit.getProperty("options.editing.maxLineLen"),
			maxLineLen);
		

		
		String[] tabSizes = { "2", "4", "8" };
		tabSize = new JComboBox(tabSizes);
		tabSize.setEditable(true);
		tabSize.setSelectedItem(buffer.getStringProperty("tabSize"));
		panel.addComponent(jEdit.getProperty("options.editing.tabSize"),tabSize);
		

		
		indentSize = new JComboBox(tabSizes);
		indentSize.setEditable(true);
		indentSize.setSelectedItem(buffer.getStringProperty("indentSize"));
		panel.addComponent(jEdit.getProperty("options.editing.indentSize"),
			indentSize);
		

		
		noTabs = new JCheckBox(jEdit.getProperty(
			"options.editing.noTabs"));
		noTabs.setSelected(buffer.getBooleanProperty("noTabs"));
		panel.addComponent(noTabs);
		

		content.add(BorderLayout.NORTH,panel);

		
		JPanel buttons = new JPanel();
		buttons.setLayout(new BoxLayout(buttons,BoxLayout.X_AXIS));
		buttons.setBorder(new EmptyBorder(12,0,0,0));
		buttons.add(Box.createGlue());
		ok = new JButton(jEdit.getProperty("common.ok"));
		ok.addActionListener(actionListener);
		getRootPane().setDefaultButton(ok);
		buttons.add(ok);
		buttons.add(Box.createHorizontalStrut(6));
		cancel = new JButton(jEdit.getProperty("common.cancel"));
		cancel.addActionListener(actionListener);
		buttons.add(cancel);
		buttons.add(Box.createGlue());
		content.add(BorderLayout.SOUTH,buttons);
		

		pack();
		setLocationRelativeTo(view);
		show();
	} 

	
	public void ok()
	{
		int index = lineSeparator.getSelectedIndex();
		String lineSep;
		if(index == 0)
			lineSep = "\n";
		else if(index == 1)
			lineSep = "\r\n";
		else if(index == 2)
			lineSep = "\r";
		else
			throw new InternalError();

		String oldLineSep = buffer.getStringProperty(Buffer.LINESEP);
		if(oldLineSep == null)
			oldLineSep = System.getProperty("line.separator");
		if(!oldLineSep.equals(lineSep))
		{
			buffer.setStringProperty("lineSeparator",lineSep);
			buffer.setDirty(true);
		}

		String encoding = (String)this.encoding.getSelectedItem();
		String oldEncoding = buffer.getStringProperty(Buffer.ENCODING);
		if(!oldEncoding.equals(encoding))
		{
			buffer.setStringProperty(Buffer.ENCODING,encoding);
			buffer.setDirty(true);
		}

		boolean gzippedValue = gzipped.isSelected();
		boolean oldGzipped = buffer.getBooleanProperty(
			Buffer.GZIPPED);
		if(gzippedValue != oldGzipped)
		{
			buffer.setBooleanProperty(Buffer.GZIPPED,gzippedValue);
			buffer.setDirty(true);
		}

		buffer.setStringProperty("folding",(String)folding.getSelectedItem());

		buffer.setStringProperty("wrap",(String)wrap.getSelectedItem());

		try
		{
			buffer.setProperty("maxLineLen",new Integer(
				maxLineLen.getSelectedItem().toString()));
		}
		catch(NumberFormatException nf)
		{
		}

		try
		{
			buffer.setProperty("tabSize",new Integer(
				tabSize.getSelectedItem().toString()));
		}
		catch(NumberFormatException nf)
		{
		}

		try
		{
			buffer.setProperty("indentSize",new Integer(
				indentSize.getSelectedItem().toString()));
		}
		catch(NumberFormatException nf)
		{
		}

		buffer.setBooleanProperty("noTabs",noTabs.isSelected());

		index = mode.getSelectedIndex();
		buffer.setMode(modes[index]);

		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

        

	
	private View view;
	private Buffer buffer;
	private Mode[] modes;
	private JComboBox mode;
	private JComboBox lineSeparator;
	private JComboBox encoding;
	private JCheckBox gzipped;
	private JComboBox folding;
	private JComboBox wrap;
	private JComboBox maxLineLen;
	private JComboBox tabSize;
	private JComboBox indentSize;
	private JCheckBox noTabs;
	private JButton ok;
	private JButton cancel;
	

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == ok)
				ok();
			else if(source == cancel)
				cancel();
			else if(source == mode)
			{
				Mode _mode = jEdit.getMode((String)
					mode.getSelectedItem());
				folding.setSelectedItem(_mode.getProperty(
					"folding"));
				wrap.setSelectedItem(_mode.getProperty(
					"wrap"));
				maxLineLen.setSelectedItem(_mode.getProperty(
					"maxLineLen"));
				tabSize.setSelectedItem(_mode.getProperty(
					"tabSize"));
				indentSize.setSelectedItem(_mode.getProperty(
					"indentSize"));
				noTabs.setSelected(_mode.getBooleanProperty(
					"noTabs"));
			}
		} 
	} 

	
}
