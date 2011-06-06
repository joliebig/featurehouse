

package org.gjt.sp.jedit.options;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;

import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.Mode;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.buffer.FoldHandler;
import org.gjt.sp.jedit.buffer.JEditBuffer;


public class BufferOptionPane extends AbstractOptionPane
{
	JComboBox encoding;
	JComboBox lineSeparator;
	JCheckBox gzipped;
	Mode[] modes;
	JComboBox mode;
	JComboBox folding;
	JComboBox wrap;
	JComboBox maxLineLen;
	JComboBox tabSize;
	JComboBox indentSize;
	JComboBox checkModStatus;
	JCheckBox noTabs;
	Buffer buffer;


	public BufferOptionPane()
	{
		super("Buffer Options");
		init();
	}

	
	protected void _init()
	{


		buffer = jEdit.getActiveView().getBuffer();
		String filename = buffer.getName();
		setName("Buffer: " + filename);
		addComponent(GUIUtilities.createMultilineLabel(
			jEdit.getProperty("buffer-options.caption")));

		addSeparator("buffer-options.loading-saving");

		
		String[] lineSeps = { jEdit.getProperty("lineSep.unix"),
			jEdit.getProperty("lineSep.windows"),
			jEdit.getProperty("lineSep.mac") };
		lineSeparator = new JComboBox(lineSeps);
		String lineSep = buffer.getStringProperty(JEditBuffer.LINESEP);
		if(lineSep == null)
			lineSep = System.getProperty("line.separator");
		if("\n".equals(lineSep))
			lineSeparator.setSelectedIndex(0);
		else if("\r\n".equals(lineSep))
			lineSeparator.setSelectedIndex(1);
		else if("\r".equals(lineSep))
			lineSeparator.setSelectedIndex(2);
		addComponent(jEdit.getProperty("buffer-options.lineSeparator"),
			lineSeparator);
		

		
		String[] encodings = MiscUtilities.getEncodings(true);
		Arrays.sort(encodings,new MiscUtilities.StringICaseCompare());
		encoding = new JComboBox(encodings);
		encoding.setEditable(true);
		encoding.setSelectedItem(buffer.getStringProperty(JEditBuffer.ENCODING));
		addComponent(jEdit.getProperty("buffer-options.encoding"),
			encoding);
		

		
		gzipped = new JCheckBox(jEdit.getProperty(
			"buffer-options.gzipped"));
		gzipped.setSelected(buffer.getBooleanProperty(Buffer.GZIPPED));
		addComponent(gzipped);
		

		
		
		String[] modCheckOptions = {
			jEdit.getProperty("options.general.checkModStatus.nothing"),
			jEdit.getProperty("options.general.checkModStatus.prompt"),
			jEdit.getProperty("options.general.checkModStatus.reload"),
			jEdit.getProperty("options.general.checkModStatus.silentReload")
		};
		checkModStatus = new JComboBox(modCheckOptions);
		if(buffer.getAutoReload())
		{
			if(buffer.getAutoReloadDialog())
				
				checkModStatus.setSelectedIndex(2);
			else	
				checkModStatus.setSelectedIndex(3);
		}
		else
		{
			if(buffer.getAutoReloadDialog())
				
				checkModStatus.setSelectedIndex(1);
			else	
				checkModStatus.setSelectedIndex(0);
		}
		addComponent(jEdit.getProperty("options.general.checkModStatus"),
			checkModStatus);

		

		addSeparator("buffer-options.editing");

		
		modes = jEdit.getModes();
		Arrays.sort(modes,new MiscUtilities.StringICaseCompare());
		mode = new JComboBox(modes);
		mode.setSelectedItem(buffer.getMode());
		ActionHandler actionListener = new ActionHandler();
		mode.addActionListener(actionListener);
		addComponent(jEdit.getProperty("buffer-options.mode"),mode);
		

		
		String[] foldModes = FoldHandler.getFoldModes();

		folding = new JComboBox(foldModes);
		folding.setSelectedItem(buffer.getStringProperty("folding"));
		addComponent(jEdit.getProperty("options.editing.folding"),
			folding);
		

		
		String[] wrapModes = {
			"none",
			"soft",
			"hard"
		};

		wrap = new JComboBox(wrapModes);
		wrap.setSelectedItem(buffer.getStringProperty("wrap"));
		addComponent(jEdit.getProperty("options.editing.wrap"),
			wrap);
		

		
		String[] lineLengths = { "0", "72", "76", "80" };

		maxLineLen = new JComboBox(lineLengths);
		maxLineLen.setEditable(true);
		maxLineLen.setSelectedItem(buffer.getStringProperty("maxLineLen"));
		addComponent(jEdit.getProperty("options.editing.maxLineLen"),
			maxLineLen);
		

		
		String[] tabSizes = { "2", "4", "8" };
		tabSize = new JComboBox(tabSizes);
		tabSize.setEditable(true);
		tabSize.setSelectedItem(buffer.getStringProperty("tabSize"));
		addComponent(jEdit.getProperty("options.editing.tabSize"),tabSize);
		

		
		indentSize = new JComboBox(tabSizes);
		indentSize.setEditable(true);
		indentSize.setSelectedItem(buffer.getStringProperty("indentSize"));
		addComponent(jEdit.getProperty("options.editing.indentSize"),
			indentSize);
		

		
		noTabs = new JCheckBox(jEdit.getProperty(
			"options.editing.noTabs"));
		noTabs.setSelected(buffer.getBooleanProperty("noTabs"));
		addComponent(noTabs);
		
	} 

	
	protected void _save()
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

		String oldLineSep = buffer.getStringProperty(JEditBuffer.LINESEP);
		if(oldLineSep == null)
			oldLineSep = System.getProperty("line.separator");
		if(!oldLineSep.equals(lineSep))
		{
			buffer.setStringProperty("lineSeparator",lineSep);
			buffer.setDirty(true);
		}

		String encoding = (String)this.encoding.getSelectedItem();
		String oldEncoding = buffer.getStringProperty(JEditBuffer.ENCODING);
		if(!oldEncoding.equals(encoding))
		{
			buffer.setStringProperty(JEditBuffer.ENCODING,encoding);
			buffer.setDirty(true);
			
			
			buffer.setBooleanProperty(Buffer.ENCODING_AUTODETECT,false);
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
		switch(checkModStatus.getSelectedIndex())
		{
		case 0:
			buffer.setAutoReloadDialog(false);
			buffer.setAutoReload(false);
			break;
		case 1:
			buffer.setAutoReloadDialog(true);
			buffer.setAutoReload(false);
			break;
		case 2:
			buffer.setAutoReloadDialog(true);
			buffer.setAutoReload(true);
			break;
		case 3:
			buffer.setAutoReloadDialog(false);
			buffer.setAutoReload(true);
			break;
		}
	} 

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == mode)
			{
				Mode _mode = (Mode)mode.getSelectedItem();
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
