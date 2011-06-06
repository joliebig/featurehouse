

package org.gjt.sp.jedit.options;


import javax.swing.*;

import java.awt.event.*;
import java.util.Arrays;
import org.gjt.sp.jedit.*;




public class GeneralOptionPane extends AbstractOptionPane
{
	
	private JComboBox lineSeparator;
	private JComboBox encoding;
	private JCheckBox encodingAutodetect;
	private JComboBox checkModStatus;
	private JTextField recentFiles;
	private JCheckBox saveCaret;
	private JCheckBox sortRecent;
	private JCheckBox persistentMarkers;
	private JCheckBox restore;
	private JCheckBox restoreRemote;
	private JCheckBox restoreCLI;
	private JCheckBox sortBuffers;
	private JCheckBox sortByName;
	private JCheckBox newKeyboardHandling;
	private JCheckBox xAutodetect;
	
	
	
	public GeneralOptionPane()
	{
		super("general");
	} 

	
	protected void _init()
	{
		
		
		String[] lineSeps = { jEdit.getProperty("lineSep.unix"),
			jEdit.getProperty("lineSep.windows"),
			jEdit.getProperty("lineSep.mac") };
		lineSeparator = new JComboBox(lineSeps);
		String lineSep = jEdit.getProperty("buffer."+Buffer.LINESEP,
			System.getProperty("line.separator"));
		if("\n".equals(lineSep))
			lineSeparator.setSelectedIndex(0);
		else if("\r\n".equals(lineSep))
			lineSeparator.setSelectedIndex(1);
		else if("\r".equals(lineSep))
			lineSeparator.setSelectedIndex(2);
		addComponent(jEdit.getProperty("options.general.lineSeparator"),
			lineSeparator);

		
		String[] encodings = MiscUtilities.getEncodings(true);
		Arrays.sort(encodings,new MiscUtilities.StringICaseCompare());
		encoding = new JComboBox(encodings);
		encoding.setEditable(true);
		encoding.setSelectedItem(jEdit.getProperty("buffer."+Buffer.ENCODING,
			System.getProperty("file.encoding")));
		addComponent(jEdit.getProperty("options.general.encoding"),encoding);

		
		encodingAutodetect = new JCheckBox(jEdit.getProperty(
			"options.general.encodingAutodetect"));
		encodingAutodetect.setSelected(jEdit.getBooleanProperty("buffer.encodingAutodetect"));
		addComponent(encodingAutodetect);

		
		String[] modCheckOptions = {
			jEdit.getProperty("options.general.checkModStatus.nothing"),
			jEdit.getProperty("options.general.checkModStatus.prompt"),
			jEdit.getProperty("options.general.checkModStatus.reload"),
			jEdit.getProperty("options.general.checkModStatus.silentReload")
		};
		checkModStatus = new JComboBox(modCheckOptions);
		if(jEdit.getBooleanProperty("autoReload"))
		{
			if (jEdit.getBooleanProperty("autoReloadDialog"))
				
				checkModStatus.setSelectedIndex(2);
			else	
				checkModStatus.setSelectedIndex(3);
		}
		else
		{
			if (jEdit.getBooleanProperty("autoReloadDialog"))
				
				checkModStatus.setSelectedIndex(1);
			else	
				checkModStatus.setSelectedIndex(0);
		}
		addComponent(jEdit.getProperty("options.general.checkModStatus"),
			checkModStatus);

		
		recentFiles = new JTextField(jEdit.getProperty(
			"options.general.recentFiles"));
		recentFiles.setText(jEdit.getProperty("recentFiles"));
		addComponent(jEdit.getProperty("options.general.recentFiles"),
			recentFiles);

		
		sortRecent = new JCheckBox(jEdit.getProperty(
			"options.general.sortRecent"));
		sortRecent.setSelected(jEdit.getBooleanProperty("sortRecent"));
		addComponent(sortRecent);

		
		saveCaret = new JCheckBox(jEdit.getProperty(
			"options.general.saveCaret"));
		saveCaret.setSelected(jEdit.getBooleanProperty("saveCaret"));
		addComponent(saveCaret);

		
		persistentMarkers = new JCheckBox(jEdit.getProperty(
			"options.general.persistentMarkers"));
		persistentMarkers.setSelected(jEdit.getBooleanProperty(
			"persistentMarkers"));
		addComponent(persistentMarkers);

		
		restore = new JCheckBox(jEdit.getProperty(
			"options.general.restore"));
		
		restore.setSelected(jEdit.getBooleanProperty("restore"));
		restore.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				restoreCLI.setEnabled(restore.isSelected());
				restoreRemote.setEnabled(restore.isSelected());
			}
		});

		addComponent(restore);
		
		restoreRemote = new JCheckBox(jEdit.getProperty(
			"options.general.restore.remote"));
		restoreRemote.setSelected(jEdit.getBooleanProperty("restore.remote", false));
		restoreRemote.setEnabled(restore.isSelected());
		addComponent(restoreRemote);
		
		restoreCLI = new JCheckBox(jEdit.getProperty(
			"options.general.restore.cli"));
		restoreCLI.setSelected(jEdit.getBooleanProperty("restore.cli"));
		restoreCLI.setEnabled(restore.isSelected());
		addComponent(restoreCLI);

		
		sortBuffers = new JCheckBox(jEdit.getProperty(
			"options.general.sortBuffers"));
		sortBuffers.setSelected(jEdit.getBooleanProperty("sortBuffers"));
		sortBuffers.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				sortByName.setEnabled(sortBuffers.isSelected());
			}
		});

		addComponent(sortBuffers);

		
		sortByName = new JCheckBox(jEdit.getProperty(
			"options.general.sortByName"));
		sortByName.setSelected(jEdit.getBooleanProperty("sortByName"));
		sortByName.setEnabled(sortBuffers.isSelected());
		addComponent(sortByName);
		
		newKeyboardHandling = new JCheckBox(jEdit.getProperty("options.general.newkeyhandling"));
		newKeyboardHandling.setSelected(jEdit.getBooleanProperty("newkeyhandling"));
		addComponent(newKeyboardHandling);

		xAutodetect = new JCheckBox(jEdit.getProperty("options.general.xAutodetect"));
		xAutodetect.setSelected(jEdit.getBooleanProperty("xAutodetect"));
		addComponent(xAutodetect);

	} 

	
	protected void _save()
	{
		
		String lineSep = null;
		switch(lineSeparator.getSelectedIndex())
		{
		case 0:
			lineSep = "\n";
			break;
		case 1:
			lineSep = "\r\n";
			break;
		case 2:
			lineSep = "\r";
			break;
		}
		jEdit.setProperty("buffer."+Buffer.LINESEP,lineSep);
		jEdit.setProperty("buffer."+Buffer.ENCODING,(String)
			encoding.getSelectedItem());
		jEdit.setBooleanProperty("buffer.encodingAutodetect",
			encodingAutodetect.isSelected());
		switch(checkModStatus.getSelectedIndex())
		{
		case 0:
			jEdit.setBooleanProperty("autoReloadDialog",false);
			jEdit.setBooleanProperty("autoReload",false);
			break;
		case 1:
			jEdit.setBooleanProperty("autoReloadDialog",true);
			jEdit.setBooleanProperty("autoReload",false);
			break;
		case 2:
			jEdit.setBooleanProperty("autoReloadDialog",true);
			jEdit.setBooleanProperty("autoReload",true);
			break;
		case 3:
			jEdit.setBooleanProperty("autoReloadDialog",false);
			jEdit.setBooleanProperty("autoReload",true);
			break;
		}
		jEdit.setProperty("recentFiles",recentFiles.getText());
		jEdit.setBooleanProperty("sortRecent",sortRecent.isSelected());
		boolean nkh = newKeyboardHandling.isSelected();
		jEdit.setBooleanProperty("newkeyhandling", nkh);
		Options.SIMPLIFIED_KEY_HANDLING = nkh;
		jEdit.setBooleanProperty("xAutodetect", xAutodetect.isSelected());
		Options.X_AUTODETECT = xAutodetect.isSelected();
		jEdit.setBooleanProperty("saveCaret",saveCaret.isSelected());
		jEdit.setBooleanProperty("persistentMarkers",
			persistentMarkers.isSelected());
		jEdit.setBooleanProperty("restore",restore.isSelected());
		jEdit.setBooleanProperty("restore.cli",restoreCLI.isSelected());
		jEdit.setBooleanProperty("restore.remote", restoreRemote.isSelected());
		jEdit.setBooleanProperty("sortBuffers",sortBuffers.isSelected());
		jEdit.setBooleanProperty("sortByName",sortByName.isSelected());
	} 


}
