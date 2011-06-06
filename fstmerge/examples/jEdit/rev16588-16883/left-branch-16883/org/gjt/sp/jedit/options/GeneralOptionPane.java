

package org.gjt.sp.jedit.options;



import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.Log;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;




public class GeneralOptionPane extends AbstractOptionPane
{
	
	
	public static final int checkFileStatus_focus = 0;
	
	
	public static final int checkFileStatus_all = 1;
	
	public static final int checkFileStatus_operations = 2;
	
	public static final int checkFileStatus_focusBuffer = 3;
	
	public static final int checkFileStatus_none = 4;
	

	
	private JComboBox lineSeparator;
	private JComboBox checkModStatus;
	private JComboBox checkModStatusUpon;
	private JTextField recentFiles;
	private JTextField hypersearchResultsWarning;
	private JCheckBox saveCaret;
	private JCheckBox sortRecent;
	private JCheckBox persistentMarkers;
	private JCheckBox restore;
	private JCheckBox restoreRemote;
	private JCheckBox restoreCLI;
	

	
	public GeneralOptionPane()
	{
		super("general");
	} 

	
	@Override
	protected void _init()
	{

		
		String[] lineSeps = { jEdit.getProperty("lineSep.unix"),
			jEdit.getProperty("lineSep.windows"),
			jEdit.getProperty("lineSep.mac") };
		lineSeparator = new JComboBox(lineSeps);
		String lineSep = jEdit.getProperty("buffer."+ JEditBuffer.LINESEP,
			System.getProperty("line.separator"));
		if("\n".equals(lineSep))
			lineSeparator.setSelectedIndex(0);
		else if("\r\n".equals(lineSep))
			lineSeparator.setSelectedIndex(1);
		else if("\r".equals(lineSep))
			lineSeparator.setSelectedIndex(2);
		addComponent(jEdit.getProperty("options.general.lineSeparator"),
			lineSeparator);


		
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

		
		String[] modCheckUponOptions = {
			jEdit.getProperty("options.general.checkModStatusUpon.focus"),
			jEdit.getProperty("options.general.checkModStatusUpon.all"),
			jEdit.getProperty("options.general.checkModStatusUpon.operations"),
			jEdit.getProperty("options.general.checkModStatusUpon.focusBuffer"),
			jEdit.getProperty("options.general.checkModStatusUpon.none")
		};
		checkModStatusUpon = new JComboBox(modCheckUponOptions);
		checkModStatusUpon.setSelectedIndex(jEdit.getIntegerProperty("checkFileStatus"));
		addComponent(jEdit.getProperty("options.general.checkModStatusUpon"),
			checkModStatusUpon);

		
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

		
		hypersearchResultsWarning = new JTextField(jEdit.getProperty("hypersearch.maxWarningResults"));
		addComponent(jEdit.getProperty("options.general.hypersearch.maxWarningResults"),
			hypersearchResultsWarning);



	} 

	
	@Override
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
		jEdit.setProperty("buffer."+ JEditBuffer.LINESEP,lineSep);
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
		jEdit.setIntegerProperty("checkFileStatus",checkModStatusUpon.getSelectedIndex());
		jEdit.setProperty("recentFiles",recentFiles.getText());
		jEdit.setBooleanProperty("sortRecent",sortRecent.isSelected());
		jEdit.setBooleanProperty("saveCaret",saveCaret.isSelected());
		jEdit.setBooleanProperty("persistentMarkers",
			persistentMarkers.isSelected());
		jEdit.setBooleanProperty("restore",restore.isSelected());
		jEdit.setBooleanProperty("restore.cli",restoreCLI.isSelected());
		jEdit.setBooleanProperty("restore.remote", restoreRemote.isSelected());
		try
		{
			jEdit.setIntegerProperty("hypersearch.maxWarningResults", Integer.parseInt(hypersearchResultsWarning.getText()));
		}
		catch (NumberFormatException e)
		{
			Log.log(Log.WARNING, this, "hypersearchResultsWarning: " + hypersearchResultsWarning.getText() + " is not a valid value for this option");
		}
	} 


}
