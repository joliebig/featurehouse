

package org.gjt.sp.jedit.options;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.VFSBrowser;



public class SaveBackupOptionPane extends AbstractOptionPane
{
	
	public SaveBackupOptionPane()
	{
		super("save-back");
	} 

	
	protected void _init()
	{
		
		twoStageSave = new JCheckBox(jEdit.getProperty(
			"options.save-back.twoStageSave"));
		twoStageSave.setSelected(jEdit.getBooleanProperty(
			"twoStageSave"));
		addComponent(twoStageSave);

		
		confirmSaveAll = new JCheckBox(jEdit.getProperty(
			"options.save-back.confirmSaveAll"));
		confirmSaveAll.setSelected(jEdit.getBooleanProperty(
			"confirmSaveAll"));
		addComponent(confirmSaveAll);

		
		autosave = new JTextField(jEdit.getProperty("autosave"));
		addComponent(jEdit.getProperty("options.save-back.autosave"),autosave);

		
		backups = new JTextField(jEdit.getProperty("backups"));
		addComponent(jEdit.getProperty("options.save-back.backups"),backups);

		
		backupDirectory = new JTextField(jEdit.getProperty(
			"backup.directory"));
		JButton browseBackupDirectory = new JButton("...");
		browseBackupDirectory.addActionListener(new MyActionListener());
		JPanel panel = new JPanel(new BorderLayout());
		panel.add(backupDirectory);
		panel.add(browseBackupDirectory, BorderLayout.EAST);
		addComponent(jEdit.getProperty("options.save-back.backupDirectory"),
			panel);

		
		backupPrefix = new JTextField(jEdit.getProperty("backup.prefix"));
		addComponent(jEdit.getProperty("options.save-back.backupPrefix"),
			backupPrefix);

		
		backupSuffix = new JTextField(jEdit.getProperty(
			"backup.suffix"));
		addComponent(jEdit.getProperty("options.save-back.backupSuffix"),
			backupSuffix);

		
		backupEverySave = new JCheckBox(jEdit.getProperty(
			"options.save-back.backupEverySave"));
		backupEverySave.setSelected(jEdit.getBooleanProperty("backupEverySave"));
		addComponent(backupEverySave);
	} 

	
	protected void _save()
	{
		jEdit.setBooleanProperty("twoStageSave",twoStageSave.isSelected());
		jEdit.setBooleanProperty("confirmSaveAll",confirmSaveAll.isSelected());
		jEdit.setProperty("autosave",autosave.getText());
		jEdit.setProperty("backups",backups.getText());
		jEdit.setProperty("backup.directory",backupDirectory.getText());
		jEdit.setProperty("backup.prefix",backupPrefix.getText());
		jEdit.setProperty("backup.suffix",backupSuffix.getText());
		jEdit.setBooleanProperty("backupEverySave", backupEverySave.isSelected());
	} 

	
	private JCheckBox twoStageSave;
	private JCheckBox confirmSaveAll;
	private JTextField autosave;
	private JTextField backups;
	private JTextField backupDirectory;
	private JTextField backupPrefix;
	private JTextField backupSuffix;
	private JCheckBox backupEverySave;
	

	private class MyActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String[] choosenFolder = 
				GUIUtilities.showVFSFileDialog(null,
				   			       backupDirectory.getText(),
				   			       VFSBrowser.CHOOSE_DIRECTORY_DIALOG,
				   			       false);
			if (choosenFolder != null)
				backupDirectory.setText(choosenFolder[0]);
		}
	}
}
