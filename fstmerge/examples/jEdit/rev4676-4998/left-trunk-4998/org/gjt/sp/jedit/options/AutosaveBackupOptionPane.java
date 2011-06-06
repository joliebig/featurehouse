

package org.gjt.sp.jedit.options;


import javax.swing.*;
import java.awt.event.*;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.*;


public class AutosaveBackupOptionPane extends AbstractOptionPane
{
	
	public AutosaveBackupOptionPane()
	{
		super("auto-back");
	} 

	
	protected void _init()
	{
		
		autosave = new JTextField(jEdit.getProperty("autosave"));
		addComponent(jEdit.getProperty("options.auto-back.autosave"),autosave);

		
		backups = new JTextField(jEdit.getProperty("backups"));
		addComponent(jEdit.getProperty("options.auto-back.backups"),backups);

		
		backupDirectory = new JTextField(jEdit.getProperty(
			"backup.directory"));
		addComponent(jEdit.getProperty("options.auto-back.backupDirectory"),
			backupDirectory);

		
		backupPrefix = new JTextField(jEdit.getProperty("backup.prefix"));
		addComponent(jEdit.getProperty("options.auto-back.backupPrefix"),
			backupPrefix);

		
		backupSuffix = new JTextField(jEdit.getProperty(
			"backup.suffix"));
		addComponent(jEdit.getProperty("options.auto-back.backupSuffix"),
			backupSuffix);

		
		backupEverySave = new JCheckBox(jEdit.getProperty(
			"options.auto-back.backupEverySave"));
		backupEverySave.setSelected(jEdit.getBooleanProperty("backupEverySave"));
		addComponent(backupEverySave);
	} 

	
	protected void _save()
	{
		jEdit.setProperty("autosave",autosave.getText());
		jEdit.setProperty("backups",backups.getText());
		jEdit.setProperty("backup.directory",backupDirectory.getText());
		jEdit.setProperty("backup.prefix",backupPrefix.getText());
		jEdit.setProperty("backup.suffix",backupSuffix.getText());
		jEdit.setBooleanProperty("backupEverySave", backupEverySave.isSelected());
	} 

	
	private JTextField autosave;
	private JTextField backups;
	private JTextField backupDirectory;
	private JTextField backupPrefix;
	private JTextField backupSuffix;
	private JCheckBox backupEverySave;
	
}
