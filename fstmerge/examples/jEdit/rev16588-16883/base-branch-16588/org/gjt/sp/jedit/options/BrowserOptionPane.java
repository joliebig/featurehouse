

package org.gjt.sp.jedit.options;


import javax.swing.*;
import org.gjt.sp.jedit.*;




public class BrowserOptionPane extends AbstractOptionPane
{
	
	public BrowserOptionPane()
	{
		super("browser.general");
	} 

	
	public void _init()
	{
		
		String[] dirs = {
			jEdit.getProperty("options.browser.general.defaultPath.favorites"),
			jEdit.getProperty("options.browser.general.defaultPath.home"),
			jEdit.getProperty("options.browser.general.defaultPath.last"),
			jEdit.getProperty("options.browser.general.defaultPath.buffer"),
			jEdit.getProperty("options.browser.general.defaultPath.working")
		};

		defaultDirectory = new JComboBox(dirs);
		String defaultDir = jEdit.getProperty("vfs.browser.defaultPath");
		if("favorites".equals(defaultDir))
			defaultDirectory.setSelectedIndex(0);
		else if("home".equals(defaultDir))
			defaultDirectory.setSelectedIndex(1);
		else if("last".equals(defaultDir))
			defaultDirectory.setSelectedIndex(2);
		else if("buffer".equals(defaultDir))
			defaultDirectory.setSelectedIndex(3);
		else if("working".equals(defaultDir))
			defaultDirectory.setSelectedIndex(4);
		addComponent(jEdit.getProperty("options.browser.general.defaultPath"),
			defaultDirectory);

		
		showToolbar = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.showToolbar"));
		showToolbar.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".showToolbar"));
		addComponent(showToolbar);

		
		showMenubar = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.showMenubar"));
		showMenubar.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".showMenubar"));
		addComponent(showMenubar);

		
		showIcons = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.showIcons"));
		showIcons.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".showIcons"));
		addComponent(showIcons);

		
		showHiddenFiles = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.showHiddenFiles"));
		showHiddenFiles.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".showHiddenFiles"));
		addComponent(showHiddenFiles);

		
		sortIgnoreCase = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.sortIgnoreCase"));
		sortIgnoreCase.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".sortIgnoreCase"));
		addComponent(sortIgnoreCase);

		
		sortMixFilesAndDirs = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.sortMixFilesAndDirs"));
		sortMixFilesAndDirs.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".sortMixFilesAndDirs"));
		addComponent(sortMixFilesAndDirs);

		
		doubleClickClose = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.doubleClickClose"));
		doubleClickClose.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".doubleClickClose"));
		addComponent(doubleClickClose);

		
		currentBufferFilter = new JCheckBox(jEdit.getProperty("options.browser"
			+ ".general.currentBufferFilter"));
		currentBufferFilter.setSelected(jEdit.getBooleanProperty("vfs.browser"
			+ ".currentBufferFilter"));
		addComponent(currentBufferFilter);

		
		useDefaultIcons = new JCheckBox(jEdit.getProperty("options.browser.general.useDefaultIcons"));
		useDefaultIcons.setSelected(jEdit.getBooleanProperty("vfs.browser.useDefaultIcons"));
		addComponent(useDefaultIcons);
	} 

	
	public void _save()
	{
		String[] dirs = { "favorites", "home", "last", "buffer", "working"};
		jEdit.setProperty("vfs.browser.defaultPath",dirs[defaultDirectory
			.getSelectedIndex()]);
		jEdit.setBooleanProperty("vfs.browser.showToolbar",
			showToolbar.isSelected());
		jEdit.setBooleanProperty("vfs.browser.showMenubar",
			showMenubar.isSelected());
		jEdit.setBooleanProperty("vfs.browser.showIcons",
			showIcons.isSelected());
		jEdit.setBooleanProperty("vfs.browser.showHiddenFiles",
			showHiddenFiles.isSelected());
		jEdit.setBooleanProperty("vfs.browser.sortIgnoreCase",
			sortIgnoreCase.isSelected());
		jEdit.setBooleanProperty("vfs.browser.sortMixFilesAndDirs",
			sortMixFilesAndDirs.isSelected());
		jEdit.setBooleanProperty("vfs.browser.doubleClickClose",
			doubleClickClose.isSelected());
		jEdit.setBooleanProperty("vfs.browser.currentBufferFilter",
			currentBufferFilter.isSelected());
		jEdit.setBooleanProperty("vfs.browser.useDefaultIcons",
			useDefaultIcons.isSelected());
	} 

	
	private JComboBox defaultDirectory;
	private JCheckBox showToolbar;
	private JCheckBox showMenubar;
	private JCheckBox showIcons;
	private JCheckBox showHiddenFiles;
	private JCheckBox sortIgnoreCase;
	private JCheckBox sortMixFilesAndDirs;
	private JCheckBox doubleClickClose;
	private JCheckBox currentBufferFilter;
	private JCheckBox useDefaultIcons;
	
} 
