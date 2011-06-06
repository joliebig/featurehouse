

package org.gjt.sp.jedit.search;


import javax.swing.border.*;
import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.HashMap;
import java.util.Map;

import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.msg.SearchSettingsChanged;
import org.gjt.sp.jedit.*;



public class SearchDialog extends EnhancedDialog implements EBComponent
{
	
	
	public static final int CURRENT_BUFFER = 0;
	public static final int ALL_BUFFERS = 1;
	public static final int DIRECTORY = 2;
	

	
	public static SearchDialog getSearchDialog(View view)
	{
		if(Debug.DISABLE_SEARCH_DIALOG_POOL)
			return new SearchDialog(view);
		else
		{

			SearchDialog searchDialog = viewHash.get(view);
			if (searchDialog == null)
			{
				searchDialog = new SearchDialog(view);
				viewHash.put(view, searchDialog);
			}
			return searchDialog;
		}
	} 

	
	
	@Deprecated
	public static void preloadSearchDialog(View view)
	{
		if(Debug.DISABLE_SEARCH_DIALOG_POOL)
			return;

		SearchDialog dialog = new SearchDialog(view);
		viewHash.put(view,dialog);
	} 

	
	
	public static void showSearchDialog(View view, String searchString,
		int searchIn)
	{
		final SearchDialog dialog = getSearchDialog(view);

		dialog.setSearchString(searchString,searchIn);

		
		if(OperatingSystem.isUnix() && !OperatingSystem.isMacOS())
			dialog.setVisible(false);

		
		
		
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				dialog.toFront();
				dialog.find.requestFocus();
			}
		});
		dialog.setVisible(true);
	} 

	
	
	public void setSearchString(String searchString, int searchIn)
	{
		find.setText(null);
		replace.setText(null);

		if(searchString == null)
			searchCurrentBuffer.setSelected(true);
		else
		{
			if(searchString.indexOf('\n') == -1)
			{
				if(SearchAndReplace.getRegexp())
				{
					find.setText(SearchAndReplace.escapeRegexp(
						searchString,true));
				}
				else
					find.setText(searchString);
				find.selectAll();
				searchCurrentBuffer.setSelected(true);
			}
			else if(searchIn == CURRENT_BUFFER)
			{
				searchSelection.setSelected(true);
				hyperSearch.setSelected(true);
			}
		}

		if(searchIn == CURRENT_BUFFER)
		{
			if(!searchSelection.isSelected())
			{
				
				searchCurrentBuffer.setSelected(true);

				
				hyperSearch.setSelected(jEdit.getBooleanProperty(
					"search.hypersearch.toggle"));
			}
		}
		else if(searchIn == ALL_BUFFERS)
		{
			searchAllBuffers.setSelected(true);
			hyperSearch.setSelected(true);
		}
		else if(searchIn == DIRECTORY)
		{
			SearchFileSet fileset = SearchAndReplace.getSearchFileSet();

			if(fileset instanceof DirectoryListSet)
			{
				filter.setText(((DirectoryListSet)fileset)
					.getFileFilter());
				directory.setText(((DirectoryListSet)fileset)
					.getDirectory());
				searchSubDirectories.setSelected(((DirectoryListSet)fileset)
					.isRecursive());
			}

			hyperSearch.setSelected(true);
			searchDirectory.setSelected(true);
		}

		updateEnabled();
	} 

	
	public void ok()
	{
		try
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			if(!save(false))
				return;

			if(searchSelection.isSelected()
				&& view.getTextArea().getSelectionCount() == 0)
			{
				GUIUtilities.error(view,"search-no-selection",null);
				return;
			}

			if(hyperSearch.isSelected() || searchSelection.isSelected())
			{
				if(SearchAndReplace.hyperSearch(view,
					searchSelection.isSelected()))
					closeOrKeepDialog();
			}
			else
			{
				if(SearchAndReplace.find(view))
					closeOrKeepDialog();
				else
				{
					toFront();
					requestFocus();
					find.requestFocus();
				}
			}
		}
		finally
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	} 

	
	public void cancel()
	{
		save(true);
		GUIUtilities.saveGeometry(this,"search");
		setVisible(false);
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof SearchSettingsChanged)
		{
			if(!saving)
				load();
		}
	} 

	
	public void dispose()
	{
		EditBus.removeFromBus(this);
		viewHash.remove(view);
		super.dispose();
	} 

	

	private static final Map<View, SearchDialog> viewHash = new HashMap<View, SearchDialog>();

	
	private final View view;

	
	private HistoryTextArea find, replace;

	private JRadioButton stringReplace, beanShellReplace;

	
	private JCheckBox keepDialog, ignoreCase, regexp, hyperSearch,
		wrap;
	private JRadioButton searchBack, searchForward;
	private JRadioButton searchSelection, searchCurrentBuffer,
		searchAllBuffers, searchDirectory;

	
	private HistoryTextField filter, directory;
	private JCheckBox searchSubDirectories;
	private JCheckBox skipBinaryFiles;
	private JCheckBox skipHidden;
	
	private JButton choose;
	private JButton synchronize;

	
	private JButton findBtn,  replaceAndFindBtn, replaceAllBtn,
		closeBtn;

	private boolean saving;
	

	
	
	private SearchDialog(View view)
	{
		super(view,jEdit.getProperty("search.title"),false);

		this.view = view;

		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(new EmptyBorder(0,12,12,12));
		setContentPane(content);

		JPanel centerPanel = new JPanel(new BorderLayout());
		centerPanel.add(BorderLayout.CENTER,createFieldPanel());
		centerPanel.add(BorderLayout.SOUTH,createSearchSettingsPanel());
		content.add(BorderLayout.CENTER,centerPanel);
		content.add(BorderLayout.SOUTH,createMultiFilePanel());

		content.add(BorderLayout.EAST,createButtonsPanel());

		pack();
		jEdit.unsetProperty("search.width");
		jEdit.unsetProperty("search.d-width");
		jEdit.unsetProperty("search.height");
		jEdit.unsetProperty("search.d-height");
		GUIUtilities.loadGeometry(this,"search");

		load();

		EditBus.addToBus(this);
	} 

	
	private void createFindLabelAndField(JPanel fieldPanel,
		GridBagConstraints cons)
	{
		JLabel label = new JLabel(jEdit.getProperty("search.find"));
		
		label.setDisplayedMnemonic(jEdit.getProperty("search.find.mnemonic")
			.charAt(0));
		find = new HistoryTextArea("find");
		find.setColumns(25);
		find.setToolTipText(jEdit.getProperty("search.find.tooltip"));
		label.setToolTipText(jEdit.getProperty("search.find.tooltip"));
		label.setLabelFor(find);
		label.setBorder(new EmptyBorder(12,0,2,0));

		cons.gridx = 0;
		cons.weightx = 0.0;
		cons.weighty = 0.0;
		fieldPanel.add(label,cons);
		cons.gridy++;
		cons.weightx = 1.0;
		cons.weighty = 1.0;
		fieldPanel.add(new JScrollPane(find),cons);
		cons.gridy++;
	} 

	
	private void createReplaceLabelAndField(JPanel fieldPanel,
		GridBagConstraints cons)
	{
		JLabel label = new JLabel(jEdit.getProperty("search.replace"));
		label.setDisplayedMnemonic(jEdit.getProperty("search.replace.mnemonic")
			.charAt(0));
		label.setBorder(new EmptyBorder(12,0,0,0));

		cons.gridx = 0;
		cons.weightx = 0.0;
		cons.weighty = 0.0;
		fieldPanel.add(label,cons);
		cons.gridy++;

		ButtonGroup grp = new ButtonGroup();
		ReplaceActionHandler replaceActionHandler = new ReplaceActionHandler();

		
		
		
		

		stringReplace = new MyJRadioButton(jEdit.getProperty(
			"search.string-replace-btn"));
		stringReplace.addActionListener(replaceActionHandler);
		grp.add(stringReplace);
		cons.gridwidth = 1;
		fieldPanel.add(stringReplace,cons);
		cons.gridx++;
		cons.insets = new Insets(0,12,0,0);

		beanShellReplace = new MyJRadioButton(jEdit.getProperty(
			"search.beanshell-replace-btn"));
		beanShellReplace.addActionListener(replaceActionHandler);
		grp.add(beanShellReplace);
		fieldPanel.add(beanShellReplace,cons);
		cons.gridx = 0;
		cons.gridwidth = 2;
		cons.insets = new Insets(0,0,0,0);

		replace = new HistoryTextArea("replace");
		replace.setToolTipText(jEdit.getProperty("search.find.tooltip"));
		label.setLabelFor(replace);

		cons.gridx = 0;
		cons.gridy++;
		cons.weightx = 1.0;
		cons.weighty = 1.0;
		fieldPanel.add(new JScrollPane(replace),cons);
		cons.gridy++;
	} 

	
	private JPanel createFieldPanel()
	{
		JPanel fieldPanel = new JPanel(new GridBagLayout());
		fieldPanel.setBorder(new EmptyBorder(0,0,12,12));

		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.BOTH;
		cons.gridy = 0;
		cons.gridwidth = 2;

		createFindLabelAndField(fieldPanel,cons);
		createReplaceLabelAndField(fieldPanel,cons);

		return fieldPanel;
	} 

	
	private JPanel createSearchSettingsPanel()
	{
		JPanel searchSettings = new JPanel(new VariableGridLayout(
			VariableGridLayout.FIXED_NUM_COLUMNS,3));
		searchSettings.setBorder(new EmptyBorder(0,0,12,12));

		SettingsActionHandler actionHandler = new SettingsActionHandler();
		ButtonGroup fileset = new ButtonGroup();
		ButtonGroup direction = new ButtonGroup();

		searchSettings.add(new JLabel(jEdit.getProperty("search.fileset")));

		searchSettings.add(new JLabel(jEdit.getProperty("search.settings")));

		searchSettings.add(new JLabel(jEdit.getProperty("search.direction")));

		searchSelection = new JRadioButton(jEdit.getProperty("search.selection"));
		searchSelection.setMnemonic(jEdit.getProperty("search.selection.mnemonic")
			.charAt(0));
		fileset.add(searchSelection);
		searchSettings.add(searchSelection);
		searchSelection.addActionListener(actionHandler);

		keepDialog = new JCheckBox(jEdit.getProperty("search.keep"));
		keepDialog.setMnemonic(jEdit.getProperty("search.keep.mnemonic")
			.charAt(0));
		searchSettings.add(keepDialog);

		searchBack = new JRadioButton(jEdit.getProperty("search.back"));
		searchBack.setMnemonic(jEdit.getProperty("search.back.mnemonic")
			.charAt(0));
		direction.add(searchBack);
		searchSettings.add(searchBack);
		searchBack.addActionListener(actionHandler);

		searchCurrentBuffer = new JRadioButton(jEdit.getProperty("search.current"));
		searchCurrentBuffer.setMnemonic(jEdit.getProperty("search.current.mnemonic")
			.charAt(0));
		fileset.add(searchCurrentBuffer);
		searchSettings.add(searchCurrentBuffer);
		searchCurrentBuffer.addActionListener(actionHandler);

		ignoreCase = new JCheckBox(jEdit.getProperty("search.case"));
		ignoreCase.setMnemonic(jEdit.getProperty("search.case.mnemonic")
			.charAt(0));
		searchSettings.add(ignoreCase);
		ignoreCase.addActionListener(actionHandler);

		searchForward = new JRadioButton(jEdit.getProperty("search.forward"));
		searchForward.setMnemonic(jEdit.getProperty("search.forward.mnemonic")
			.charAt(0));
		direction.add(searchForward);
		searchSettings.add(searchForward);
		searchForward.addActionListener(actionHandler);

		searchAllBuffers = new JRadioButton(jEdit.getProperty("search.all"));
		searchAllBuffers.setMnemonic(jEdit.getProperty("search.all.mnemonic")
			.charAt(0));
		fileset.add(searchAllBuffers);
		searchSettings.add(searchAllBuffers);
		searchAllBuffers.addActionListener(actionHandler);

		regexp = new JCheckBox(jEdit.getProperty("search.regexp"));
		regexp.setMnemonic(jEdit.getProperty("search.regexp.mnemonic")
			.charAt(0));
		searchSettings.add(regexp);
		regexp.addActionListener(actionHandler);

		wrap = new JCheckBox(jEdit.getProperty("search.wrap"));
		wrap.setMnemonic(jEdit.getProperty("search.wrap.mnemonic")
			.charAt(0));
		searchSettings.add(wrap);
		wrap.addActionListener(actionHandler);

		searchDirectory = new JRadioButton(jEdit.getProperty("search.directory"));
		searchDirectory.setMnemonic(jEdit.getProperty("search.directory.mnemonic")
			.charAt(0));
		fileset.add(searchDirectory);
		searchSettings.add(searchDirectory);
		searchDirectory.addActionListener(actionHandler);

		hyperSearch = new JCheckBox(jEdit.getProperty("search.hypersearch"));
		hyperSearch.setMnemonic(jEdit.getProperty("search.hypersearch.mnemonic")
			.charAt(0));
		searchSettings.add(hyperSearch);
		hyperSearch.addActionListener(actionHandler);

		return searchSettings;
	} 

	
	private JPanel createMultiFilePanel()
	{
		JPanel multifile = new JPanel();

		GridBagLayout layout = new GridBagLayout();
		multifile.setLayout(layout);

		GridBagConstraints cons = new GridBagConstraints();
		cons.gridy = cons.gridwidth = cons.gridheight = 1;
		cons.anchor = GridBagConstraints.WEST;
		cons.fill = GridBagConstraints.HORIZONTAL;

		MultiFileActionHandler actionListener = new MultiFileActionHandler();
		filter = new HistoryTextField("search.filter");
		
		filter.setToolTipText(jEdit.getProperty("search.filterField.tooltip"));
		filter.addActionListener(actionListener);

		cons.insets = new Insets(0,0,3,0);

		JLabel label = new JLabel(jEdit.getProperty("search.filterField"),
			SwingConstants.RIGHT);
		label.setBorder(new EmptyBorder(0,0,0,12));
		label.setDisplayedMnemonic(jEdit.getProperty("search.filterField.mnemonic")
			.charAt(0));
		label.setLabelFor(filter);
		cons.weightx = 0.0;
		layout.setConstraints(label,cons);
		multifile.add(label);

		cons.gridwidth = 2;
		cons.insets = new Insets(0,0,3,6);
		cons.weightx = 1.0;
		layout.setConstraints(filter,cons);
		multifile.add(filter);

		cons.gridwidth = 1;
		cons.weightx = 0.0;
		cons.insets = new Insets(0,0,3,0);

		synchronize = new JButton(jEdit.getProperty(
			"search.synchronize"));
		synchronize.setMnemonic(jEdit.getProperty(
			"search.synchronize.mnemonic")
			.charAt(0));
		synchronize.addActionListener(actionListener);
		layout.setConstraints(synchronize,cons);
		multifile.add(synchronize);

		cons.gridy++;

		directory = new HistoryTextField("search.directory");
		directory.setColumns(25);
		directory.addActionListener(actionListener);

		label = new JLabel(jEdit.getProperty("search.directoryField"),
			SwingConstants.RIGHT);
		label.setBorder(new EmptyBorder(0,0,0,12));

		label.setDisplayedMnemonic(jEdit.getProperty("search.directoryField.mnemonic")
			.charAt(0));
		label.setLabelFor(directory);
		cons.insets = new Insets(0,0,3,0);
		cons.weightx = 0.0;
		layout.setConstraints(label,cons);
		multifile.add(label);

		cons.insets = new Insets(0,0,3,6);
		cons.weightx = 1.0;
		cons.gridwidth = 2;
		layout.setConstraints(directory,cons);
		multifile.add(directory);

		choose = new JButton(jEdit.getProperty("search.choose"));
		choose.setMnemonic(jEdit.getProperty("search.choose.mnemonic")
			.charAt(0));
		cons.insets = new Insets(0,0,3,0);
		cons.weightx = 0.0;
		cons.gridwidth = 1;
		layout.setConstraints(choose,cons);
		multifile.add(choose);
		choose.addActionListener(actionListener);

		cons.insets = new Insets(0,0,0,0);
		cons.gridy++;
		cons.gridwidth = 3;

		JPanel dirCheckBoxPanel = new JPanel();
 		searchSubDirectories = new JCheckBox(jEdit.getProperty(
 			"search.subdirs"));
 		String mnemonic = jEdit.getProperty(
			"search.subdirs.mnemonic");
		searchSubDirectories.setMnemonic(mnemonic.charAt(0));
		searchSubDirectories.setSelected(jEdit.getBooleanProperty("search.subdirs.toggle"));
		skipHidden = new JCheckBox(jEdit.getProperty("search.skipHidden"));
		skipHidden.setSelected(jEdit.getBooleanProperty("search.skipHidden.toggle", true));
		skipBinaryFiles = new JCheckBox(jEdit.getProperty("search.skipBinary"));
		skipBinaryFiles.setSelected(jEdit.getBooleanProperty("search.skipBinary.toggle", true));
		dirCheckBoxPanel.add(searchSubDirectories);
		dirCheckBoxPanel.add(skipHidden);
		dirCheckBoxPanel.add(skipBinaryFiles);

		cons.insets = new Insets(0, 0, 0, 0);
		cons.gridy++;
		cons.gridwidth = 4;
		layout.setConstraints(dirCheckBoxPanel, cons);

 		multifile.add(dirCheckBoxPanel);

		return multifile;
	} 

	
	private Box createButtonsPanel()
	{
		Box box = new Box(BoxLayout.Y_AXIS);

		ButtonActionHandler actionHandler = new ButtonActionHandler();

		box.add(Box.createVerticalStrut(12));

		JPanel grid = new JPanel(new GridLayout(5,1,0,12));

		findBtn = new JButton(jEdit.getProperty("search.findBtn"));
		
		getRootPane().setDefaultButton(findBtn);
		grid.add(findBtn);
		findBtn.addActionListener(actionHandler);

		

		replaceAndFindBtn = new JButton(jEdit.getProperty("search.replaceAndFindBtn"));
		replaceAndFindBtn.setMnemonic(jEdit.getProperty("search.replaceAndFindBtn.mnemonic")
			.charAt(0));
		grid.add(replaceAndFindBtn);
		replaceAndFindBtn.addActionListener(actionHandler);

		replaceAllBtn = new JButton(jEdit.getProperty("search.replaceAllBtn"));
		replaceAllBtn.setMnemonic(jEdit.getProperty("search.replaceAllBtn.mnemonic")
			.charAt(0));
		grid.add(replaceAllBtn);
		replaceAllBtn.addActionListener(actionHandler);

		closeBtn = new JButton(jEdit.getProperty("common.close"));
		grid.add(closeBtn);
		closeBtn.addActionListener(actionHandler);

		grid.setMaximumSize(grid.getPreferredSize());

		box.add(grid);
		box.add(Box.createGlue());

		return box;
	} 

	
	private void updateEnabled()
	{
		wrap.setEnabled(!hyperSearch.isSelected()
			&& !searchSelection.isSelected());

		boolean reverseEnabled = !hyperSearch.isSelected()
			&& searchCurrentBuffer.isSelected()
			&& !regexp.isSelected();
		searchBack.setEnabled(reverseEnabled);
		searchForward.setEnabled(reverseEnabled);
		if(!reverseEnabled)
			searchForward.setSelected(true);

		filter.setEnabled(searchAllBuffers.isSelected()
			|| searchDirectory.isSelected());

		boolean searchDirs = searchDirectory.isSelected();
		directory.setEnabled(searchDirs);
		choose.setEnabled(searchDirs);
		searchSubDirectories.setEnabled(searchDirs);
		skipHidden.setEnabled(searchDirs);
		skipBinaryFiles.setEnabled(searchDirs);
		
		synchronize.setEnabled(searchAllBuffers.isSelected()
			|| searchDirectory.isSelected());

		findBtn.setEnabled(!searchSelection.isSelected()
			|| hyperSearch.isSelected());
		replaceAndFindBtn.setEnabled(!hyperSearch.isSelected()
			&& !searchSelection.isSelected());
	} 

	
	
	private boolean save(boolean cancel)
	{
		try
		{
			
			
			saving = true;
			SearchAndReplace.setIgnoreCase(ignoreCase.isSelected());
			SearchAndReplace.setRegexp(regexp.isSelected());
			SearchAndReplace.setReverseSearch(searchBack.isSelected());
			SearchAndReplace.setAutoWrapAround(wrap.isSelected());
			jEdit.setBooleanProperty("search.subdirs.toggle", searchSubDirectories.isSelected());
			jEdit.setBooleanProperty("search.skipHidden.toggle", skipHidden.isSelected());
			jEdit.setBooleanProperty("search.skipBinary.toggle", skipBinaryFiles.isSelected());

			String filter = this.filter.getText();
			this.filter.addCurrentToHistory();
			if(filter.length() == 0)
				filter = "*";

			SearchFileSet fileset = SearchAndReplace.getSearchFileSet();

			boolean recurse = searchSubDirectories.isSelected();

			if(searchSelection.isSelected())
				fileset = new CurrentBufferSet();
			else if(searchCurrentBuffer.isSelected())
			{
				fileset = new CurrentBufferSet();

				jEdit.setBooleanProperty("search.hypersearch.toggle",
					hyperSearch.isSelected());
			}
			else if(searchAllBuffers.isSelected())
				fileset = new AllBufferSet(filter);
			else if(searchDirectory.isSelected())
			{
				String directory = this.directory.getText();
				this.directory.addCurrentToHistory();
				directory = MiscUtilities.constructPath(
					view.getBuffer().getDirectory(),directory);

				if((VFSManager.getVFSForPath(directory).getCapabilities()
					& VFS.LOW_LATENCY_CAP) == 0)
				{
					if(cancel)
						return false;

					int retVal = GUIUtilities.confirm(
						this,"remote-dir-search",
						null,JOptionPane.YES_NO_OPTION,
						JOptionPane.WARNING_MESSAGE);
					if(retVal != JOptionPane.YES_OPTION)
						return false;
				}

				if(fileset instanceof DirectoryListSet)
				{
					DirectoryListSet dset = (DirectoryListSet)fileset;
					dset.setDirectory(directory);
					dset.setFileFilter(filter);
					dset.setRecursive(recurse);
					EditBus.send(new SearchSettingsChanged(null));
				}
				else
					fileset = new DirectoryListSet(directory,filter,recurse);
			}
			else
			{
				
				fileset = null;
			}

			jEdit.setBooleanProperty("search.subdirs.toggle",
				recurse);
			jEdit.setBooleanProperty("search.keepDialog.toggle",
				keepDialog.isSelected());

			SearchAndReplace.setSearchFileSet(fileset);

			replace.addCurrentToHistory();
			SearchAndReplace.setReplaceString(replace.getText());

			if(find.getText().length() == 0)
			{
				if(!cancel)
					getToolkit().beep();
				return false;
			}

			find.addCurrentToHistory();
			SearchAndReplace.setSearchString(find.getText());

			return true;
		}
		finally
		{
			saving = false;
		}
	} 

	
	private void closeOrKeepDialog()
	{
		if(keepDialog.isSelected())
		{
			
			

			
			
			if(!hyperSearch.isSelected())
			{
				toFront();
				requestFocus();
				find.requestFocus();
			}
		}
		else
		{
			GUIUtilities.saveGeometry(this,"search");
			setVisible(false);
		}
	} 

	
	private void load()
	{
		ignoreCase.setSelected(SearchAndReplace.getIgnoreCase());
		regexp.setSelected(SearchAndReplace.getRegexp());
		wrap.setSelected(SearchAndReplace.getAutoWrapAround());

		if(SearchAndReplace.getReverseSearch())
			searchBack.setSelected(true);
		else
			searchForward.setSelected(true);

		if(SearchAndReplace.getBeanShellReplace())
		{
			replace.setModel("replace.script");
			beanShellReplace.setSelected(true);
		}
		else
		{
			replace.setModel("replace");
			stringReplace.setSelected(true);
		}

		SearchFileSet fileset = SearchAndReplace.getSearchFileSet();

		HistoryModel model = filter.getModel();
		if(model.getSize() != 0)
			filter.setText(model.getItem(0));
		else
		{
			filter.setText('*' + MiscUtilities
				.getFileExtension(view.getBuffer()
				.getName()));
		}
		model = directory.getModel();
		if(model.getSize() != 0)
			directory.setText(model.getItem(0));
		else
			directory.setText(view.getBuffer().getDirectory());

		searchSubDirectories.setSelected(jEdit.getBooleanProperty(
			"search.subdirs.toggle"));

		if(fileset instanceof DirectoryListSet)
		{
			filter.setText(((DirectoryListSet)fileset)
				.getFileFilter());
			directory.setText(((DirectoryListSet)fileset)
				.getDirectory());
			searchSubDirectories.setSelected(((DirectoryListSet)fileset)
				.isRecursive());
		}
		else if(fileset instanceof AllBufferSet)
		{
			filter.setText(((AllBufferSet)fileset)
				.getFileFilter());
		}

		directory.addCurrentToHistory();

		keepDialog.setSelected(jEdit.getBooleanProperty(
			"search.keepDialog.toggle"));
	} 

	

	

	

	
	
	
	static class MyJRadioButton extends JRadioButton
	{
		MyJRadioButton(String label)
		{
			super(label);
		}

		public boolean isFocusable()
		{
			return false;
		}
	} 

	
	class ReplaceActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			replace.setModel(beanShellReplace.isSelected()
				? "replace.script"
				: "replace");
			SearchAndReplace.setBeanShellReplace(
				beanShellReplace.isSelected());
		}
	} 

	
	class SettingsActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();

			if(source == searchCurrentBuffer)
				hyperSearch.setSelected(false);
			else if(source == searchSelection
				|| source == searchAllBuffers
				|| source == searchDirectory)
				hyperSearch.setSelected(true);

			save(true);
			updateEnabled();
		}
	} 

	
	class MultiFileActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == choose)
			{
				String[] dirs = GUIUtilities.showVFSFileDialog(
					SearchDialog.this,
					view,directory.getText(),
					VFSBrowser.CHOOSE_DIRECTORY_DIALOG,
					false);
				if(dirs != null)
					directory.setText(dirs[0]);
			}
			else if(evt.getSource() == synchronize)
			{
				synchronizeMultiFileSettings();
			}
			else 
			{
				
				
				ok();
			}
		}


		
		private void synchronizeMultiFileSettings()
		{
			directory.setText(view.getBuffer().getDirectory());

			SearchFileSet fileset = SearchAndReplace.getSearchFileSet();

			if(fileset instanceof AllBufferSet)
			{
				filter.setText(((AllBufferSet)fileset)
					.getFileFilter());
			}
			else
			{
				filter.setText('*' + MiscUtilities
					.getFileExtension(view.getBuffer()
					.getName()));
			}
		} 
	} 

	
	class ButtonActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();

			if(source == closeBtn)
				cancel();
			else if(source == findBtn || source == find
				|| source == replace)
			{
				ok();
			}
			else if(source == replaceAndFindBtn)
			{
				save(false);
				if(SearchAndReplace.replace(view))
					ok();
				else
					getToolkit().beep();
			}
			else if(source == replaceAllBtn)
			{
				if(searchSelection.isSelected() &&
					view.getTextArea().getSelectionCount()
					== 0)
				{
					GUIUtilities.error(view,"search-no-selection",null);
					return;
				}

				setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				if(!save(false))
				{
					setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					getToolkit().beep();
					return;
				}

				if(searchSelection.isSelected())
				{
					if(SearchAndReplace.replace(view))
						closeOrKeepDialog();
					else
						getToolkit().beep();
				}
				else
				{
					if(SearchAndReplace.replaceAll(view))
						closeOrKeepDialog();
					else
						getToolkit().beep();
				}

				setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		}
	} 

	
}
