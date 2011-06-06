

package org.gjt.sp.jedit.options;


import javax.swing.*;
import java.awt.event.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.buffer.FoldHandler;


public class EditingOptionPane extends AbstractOptionPane
{
	
	public EditingOptionPane()
	{
		super("editing");
	} 

	
	protected void _init()
	{
		Mode[] modes = jEdit.getModes();

		defaultMode = new JComboBox(modes);
		defaultMode.setSelectedItem(jEdit.getMode(
			jEdit.getProperty("buffer.defaultMode")));
		addComponent(jEdit.getProperty("options.editing.defaultMode"),
			defaultMode);

		undoCount = new JTextField(jEdit.getProperty("buffer.undoCount"));
		addComponent(jEdit.getProperty("options.editing.undoCount"),undoCount);

		addSeparator();

		global = new ModeProperties();
		modeProps = new ModeProperties[modes.length];

		String[] modeNames = new String[modes.length + 1];
		modeNames[0] = jEdit.getProperty("options.editing.global");

		for(int i = 0; i < modes.length; i++)
		{
			modeProps[i] = new ModeProperties(modes[i]);
			modeNames[i + 1] = modes[i].getName();
		}

		mode = new JComboBox(modeNames);
		mode.addActionListener(new ActionHandler());

		addComponent(jEdit.getProperty("options.editing.mode"),mode);

		useDefaults = new JCheckBox(jEdit.getProperty("options.editing.useDefaults"));
		useDefaults.addActionListener(new ActionHandler());
		addComponent(useDefaults);

		addComponent(jEdit.getProperty("options.editing.noWordSep"),
			noWordSep = new JTextField());

		String[] foldModes = FoldHandler.getFoldModes();
		addComponent(jEdit.getProperty("options.editing.folding"),
			folding = new JComboBox(foldModes));

		addComponent(jEdit.getProperty("options.editing.collapseFolds"),
			collapseFolds = new JTextField());

		String[] wrapModes = {
			"none",
			"soft",
			"hard"
		};
		addComponent(jEdit.getProperty("options.editing.wrap"),
			wrap = new JComboBox(wrapModes));

		String[] lineLens = { "0", "72", "76", "80" };
		addComponent(jEdit.getProperty("options.editing.maxLineLen"),
			maxLineLen = new JComboBox(lineLens));
		maxLineLen.setEditable(true);

		String[] tabSizes = { "2", "4", "8" };
		addComponent(jEdit.getProperty("options.editing.tabSize"),
			tabSize = new JComboBox(tabSizes));
		tabSize.setEditable(true);

		addComponent(jEdit.getProperty("options.editing.indentSize"),
			indentSize = new JComboBox(tabSizes));
		indentSize.setEditable(true);

		addComponent(noTabs = new JCheckBox(jEdit.getProperty(
			"options.editing.noTabs")));

		addComponent(jEdit.getProperty("options.editing.filenameGlob"),
			filenameGlob = new JTextField());

		addComponent(jEdit.getProperty("options.editing.firstlineGlob"),
			firstlineGlob = new JTextField());

		selectMode();
	} 

	
	protected void _save()
	{
		jEdit.setProperty("buffer.defaultMode",
			((Mode)defaultMode.getSelectedItem()).getName());
		jEdit.setProperty("buffer.undoCount",undoCount.getText());

		saveMode();

		global.save();

		for(int i = 0; i < modeProps.length; i++)
		{
			modeProps[i].save();
		}
	} 

	

	
	private JComboBox defaultMode;
	private JTextField undoCount;
	private ModeProperties global;
	private ModeProperties[] modeProps;
	private ModeProperties current;
	private JComboBox mode;
	private JCheckBox useDefaults;
	private JTextField filenameGlob;
	private JTextField firstlineGlob;
	private JTextField noWordSep;
	private JComboBox folding;
	private JTextField collapseFolds;
	private JComboBox wrap;
	private JComboBox maxLineLen;
	private JComboBox tabSize;
	private JComboBox indentSize;
	private JCheckBox noTabs;
	

	
	private void saveMode()
	{
		current.useDefaults = useDefaults.isSelected();
		current.filenameGlob = filenameGlob.getText();
		current.firstlineGlob = firstlineGlob.getText();
		current.noWordSep = noWordSep.getText();
		current.folding = (String)folding.getSelectedItem();
		current.collapseFolds = collapseFolds.getText();
		current.wrap = (String)wrap.getSelectedItem();
		current.maxLineLen = (String)maxLineLen.getSelectedItem();
		current.tabSize = (String)tabSize.getSelectedItem();
		current.indentSize = (String)indentSize.getSelectedItem();
		current.noTabs = noTabs.isSelected();
	} 

	
	private void selectMode()
	{
		int index = mode.getSelectedIndex();
		current = (index == 0 ? global : modeProps[index - 1]);
		current.edited = true;
		current.load();

		useDefaults.setSelected(current.useDefaults);
		filenameGlob.setText(current.filenameGlob);
		firstlineGlob.setText(current.firstlineGlob);
		noWordSep.setText(current.noWordSep);
		folding.setSelectedItem(current.folding);
		collapseFolds.setText(current.collapseFolds);
		wrap.setSelectedItem(current.wrap);
		maxLineLen.setSelectedItem(current.maxLineLen);
		tabSize.setSelectedItem(current.tabSize);
		indentSize.setSelectedItem(current.indentSize);
		noTabs.setSelected(current.noTabs);

		updateEnabled();
	} 

	
	private void updateEnabled()
	{
		if(current == global)
		{
			useDefaults.setEnabled(false);
			filenameGlob.setEnabled(false);
			firstlineGlob.setEnabled(false);
			noWordSep.setEnabled(true);
			folding.setEnabled(true);
			collapseFolds.setEnabled(true);
			wrap.setEnabled(true);
			maxLineLen.setEnabled(true);
			tabSize.setEnabled(true);
			indentSize.setEnabled(true);
			noTabs.setEnabled(true);
		}
		else
		{
			useDefaults.setEnabled(true);
			boolean enabled = !modeProps[mode.getSelectedIndex() - 1].useDefaults;
			filenameGlob.setEnabled(enabled);
			firstlineGlob.setEnabled(enabled);
			noWordSep.setEnabled(enabled);
			folding.setEnabled(enabled);
			collapseFolds.setEnabled(enabled);
			wrap.setEnabled(enabled);
			maxLineLen.setEnabled(enabled);
			tabSize.setEnabled(enabled);
			indentSize.setEnabled(enabled);
			noTabs.setEnabled(enabled);
		}
	} 

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == mode)
			{
				saveMode();
				selectMode();
			}
			else if(evt.getSource() == useDefaults)
			{
				modeProps[mode.getSelectedIndex() - 1].useDefaults =
					useDefaults.isSelected();
				updateEnabled();
			}
		}
	} 

	
	class ModeProperties
	{
		
		Mode mode;
		boolean edited;
		boolean loaded;

		boolean useDefaults;
		String filenameGlob;
		String firstlineGlob;
		String noWordSep;
		String folding;
		String collapseFolds;
		String wrap;
		String maxLineLen;
		String tabSize;
		String indentSize;
		boolean noTabs;
		

		
		ModeProperties()
		{
		} 

		
		ModeProperties(Mode mode)
		{
			this.mode = mode;
		} 

		
		void load()
		{
			if(loaded)
				return;

			loaded = true;

			if(mode != null)
			{
				mode.loadIfNecessary();

				useDefaults = !jEdit.getBooleanProperty("mode."
					+ mode.getName() + ".customSettings");
				filenameGlob = (String)mode.getProperty("filenameGlob");
				firstlineGlob = (String)mode.getProperty("firstlineGlob");
				noWordSep = (String)mode.getProperty("noWordSep");
				folding = mode.getProperty("folding").toString();
				collapseFolds = mode.getProperty("collapseFolds").toString();
				wrap = mode.getProperty("wrap").toString();
				maxLineLen = mode.getProperty("maxLineLen").toString();
				tabSize = mode.getProperty("tabSize").toString();
				indentSize = mode.getProperty("indentSize").toString();
				noTabs = mode.getBooleanProperty("noTabs");
			}
			else
			{
				noWordSep = jEdit.getProperty("buffer.noWordSep");
				folding = jEdit.getProperty("buffer.folding");
				collapseFolds = jEdit.getProperty("buffer.collapseFolds");
				wrap = jEdit.getProperty("buffer.wrap");
				maxLineLen = jEdit.getProperty("buffer.maxLineLen");
				tabSize = jEdit.getProperty("buffer.tabSize");
				indentSize = jEdit.getProperty("buffer.indentSize");
				noTabs = jEdit.getBooleanProperty("buffer.noTabs");
			}
		} 

		
		void save()
		{
			
			
			if(!edited)
				return;

			String prefix;
			if(mode != null)
			{
				prefix = "mode." + mode.getName() + ".";
				jEdit.setBooleanProperty(prefix + "customSettings",!useDefaults);

				
				
				String oldFilenameGlob = (String)mode.getProperty("filenameGlob");
				String oldFirstlineGlob = (String)mode.getProperty("firstlineGlob");
				if(useDefaults)
				{
					jEdit.resetProperty(prefix + "filenameGlob");
					jEdit.resetProperty(prefix + "firstlineGlob");
					jEdit.resetProperty(prefix + "noWordSep");
					jEdit.resetProperty(prefix + "folding");
					jEdit.resetProperty(prefix + "collapseFolds");
					jEdit.resetProperty(prefix + "wrap");
					jEdit.resetProperty(prefix + "maxLineLen");
					jEdit.resetProperty(prefix + "tabSize");
					jEdit.resetProperty(prefix + "indentSize");
					jEdit.resetProperty(prefix + "noTabs");
	
					if(!(MiscUtilities.objectsEqual(oldFilenameGlob,
						(String)mode.getProperty("filenameGlob"))
						&& MiscUtilities.objectsEqual(oldFirstlineGlob,
						(String)mode.getProperty("firstlineGlob"))))
					{
						mode.init();
					}

					return;
				}
				else
				{
					jEdit.setProperty(prefix + "filenameGlob",filenameGlob);
					jEdit.setProperty(prefix + "firstlineGlob",firstlineGlob);

					if(!(MiscUtilities.objectsEqual(oldFilenameGlob,
						filenameGlob)
						&& MiscUtilities.objectsEqual(oldFirstlineGlob,
						firstlineGlob)))
					{
						mode.init();
					}
				}
			}
			else
			{
				prefix = "buffer.";
			}

			jEdit.setProperty(prefix + "noWordSep",noWordSep);
			jEdit.setProperty(prefix + "folding",folding);
			jEdit.setProperty(prefix + "collapseFolds",collapseFolds);
			jEdit.setProperty(prefix + "wrap",wrap);
			jEdit.setProperty(prefix + "maxLineLen",maxLineLen);
			jEdit.setProperty(prefix + "tabSize",tabSize);
			jEdit.setProperty(prefix + "indentSize",indentSize);
			jEdit.setBooleanProperty(prefix + "noTabs",noTabs);
		} 
	} 
}
