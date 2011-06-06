

package org.gjt.sp.jedit.options;


import javax.swing.*;

import java.awt.event.*;
import java.io.*;
import org.gjt.sp.jedit.gui.FontSelector;
import org.gjt.sp.jedit.gui.NumericTextField;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.IOUtilities;


public class AppearanceOptionPane extends AbstractOptionPane
{
	
	public static final String[] builtInIconThemes = {"tango", "old"};
	
	
	public AppearanceOptionPane()
	{
		super("appearance");
	} 

	
	@Override
	protected void _init()
	{
		
		addComponent(new JLabel(jEdit.getProperty("options.appearance.lf.note")));

		lfs = UIManager.getInstalledLookAndFeels();
		String[] names = new String[lfs.length];
		String lf = UIManager.getLookAndFeel().getClass().getName();
		int index = 0;
		for(int i = 0; i < names.length; i++)
		{
			names[i] = lfs[i].getName();
			if(lf.equals(lfs[i].getClassName()))
				index = i;
		}

		lookAndFeel = new JComboBox(names);
		lookAndFeel.setSelectedIndex(index);
		lookAndFeel.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				updateEnabled();
			}
		});

		
		addComponent(jEdit.getProperty("options.appearance.lf"),
			lookAndFeel);
		addDockingFrameworkChooser();

		
		String[] themes = IconTheme.builtInNames();
		iconThemes = new JComboBox(themes);
		addComponent(jEdit.getProperty("options.appearance.iconTheme"), iconThemes);
		oldTheme = IconTheme.get();
		for (int i=0; i<themes.length; ++i)
		{
			if (themes[i].equals(oldTheme))
			{
				iconThemes.setSelectedIndex(i);
				break;
			}
		}
		
		
		primaryFont = new FontSelector(jEdit.getFontProperty(
			"metal.primary.font"));
		addComponent(jEdit.getProperty("options.appearance.primaryFont"),
			primaryFont);

		
		secondaryFont = new FontSelector(jEdit.getFontProperty(
			"metal.secondary.font"));
		addComponent(jEdit.getProperty("options.appearance.secondaryFont"),
			secondaryFont);

		
		updateEnabled();

		
		history = new NumericTextField(jEdit.getProperty("history"), true);
		addComponent(jEdit.getProperty("options.appearance.history"),history);

		
		menuSpillover = new NumericTextField(jEdit.getProperty("menu.spillover"), true);
		addComponent(jEdit.getProperty("options.appearance.menuSpillover"),menuSpillover);

		continuousLayout = new JCheckBox(jEdit.getProperty(
			"options.appearance.continuousLayout.label"));
		continuousLayout.setSelected(jEdit.getBooleanProperty("appearance.continuousLayout"));
		addComponent(continuousLayout);

		addSeparator("options.appearance.startup.label");

		
		showSplash = new JCheckBox(jEdit.getProperty(
			"options.appearance.showSplash"));
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			showSplash.setSelected(true);
		else
			showSplash.setSelected(!new File(settingsDirectory,"nosplash").exists());
		addComponent(showSplash);

		
		showTips = new JCheckBox(jEdit.getProperty(
			"options.appearance.showTips"));
		showTips.setSelected(jEdit.getBooleanProperty("tip.show"));
		addComponent(showTips);

		addSeparator("options.appearance.experimental.label");
		addComponent(GUIUtilities.createMultilineLabel(
			jEdit.getProperty("options.appearance.experimental.caption")));

		
		textColors = new JCheckBox(jEdit.getProperty(
			"options.appearance.textColors"));
		textColors.setSelected(jEdit.getBooleanProperty("textColors"));
		addComponent(textColors);

		
		decorateFrames = new JCheckBox(jEdit.getProperty(
			"options.appearance.decorateFrames"));
		decorateFrames.setSelected(jEdit.getBooleanProperty("decorate.frames"));
		addComponent(decorateFrames);

		
		decorateDialogs = new JCheckBox(jEdit.getProperty(
			"options.appearance.decorateDialogs"));
		decorateDialogs.setSelected(jEdit.getBooleanProperty("decorate.dialogs"));
		addComponent(decorateDialogs);
	} 

	
	@Override
	protected void _save()
	{
		String lf = lfs[lookAndFeel.getSelectedIndex()].getClassName();
		jEdit.setProperty("lookAndFeel",lf);
		jEdit.setFontProperty("metal.primary.font",primaryFont.getFont());
		jEdit.setFontProperty("metal.secondary.font",secondaryFont.getFont());
		jEdit.setProperty("history",history.getText());
		jEdit.setProperty("menu.spillover",menuSpillover.getText());
		jEdit.setBooleanProperty("tip.show",showTips.isSelected());
		jEdit.setBooleanProperty("appearance.continuousLayout",continuousLayout.isSelected());
		IconTheme.set(iconThemes.getSelectedItem().toString());

		jEdit.setProperty(View.VIEW_DOCKING_FRAMEWORK_PROPERTY,
			(String) dockingFramework.getSelectedItem());

		

		
		
		
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory != null)
		{
			File file = new File(settingsDirectory,"nosplash");
			if(showSplash.isSelected())
				file.delete();
			else
			{
				FileOutputStream out = null;
				try
				{
					out = new FileOutputStream(file);
					out.write('\n');
					out.close();
				}
				catch(IOException io)
				{
					Log.log(Log.ERROR,this,io);
				}
				finally
				{
					IOUtilities.closeQuietly(out);
				}
			}
		}
		jEdit.setBooleanProperty("textColors",textColors.isSelected());
		jEdit.setBooleanProperty("decorate.frames",decorateFrames.isSelected());
		jEdit.setBooleanProperty("decorate.dialogs",decorateDialogs.isSelected());
	} 

	

	
	private String oldTheme;
	private UIManager.LookAndFeelInfo[] lfs;
	private JComboBox lookAndFeel;
	private FontSelector primaryFont;
	private FontSelector secondaryFont;
	private JComboBox dockingFramework;
	private JTextField history;
	private JTextField menuSpillover;
	private JCheckBox showTips;
	private JCheckBox continuousLayout;
	private JCheckBox showSplash;
	private JCheckBox textColors;
	private JCheckBox decorateFrames;
	private JCheckBox decorateDialogs;
	private JComboBox antiAliasExtras;
	private JComboBox iconThemes;
	

	
	private void updateEnabled()
	{
		String className = lfs[lookAndFeel.getSelectedIndex()]
			.getClassName();

		if(className.equals("javax.swing.plaf.metal.MetalLookAndFeel")
			|| className.equals("com.incors.plaf.kunststoff.KunststoffLookAndFeel"))
		{
			primaryFont.setEnabled(true);
			secondaryFont.setEnabled(true);
		}
		else
		{
			primaryFont.setEnabled(false);
			secondaryFont.setEnabled(false);
		}
	} 
	private void addDockingFrameworkChooser()
	{	
		String [] frameworks =
			ServiceManager.getServiceNames(View.DOCKING_FRAMEWORK_PROVIDER_SERVICE);
		dockingFramework = new JComboBox(frameworks);
		String framework = View.getDockingFrameworkName();
		for (int i = 0; i < frameworks.length; i++)
		{
			if (frameworks[i].equals(framework))
			{
				dockingFramework.setSelectedIndex(i);
				break;
			}
		}
		addComponent(new JLabel(jEdit.getProperty("options.appearance.selectFramework.label")), dockingFramework);
	}

	
}
