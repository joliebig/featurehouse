

package org.gjt.sp.jedit.options;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.gui.ColorWellButton;
import org.gjt.sp.jedit.*;


public class StatusBarOptionPane extends AbstractOptionPane
{
	
	public StatusBarOptionPane()
	{
		super("status");
	} 

	
	protected void _init()
	{
		
		statusVisible = new JCheckBox(jEdit.getProperty(
			"options.status.visible"));
		statusVisible.setSelected(jEdit.getBooleanProperty(
			"view.status.visible"));
		statusVisible.addActionListener(new ActionHandler());
		addComponent(statusVisible);

		
		addComponent(jEdit.getProperty("options.status.foreground"),
			foregroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.foreground")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.status.background"),
			backgroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.background")),
			GridBagConstraints.VERTICAL);

		
		showCaretStatus = new JCheckBox(jEdit.getProperty(
			"options.status.show-caret-status"));
		showCaretStatus.setSelected(jEdit.getBooleanProperty(
			"view.status.show-caret-status"));
		addComponent(showCaretStatus);

		
		showEditMode = new JCheckBox(jEdit.getProperty(
			"options.status.show-edit-mode"));
		showEditMode.setSelected(jEdit.getBooleanProperty(
			"view.status.show-edit-mode"));
		addComponent(showEditMode);

		
		showFoldMode = new JCheckBox(jEdit.getProperty(
			"options.status.show-fold-mode"));
		showFoldMode.setSelected(jEdit.getBooleanProperty(
			"view.status.show-fold-mode"));
		addComponent(showFoldMode);

		
		showEncoding = new JCheckBox(jEdit.getProperty(
			"options.status.show-encoding"));
		showEncoding.setSelected(jEdit.getBooleanProperty(
			"view.status.show-encoding"));
		addComponent(showEncoding);

		
		showWrap = new JCheckBox(jEdit.getProperty(
			"options.status.show-wrap"));
		showWrap.setSelected(jEdit.getBooleanProperty(
			"view.status.show-wrap"));
		addComponent(showWrap);

		
		showMultiSelect = new JCheckBox(jEdit.getProperty(
			"options.status.show-multi-select"));
		showMultiSelect.setSelected(jEdit.getBooleanProperty(
			"view.status.show-multi-select"));
		addComponent(showMultiSelect);

		
		showRectSelect = new JCheckBox(jEdit.getProperty(
			"options.status.show-rect-select"));
		showRectSelect.setSelected(jEdit.getBooleanProperty(
			"view.status.show-rect-select"));
		addComponent(showRectSelect);

		
		showOverwrite = new JCheckBox(jEdit.getProperty(
			"options.status.show-overwrite"));
		showOverwrite.setSelected(jEdit.getBooleanProperty(
			"view.status.show-overwrite"));
		addComponent(showOverwrite);

		
		showLineSeperator = new JCheckBox(jEdit.getProperty(
			"options.status.show-line-seperator"));
		showLineSeperator.setSelected(jEdit.getBooleanProperty(
			"view.status.show-line-seperator"));
		addComponent(showLineSeperator);

		
		showMemory = new JCheckBox(jEdit.getProperty(
			"options.status.show-memory"));
		showMemory.setSelected(jEdit.getBooleanProperty(
			"view.status.show-memory"));
		showMemory.addActionListener(new ActionHandler());
		addComponent(showMemory);

		
		addComponent(jEdit.getProperty("options.status.memory.foreground"),
			memForegroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.memory.foreground")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.status.memory.background"),
			memBackgroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.memory.background")),
			GridBagConstraints.VERTICAL);

		
		showClock = new JCheckBox(jEdit.getProperty(
			"options.status.show-clock"));
		showClock.setSelected(jEdit.getBooleanProperty(
			"view.status.show-clock"));
		showClock.addActionListener(new ActionHandler());
		addComponent(showClock);

		updateEnabled();
	} 

	
	protected void _save()
	{
		jEdit.setBooleanProperty("view.status.visible",
			statusVisible.isSelected());
		jEdit.setColorProperty("view.status.foreground",foregroundColor
			.getSelectedColor());
		jEdit.setColorProperty("view.status.background",backgroundColor
			.getSelectedColor());
		jEdit.setBooleanProperty("view.status.show-caret-status",
			showCaretStatus.isSelected());
		jEdit.setBooleanProperty("view.status.show-edit-mode",
			showEditMode.isSelected());
		jEdit.setBooleanProperty("view.status.show-fold-mode",
			showFoldMode.isSelected());
		jEdit.setBooleanProperty("view.status.show-encoding",
			showEncoding.isSelected());
		jEdit.setBooleanProperty("view.status.show-wrap",
			showWrap.isSelected());
		jEdit.setBooleanProperty("view.status.show-multi-select",
			showMultiSelect.isSelected());
		jEdit.setBooleanProperty("view.status.show-rect-select",
			showRectSelect.isSelected());
		jEdit.setBooleanProperty("view.status.show-overwrite",
			showOverwrite.isSelected());
		jEdit.setBooleanProperty("view.status.show-line-seperator",
			showLineSeperator.isSelected());
		jEdit.setBooleanProperty("view.status.show-memory",
			showMemory.isSelected());
		jEdit.setColorProperty("view.status.memory.foreground",memForegroundColor
			.getSelectedColor());
		jEdit.setColorProperty("view.status.memory.background",memBackgroundColor
			.getSelectedColor());
		jEdit.setBooleanProperty("view.status.show-clock",
			showClock.isSelected());
	} 

	
	private JCheckBox statusVisible;
	private ColorWellButton foregroundColor;
	private ColorWellButton backgroundColor;
	private JCheckBox showCaretStatus;
	private JCheckBox showEditMode;
	private JCheckBox showFoldMode;
	private JCheckBox showEncoding;
	private JCheckBox showWrap;
	private JCheckBox showMultiSelect;
	private JCheckBox showRectSelect;
	private JCheckBox showOverwrite;
	private JCheckBox showLineSeperator;
	private JCheckBox showMemory;
	private ColorWellButton memForegroundColor;
	private ColorWellButton memBackgroundColor;
	private JCheckBox showClock;

	private void updateEnabled()
	{
		boolean enabled = statusVisible.isSelected();
		showCaretStatus.setEnabled(enabled);
		showEditMode.setEnabled(enabled);
		showFoldMode.setEnabled(enabled);
		showEncoding.setEnabled(enabled);
		showWrap.setEnabled(enabled);
		showMultiSelect.setEnabled(enabled);
		showRectSelect.setEnabled(enabled);
		showOverwrite.setEnabled(enabled);
		showLineSeperator.setEnabled(enabled);
		showMemory.setEnabled(enabled);
		memForegroundColor.setEnabled(enabled && showMemory.isSelected());
		memBackgroundColor.setEnabled(enabled && showMemory.isSelected());
		showClock.setEnabled(enabled);
	}
	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			updateEnabled();
		}
	} 
}
