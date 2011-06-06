

package org.gjt.sp.jedit.options;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class MouseOptionPane extends AbstractOptionPane
{
	
	public MouseOptionPane()
	{
		super("mouse");
	} 

	
	protected void _init()
	{
		
		dragAndDrop = new JCheckBox(jEdit.getProperty(
			"options.mouse.dragAndDrop"));
		dragAndDrop.setSelected(jEdit.getBooleanProperty(
			"view.dragAndDrop"));
		addComponent(dragAndDrop);

		
		joinNonWordChars = new JCheckBox(jEdit.getProperty(
			"options.mouse.joinNonWordChars"));
		joinNonWordChars.setSelected(jEdit.getBooleanProperty(
			"view.joinNonWordChars"));
		addComponent(joinNonWordChars);

		
		middleMousePaste = new JCheckBox(jEdit.getProperty("options.mouse"
			+ ".middleMousePaste"));
		middleMousePaste.setSelected(jEdit.getBooleanProperty(
			"view.middleMousePaste"));
		addComponent(middleMousePaste);

		
		int c = clickActionKeys.length;
		String[] clickActionNames = new String[c];
		for(int i = 0; i < c; i++)
		{
			clickActionNames[i] = jEdit.getProperty(
				"options.mouse.gutter."+clickActionKeys[i]);
		}

		c = clickModifierKeys.length;
		String[] clickModifierNames = new String[c];
		for(int i = 0; i < c; i++)
		{
			clickModifierNames[i] = jEdit.getProperty(
				"options.mouse.gutter."+clickModifierKeys[i]);
		}

		gutterClickActions = new JComboBox[c];

		for(int i = 0; i < c; i++)
		{
			JComboBox cb = new JComboBox(clickActionNames);
			gutterClickActions[i] = cb;

			String val = jEdit.getProperty("view.gutter."+clickModifierKeys[i]);
			for(int j = 0; j < clickActionKeys.length; j++)
			{
				if(val.equals(clickActionKeys[j]))
				{
					cb.setSelectedIndex(j);
				}
			}

			addComponent(clickModifierNames[i],cb);
		}
	} 

	
	public void _save()
	{
		jEdit.setBooleanProperty("view.dragAndDrop",dragAndDrop.isSelected());
		jEdit.setBooleanProperty("view.joinNonWordChars",joinNonWordChars.isSelected());
		jEdit.setBooleanProperty("view.middleMousePaste",
			middleMousePaste.isSelected());

		int c = clickModifierKeys.length;
		for(int i = 0; i < c; i++)
		{
			int idx = gutterClickActions[i].getSelectedIndex();
			jEdit.setProperty("view.gutter."+clickModifierKeys[i],
				clickActionKeys[idx]);
		}
	} 

	
	private JCheckBox dragAndDrop;
	private JCheckBox middleMousePaste;
	private JCheckBox joinNonWordChars;

	private JComboBox[] gutterClickActions;

	
	private static final String[] clickActionKeys = new String[] {
		"toggle-fold",
		"toggle-fold-fully"
	};
	
	private static final String[] clickModifierKeys = new String[] {
		"foldClick",
		"SfoldClick"
	}; 
}
