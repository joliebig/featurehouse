

package org.gjt.sp.jedit.options;


import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.*;



public class StatusBarOptionPane extends AbstractOptionPane
{
	
	public StatusBarOptionPane()
	{
		super("status");
	} 

	
	@Override
	protected void _init()
	{
		setLayout(new BorderLayout());

		
		JPanel panel = new JPanel(new GridLayout(2,1));
		showStatusbar = new JCheckBox(jEdit.getProperty(
			"options.status.visible"));
		showStatusbar.setSelected(jEdit.getBooleanProperty("view.status.visible"));
		panel.add(showStatusbar);
		showStatusbarPlain = new JCheckBox(jEdit.getProperty(
			"options.status.plainview.visible"));
		showStatusbarPlain.setSelected(jEdit.getBooleanProperty("view.status.plainview.visible"));
		panel.add(showStatusbarPlain);
		panel.add(new JLabel(jEdit.getProperty(
			"options.status.caption")));
		add(panel, BorderLayout.NORTH);
		

		
		AbstractOptionPane optionsPanel = new AbstractOptionPane("Status Options");
		
		optionsPanel.addComponent(jEdit.getProperty("options.status.foreground"),
			foregroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.foreground")),
			GridBagConstraints.VERTICAL);

		
		optionsPanel.addComponent(jEdit.getProperty("options.status.background"),
			backgroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.background")),
			GridBagConstraints.VERTICAL);

		
		optionsPanel.addComponent(jEdit.getProperty("options.status.memory.foreground"),
			memForegroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.memory.foreground")),
			GridBagConstraints.VERTICAL);

		
		optionsPanel.addComponent(jEdit.getProperty("options.status.memory.background"),
			memBackgroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.status.memory.background")),
			GridBagConstraints.VERTICAL);

		optionsPanel.addSeparator();
		optionsPanel.addComponent(new JLabel(jEdit.getProperty("options.status.caret.title", "Caret position display options:")));

		
		showCaretLineNumber = new JCheckBox(jEdit.getProperty("options.status.caret.linenumber", "Show caret line number"),
			jEdit.getBooleanProperty("view.status.show-caret-linenumber", true));
		showCaretLineNumber.setName("showCaretLineNumber");
		showCaretDot = new JCheckBox(jEdit.getProperty("options.status.caret.dot", "Show caret offset from start of line"),
			jEdit.getBooleanProperty("view.status.show-caret-dot", true));
		showCaretDot.setName("showCaretDot");
		showCaretVirtual = new JCheckBox(jEdit.getProperty("options.status.caret.virtual", "Show caret virtual offset from start of line"),
			jEdit.getBooleanProperty("view.status.show-caret-virtual", true));
		showCaretVirtual.setName("showCaretVirtual");
		showCaretOffset = new JCheckBox(jEdit.getProperty("options.status.caret.offset", "Show caret offset from start of file"),
			jEdit.getBooleanProperty("view.status.show-caret-offset", true));
		showCaretOffset.setName("showCaretOffset");
		showCaretBufferLength = new JCheckBox(jEdit.getProperty("options.status.caret.bufferlength", "Show length of file"),
			jEdit.getBooleanProperty("view.status.show-caret-bufferlength", true));
		showCaretBufferLength.setName("showCaretBufferLength");
		optionsPanel.addComponent(showCaretLineNumber);
		optionsPanel.addComponent(showCaretDot);
		optionsPanel.addComponent(showCaretVirtual);
		optionsPanel.addComponent(showCaretOffset);
		optionsPanel.addComponent(showCaretBufferLength);

		


		
		String statusbar = jEdit.getProperty("view.status");
		StringTokenizer st = new StringTokenizer(statusbar);
		listModel = new DefaultListModel();
		while (st.hasMoreTokens())
		{
			String token = st.nextToken();
			listModel.addElement(token);
		}


		list = new JList(listModel);
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.addListSelectionListener(new ListHandler());

		JPanel widgetsPanel = new JPanel(new BorderLayout());
		widgetsPanel.add(new JScrollPane(list), BorderLayout.CENTER);
		

		
		JPanel buttons = new JPanel();
		buttons.setBorder(new EmptyBorder(3,0,0,0));
		buttons.setLayout(new BoxLayout(buttons,BoxLayout.X_AXIS));
		ActionHandler actionHandler = new ActionHandler();
		add = new RolloverButton(GUIUtilities.loadIcon("Plus.png"));
		add.setToolTipText(jEdit.getProperty("options.status.add"));
		add.addActionListener(actionHandler);
		buttons.add(add);
		buttons.add(Box.createHorizontalStrut(6));
		remove = new RolloverButton(GUIUtilities.loadIcon("Minus.png"));
		remove.setToolTipText(jEdit.getProperty("options.status.remove"));
		remove.addActionListener(actionHandler);
		buttons.add(remove);
		buttons.add(Box.createHorizontalStrut(6));
		moveUp = new RolloverButton(GUIUtilities.loadIcon("ArrowU.png"));
		moveUp.setToolTipText(jEdit.getProperty("options.status.moveUp"));
		moveUp.addActionListener(actionHandler);
		buttons.add(moveUp);
		buttons.add(Box.createHorizontalStrut(6));
		moveDown = new RolloverButton(GUIUtilities.loadIcon("ArrowD.png"));
		moveDown.setToolTipText(jEdit.getProperty("options.status.moveDown"));
		moveDown.addActionListener(actionHandler);
		buttons.add(moveDown);
		buttons.add(Box.createHorizontalStrut(6));
		edit = new RolloverButton(GUIUtilities.loadIcon("ButtonProperties.png"));
		edit.setToolTipText(jEdit.getProperty("options.status.edit"));
		edit.addActionListener(actionHandler);
		buttons.add(edit);
		buttons.add(Box.createGlue());
		

		updateButtons();
		widgetsPanel.add(buttons, BorderLayout.SOUTH);


		JTabbedPane tabs = new JTabbedPane();
		tabs.addTab("Options",optionsPanel);
		tabs.add("Widgets", widgetsPanel);

		add(tabs, BorderLayout.CENTER);
	} 

	
	@Override
	protected void _save()
	{
		jEdit.setColorProperty("view.status.foreground",foregroundColor
			.getSelectedColor());
		jEdit.setColorProperty("view.status.background",backgroundColor
			.getSelectedColor());
		jEdit.setColorProperty("view.status.memory.foreground",memForegroundColor
			.getSelectedColor());
		jEdit.setColorProperty("view.status.memory.background",memBackgroundColor
			.getSelectedColor());

		jEdit.setBooleanProperty("view.status.visible",showStatusbar
			.isSelected());

		jEdit.setBooleanProperty("view.status.plainview.visible",showStatusbarPlain
			.isSelected());

		StringBuilder buf = new StringBuilder();
		for(int i = 0; i < listModel.getSize(); i++)
		{
			if(i != 0)
				buf.append(' ');

			String widgetName = (String) listModel.elementAt(i);
			buf.append(widgetName);
		}
		jEdit.setProperty("view.status",buf.toString());

		jEdit.setBooleanProperty("view.status.show-caret-linenumber", showCaretLineNumber.isSelected());
		jEdit.setBooleanProperty("view.status.show-caret-dot", showCaretDot.isSelected());
		jEdit.setBooleanProperty("view.status.show-caret-virtual", showCaretVirtual.isSelected());
		jEdit.setBooleanProperty("view.status.show-caret-offset", showCaretOffset.isSelected());
		jEdit.setBooleanProperty("view.status.show-caret-bufferlength", showCaretBufferLength.isSelected());

	} 

	

	
	private ColorWellButton foregroundColor;
	private ColorWellButton backgroundColor;
	private ColorWellButton memForegroundColor;
	private ColorWellButton memBackgroundColor;
	private JCheckBox showStatusbar;
	private JCheckBox showStatusbarPlain;
	private DefaultListModel listModel;
	private JList list;
	private RolloverButton add;
	private RolloverButton remove;
	private RolloverButton moveUp, moveDown;
	private RolloverButton edit;

	private JCheckBox showCaretLineNumber;
	private JCheckBox showCaretDot;
	private JCheckBox showCaretVirtual;
	private JCheckBox showCaretOffset;
	private JCheckBox showCaretBufferLength;
	

	
	private void updateButtons()
	{
		int index = list.getSelectedIndex();
		remove.setEnabled(index != -1 && listModel.getSize() != 0);
		moveUp.setEnabled(index > 0);
		moveDown.setEnabled(index != -1 && index != listModel.getSize() - 1);
		edit.setEnabled(index != -1);
	} 

	

	

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();

			if(source == add)
			{
				String value = selectWidget();
				if (value == null)
					return;


				int index = list.getSelectedIndex();
				if(index == -1)
					index = listModel.getSize();
				else
					index++;

				listModel.insertElementAt(value,index);
				list.setSelectedIndex(index);
				list.ensureIndexIsVisible(index);
			}
			else if(source == remove)
			{
				int index = list.getSelectedIndex();
				listModel.removeElementAt(index);
				if(listModel.getSize() != 0)
				{
					if(listModel.getSize() == index)
						list.setSelectedIndex(index-1);
					else
						list.setSelectedIndex(index);
				}
				updateButtons();
			}
			else if(source == moveUp)
			{
				int index = list.getSelectedIndex();
				Object selected = list.getSelectedValue();
				listModel.removeElementAt(index);
				listModel.insertElementAt(selected,index-1);
				list.setSelectedIndex(index-1);
				list.ensureIndexIsVisible(index-1);
			}
			else if(source == moveDown)
			{
				int index = list.getSelectedIndex();
				Object selected = list.getSelectedValue();
				listModel.removeElementAt(index);
				listModel.insertElementAt(selected,index+1);
				list.setSelectedIndex(index+1);
				list.ensureIndexIsVisible(index+1);
			}
			else if(source == edit)
			{
				String value = selectWidget();
				if (value == null)
					return;

				int index = list.getSelectedIndex();

				listModel.insertElementAt(value,index);
				list.setSelectedIndex(index);
				list.ensureIndexIsVisible(index);
			}
		}

		private String selectWidget()
		{
			WidgetSelectionDialog dialog = new WidgetSelectionDialog(StatusBarOptionPane.this);
			String value = dialog.getValue();
			if (value != null && value.length() == 0)
				value = null;
			return value;
		}
	} 

	
	private class ListHandler implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent evt)
		{
			updateButtons();
		}
	} 

	

	
	private class WidgetSelectionDialog extends EnhancedDialog
	{
		private JButton ok;
		private JButton cancel;
		private JTextField labelField;
		private JLabel labelLabel;
		private JRadioButton labelRadio;
		private JComboBox widgetCombo;
		private JLabel widgetLabel;
		private JRadioButton widgetRadio;
		private String value;

		
		WidgetSelectionDialog(Component comp)
		{
			super(GUIUtilities.getParentDialog(comp),
			      jEdit.getProperty("options.status.edit.title"),
			      true);
			ButtonGroup buttonGroup = new ButtonGroup();
			labelRadio = new JRadioButton(jEdit.getProperty("options.status.edit.labelRadioButton"));
			widgetRadio = new JRadioButton(jEdit.getProperty("options.status.edit.widgetRadioButton"));
			buttonGroup.add(labelRadio);
			buttonGroup.add(widgetRadio);

			labelLabel = new JLabel(jEdit.getProperty("options.status.edit.labelLabel"));
			labelField = new JTextField();

			widgetLabel = new JLabel(jEdit.getProperty("options.status.edit.widgetLabel"));

			String[] allWidgets = ServiceManager.getServiceNames("org.gjt.sp.jedit.gui.statusbar.StatusWidget");
			Vector<String> widgets = new Vector<String>(allWidgets.length);
			Set<String> usedWidget = new HashSet<String>(listModel.getSize());
			for (int i = 0; i < listModel.getSize(); i++)
			{
				usedWidget.add((String) listModel.get(i));
			}
			for (String widget : allWidgets)
			{
				if (!usedWidget.contains(widget))
					widgets.add(widget);
			}
			widgetCombo = new JComboBox(widgets);
			ActionHandler actionHandler = new ActionHandler();
			labelRadio.addActionListener(actionHandler);
			widgetRadio.addActionListener(actionHandler);
			
			JPanel southPanel = new JPanel();
			southPanel.setLayout(new BoxLayout(southPanel,BoxLayout.X_AXIS));
			southPanel.setBorder(new EmptyBorder(12,0,0,0));
			southPanel.add(Box.createGlue());
			ok = new JButton(jEdit.getProperty("common.ok"));
			ok.addActionListener(actionHandler);
			getRootPane().setDefaultButton(ok);
			southPanel.add(ok);
			southPanel.add(Box.createHorizontalStrut(6));
			cancel = new JButton(jEdit.getProperty("common.cancel"));
			cancel.addActionListener(actionHandler);
			southPanel.add(cancel);
			southPanel.add(Box.createGlue());
			

			labelField.setEnabled(false);
			widgetRadio.setSelected(true);


			JPanel content = new JPanel(new BorderLayout());
			content.setBorder(new EmptyBorder(12,12,12,12));
			setContentPane(content);
			JPanel center = new JPanel();
			center.setLayout(new BoxLayout(center,BoxLayout.Y_AXIS));

			center.add(labelRadio);
			JPanel p = new JPanel(new BorderLayout());
			p.add(labelLabel, BorderLayout.WEST);
			p.add(labelField);
			center.add(p);
			center.add(widgetRadio);
			p = new JPanel(new BorderLayout());
			p.add(widgetLabel, BorderLayout.WEST);
			p.add(widgetCombo);
			center.add(p);



			getContentPane().add(center, BorderLayout.CENTER);
			getContentPane().add(southPanel, BorderLayout.SOUTH);
			pack();
			setLocationRelativeTo(GUIUtilities.getParentDialog(comp));
			setVisible(true);
		} 

		
		@Override
		public void ok()
		{
			if (widgetRadio.isSelected())
			{
				value = (String) widgetCombo.getSelectedItem();
			}
			else
			{
				value = labelField.getText().trim();
			}
			dispose();
		} 

		
		@Override
		public void cancel()
		{
			value = null;
			dispose();
		} 

		
		public String getValue()
		{
			return value;
		} 

		
		private class ActionHandler implements ActionListener
		{
			
			public void actionPerformed(ActionEvent evt)
			{
				Object source = evt.getSource();
				if (source == ok)
				{
					ok();
				}
				else if (source == cancel)
				{
					cancel();
				}
				else if (source == labelRadio)
				{
					labelField.setEnabled(true);
					widgetCombo.setEnabled(false);
					validate();
				}
				else if (source == widgetRadio)
				{
					labelField.setEnabled(false);
					widgetCombo.setEnabled(true);
					validate();
				}
			} 

		} 

	} 

}

