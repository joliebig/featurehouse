

package org.gjt.sp.jedit.options;


import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import java.awt.*;

import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.*;

import org.gjt.sp.util.SyntaxUtilities;

public class GutterOptionPane extends AbstractOptionPane
{
	
	public GutterOptionPane()
	{
		super("gutter");
	} 

	
	public void _init()
	{
		
		gutterEnabled = new JCheckBox(jEdit.getProperty(
			"options.gutter.enabled"));
		gutterEnabled.setSelected(isGutterEnabled());
		addComponent(gutterEnabled);

		
		GridBagConstraints cons = new GridBagConstraints();
		cons.gridheight = 1;
		cons.gridwidth = GridBagConstraints.REMAINDER;
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.anchor = GridBagConstraints.WEST;
		cons.weightx = 1.0f;
		cons.ipadx = 0;
		cons.ipady = 0;
		cons.insets = new Insets(0,0,0,0);
		gutterComponents = new JPanel(new GridBagLayout());
		gutterComponents.setBorder(BorderFactory.createTitledBorder(
			jEdit.getProperty("options.gutter.optionalComponents")));

		
		lineNumbersEnabled = new JCheckBox(jEdit.getProperty(
			"options.gutter.lineNumbers"));
		lineNumbersEnabled.setSelected(jEdit.getBooleanProperty(
			"view.gutter.lineNumbers"));
		gutterComponents.add(lineNumbersEnabled, cons);

		InputVerifier integerInputVerifier = new InputVerifier()
		{
			@Override
			public boolean verify(JComponent input)
			{
				if (! (input instanceof JTextField))
					return true;
				JTextField tf = (JTextField) input;
				int i;
				try
				{
					i = Integer.valueOf(tf.getText()).intValue();
				}
				catch (Exception e)
				{
					return false;
				}
				return (i >= 0);
			}
		};
		minLineNumberDigits = new JTextField(String.valueOf(
				getMinLineNumberDigits()),1);
		minLineNumberDigits.setInputVerifier(integerInputVerifier);
		JPanel minLineNumberDigitsPanel = new JPanel();
		minLineNumberDigitsPanel.add(new JLabel(
			jEdit.getProperty("options.gutter.minLineNumberDigits")));
		minLineNumberDigitsPanel.add(minLineNumberDigits);
		cons.gridy = 1;
		gutterComponents.add(minLineNumberDigitsPanel, cons);

		
		selectionAreaEnabled = new JCheckBox(jEdit.getProperty(
			"options.gutter.selectionAreaEnabled"));
		selectionAreaEnabled.setSelected(isSelectionAreaEnabled());
		cons.gridy = 2;
		gutterComponents.add(selectionAreaEnabled, cons);

		addComponent(gutterComponents);
		
		setGutterComponentsEnabledState();
		gutterEnabled.addChangeListener(new ChangeListener()
		{
			public void stateChanged(ChangeEvent e)
			{
				setGutterComponentsEnabledState();
			}
		});

		
		addComponent(jEdit.getProperty("options.gutter.selectionAreaBgColor"),
			selectionAreaBgColor = new ColorWellButton(
				getSelectionAreaBackground()), GridBagConstraints.VERTICAL);

		
		selectionAreaWidth = new JTextField(String.valueOf(
			getSelectionAreaWidth()),DEFAULT_SELECTION_GUTTER_WIDTH);
		selectionAreaWidth.setInputVerifier(integerInputVerifier);
		addComponent(jEdit.getProperty("options.gutter.selectionAreaWidth"),
			selectionAreaWidth);

		
		gutterFont = new FontSelector(
			jEdit.getFontProperty("view.gutter.font",
			new Font("Monospaced",Font.PLAIN,10)));

		addComponent(jEdit.getProperty("options.gutter.font"),gutterFont);

		
		addComponent(jEdit.getProperty("options.gutter.foreground"),
			gutterForeground = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.fgColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.gutter.background"),
			gutterBackground = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.bgColor")),
			GridBagConstraints.VERTICAL);

		
		

		
		

		
		gutterCurrentLineHighlightEnabled = new JCheckBox(jEdit.getProperty(
			"options.gutter.currentLineHighlight"));
		gutterCurrentLineHighlightEnabled.setSelected(jEdit.getBooleanProperty(
			"view.gutter.highlightCurrentLine"));
		addComponent(gutterCurrentLineHighlightEnabled,
			gutterCurrentLineHighlight = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.currentLineColor")),
			GridBagConstraints.VERTICAL);

		
		gutterHighlightInterval = new JTextField(jEdit.getProperty(
			"view.gutter.highlightInterval"),3);

		Box gutterHighlightBox = new Box(BoxLayout.X_AXIS);
		gutterHighlightBox.add(new JLabel(jEdit.getProperty(
			"options.gutter.interval-1")));
		gutterHighlightBox.add(Box.createHorizontalStrut(3));
		gutterHighlightBox.add(gutterHighlightInterval);
		gutterHighlightBox.add(Box.createHorizontalStrut(3));
		gutterHighlightBox.add(new JLabel(jEdit.getProperty(
			"options.gutter.interval-2")));
		gutterHighlightBox.add(Box.createHorizontalStrut(12));

		addComponent(gutterHighlightBox,gutterHighlightColor
			= new ColorWellButton(jEdit.getColorProperty(
			"view.gutter.highlightColor")),
			GridBagConstraints.VERTICAL);

		
		gutterStructureHighlightEnabled = new JCheckBox(jEdit.getProperty(
			"options.gutter.structureHighlight"));
		gutterStructureHighlightEnabled.setSelected(jEdit.getBooleanProperty(
			"view.gutter.structureHighlight"));
		addComponent(gutterStructureHighlightEnabled,
			gutterStructureHighlight = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.structureHighlightColor")),
			GridBagConstraints.VERTICAL);

		
		gutterMarkerHighlightEnabled = new JCheckBox(jEdit.getProperty(
			"options.gutter.markerHighlight"));
		gutterMarkerHighlightEnabled.setSelected(jEdit.getBooleanProperty(
			"view.gutter.markerHighlight"));
		addComponent(gutterMarkerHighlightEnabled,
			gutterMarkerHighlight = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.markerColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.gutter.foldColor"),
			gutterFoldMarkers = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.foldColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.gutter.focusBorderColor"),
			gutterFocusBorder = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.focusBorderColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.gutter.noFocusBorderColor"),
			gutterNoFocusBorder = new ColorWellButton(
			jEdit.getColorProperty("view.gutter.noFocusBorderColor")),
			GridBagConstraints.VERTICAL);
		
		addFoldStyleChooser();
	} 

	
	public void _save()
	{
		jEdit.setBooleanProperty("view.gutter.lineNumbers", lineNumbersEnabled
			.isSelected());
		jEdit.setIntegerProperty("view.gutter.minDigitCount",
			Integer.valueOf(minLineNumberDigits.getText()));

		jEdit.setFontProperty("view.gutter.font",gutterFont.getFont());
		jEdit.setColorProperty("view.gutter.fgColor",gutterForeground
			.getSelectedColor());
		jEdit.setColorProperty("view.gutter.bgColor",gutterBackground
			.getSelectedColor());

		

		jEdit.setBooleanProperty("view.gutter.highlightCurrentLine",
			gutterCurrentLineHighlightEnabled.isSelected());
		jEdit.setColorProperty("view.gutter.currentLineColor",
			gutterCurrentLineHighlight.getSelectedColor());
		jEdit.setProperty("view.gutter.highlightInterval",
			gutterHighlightInterval.getText());
		jEdit.setColorProperty("view.gutter.highlightColor",
			gutterHighlightColor.getSelectedColor());

		jEdit.setBooleanProperty("view.gutter.structureHighlight",
			gutterStructureHighlightEnabled.isSelected());
		jEdit.setColorProperty("view.gutter.structureHighlightColor",
			gutterStructureHighlight.getSelectedColor());
		jEdit.setBooleanProperty("view.gutter.markerHighlight",
			gutterMarkerHighlightEnabled.isSelected());
		jEdit.setColorProperty("view.gutter.markerColor",
			gutterMarkerHighlight.getSelectedColor());
		jEdit.setColorProperty("view.gutter.foldColor",
			gutterFoldMarkers.getSelectedColor());
		jEdit.setProperty(JEditTextArea.FOLD_PAINTER_PROPERTY,
			painters[foldPainter.getSelectedIndex()]);
		jEdit.setColorProperty("view.gutter.focusBorderColor",
			gutterFocusBorder.getSelectedColor());
		jEdit.setColorProperty("view.gutter.noFocusBorderColor",
			gutterNoFocusBorder.getSelectedColor());
		jEdit.setBooleanProperty(GUTTER_ENABLED_PROPERTY,
			gutterEnabled.isSelected());
		jEdit.setBooleanProperty(SELECTION_AREA_ENABLED_PROPERTY,
			selectionAreaEnabled.isSelected());
		jEdit.setColorProperty(SELECTION_AREA_BGCOLOR_PROPERTY,
			selectionAreaBgColor.getSelectedColor());
		jEdit.setIntegerProperty("view.gutter.selectionAreaWidth",
			Integer.valueOf(selectionAreaWidth.getText()));
	} 

	
	private void setGutterComponentsEnabledState()
	{
		GUIUtilities.setEnabledRecursively(gutterComponents,
			gutterEnabled.isSelected());
	} 

	
	private void addFoldStyleChooser()
	{
		painters = ServiceManager.getServiceNames(JEditTextArea.FOLD_PAINTER_SERVICE);
		foldPainter = new JComboBox();
		String current = JEditTextArea.getFoldPainterName();
		int selected = 0;
		for (int i = 0; i < painters.length; i++)
		{
			String painter = painters[i];
			foldPainter.addItem(jEdit.getProperty(
				"options.gutter.foldStyleNames." + painter, painter));
			if (painter.equals(current))
				selected = i;
		}
		foldPainter.setSelectedIndex(selected);
		addComponent(new JLabel(jEdit.getProperty("options.gutter.foldStyle.label")), foldPainter);
	} 

	
	public static boolean isGutterEnabled()
	{
		return jEdit.getBooleanProperty(GUTTER_ENABLED_PROPERTY);
	} 

	
	public static int getMinLineNumberDigits()
	{
		int n = jEdit.getIntegerProperty("view.gutter.minDigitCount", 2);
		if (n < 0)
			n = 2;
		return n;
	} 

	
	public static boolean isSelectionAreaEnabled()
	{
		return jEdit.getBooleanProperty(SELECTION_AREA_ENABLED_PROPERTY);
	} 

	
	public static Color getSelectionAreaBackground()
	{
		String color = jEdit.getProperty(SELECTION_AREA_BGCOLOR_PROPERTY);
		if (color == null)
			return jEdit.getColorProperty("view.gutter.bgColor");
		return SyntaxUtilities.parseColor(color, Color.black);
	} 

	
	public static int getSelectionAreaWidth()
	{
		int n = jEdit.getIntegerProperty("view.gutter.selectionAreaWidth",
			DEFAULT_SELECTION_GUTTER_WIDTH);
		if (n < 0)
			n = DEFAULT_SELECTION_GUTTER_WIDTH;
		return n;
	} 

	
	private static final String GUTTER_ENABLED_PROPERTY =
		"view.gutter.enabled";
	private static final String SELECTION_AREA_ENABLED_PROPERTY =
		"view.gutter.selectionAreaEnabled";
	private static final String SELECTION_AREA_BGCOLOR_PROPERTY =
		"view.gutter.selectionAreaBgColor";
	private static final int DEFAULT_SELECTION_GUTTER_WIDTH = 12;

	private FontSelector gutterFont;
	private ColorWellButton gutterForeground;
	private ColorWellButton gutterBackground;
	private JTextField gutterHighlightInterval;
	private ColorWellButton gutterHighlightColor;
	private JCheckBox lineNumbersEnabled;
	private JCheckBox gutterCurrentLineHighlightEnabled;
	private ColorWellButton gutterCurrentLineHighlight;
	private JCheckBox gutterStructureHighlightEnabled;
	private ColorWellButton gutterStructureHighlight;
	private JCheckBox gutterMarkerHighlightEnabled;
	private ColorWellButton gutterMarkerHighlight;
	private ColorWellButton gutterFoldMarkers;
	private JComboBox foldPainter;
	private ColorWellButton gutterFocusBorder;
	private ColorWellButton gutterNoFocusBorder;
	private String [] painters;
	private JCheckBox gutterEnabled;
	private JPanel gutterComponents;
	private JTextField minLineNumberDigits;
	private JCheckBox selectionAreaEnabled;
	private ColorWellButton selectionAreaBgColor;
	private JTextField selectionAreaWidth;
	
}
