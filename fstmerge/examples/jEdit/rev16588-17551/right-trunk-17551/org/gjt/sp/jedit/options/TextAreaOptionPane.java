

package org.gjt.sp.jedit.options;


import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.*;
import org.gjt.sp.jedit.textarea.AntiAlias;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.gui.FontSelector;
import org.gjt.sp.jedit.gui.FontSelectorDialog;
import org.gjt.sp.jedit.gui.ColorWellButton;
import org.gjt.sp.jedit.gui.RolloverButton;



public class TextAreaOptionPane extends AbstractOptionPane
{
	
	public TextAreaOptionPane()
	{
		super("textarea");
	} 

	
	public void _init()
	{
		
		font = new FontSelector(jEdit.getFontProperty("view.font"));

		addComponent(jEdit.getProperty("options.textarea.font"),font);

		fontSubst = new JCheckBox(jEdit.getProperty("options.textarea.fontsubst"));
		fontSubst.setToolTipText(jEdit.getProperty("options.textarea.fontsubst.tooltip"));
		fontSubst.setSelected(jEdit.getBooleanProperty("view.enableFontSubst"));
		fontSubst.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					fontSubstList.setVisible(fontSubst.isSelected());
				}
			});
		addComponent(fontSubst);

		fontSubstList = new FontList();
		fontSubstList.setVisible(fontSubst.isSelected());
		addComponent(fontSubstList, GridBagConstraints.HORIZONTAL);

		
		addComponent(jEdit.getProperty("options.textarea.foreground"),
			foregroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.fgColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.textarea.background"),
			backgroundColor = new ColorWellButton(
			jEdit.getColorProperty("view.bgColor")),
			GridBagConstraints.VERTICAL);

		
		blinkCaret = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".blinkCaret"));
		blinkCaret.setSelected(jEdit.getBooleanProperty("view.caretBlink"));

		blockCaret = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".blockCaret"));
		blockCaret.setSelected(jEdit.getBooleanProperty("view.blockCaret"));

		thickCaret = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".thickCaret"));
		thickCaret.setSelected(jEdit.getBooleanProperty("view.thickCaret"));

		Box caretSettings = new Box(BoxLayout.X_AXIS);
		caretSettings.add(new JLabel(jEdit.getProperty(
			"options.textarea.caret")));
		caretSettings.add(Box.createHorizontalStrut(6));
		caretSettings.add(blinkCaret);
		caretSettings.add(blockCaret);
		caretSettings.add(thickCaret);

		addComponent(caretSettings,caretColor = new ColorWellButton(
			jEdit.getColorProperty("view.caretColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.textarea.selection"),
			selectionColor = new ColorWellButton(
			jEdit.getColorProperty("view.selectionColor")),
			GridBagConstraints.VERTICAL);

		
		addComponent(jEdit.getProperty("options.textarea.multipleSelection"),
			multipleSelectionColor = new ColorWellButton(
			jEdit.getColorProperty("view.multipleSelectionColor")),
			GridBagConstraints.VERTICAL);

		
		selectionFg = new JCheckBox(jEdit.getProperty("options.textarea.selectionFg"));
		selectionFg.setName("selectionFg");
		selectionFg.setSelected(jEdit.getBooleanProperty("view.selectionFg"));
		addComponent(selectionFg, selectionFgColor = new ColorWellButton(
			jEdit.getColorProperty("view.selectionFgColor")),
			GridBagConstraints.VERTICAL);

		
		lineHighlight = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".lineHighlight"));
		lineHighlight.setSelected(jEdit.getBooleanProperty("view.lineHighlight"));
		addComponent(lineHighlight,lineHighlightColor = new ColorWellButton(
			jEdit.getColorProperty("view.lineHighlightColor")),
			GridBagConstraints.VERTICAL);

		
		structureHighlight = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".structureHighlight"));
		structureHighlight.setSelected(jEdit.getBooleanProperty(
			"view.structureHighlight"));
		addComponent(structureHighlight,structureHighlightColor = new ColorWellButton(
			jEdit.getColorProperty("view.structureHighlightColor")),
			GridBagConstraints.VERTICAL);

		
		eolMarkers = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".eolMarkers"));
		eolMarkers.setSelected(jEdit.getBooleanProperty("view.eolMarkers"));
		addComponent(eolMarkers,eolMarkerColor =new ColorWellButton(
			jEdit.getColorProperty("view.eolMarkerColor")),
			GridBagConstraints.VERTICAL);

		
		wrapGuide = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".wrapGuide"));
		wrapGuide.setSelected(jEdit.getBooleanProperty("view.wrapGuide"));
		addComponent(wrapGuide,wrapGuideColor = new ColorWellButton(
			jEdit.getColorProperty("view.wrapGuideColor")),
			GridBagConstraints.VERTICAL);

		
		electricBorders = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".electricBorders"));
		electricBorders.setSelected(!"0".equals(jEdit.getProperty(
			"view.electricBorders")));
		addComponent(electricBorders);

		

		antiAlias = new JComboBox(AntiAlias.comboChoices);
		antiAlias.setToolTipText(jEdit.getProperty("options.textarea.antiAlias.tooltip"));
		AntiAlias antiAliasValue = new AntiAlias(jEdit.getProperty("view.antiAlias"));
		font.setAntiAliasEnabled(antiAliasValue.val()>0);
		antiAlias.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					int idx = antiAlias.getSelectedIndex();
					font.setAntiAliasEnabled(idx > 0);
					font.repaint();
				}
			});
		antiAlias.setSelectedIndex(antiAliasValue.val());
		addComponent(jEdit.getProperty("options.textarea"+ ".antiAlias"), antiAlias);

		
		fracFontMetrics = new JCheckBox(jEdit.getProperty("options.textarea"
			+ ".fracFontMetrics"));
		fracFontMetrics.setToolTipText(jEdit.getProperty("options.textarea.fracFontMetrics.tooltip"));
		fracFontMetrics.setSelected(jEdit.getBooleanProperty("view.fracFontMetrics"));
		addComponent(fracFontMetrics);

		
		stripTrailingEOL = new JCheckBox(jEdit.getProperty(
			"options.textArea.stripTrailingEOL"));
		stripTrailingEOL.setSelected(jEdit.getBooleanProperty("stripTrailingEOL"));
		addComponent(stripTrailingEOL);

		completeFromAllBuffers = new JCheckBox(jEdit.getProperty(
			"options.textArea.completeFromAllBuffers"));
		completeFromAllBuffers.setSelected(jEdit.getBooleanProperty("completeFromAllBuffers"));
		addComponent(completeFromAllBuffers);

	} 

	
	public void _save()
	{
		jEdit.setFontProperty("view.font",font.getFont());
		jEdit.setBooleanProperty("view.enableFontSubst",fontSubst.isSelected());
		fontSubstList.save();

		jEdit.setColorProperty("view.fgColor",foregroundColor
			.getSelectedColor());
		jEdit.setColorProperty("view.bgColor",backgroundColor
			.getSelectedColor());
		jEdit.setBooleanProperty("view.caretBlink",blinkCaret.isSelected());
		jEdit.setBooleanProperty("view.blockCaret",blockCaret.isSelected());
		jEdit.setBooleanProperty("view.thickCaret",thickCaret.isSelected());
		jEdit.setColorProperty("view.caretColor",caretColor
			.getSelectedColor());
		jEdit.setColorProperty("view.selectionColor",selectionColor
			.getSelectedColor());
		jEdit.setColorProperty("view.multipleSelectionColor",multipleSelectionColor
			.getSelectedColor());
		jEdit.setBooleanProperty("view.selectionFg",selectionFg.isSelected());
		jEdit.setColorProperty("view.selectionFgColor",selectionFgColor
				.getSelectedColor());
		jEdit.setBooleanProperty("view.lineHighlight",lineHighlight
			.isSelected());
		jEdit.setColorProperty("view.lineHighlightColor",
			lineHighlightColor.getSelectedColor());
		jEdit.setBooleanProperty("view.structureHighlight",structureHighlight
			.isSelected());
		jEdit.setColorProperty("view.structureHighlightColor",
			structureHighlightColor.getSelectedColor());
		jEdit.setBooleanProperty("view.eolMarkers",eolMarkers
			.isSelected());
		jEdit.setColorProperty("view.eolMarkerColor",
			eolMarkerColor.getSelectedColor());
		jEdit.setBooleanProperty("view.wrapGuide",wrapGuide
			.isSelected());
		jEdit.setColorProperty("view.wrapGuideColor",
			wrapGuideColor.getSelectedColor());
		jEdit.setIntegerProperty("view.electricBorders",electricBorders
			.isSelected() ? 3 : 0);
		AntiAlias nv = new AntiAlias(jEdit.getProperty("view.antiAlias"));
		nv.set(antiAlias.getSelectedIndex());
		jEdit.setProperty("view.antiAlias", nv.toString());
		jEdit.setBooleanProperty("view.fracFontMetrics",fracFontMetrics.isSelected());
		jEdit.setBooleanProperty("stripTrailingEOL", stripTrailingEOL.isSelected());
		jEdit.setBooleanProperty("completeFromAllBuffers", completeFromAllBuffers.isSelected());
	} 

	
	private FontSelector font;
	private JCheckBox fontSubst;
	private FontList fontSubstList;
	private ColorWellButton foregroundColor;
	private ColorWellButton backgroundColor;
	private JCheckBox blinkCaret;
	private JCheckBox blockCaret;
	private JCheckBox thickCaret;
	private ColorWellButton caretColor;
	private ColorWellButton selectionColor;
	private JCheckBox selectionFg;
	private ColorWellButton selectionFgColor;
	private ColorWellButton multipleSelectionColor;
	private JCheckBox lineHighlight;
	private ColorWellButton lineHighlightColor;
	private JCheckBox structureHighlight;
	private ColorWellButton structureHighlightColor;
	private JCheckBox eolMarkers;
	private ColorWellButton eolMarkerColor;
	private JCheckBox wrapGuide;
	private ColorWellButton wrapGuideColor;
	private JCheckBox electricBorders;
	
	private JComboBox antiAlias;
	private JCheckBox fracFontMetrics;
	private JCheckBox stripTrailingEOL;
	private JCheckBox completeFromAllBuffers;
	

	
	
	private static class FontList
		extends JPanel
		implements ActionListener
	{

		public FontList()
		{
			int i = 0;

			setLayout(new BorderLayout());

			
			JLabel l = new JLabel(jEdit.getProperty("options.textarea.fontsubstlist"));

			
			Font f;
			fontsModel = new DefaultListModel();
			fonts = new JList(fontsModel);
			fonts.setCellRenderer(new FontItemRenderer());
			while ((f = jEdit.getFontProperty("view.fontSubstList." + i)) != null)
			{
				fontsModel.addElement(f);
				i++;
			}

			
			Box buttons = new Box(BoxLayout.Y_AXIS);

			add = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.context.add.icon")));
			add.setToolTipText(jEdit.getProperty("common.add"));
			add.addActionListener(this);
			buttons.add(add);
			buttons.add(Box.createVerticalStrut(2));

			remove = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.context.remove.icon")));
			remove.setToolTipText(jEdit.getProperty("common.remove"));
			remove.addActionListener(this);
			buttons.add(remove);
			buttons.add(Box.createVerticalStrut(2));

			up = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.context.moveUp.icon")));
			up.setToolTipText(jEdit.getProperty("common.moveUp"));
			up.addActionListener(this);
			buttons.add(up);
			buttons.add(Box.createVerticalStrut(2));

			down = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.context.moveDown.icon")));
			down.setToolTipText(jEdit.getProperty("common.moveDown"));
			down.addActionListener(this);
			buttons.add(down);
			buttons.add(Box.createGlue());


			add(BorderLayout.NORTH, l);
			add(BorderLayout.CENTER, new JScrollPane(fonts));
			add(BorderLayout.EAST, buttons);
		}


		public void actionPerformed(ActionEvent ae)
		{
			if (ae.getSource() == add)
			{
				JDialog parent = GUIUtilities.getParentDialog(this);
				Font selected =
					new FontSelectorDialog(parent, null).getSelectedFont();

				if (selected != null)
				{
					selected = selected.deriveFont(Font.PLAIN, 12);
					fontsModel.addElement(selected);
					fonts.setSelectedIndex(fontsModel.size() - 1);
				}
			}
			else if (ae.getSource() ==  remove)
			{
				int idx = fonts.getSelectedIndex();
				if (idx != -1)
					fontsModel.removeElementAt(idx);
			}
			else if (ae.getSource() == up)
			{
				int idx = fonts.getSelectedIndex();
				if (idx > 0)
				{
					Object o = fontsModel.getElementAt(idx);
					fontsModel.removeElementAt(idx);
					fontsModel.add(idx - 1, o);
					fonts.setSelectedIndex(idx - 1);
				}
			}
			else if (ae.getSource() == down)
			{
				int idx = fonts.getSelectedIndex();
				if (idx != -1 && idx < fontsModel.size() - 1)
				{
					Object o = fontsModel.getElementAt(idx);
					fontsModel.removeElementAt(idx);
					fontsModel.add(idx + 1, o);
					fonts.setSelectedIndex(idx + 1);
				}
			}
		}


		public void save()
		{
			Font f;
			int i = 0;
			while ((f = jEdit.getFontProperty("view.fontSubstList." + i)) != null)
			{
				jEdit.unsetProperty("view.fontSubstList." + i);
				i++;
			}
			for (i = 0; i < fontsModel.size(); i++)
			{
				f = (Font) fontsModel.getElementAt(i);
				jEdit.setFontProperty("view.fontSubstList." + i, f);
			}
		}


		private DefaultListModel fontsModel;
		private JList fonts;
		private JButton add;
		private JButton remove;
		private JButton up;
		private JButton down;


		private static class FontItemRenderer extends DefaultListCellRenderer
		{

			public Component getListCellRendererComponent(JList list,
								      Object value,
								      int index,
								      boolean isSelected,
								      boolean cellHasFocus)
			{
				Font f = (Font) value;
				super.getListCellRendererComponent(list,
								   value,
								   index,
								   isSelected,
								   cellHasFocus);
				setText(f.getFamily());
				return this;
			}

		}

	} 

}
