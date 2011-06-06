

package org.gjt.sp.jedit.options;


import javax.swing.border.EmptyBorder;
import javax.swing.table.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.Vector;
import java.util.Collections;

import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.gui.StyleEditor;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.StandardUtilities;




public class SyntaxHiliteOptionPane extends AbstractOptionPane
{
	public static final EmptyBorder noFocusBorder = new EmptyBorder(1,1,1,1);

	
	public SyntaxHiliteOptionPane()
	{
		super("syntax");
	}
	

	

	
	@Override
	protected void _init()
	{
		setLayout(new BorderLayout(6,6));

		add(BorderLayout.CENTER,createStyleTableScroller());
	} 

	
	@Override
	protected void _save()
	{
		styleModel.save();
	} 

	

	
	private StyleTableModel styleModel;
	private JTable styleTable;

	
	private JScrollPane createStyleTableScroller()
	{
		styleModel = createStyleTableModel();
		styleTable = new JTable(styleModel);
		styleTable.setRowSelectionAllowed(false);
		styleTable.setColumnSelectionAllowed(false);
		styleTable.setCellSelectionEnabled(false);
		styleTable.getTableHeader().setReorderingAllowed(false);
		styleTable.addMouseListener(new MouseHandler());
		TableColumnModel tcm = styleTable.getColumnModel();
 		TableColumn styleColumn = tcm.getColumn(1);
		styleColumn.setCellRenderer(new StyleTableModel.StyleRenderer());
		Dimension d = styleTable.getPreferredSize();
		d.height = Math.min(d.height,100);
		JScrollPane scroller = new JScrollPane(styleTable);
		scroller.setPreferredSize(d);
		return scroller;
	} 

	
	private static StyleTableModel createStyleTableModel()
	{
		return new StyleTableModel();
	} 

	

	
	private class MouseHandler extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent evt)
		{
			int row = styleTable.rowAtPoint(evt.getPoint());
			if(row == -1)
				return;

			SyntaxStyle style;
			SyntaxStyle current = (SyntaxStyle)styleModel.getValueAt(row,1);
			String token = (String) styleModel.getValueAt(row, 0);
			JDialog dialog = GUIUtilities.getParentDialog(
					SyntaxHiliteOptionPane.this);
			if (dialog != null)
				style = new StyleEditor(dialog, current, token).getStyle();
			else
			{
				View view = GUIUtilities.getView(SyntaxHiliteOptionPane.this);
				style = new StyleEditor(view, current, token).getStyle();
			}
			if(style != null)
				styleModel.setValueAt(style,row,1);
		}
	} 

	
	private static class StyleTableModel extends AbstractTableModel
	{
		private final java.util.List<StyleChoice> styleChoices;

		
		StyleTableModel()
		{
			styleChoices = new Vector<StyleChoice>(Token.ID_COUNT + 4);
			
			for(int i = 1; i < Token.ID_COUNT; i++)
			{
				String tokenName = Token.tokenToString((byte)i);
				addStyleChoice(tokenName,"view.style." + tokenName.toLowerCase());
			}

			addStyleChoice(jEdit.getProperty("options.syntax.foldLine.1"),
			               "view.style.foldLine.1");
			addStyleChoice(jEdit.getProperty("options.syntax.foldLine.2"),
			               "view.style.foldLine.2");
			addStyleChoice(jEdit.getProperty("options.syntax.foldLine.3"),
			               "view.style.foldLine.3");
			addStyleChoice(jEdit.getProperty("options.syntax.foldLine.0"),
			               "view.style.foldLine.0");

			Collections.sort(styleChoices,new StandardUtilities.StringCompare<StyleChoice>(true));
		} 

		
		public int getColumnCount()
		{
			return 2;
		} 

		
		public int getRowCount()
		{
			return styleChoices.size();
		} 

		
		public Object getValueAt(int row, int col)
		{
			StyleChoice ch = styleChoices.get(row);
			switch(col)
			{
				case 0:
					return ch.label;
				case 1:
					return ch.style;
				default:
					return null;
			}
		} 

		
		@Override
		public void setValueAt(Object value, int row, int col)
		{
			StyleChoice ch = styleChoices.get(row);
			if(col == 1)
				ch.style = (SyntaxStyle)value;
			fireTableRowsUpdated(row,row);
		} 

		
		@Override
		public String getColumnName(int index)
		{
			switch(index)
			{
				case 0:
					return jEdit.getProperty("options.syntax.object");
				case 1:
					return jEdit.getProperty("options.syntax.style");
				default:
					return null;
			}
		} 

		
		public void save()
		{
			for(int i = 0; i < styleChoices.size(); i++)
			{
				StyleChoice ch = styleChoices
					.get(i);
				jEdit.setProperty(ch.property,
				                  GUIUtilities.getStyleString(ch.style));
			}
		} 

		
		private void addStyleChoice(String label, String property)
		{
			styleChoices.add(new StyleChoice(label,
			                                 property,
			                                 GUIUtilities.parseStyle(jEdit.getProperty(property),
			                                                         "Dialog",12)));
		} 

		
		private static class StyleChoice
		{
			private String label;
			private String property;
			private SyntaxStyle style;

			StyleChoice(String label, String property, SyntaxStyle style)
			{
				this.label = label;
				this.property = property;
				this.style = style;
			}

			
			@Override
			public String toString()
			{
				return label;
			}
		} 

		
		static class StyleRenderer extends JLabel
			implements TableCellRenderer
		{
			
			StyleRenderer()
			{
				setOpaque(true);
				setBorder(SyntaxHiliteOptionPane.noFocusBorder);
				setText("Hello World");
			} 

			
			public Component getTableCellRendererComponent(
				JTable table,
				Object value,
				boolean isSelected,
				boolean cellHasFocus,
				int row,
				int col)
			{
				if (value != null)
				{
					SyntaxStyle style = (SyntaxStyle)value;
					setForeground(style.getForegroundColor());
					if (style.getBackgroundColor() != null)
						setBackground(style.getBackgroundColor());
					else
					{
						
						setBackground(jEdit.getColorProperty(
							"view.bgColor"));
					}
					setFont(style.getFont());
				}

				setBorder(cellHasFocus ? UIManager.getBorder(
					"Table.focusCellHighlightBorder")
				                       : SyntaxHiliteOptionPane.noFocusBorder);
				return this;
			} 
		} 
	} 

} 

