

package org.gjt.sp.jedit.options;


import javax.swing.*;
import org.gjt.sp.jedit.gui.FontSelector;
import org.gjt.sp.jedit.*;


public class PrintOptionPane extends AbstractOptionPane
{
	
	public PrintOptionPane()
	{
		super("print");
	} 

	
	protected void _init()
	{
		
		font = new FontSelector(jEdit.getFontProperty("print.font"));
		addComponent(jEdit.getProperty("options.print.font"),font);

		
		printHeader = new JCheckBox(jEdit.getProperty("options.print"
			+ ".header"));
		printHeader.setSelected(jEdit.getBooleanProperty("print.header"));
		addComponent(printHeader);

		
		printFooter = new JCheckBox(jEdit.getProperty("options.print"
			+ ".footer"));
		printFooter.setSelected(jEdit.getBooleanProperty("print.footer"));
		addComponent(printFooter);

		
		printLineNumbers = new JCheckBox(jEdit.getProperty("options.print"
			+ ".lineNumbers"));
		printLineNumbers.setSelected(jEdit.getBooleanProperty("print.lineNumbers"));
		addComponent(printLineNumbers);

		
		color = new JCheckBox(jEdit.getProperty("options.print"
			+ ".color"));
		color.setSelected(jEdit.getBooleanProperty("print.color"));
		addComponent(color);

		
		String[] tabSizes = { "2", "4", "8" };
		tabSize = new JComboBox(tabSizes);
		tabSize.setEditable(true);
		tabSize.setSelectedItem(jEdit.getProperty("print.tabSize"));
		addComponent(jEdit.getProperty("options.print.tabSize"),tabSize);
	} 

	
	protected void _save()
	{
		jEdit.setFontProperty("print.font",font.getFont());
		jEdit.setBooleanProperty("print.header",printHeader.isSelected());
		jEdit.setBooleanProperty("print.footer",printFooter.isSelected());
		jEdit.setBooleanProperty("print.lineNumbers",printLineNumbers.isSelected());
		jEdit.setBooleanProperty("print.color",color.isSelected());
		jEdit.setProperty("print.tabSize",(String)tabSize.getSelectedItem());
	} 

	
	private FontSelector font;
	private JCheckBox printHeader;
	private JCheckBox printFooter;
	private JCheckBox printLineNumbers;
	private JCheckBox color;
	private JComboBox tabSize;
	
}
