

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.*;
import org.gjt.sp.jedit.*;


public class AbbrevEditor extends JPanel
{
	
	public AbbrevEditor()
	{
		GridBagLayout layout = new GridBagLayout();
		setLayout(layout);

		GridBagConstraints cons = new GridBagConstraints();
		cons.anchor = cons.WEST;
		cons.fill = cons.BOTH;
		cons.weightx = 0.0f;
		cons.gridx = 1;
		cons.gridy = 1;

		JLabel label = new JLabel(jEdit.getProperty("abbrev-editor.abbrev"),
			SwingConstants.RIGHT);
		label.setBorder(new EmptyBorder(0,0,0,12));
		layout.setConstraints(label,cons);
		add(label);
		cons.gridx++;
		cons.weightx = 1.0f;
		abbrev = new JTextField();
		layout.setConstraints(abbrev,cons);
		add(abbrev);

		cons.gridx = 1;
		cons.weightx = 0.0f;
		cons.gridwidth = 2;

		cons.gridy++;
		label = new JLabel(jEdit.getProperty("abbrev-editor.before"));
		label.setBorder(new EmptyBorder(6,0,3,0));
		layout.setConstraints(label,cons);
		add(label);

		cons.gridy++;
		cons.weighty = 1.0f;
		beforeCaret = new JTextArea(4,40);
		JScrollPane scroller = new JScrollPane(beforeCaret);
		layout.setConstraints(scroller,cons);
		add(scroller);

		cons.gridy++;
		cons.weighty = 0.0f;
		label = new JLabel(jEdit.getProperty("abbrev-editor.after"));
		label.setBorder(new EmptyBorder(6,0,3,0));
		layout.setConstraints(label,cons);
		add(label);

		cons.gridy++;
		cons.weighty = 1.0f;
		afterCaret = new JTextArea(4,40);
		scroller = new JScrollPane(afterCaret);
		layout.setConstraints(scroller,cons);
		add(scroller);
	} 

	
	public String getAbbrev()
	{
		return abbrev.getText();
	} 

	
	public void setAbbrev(String abbrev)
	{
		this.abbrev.setText(abbrev);
	} 

	
	public String getExpansion()
	{
		StringBuilder buf = new StringBuilder();

		String beforeCaretText = beforeCaret.getText();
		String afterCaretText = afterCaret.getText();

		for(int i = 0; i < beforeCaretText.length(); i++)
		{
			char ch = beforeCaretText.charAt(i);
			switch(ch)
			{
			case '\n':
				buf.append("\\n");
				break;
			case '\t':
				buf.append("\\t");
				break;
			case '\\':
				buf.append("\\\\");
				break;
			default:
				buf.append(ch);
				break;
			}
		}

		if(afterCaretText.length() != 0)
		{
			buf.append("\\|");

			for(int i = 0; i < afterCaretText.length(); i++)
			{
				char ch = afterCaretText.charAt(i);
				switch(ch)
				{
				case '\n':
					buf.append("\\n");
					break;
				case '\t':
					buf.append("\\t");
					break;
				case '\\':
					buf.append("\\\\");
					break;
				default:
					buf.append(ch);
					break;
				}
			}
		}

		return buf.toString();
	} 

	
	public void setExpansion(String expansion)
	{
		if(expansion == null)
		{
			beforeCaret.setText(null);
			afterCaret.setText(null);
			return;
		}

		String beforeCaretText = null;
		String afterCaretText = null;
		StringBuilder buf = new StringBuilder();

		for(int i = 0; i < expansion.length(); i++)
		{
			char ch = expansion.charAt(i);

			if(ch == '\\' && i != expansion.length() - 1)
			{
				ch = expansion.charAt(++i);
				switch(ch)
				{
				case 't':
					buf.append('\t');
					break;
				case 'n':
					buf.append('\n');
					break;
				case '|':
					beforeCaretText = buf.toString();
					buf.setLength(0);
					break;
				default:
					buf.append(ch);
					break;
				}
			}
			else
				buf.append(ch);
		}

		if(beforeCaretText == null)
			beforeCaretText = buf.toString();
		else
			afterCaretText = buf.toString();

		beforeCaret.setText(beforeCaretText);
		afterCaret.setText(afterCaretText);
	} 

	
	public JTextField getAbbrevField()
	{
		return abbrev;
	} 

	
	public JTextArea getBeforeCaretTextArea()
	{
		return beforeCaret;
	} 

	
	public JTextArea getAfterCaretTextArea()
	{
		return afterCaret;
	} 

	
	private JTextField abbrev;
	private JTextArea beforeCaret, afterCaret;
	
}
