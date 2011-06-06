

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.util.Log;

public class QuickNotepadToolPanel extends JPanel
{
	private QuickNotepad pad;
	private JLabel label;

	public QuickNotepadToolPanel(QuickNotepad qnpad)
	{
		pad = qnpad;
		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);

		toolBar.putClientProperty("JToolBar.isRollover",Boolean.TRUE);

		toolBar.add(makeCustomButton("quicknotepad.choose-file",
			new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					QuickNotepadToolPanel.this.pad.chooseFile();
				}
			}));
		toolBar.add(makeCustomButton("quicknotepad.save-file",
			new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					QuickNotepadToolPanel.this.pad.saveFile();
				}
			}));
		toolBar.add(makeCustomButton("quicknotepad.copy-to-buffer",
			new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					QuickNotepadToolPanel.this.pad.copyToBuffer();
				}
			}));
		label = new JLabel(pad.getFilename(), SwingConstants.RIGHT);
		label.setForeground(Color.black);
		label.setVisible(jEdit.getProperty(
			QuickNotepadPlugin.OPTION_PREFIX + "show-filepath").equals("true"));
		this.setLayout(new BorderLayout(10, 0));
		this.add(BorderLayout.WEST, toolBar);
		this.add(BorderLayout.CENTER, label);
		this.setBorder(BorderFactory.createEmptyBorder(0, 0, 3, 10));
	}


	void propertiesChanged()
	{
		label.setText(pad.getFilename());
		label.setVisible(jEdit.getProperty(
			QuickNotepadPlugin.OPTION_PREFIX + "show-filepath").equals("true"));
	}

	private AbstractButton makeCustomButton(String name, ActionListener listener)
	{
		String toolTip = jEdit.getProperty(name.concat(".label"));
		AbstractButton b = new JButton(GUIUtilities.loadIcon(
			jEdit.getProperty(name + ".icon")));
		if(listener != null)
		{
			b.addActionListener(listener);
			b.setEnabled(true);
		}
		else
		{
			b.setEnabled(false);
		}
		b.setToolTipText(toolTip);
		b.setMargin(new Insets(0,0,0,0));
		b.setAlignmentY(0.0f);
		b.setRequestFocusEnabled(false);
		return b;
	}

}

