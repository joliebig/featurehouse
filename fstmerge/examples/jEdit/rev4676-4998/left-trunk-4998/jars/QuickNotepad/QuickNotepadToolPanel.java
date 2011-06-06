

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
		setLayout(new BoxLayout(this,BoxLayout.X_AXIS));
		pad = qnpad;

		Box labelBox = new Box(BoxLayout.Y_AXIS);
		labelBox.add(Box.createGlue());

		label = new JLabel(pad.getFilename());
		label.setVisible(jEdit.getProperty(
			QuickNotepadPlugin.OPTION_PREFIX + "show-filepath").equals("true"));

		labelBox.add(label);
		labelBox.add(Box.createGlue());

		add(labelBox);

		add(Box.createGlue());

		add(makeCustomButton("quicknotepad.choose-file",
			new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					QuickNotepadToolPanel.this.pad.chooseFile();
				}
			}));
		add(makeCustomButton("quicknotepad.save-file",
			new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					QuickNotepadToolPanel.this.pad.saveFile();
				}
			}));
		add(makeCustomButton("quicknotepad.copy-to-buffer",
			new ActionListener() {
				public void actionPerformed(ActionEvent evt) {
					QuickNotepadToolPanel.this.pad.copyToBuffer();
				}
			}));
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
		AbstractButton b = new RolloverButton(GUIUtilities.loadIcon(
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
		return b;
	}

}

