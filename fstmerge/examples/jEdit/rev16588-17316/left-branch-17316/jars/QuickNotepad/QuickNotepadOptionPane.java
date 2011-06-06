

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.FontSelector;

public class QuickNotepadOptionPane extends AbstractOptionPane implements
		ActionListener {
	private JCheckBox showPath;

	private JTextField pathName;

	private FontSelector font;

	public QuickNotepadOptionPane() {
		super(QuickNotepadPlugin.NAME);
	}

	public void _init() {
		showPath = new JCheckBox(jEdit
				.getProperty(QuickNotepadPlugin.OPTION_PREFIX
						+ "show-filepath.title"), jEdit.getProperty(
				QuickNotepadPlugin.OPTION_PREFIX + "show-filepath").equals(
				"true"));
		addComponent(showPath);

		pathName = new JTextField(jEdit
				.getProperty(QuickNotepadPlugin.OPTION_PREFIX + "filepath"));
		JButton pickPath = new JButton(jEdit
				.getProperty(QuickNotepadPlugin.OPTION_PREFIX + "choose-file"));
		pickPath.addActionListener(this);

		JPanel pathPanel = new JPanel(new BorderLayout(0, 0));
		pathPanel.add(pathName, BorderLayout.CENTER);
		pathPanel.add(pickPath, BorderLayout.EAST);

		addComponent(jEdit.getProperty(QuickNotepadPlugin.OPTION_PREFIX
				+ "file"), pathPanel);

		font = new FontSelector(makeFont());
		addComponent(jEdit.getProperty(QuickNotepadPlugin.OPTION_PREFIX
				+ "choose-font"), font);
	}

	public void _save() {
		jEdit.setProperty(QuickNotepadPlugin.OPTION_PREFIX + "filepath",
				pathName.getText());
		Font _font = font.getFont();
		jEdit.setProperty(QuickNotepadPlugin.OPTION_PREFIX + "font", _font
				.getFamily());
		jEdit.setProperty(QuickNotepadPlugin.OPTION_PREFIX + "fontsize", String
				.valueOf(_font.getSize()));
		jEdit.setProperty(QuickNotepadPlugin.OPTION_PREFIX + "fontstyle",
				String.valueOf(_font.getStyle()));
		jEdit.setProperty(QuickNotepadPlugin.OPTION_PREFIX + "show-filepath",
				String.valueOf(showPath.isSelected()));
	}

	

	
	public void actionPerformed(ActionEvent evt) {
		String[] paths = GUIUtilities.showVFSFileDialog(null, null,
				JFileChooser.OPEN_DIALOG, false);
		if (paths != null) {
			pathName.setText(paths[0]);
		}
	}

	
	static public Font makeFont() {
		int style, size;
		String family = jEdit.getProperty(QuickNotepadPlugin.OPTION_PREFIX
				+ "font");
		try {
			size = Integer
					.parseInt(jEdit
							.getProperty(QuickNotepadPlugin.OPTION_PREFIX
									+ "fontsize"));
		} catch (NumberFormatException nf) {
			size = 14;
		}
		try {
			style = Integer
					.parseInt(jEdit
							.getProperty(QuickNotepadPlugin.OPTION_PREFIX
									+ "fontstyle"));
		} catch (NumberFormatException nf) {
			style = Font.PLAIN;
		}
		return new Font(family, style, size);
	}

}
