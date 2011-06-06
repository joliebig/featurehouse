

import javax.swing.JTextArea;

public class QuickNotepadTextArea extends JTextArea {
	public QuickNotepadTextArea() {
		super();
		setLineWrap(true);
		setWrapStyleWord(true);
		setTabSize(4);
	}
}
