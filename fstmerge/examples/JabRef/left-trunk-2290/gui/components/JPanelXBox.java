package net.sf.jabref.gui.components;

import java.awt.Component;
import javax.swing.BoxLayout;
import javax.swing.JPanel;


public class JPanelXBox extends JPanel {
	
	public JPanelXBox() {
		super();
		setLayout(new BoxLayout(this,BoxLayout.X_AXIS));
	}
	public JPanelXBox(Component comp) {
		this();
		add(comp);
	}
}

