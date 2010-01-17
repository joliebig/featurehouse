package net.sf.jabref.gui.components;

import java.awt.Component;
import javax.swing.JPanel;

public class JPanelXBoxLeftJustify extends JPanelXBox {
	public JPanelXBoxLeftJustify() {
		super();
		add(new JPanel()); 
	}
	public JPanelXBoxLeftJustify(Component c) {
		this();
		add(c);
	}
	public Component add(Component c) {
		return super.add(c,Math.max(0,getComponentCount()-1));
	}
}

