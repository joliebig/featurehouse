
package org.jhotdraw.util; 
import javax.swing.*; 
import java.awt.*; 
import java.awt.event.*; 
public  class  FloatingTextField {
		private JTextField fEditWidget;

		private Container fContainer;

		public FloatingTextField() {	fEditWidget = new JTextField(20);	}

		public void createOverlay(Container container) {	createOverlay(container, null);	}

		public void createOverlay(Container container, Font font) {	container.add(fEditWidget, 0);	if (font != null) {	fEditWidget.setFont(font);	}	fContainer = container;	}

		public void addActionListener(ActionListener listener) {	fEditWidget.addActionListener(listener);	}

		public void removeActionListener(ActionListener listener) {	fEditWidget.removeActionListener(listener);	}

		public void setBounds(Rectangle r, String text) {	fEditWidget.setText(text);	fEditWidget.setLocation(r.x, r.y);	fEditWidget.setSize(r.width, r.height);	fEditWidget.setVisible(true);	fEditWidget.selectAll();	fEditWidget.requestFocus();	}

		public String getText() {	return fEditWidget.getText();	}

		public Dimension getPreferredSize(int cols) {	fEditWidget.setColumns(cols);	return fEditWidget.getPreferredSize();	}

		public void endOverlay() {	fContainer.requestFocus();	if (fEditWidget != null) {	fEditWidget.setVisible(false);	fContainer.remove(fEditWidget);	Rectangle bounds = fEditWidget.getBounds();	fContainer.repaint(bounds.x, bounds.y, bounds.width, bounds.height);	}	}


}
