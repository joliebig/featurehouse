
package org.jhotdraw.contrib; 
import java.awt.Color; 
import java.awt.Container; 
import java.awt.Cursor; 
import java.awt.Dimension; 
import java.awt.Font; 
import java.awt.Rectangle; 
import javax.swing.BorderFactory; 
import javax.swing.JEditorPane; 
import javax.swing.JScrollPane; 
public  class  FloatingTextArea {
		protected JScrollPane fEditScrollContainer;

		protected JEditorPane fEditWidget;

		protected Container fContainer;

		public FloatingTextArea() {	fEditWidget = new JEditorPane();	fEditScrollContainer = new JScrollPane(fEditWidget,	JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,	JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);	fEditScrollContainer.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));	fEditScrollContainer.setBorder(BorderFactory.createLineBorder(Color.black));	}

		public void createOverlay(Container container) {	createOverlay(container, null);	}

		public void createOverlay(Container container, Font font) {	container.add(fEditScrollContainer, 0);	if (font != null) {	fEditWidget.setFont(font);	}	fContainer = container;	}

		public void setBounds(Rectangle r, String text) {	fEditWidget.setText(text);	fEditScrollContainer.setBounds(r.x, r.y, r.width, r.height);	fEditScrollContainer.setVisible(true);	fEditWidget.setCaretPosition(0);	fEditWidget.requestFocus();	}

		public String getText() {	return fEditWidget.getText();	}

		public Dimension getPreferredSize(int cols) {	return new Dimension(fEditWidget.getWidth(), fEditWidget.getHeight());	}

		public void endOverlay() {	fContainer.requestFocus();	if (fEditScrollContainer != null) {	fEditScrollContainer.setVisible(false);	fContainer.remove(fEditScrollContainer);	Rectangle bounds = fEditScrollContainer.getBounds();	fContainer.repaint(bounds.x, bounds.y, bounds.width, bounds.height);	}	}


}
