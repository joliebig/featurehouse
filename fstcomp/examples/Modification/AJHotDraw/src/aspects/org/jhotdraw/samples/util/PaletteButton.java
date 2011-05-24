
package org.jhotdraw.util; 
import javax.swing.*; 
import java.awt.event.*; 
public abstract  class  PaletteButton 	extends JButton 	implements MouseListener, MouseMotionListener {
		protected static final int NORMAL = 1;

		protected static final int PRESSED = 2;

		protected static final int SELECTED = 3;

		private int fState;

		private int fOldState;

		private PaletteListener fListener;

		public PaletteButton(PaletteListener listener) {	fListener = listener;	fState = fOldState = NORMAL;	addMouseListener(this);	addMouseMotionListener(this);	}

		public Object value() {	return null;	}

		public String name() {	return "";	}

		public void reset() {	if (isEnabled()) {	fState = NORMAL;	setSelected(false);	repaint();	}	}

		public void select() {	if (isEnabled()) {	fState = SELECTED;	setSelected(true);	repaint();	}	}

		public void mousePressed(MouseEvent e) {	if (isEnabled()) {	fOldState = fState;	fState = PRESSED;	repaint();	}	}

		public void mouseDragged(MouseEvent e) {	if (isEnabled()) {	if (contains(e.getX(),e.getY())) {	fState = PRESSED;	}	else {	fState = fOldState;	}	repaint();	}	}

		public void mouseReleased(MouseEvent e) {	if (isEnabled()) {	fState = fOldState;	repaint();	if (contains(e.getX(),e.getY())) {	fListener.paletteUserSelected(this);	}	}	}

		public void mouseMoved(MouseEvent e) {	fListener.paletteUserOver(this, true);	}

		public void mouseExited(MouseEvent e) {	if (fState == PRESSED) {	mouseDragged(e);	}	fListener.paletteUserOver(this, false);	}

		public void mouseClicked(MouseEvent e) {}

		public void mouseEntered(MouseEvent e) {}


}
