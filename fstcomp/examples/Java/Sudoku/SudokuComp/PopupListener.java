import java.awt.event.MouseAdapter; 
import java.awt.event.MouseEvent; 

import javax.swing.JPopupMenu; 

/**
 *
 *
 */
public  class  PopupListener  extends MouseAdapter {
	

    private JPopupMenu menu;

	

    /**
     * @param popupMenu
     *            popupMenu
     */
    public PopupListener(JPopupMenu popupMenu) {

        this.menu = popupMenu;
    }

	

    /**
     * @param e
     *            MouseEvent
     */
    public void mousePressed(MouseEvent e) {

        menu.show(e.getComponent(), e.getX(), e.getY());
    }

	

    /**
     * @param e
     *            MouseEvent
     */
    public void mouseReleased(MouseEvent e) {

    }


}
