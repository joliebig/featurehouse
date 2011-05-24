

import javax.swing.JMenu;
import javax.swing.JMenuItem;

public class Gui {
    protected JMenu createOptionsMenu() {
        JMenu optionsMenu = original();
        optionsMenu.add(createUndoMenuItem());
        return optionsMenu;
    }

    protected JMenuItem createUndoMenuItem() {
        JMenuItem loadMenuItem = new JMenuItem();
        loadMenuItem.setText("Undo");
        loadMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_U,
                                    Event.CTRL_MASK, true));
        loadMenuItem.addActionListener(listenerFactory
                                       .getUndoListener());
        return loadMenuItem;
    }
}