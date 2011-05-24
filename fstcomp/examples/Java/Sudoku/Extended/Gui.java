

import javax.swing.JMenu;
import javax.swing.JMenuItem;

public class Gui {
    protected JMenu createOptionsMenu() {
        JMenu optionsMenu = original();
        optionsMenu.add(createSetPossibilitiesMenuItem());
        return optionsMenu;
    }

    protected JMenuItem createSetPossibilitiesMenuItem() {
        JMenuItem setPossibilitiesMenuItem = new JMenuItem();
        setPossibilitiesMenuItem.setText("Set Possibilities");
        setPossibilitiesMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
                                                Event.CTRL_MASK, true));
        setPossibilitiesMenuItem.addActionListener(listenerFactory
                .getSetPossibilitiesListener());
        return setPossibilitiesMenuItem;
    }
}