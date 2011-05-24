import javax.swing.JMenu;
import javax.swing.JMenuItem;

public class Gui {

    protected JMenu createFileMenu() {
        JMenu optionsMenu = original();
        optionsMenu.add(createOpenStateMenuItem());
        optionsMenu.add(createSaveStateMenuItem());
        return optionsMenu;
    }

    protected JMenuItem createSaveStateMenuItem() {
        JMenuItem saveStateMenuItem = new JMenuItem();
        saveStateMenuItem.setText("Save State");
        saveStateMenuItem.addActionListener(listenerFactory
                                       .getSaveStateListener());
        saveStateMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
                                        Event.CTRL_MASK, true));
        return saveStateMenuItem;
    }

    protected JMenuItem createOpenStateMenuItem() {
        JMenuItem openStateMenuItem = new JMenuItem();
        openStateMenuItem.setText("Open State");
        openStateMenuItem.addActionListener(listenerFactory
                                       .getLoadStateListener());
        openStateMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
                                        Event.CTRL_MASK, true));
        return openStateMenuItem;
    }
}