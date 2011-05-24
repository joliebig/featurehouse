

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

public class Gui {

    private static final int GLASS_PANE_ALPHA = 180;

    public void setBusy(boolean busy) {
        if (busy) {
            getGlassPane().setVisible(true);
        } else {
            getGlassPane().setVisible(false);
        }
    }

    Gui(ListenerFactory listenerFactory) {
        JPanel glassPane = new JPanel(new BorderLayout());
        glassPane.setBackground(
            new Color(Color.BLACK.getRed(), Color.BLACK.getGreen(),
                      Color.BLACK.getBlue(), GLASS_PANE_ALPHA));
        glassPane.addMouseListener(new MouseListener() {

            public void mousePressed(MouseEvent e) {
            }

            public void mouseReleased(MouseEvent e) {
            }

            public void mouseEntered(MouseEvent e) {
            }

            public void mouseExited(MouseEvent e) {
            }

            public void mouseClicked(MouseEvent e) {
            }
        });

        glassPane.addKeyListener(new KeyListener() {

            public void keyTyped(KeyEvent e) {
            }

            public void keyPressed(KeyEvent e) {
            }

            public void keyReleased(KeyEvent e) {
            }
        });

        JLabel label = new JLabel("Busy...", JLabel.CENTER);
        label.setForeground(Color.white);
        glassPane.add(label, BorderLayout.CENTER);
        setGlassPane(glassPane);
    }

    protected JMenu createOptionsMenu() {
        JMenu optionsMenu = original();
        optionsMenu.add(createSolutionHintMenuItem());
        return optionsMenu;
    }

    protected JMenuItem createSolutionHintMenuItem() {
        JMenuItem solutionMenuItem = new JMenuItem();
        solutionMenuItem.setText("Solution Hint");
        solutionMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
                                        Event.CTRL_MASK, true));
        solutionMenuItem.addActionListener(listenerFactory
                                           .getSolutionHintListener());
        return solutionMenuItem;
    }
}