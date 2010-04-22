import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagLayout;
import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

/**
 *
 *
 */
public class Gui extends JFrame {

    protected GuiBoard guiBoard;

    protected ListenerFactory listenerFactory;

    protected JMenuBar createMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(createFileMenu());
        menuBar.add(createOptionsMenu());
        return menuBar;
    }

    protected JPanel createContentPane() {
        JPanel pane = new JPanel();
        pane.setLayout(new BorderLayout());
        //pane.add(guiBoard);
        return pane;
    }

    protected JMenu createFileMenu() {
        JMenu fileMenu = new JMenu();
        fileMenu.setText("File");
        fileMenu.add(createLoadMenuItem());
        fileMenu.add(createExitMenuItem());
        return fileMenu;
    }

    protected JMenu createOptionsMenu() {
        JMenu optionsMenu = new JMenu();
        optionsMenu.setText("Options");
        return optionsMenu;
    }

    protected JMenuItem createExitMenuItem() {
        JMenuItem exitMenuItem = new JMenuItem();
        exitMenuItem.setText("Exit");
        exitMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
                                    Event.CTRL_MASK, true));
        exitMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                System.exit(0);
            }
        });
        return exitMenuItem;
    }

    protected JMenuItem createLoadMenuItem() {
        JMenuItem loadMenuItem = new JMenuItem();
        loadMenuItem.setText("Load");
        loadMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
                                    Event.CTRL_MASK, true));
        loadMenuItem.addActionListener(listenerFactory
                                       .getLoadFileListener());
        return loadMenuItem;
    }
    /**
     * This is the default constructor
     */
    public Gui(ListenerFactory listenerFactory) {
        super();
        //guiBoard = new GuiBoard(listenerFactory);
        this.listenerFactory = listenerFactory;
        setContentPane(createContentPane());
        setJMenuBar(createMenuBar());
    }

    /**
    * Create the GUI and show it.  For thread safety,
    * this method should be invoked from the
    * event-dispatching thread.
    */
    protected void createAndShowGUI() {
        setTitle("Sudoku");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        pack();
        setResizable(false);
        setVisible(true);
    }

    protected void update(Board b) {
        getGlassPane().setVisible(true);
        if (null != b) {
            if (null == guiBoard) {
                guiBoard = new GuiBoard(listenerFactory);
                getContentPane().add(guiBoard);
            }
            guiBoard.update(b);
        } else {
            if (null != guiBoard) {
                getContentPane().remove(guiBoard);
                guiBoard = null;
            }

        }
        pack();
        getGlassPane().setVisible(false);
    }
}
