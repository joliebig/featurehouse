

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

public class Gui {

    protected JMenu createFileMenu() {
        JMenu fileMenu = original();
        fileMenu.add(createGenerateSudokuMenuItem());
        return fileMenu;
    }

    protected JMenuItem createGenerateSudokuMenuItem() {
        JMenuItem generateSudokuMenuItem = new JMenuItem();
        generateSudokuMenuItem.setText("Generate Sudoku");
        generateSudokuMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
                                        Event.CTRL_MASK, true));
        generateSudokuMenuItem.addActionListener(listenerFactory
                                           .getGenerateSudokuListener());
        return generateSudokuMenuItem;
    }
}