import java.awt.PopupMenu;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;


import java.util.LinkedList;

public class GuiFieldPopupMenuListener implements MouseListener, ActionListener {

    protected GuiField field;
    protected BoardManager boardManager;

    public GuiFieldPopupMenuListener(GuiField field, BoardManager bM) {
        this.field = field;
        this.boardManager = bM;
    }

    protected JPopupMenu createPopupMenu() {
        JPopupMenu m = new JPopupMenu();
        JMenuItem mi;

        for (int i = 0; i < Field.POSSIBILITIES; i++) {
            mi = new JMenuItem(String.valueOf(i + 1));
            mi.addActionListener(this);
            m.add(mi);
        }
        return m;
    }

    public void mouseClicked(MouseEvent arg0) {
        if (!boardManager.getField(Structure.BOX, field.getBoxIndex(),
                                   field.getFieldIndex()).isInitialSet()) {

            createPopupMenu().show(arg0.getComponent(), arg0.getX(),
                                   arg0.getY());

        }
    }

    public void mouseEntered(MouseEvent arg0) {
        // TODO Auto-generated method stub

    }

    public void mouseExited(MouseEvent arg0) {
        // TODO Auto-generated method stub

    }

    public void mousePressed(MouseEvent arg0) {
        // TODO Auto-generated method stub

    }

    public void mouseReleased(MouseEvent arg0) {
        // TODO Auto-generated method stub

    }

    public void actionPerformed(ActionEvent arg0) {
        boardManager.setField(Structure.BOX, field.getBoxIndex(), field
                              .getFieldIndex(), new Field(Integer.parseInt(arg0
                                                          .getActionCommand())));
    }

}
