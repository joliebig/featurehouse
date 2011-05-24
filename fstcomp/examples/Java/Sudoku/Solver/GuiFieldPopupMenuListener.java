import javax.swing.JOptionPane;

public class GuiFieldPopupMenuListener {

    protected JPopupMenu createPopupMenu() {
        JPopupMenu m = new JPopupMenu();
        JMenuItem mi;

        for (int i = 0; i < boardManager.getField(Structure.BOX, field.getBoxIndex(), field
                .getFieldIndex()).getRemainingPos().size(); i++) {
            mi = new JMenuItem(String.valueOf(boardManager.getField(Structure.BOX, field.getBoxIndex(), field
                                              .getFieldIndex()).getRemainingPos().get(i)));
            mi.addActionListener(this);
            m.add(mi);
        }
        return m;
    }

    public void actionPerformed(ActionEvent arg0) {
        if (!boardManager.trySetField(Structure.BOX, field.getBoxIndex(), field
                                      .getFieldIndex(), new Field(Integer.parseInt(arg0
                                                                  .getActionCommand()))))
            JOptionPane.showMessageDialog(null, "Invalid state reached! Last action was undone.");
    }


}