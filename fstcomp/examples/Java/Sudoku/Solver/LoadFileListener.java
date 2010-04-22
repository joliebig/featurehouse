

public class LoadFileListener {
    public void actionPerformed(ActionEvent e) {
        showOpenDialog(null);
        if (null != getSelectedFile()) {
            try {
                if (!bM.tryLoadFile(getSelectedFile()))
                    JOptionPane.showMessageDialog(null, "Invalid sudoku! File was not loaded.");
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(null, "Invalid sudoku! File was not loaded.");
            }
        }
    }
}