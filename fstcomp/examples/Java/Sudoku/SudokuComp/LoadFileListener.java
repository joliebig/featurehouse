import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.io.IOException; 

import javax.swing.JFileChooser; 
import javax.swing.JOptionPane; 

public   class  LoadFileListener  extends JFileChooser  implements ActionListener {
	
    protected BoardManager bM;

	

    public LoadFileListener(BoardManager bM) {
        this.bM = bM;
    }

	
    public void actionPerformed  (ActionEvent e) {
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
