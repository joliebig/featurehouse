import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

public class LoadFileListener extends JFileChooser implements ActionListener {
    protected BoardManager bM;

    public LoadFileListener(BoardManager bM) {
        this.bM = bM;
    }

    /**
     * @param e
     *            ActionEvent
     *
     */
    public void actionPerformed(ActionEvent e) {
        showOpenDialog(null);
        if (null != getSelectedFile()) {
            try {
                bM.loadFile(getSelectedFile());
            } catch (IOException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            }
        }
    }
}
