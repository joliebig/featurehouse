
import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.io.IOException; 

import javax.swing.JFileChooser; 

public  class  SaveStateListener  extends JFileChooser  implements ActionListener {
	
    private BoardManager bm;

	

    public SaveStateListener(BoardManager bm) {
        this.bm = bm;
    }

	

    /**
     * @param e
     *            ActionEvent
     *
     */
    public void actionPerformed(ActionEvent e) {
        showSaveDialog(null);
        if (null != getSelectedFile()) {
            try {
                bm.saveState(getSelectedFile());
            } catch (IOException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            }
        }
    }


}
