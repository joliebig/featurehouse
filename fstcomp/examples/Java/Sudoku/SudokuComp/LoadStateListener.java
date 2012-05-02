import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.io.IOException; 

import javax.swing.JFileChooser; 
import javax.swing.JOptionPane; 

public  class  LoadStateListener  extends JFileChooser  implements ActionListener {
	
    private BoardManager bM;

	

    public LoadStateListener(BoardManager bM) {
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
                bM.loadState(getSelectedFile());
            } catch (IOException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            } catch (ClassNotFoundException ex) {
                // TODO Auto-generated catch block
                ex.printStackTrace();
            }
        }
    }


}
